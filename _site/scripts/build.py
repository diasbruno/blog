"""Render the site in a staging directory before updating public files."""

import argparse
from contextlib import closing
from datetime import datetime
import fcntl
import hashlib
from html import escape
from html.parser import HTMLParser
import json
import os
import re
import sqlite3
import subprocess
from pathlib import Path, PurePosixPath
from string import Template
from tempfile import TemporaryDirectory
from urllib.parse import urljoin, urlsplit
import xml.etree.ElementTree as ET

SOURCE = Path(__file__).resolve().parents[1]
ROOT = Path(os.environ.get("SITE_DESTINATION", SOURCE.parent.parent / "diasbruno.github.io")).expanduser().resolve()
MANIFEST = ROOT / ".site-generated.json"
ASSET_ROOT = "assets/site"
SITE_URL = "https://diasbruno.github.io"
POST_DATABASE = SOURCE / "content" / "posts.sqlite3"
POST_INDEXES = ("atom.xml", "posts-1.json", "posts-2.json", "posts-3.json", "posts-4.json")
ATOM_NS = "http://www.w3.org/2005/Atom"
PREVIOUS_POST_INDEXES = {
    "atom.xml": (
        "eef593489eb8de32d5089ec4380fbdf43eecef45c90194d8dcf00c6497e17fc3",
        "f6f194e3d3bf375df45387d1e180b97f6ee12a4e04cb7604a1f30849599a6d7a",
    ),
    "posts-1.json": ("06e5ec15b7bc03beb20640d61572e6d848a2524b18458cb16881b84b1439ef39",),
    "posts-2.json": ("1d9cfe061ca52938abffb86866b1073d697db67cd29dbf6d34dd71ffd1969393",),
    "posts-3.json": ("c1cf95f081e986ffa9bf6ba391dcae18d87c90d0076ce9643301777e52eceb1d",),
    "posts-4.json": ("87357746c1544afdddd7ad60b189b30e3076b6f838e1e1c7162f84547cee04b5",),
}
FOOTER_STYLE = f'<link rel="stylesheet" href="/{ASSET_ROOT}/footer.css">'
ARCHIVE_FOOTER = re.compile(r"\n<!-- site-footer:(?:style|markup) -->\n.*?\n<!-- /site-footer -->", re.DOTALL)
ARTICLE_REDIRECT = re.compile(
    r"(?:(?:\d{4}-\d{2}-\d{2}-[a-z0-9-]+|articles/(?:\d{4}/\d{2}/\d{2}/)?[a-z0-9-]+)\.html"
    r"|articles/(?:\d{4}/\d{2}/\d{2}/[a-z0-9-]+/)?index\.html)"
)
SECTIONS = {
    "experiments": ("Experiments", "dark"),
    "posts": ("Posts", "light"),
    "projects": ("Projects", "light"),
    "about": ("About", "dark"),
}


class Page(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.links = []
        self.ids = set()
        self.title = []
        self.main_count = 0
        self.in_title = False
        self.feed(text)
        self.close()

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        self.links.extend(attrs[key] for key in ("href", "src") if attrs.get(key))
        if attrs.get("id"):
            self.ids.add(attrs["id"])
        if tag == "main":
            self.main_count += 1
        if tag == "title":
            self.in_title = True

    def handle_endtag(self, tag):
        if tag == "title":
            self.in_title = False

    def handle_data(self, data):
        if self.in_title:
            self.title.append(data)


def legacy_paths():
    return (SOURCE / "legacy-paths.txt").read_text(encoding="utf-8").splitlines()


def load_posts():
    posts = {}
    with closing(sqlite3.connect(POST_DATABASE.as_uri() + "?mode=ro", uri=True)) as db:
        db.row_factory = sqlite3.Row
        records = db.execute("SELECT *, post_date AS published FROM posts ORDER BY slug").fetchall()
    legacy_paths_seen = set()
    for row in records:
        record = dict(row)
        slug = record["slug"]
        if not re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", slug) or slug in posts:
            raise ValueError(f"Invalid or duplicate post slug: {slug}")
        published = datetime.fromisoformat(record["published"].replace("Z", "+00:00"))
        if not record["title"].strip() or published.tzinfo is None:
            raise ValueError(f"Invalid post metadata: {slug}")
        if record["body_format"] not in {"markdown", "html"} or record["language"] not in {"en", "pt-BR"}:
            raise ValueError(f"Invalid post format or language: {slug}")
        if record["status"] not in {"publish", "draft", "hidden"}:
            raise ValueError(f"Invalid writing status: {slug}")
        if record.get("source_url"):
            source = urlsplit(record["source_url"])
            if source.scheme != "https" or not source.netloc:
                raise ValueError(f"Invalid original source URL: {slug}")
        legacy = record.get("legacy_path")
        if legacy:
            expected = f"articles/{published:%Y/%m/%d}/{slug}/"
            checksum = record.get("legacy_checksum")
            if legacy != expected or legacy in legacy_paths_seen or (checksum and not re.fullmatch(r"[a-f0-9]{64}", checksum)):
                raise ValueError(f"Invalid legacy post path or checksum: {slug}")
            legacy_paths_seen.add(legacy)
            record["legacy_path"] = legacy + "index.html"
        related = json.loads(record["related_slugs"])
        if not isinstance(related, list) or any(not isinstance(item, str) for item in related):
            raise ValueError(f"Invalid related posts for {slug}")
        posts[slug] = dict(record, date=published.date(), timestamp=published, related=related)
    for slug, post in posts.items():
        if any(related not in posts or related == slug for related in post.get("related", [])):
            raise ValueError(f"Invalid related post for {slug}")
    return posts


def post_list(posts):
    rows = []
    for post in posts:
        published = post["date"]
        rows.append(
            f'<li><time datetime="{published}">{published}</time>'
            f'<a href="/posts/{post["slug"]}/" data-site-navigation>{escape(post["title"])}</a></li>'
        )
    return '<ul class="post-list">\n' + "\n".join(rows) + "\n</ul>"


def post_indexes(posts):
    ET.register_namespace("", ATOM_NS)
    feed = ET.Element(f"{{{ATOM_NS}}}feed")
    ET.SubElement(feed, f"{{{ATOM_NS}}}title").text = "diasbruno"
    ET.SubElement(feed, f"{{{ATOM_NS}}}id").text = f"{SITE_URL}/atom.xml"
    ET.SubElement(feed, f"{{{ATOM_NS}}}updated").text = (
        posts[0]["published"] if posts else "1970-01-01T00:00:00Z"
    )
    ET.SubElement(feed, f"{{{ATOM_NS}}}link", rel="self", href=f"{SITE_URL}/atom.xml")
    ET.SubElement(feed, f"{{{ATOM_NS}}}link", rel="alternate", href=f"{SITE_URL}/posts/")
    indexes = {page: [] for page in range(1, 5)}
    for post in posts:
        route = f'/posts/{post["slug"]}/'
        address = SITE_URL + route
        entry = ET.SubElement(feed, f"{{{ATOM_NS}}}entry")
        ET.SubElement(entry, f"{{{ATOM_NS}}}id").text = post.get("feed_id") or address
        ET.SubElement(entry, f"{{{ATOM_NS}}}title", type="text").text = post["title"]
        ET.SubElement(entry, f"{{{ATOM_NS}}}updated").text = post["published"]
        author = ET.SubElement(entry, f"{{{ATOM_NS}}}author")
        ET.SubElement(author, f"{{{ATOM_NS}}}name").text = "Bruno Dias"
        ET.SubElement(entry, f"{{{ATOM_NS}}}link", rel="alternate", href=address)
        if post.get("source_url"):
            ET.SubElement(entry, f"{{{ATOM_NS}}}link", rel="via", href=post["source_url"])
        if post.get("description"):
            ET.SubElement(entry, f"{{{ATOM_NS}}}summary", type="text").text = post["description"]
        details = {"title": post["title"], "date": post["published"], "status": "publish"}
        indexes[1].append(dict(details, link=route))
        page = post.get("legacy_index_page")
        if page in (2, 3, 4):
            indexes[page].append(dict(details, slug=post["slug"]))
    ET.indent(feed, space="  ")
    outputs = {"atom.xml": ET.tostring(feed, encoding="unicode", xml_declaration=True) + "\n"}
    for page, entries in indexes.items():
        outputs[f"posts-{page}.json"] = json.dumps(
            {"page": page, "number_of_pages": 1 if page == 1 else 4, "content": entries},
            ensure_ascii=False, indent=2,
        ) + "\n"
    return outputs


def navigation(current=None, *, current_kind="page"):
    links = []
    for name, (label, _) in SECTIONS.items():
        selected = f' aria-current="{current_kind}"' if name == current else ""
        links.append(f'<li><a class="nav-{name}" href="/{name}/" data-site-navigation{selected}>{label}</a></li>')
    return "<ul>\n" + "\n".join(links) + "\n</ul>"


def render(name, **values):
    return Template((SOURCE / "templates" / f"{name}.html").read_text(encoding="utf-8")).substitute(values)


def post_body(post):
    body = post["body"]
    if post["body_format"] == "markdown":
        # Trusted local Markdown includes the original posts' iframe markup.
        try:
            body = subprocess.run(
                ["cmark-gfm", "--unsafe"], input=body, check=True, capture_output=True, encoding="utf-8",
            ).stdout
        except FileNotFoundError as error:
            raise ValueError("Install cmark-gfm to render the site’s Markdown posts.") from error
        except subprocess.CalledProcessError as error:
            raise ValueError(f'Could not render {post["slug"]}: {error.stderr.strip()}') from error
    return re.sub(r"<(/?)h1(?=[\s>])", r"<\1h2", body)


def render_post(post, catalog):
    english = post["language"] == "en"
    links = [("All posts" if english else "Todos os artigos", "/posts/")]
    links.extend((catalog[slug]["title"], f"/posts/{slug}/") for slug in post["related"] if slug in catalog)
    if post.get("source_url"):
        links.append(("Original on Substack" if english else "Original no Substack", post["source_url"]))
    rows = []
    for title, href in links:
        marker = " data-site-navigation" if href.startswith("/posts/") else ""
        rows.append(f'<li><a href="{escape(href)}"{marker}>{escape(title)}</a></li>')
    note = post.get("archive_note")
    subtitle = post.get("description")
    return render(
        "post",
        category="Post" if english else "Artigo",
        heading=escape(post["title"]),
        subtitle=f'<p class="post-subtitle">{escape(subtitle)}</p>' if subtitle else "",
        date=post["date"],
        timestamp=post["published"],
        body=post_body(post),
        note=f'<p class="archive-note">{escape(note)}</p>' if note else "",
        article_label="Article" if english else "Artigo",
        links="\n".join(rows),
    )


def post_metadata(post, url):
    properties = {
        "og:type": "article",
        "og:title": post.get("social_title") or post["title"],
        "og:url": url,
        "article:published_time": post["published"],
    }
    if post.get("social_image"):
        properties["og:image"] = urljoin(SITE_URL, post["social_image"])
    return "\n".join([
        f'  <link rel="canonical" href="{url}" data-post-metadata>',
        '  <meta name="author" content="Bruno Dias" data-post-metadata>',
        *(f'  <meta property="{key}" content="{escape(value)}" data-post-metadata>' for key, value in properties.items()),
    ])


def content_path(page):
    return page.removesuffix("index.html") + "content.json"


def companion(page, html):
    title = "".join(Page(html).title).strip()
    return json.dumps(
        {"path": "/" + page.removesuffix("index.html"), "title": title, "html": html},
        ensure_ascii=False,
    ) + "\n"


def archive_pages(legacy, *, footer=None):
    pages = {}
    for name in legacy:
        if not name.endswith("/index.html"):
            continue
        path = ROOT / name
        if path.resolve() != path:
            raise ValueError(f"Archive path crosses a symlink: {name}")
        html = ARCHIVE_FOOTER.sub("", path.read_bytes().decode("utf-8"))
        if footer is not None:
            for kind, content, tag in (
                ("style", FOOTER_STYLE, "</head>"),
                ("markup", footer, "</body>"),
            ):
                before, end, after = html.rpartition(tag)
                if not end:
                    raise ValueError(f"{name}: missing {tag}")
                html = f"{before}\n<!-- site-footer:{kind} -->\n{content.strip()}\n<!-- /site-footer -->{end}{after}"
        pages[name] = html
    return pages


def generate(catalog=None):
    if catalog is None:
        catalog = load_posts()
    public = {slug: post for slug, post in catalog.items() if post["status"] == "publish"}
    ordered = sorted(public.values(), key=lambda post: (post["timestamp"], post["title"]), reverse=True)
    private_slugs = catalog.keys() - public.keys()
    private_pages = {
        path
        for slug in private_slugs
        for path in (
            f"posts/{slug}/index.html",
            catalog[slug]["legacy_path"] or f'articles/{catalog[slug]["date"]:%Y/%m/%d}/{slug}/index.html',
        )
    }
    unavailable = private_pages | {content_path(name) for name in private_pages}
    legacy = legacy_paths()
    for name in legacy:
        if not (ROOT / name).is_file():
            raise ValueError(f"Missing published file: {name}")

    signature = (SOURCE / "static" / "signature.svg").read_text(encoding="utf-8").strip()
    signature = signature.replace("<svg ", '<svg aria-hidden="true" width="70" height="98" ', 1)
    outputs = {
        "index.html": render(
            "base",
            title="Bruno Dias",
            description="Experiments, writing, and open-source projects by Bruno Dias.",
            theme="dark",
            language="en",
            metadata="",
            header="",
            content=render("landing", navigation=navigation(), signature=signature),
            json_url="/content.json",
            intro_script=render("landing-start"),
            footer="",
        ),
    }
    section_content = json.loads((SOURCE / "content" / "sections.json").read_text(encoding="utf-8"))
    if section_content.keys() != SECTIONS.keys():
        raise ValueError("Section content must match the site navigation")
    footer = render("footer", modifier="")
    for name, (heading, theme) in SECTIONS.items():
        entry = section_content[name]
        if not entry["intro"].strip():
            raise ValueError(f"Missing {name} introduction")
        intro = escape(entry["intro"])
        links = entry.get("links", [])
        rows = []
        for label, href in links:
            url = urlsplit(href)
            external = url.scheme == "https" and url.netloc
            local = href.startswith("/") and not href.startswith("//")
            if not label.strip() or not (external or local):
                raise ValueError(f"Invalid {name} link: {href}")
            rows.append(f'<li><a href="{escape(href)}">{escape(label)}</a></li>')

        body = ""
        if name == "posts":
            body = post_list(ordered)
        elif rows:
            body = '<ul class="section-links">\n' + "\n".join(rows) + "\n</ul>"
            if name == "about":
                body = render("about-links", links=body)
        page = f"{name}/index.html"
        outputs[page] = render(
            "base",
            title=f"{heading} · Bruno Dias",
            description=intro,
            theme=theme,
            language="en",
            metadata="",
            header=render("header", navigation=navigation(name)),
            content=render("section", heading=heading, intro=intro, body=body),
            json_url="/" + content_path(page),
            intro_script="",
            footer=footer,
        )

    post_navigation = render("header", navigation=navigation("posts", current_kind="location"))
    for slug, post in public.items():
        canonical = f"posts/{slug}/index.html"
        content = render_post(post, public)
        metadata = post_metadata(post, f"{SITE_URL}/posts/{slug}/")
        for page in filter(None, (canonical, post.get("legacy_path"))):
            outputs[page] = render(
                "base",
                title=f'{escape(post["title"])} · Bruno Dias',
                description=escape(post.get("description") or post["title"]),
                theme="light",
                language=post["language"],
                metadata=metadata,
                header=post_navigation,
                content=content,
                json_url="/" + content_path(page),
                intro_script="",
                footer=footer,
            )

    for page, html in tuple(outputs.items()):
        outputs[content_path(page)] = companion(page, html)
    archived = archive_pages(legacy, footer=render("footer", modifier=" site-footer--archive"))
    for page, html in archived.items():
        outputs[content_path(page)] = companion(page, html)

    redirects = json.loads((SOURCE / "content" / "post-redirects.json").read_text(encoding="utf-8"))
    for name, destination in redirects.items():
        if not ARTICLE_REDIRECT.fullmatch(name) or name in legacy or name in outputs:
            raise ValueError(f"Invalid or conflicting article redirect: {name}")
        if not destination.startswith("/") or not destination.endswith("/"):
            raise ValueError(f"Invalid article redirect destination: {destination}")
        target = destination.removeprefix("/") + "index.html"
        if target in private_pages:
            unavailable.add(name)
            continue
        if target not in outputs and target not in archived:
            raise ValueError(f"Unknown article redirect destination: {destination}")
        outputs[name] = render("redirect", destination=escape(destination))

    outputs.update(post_indexes(ordered))
    files = {name: content.encode("utf-8") for name, content in outputs.items()}
    static = SOURCE / "static"
    for path in sorted(static.rglob("*")):
        relative = path.relative_to(static)
        if not path.is_file() or any(part.startswith(".") for part in relative.parts):
            continue
        public_path = f"{ASSET_ROOT}/{relative.as_posix()}"
        if len(relative.parts) > 2 and relative.parts[0] == "substack" and relative.parts[1] in private_slugs:
            unavailable.add(public_path)
        else:
            files[public_path] = path.read_bytes()
    validate(files, legacy)
    return files, {page: html.encode("utf-8") for page, html in archived.items()}, unavailable


def validate(files, legacy):
    pages = {
        name: Page(content.decode("utf-8"))
        for name, content in files.items()
        if name.endswith(".html")
    }
    for name, page in pages.items():
        if page.main_count != 1 or not "".join(page.title).strip():
            raise ValueError(f"{name}: expected one main element and a page title")
        base = "/" + name.removesuffix("index.html")
        for link in page.links:
            url = urlsplit(urljoin(base, link))
            if url.scheme or url.netloc:
                continue
            target = url.path.lstrip("/")
            if url.path.endswith("/"):
                target += "index.html"
            if target not in files and target not in legacy:
                raise ValueError(f"{name}: missing local resource {link}")
            if url.fragment and target in pages and url.fragment not in pages[target].ids:
                raise ValueError(f"{name}: missing anchor {link}")


def digest(content):
    return hashlib.sha256(content).hexdigest()


def owned_path(name):
    path = PurePosixPath(name)
    if str(path) != name or ".." in path.parts:
        raise ValueError(f"Invalid generated path: {name}")
    pages = {"index.html"} | {f"{section}/index.html" for section in SECTIONS}
    companions = {content_path(page) for page in pages} | {"opensource/content.json"}
    article = re.fullmatch(r"articles/\d{4}/\d{2}/\d{2}/[a-z0-9-]+/content\.json", name)
    post = re.fullmatch(r"posts/[a-z0-9-]+/(?:index\.html|content\.json)", name)
    redirect = ARTICLE_REDIRECT.fullmatch(name) and name not in legacy_paths()
    asset = PurePosixPath(ASSET_ROOT) in path.parents
    if name not in pages | companions and name not in POST_INDEXES and not (article or post or redirect or asset):
        raise ValueError(f"Generated path outside site outputs: {name}")
    target = ROOT / path
    if target.resolve() != target:
        raise ValueError(f"Generated path crosses a symlink: {name}")
    return target


def recorded_outputs():
    if MANIFEST.exists():
        return json.loads(MANIFEST.read_text(encoding="utf-8"))
    return {}


def check_replacement(name, current, expected, *, legacy_checksum=None, previous_indexes=()):
    path = owned_path(name)
    if not path.exists():
        return
    data = path.read_bytes()
    if digest(data) in (current, expected):
        return
    if current is None and digest(data) in previous_indexes:
        return
    if legacy_checksum and current is None:
        original = ARCHIVE_FOOTER.sub("", data.decode("utf-8")).encode("utf-8")
        if digest(original) == legacy_checksum:
            return
    raise ValueError(f"{name} contains changes outside the build. Update its source or move the file first.")


def remove_output(name):
    target = owned_path(name)
    if target.exists():
        target.unlink()
        for directory in target.parents:
            if directory == ROOT or any(directory.iterdir()):
                break
            directory.rmdir()


def publish():
    catalog = load_posts()
    files, archived, unavailable = generate(catalog)
    migratable = {post["legacy_path"]: post["legacy_checksum"] for post in catalog.values() if post.get("legacy_path")}
    current = recorded_outputs()
    expected = {name: digest(content) for name, content in files.items()}
    # The first build replaces the previous homepage, whose source remains in Git history.
    previous_homepage = "8e60422f086a8a0c2043c26d1ede18f56e09ad1798de9bccb8d8fb1440514dfd"
    for name in current.keys() | expected.keys() | unavailable:
        previous = current.get(name)
        if name == "index.html" and not current:
            previous = previous_homepage
        check_replacement(
            name, previous, expected.get(name),
            legacy_checksum=migratable.get(name), previous_indexes=PREVIOUS_POST_INDEXES.get(name, ()),
        )

    with TemporaryDirectory(prefix=".site-build-", dir=ROOT) as directory:
        stage = Path(directory)
        for name, content in (files | archived).items():
            staged = stage / name
            staged.parent.mkdir(parents=True, exist_ok=True)
            staged.write_bytes(content)
        manifest = stage / "generated.json"
        manifest.write_text(json.dumps(expected, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        for name in sorted(files, key=lambda name: not name.startswith(f"{ASSET_ROOT}/")):
            destination = owned_path(name)
            destination.parent.mkdir(parents=True, exist_ok=True)
            if not destination.exists() or digest(destination.read_bytes()) != expected[name]:
                (stage / name).replace(destination)
        for name, html in archived.items():
            if (ROOT / name).read_bytes() != html:
                (stage / name).replace(ROOT / name)
        for name in (current.keys() | unavailable) - expected.keys():
            remove_output(name)
        manifest.replace(MANIFEST)
    print(f"Built {len(files)} files and updated {len(archived)} archive pages at {ROOT}")


def clean():
    current = recorded_outputs()
    for name, checksum in current.items():
        check_replacement(name, checksum, None)
    archived = archive_pages(name for name in legacy_paths() if (ROOT / name).is_file())
    with TemporaryDirectory(prefix=".site-clean-", dir=ROOT) as directory:
        stage = Path(directory) / "archive.html"
        for name, html in archived.items():
            path = ROOT / name
            original = html.encode("utf-8")
            if path.read_bytes() != original:
                stage.write_bytes(original)
                stage.replace(path)
    for name in current:
        remove_output(name)
    MANIFEST.unlink(missing_ok=True)
    print(f"Removed {len(current)} generated files")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--clean", action="store_true")
    args = parser.parse_args()
    if ROOT == SOURCE.parent:
        raise ValueError("Use the publishing checkout as the build destination.")
    if not ROOT.is_dir():
        raise ValueError(f"Build destination does not exist: {ROOT}")
    with (SOURCE.parent / "Makefile").open() as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if args.clean:
            clean()
        else:
            publish()


if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValueError, sqlite3.Error, ET.ParseError) as error:
        raise SystemExit(str(error))
