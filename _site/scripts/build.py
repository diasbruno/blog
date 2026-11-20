"""Render the site in a staging directory before updating public files."""

import argparse
from datetime import date
import fcntl
import hashlib
from html import escape, unescape
from html.parser import HTMLParser
import json
import os
import re
from pathlib import Path, PurePosixPath
from string import Template
from tempfile import TemporaryDirectory
from urllib.parse import urljoin, urlsplit
import xml.etree.ElementTree as ET

SOURCE = Path(__file__).resolve().parents[1]
ROOT = Path(os.environ.get("SITE_DESTINATION", SOURCE.parent.parent / "diasbruno.github.io")).expanduser().resolve()
MANIFEST = ROOT / ".site-generated.json"
ASSET_ROOT = "assets/site"
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


def post_list(legacy):
    posts = []
    for name in legacy:
        if not name.startswith("articles/"):
            continue
        path = PurePosixPath(name)
        _, year, month, day, _ = path.parent.parts
        published = date.fromisoformat("-".join((year, month, day)))
        page = Page((ROOT / name).read_text(encoding="utf-8"))
        title = unescape("".join(page.title)).removeprefix("diasbruno - ").strip()
        if not title:
            raise ValueError(f"Missing title in {name}")
        posts.append((published, title, f"/{path.parent}/"))

    index = json.loads((ROOT / "posts-1.json").read_text(encoding="utf-8"))
    for post in index["content"]:
        url = urlsplit(post["link"])
        if url.netloc and post["status"] == "publish":
            if url.scheme != "https" or not post["title"].strip():
                raise ValueError(f"Invalid external post: {post}")
            posts.append((date.fromisoformat(post["date"][:10]), post["title"], post["link"]))

    rows = []
    for published, title, link in sorted(posts, reverse=True):
        rows.append(
            f'<li><time datetime="{published}">{published}</time>'
            f'<a href="{escape(link)}">{escape(title)}</a></li>'
        )
    return '<ul class="post-list">\n' + "\n".join(rows) + "\n</ul>"


def navigation(current=None):
    links = []
    for name, (label, _) in SECTIONS.items():
        selected = ' aria-current="page"' if name == current else ""
        links.append(f'<li><a class="nav-{name}" href="/{name}/" data-site-navigation{selected}>{label}</a></li>')
    return "<ul>\n" + "\n".join(links) + "\n</ul>"


def render(name, **values):
    return Template((SOURCE / "templates" / f"{name}.html").read_text(encoding="utf-8")).substitute(values)


def content_path(page):
    return page.removesuffix("index.html") + "content.json"


def companion(page, html):
    title = "".join(Page(html).title).strip()
    return json.dumps(
        {"path": "/" + page.removesuffix("index.html"), "title": title, "html": html},
        ensure_ascii=False,
    ) + "\n"


def generate():
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
            header="",
            content=render("landing", navigation=navigation(), signature=signature),
            json_url="/content.json",
            intro_script=render("landing-start"),
        ),
    }
    section_content = json.loads((SOURCE / "content" / "sections.json").read_text(encoding="utf-8"))
    if section_content.keys() != SECTIONS.keys():
        raise ValueError("Section content must match the site navigation")
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
            body = post_list(legacy)
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
            header=render("header", navigation=navigation(name)),
            content=render("section", heading=heading, intro=intro, body=body),
            json_url="/" + content_path(page),
            intro_script="",
        )

    for page, html in tuple(outputs.items()):
        outputs[content_path(page)] = companion(page, html)
    for page in legacy:
        if page.endswith("/index.html"):
            outputs[content_path(page)] = companion(page, (ROOT / page).read_text(encoding="utf-8"))

    files = {name: content.encode("utf-8") for name, content in outputs.items()}
    static = SOURCE / "static"
    for path in sorted(static.iterdir()):
        if path.is_file() and not path.name.startswith("."):
            files[f"{ASSET_ROOT}/{path.name}"] = path.read_bytes()
    validate(files, legacy)
    return files


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
    ET.parse(ROOT / "atom.xml")
    for name in legacy:
        if name.endswith(".json"):
            json.loads((ROOT / name).read_text(encoding="utf-8"))


def digest(content):
    return hashlib.sha256(content).hexdigest()


def owned_path(name):
    path = PurePosixPath(name)
    if str(path) != name or ".." in path.parts:
        raise ValueError(f"Invalid generated path: {name}")
    pages = {"index.html"} | {f"{section}/index.html" for section in SECTIONS}
    companions = {content_path(page) for page in pages} | {"opensource/content.json"}
    article = re.fullmatch(r"articles/\d{4}/\d{2}/\d{2}/[a-z0-9-]+/content\.json", name)
    if name not in pages | companions and not article and path.parent != PurePosixPath(ASSET_ROOT):
        raise ValueError(f"Generated path outside site outputs: {name}")
    target = ROOT / path
    if target.resolve() != target:
        raise ValueError(f"Generated path crosses a symlink: {name}")
    return target


def recorded_outputs():
    if MANIFEST.exists():
        return json.loads(MANIFEST.read_text(encoding="utf-8"))
    return {}


def check_replacement(name, current, expected):
    path = owned_path(name)
    if path.exists() and digest(path.read_bytes()) not in (current, expected):
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
    files = generate()
    current = recorded_outputs()
    expected = {name: digest(content) for name, content in files.items()}
    # The first build replaces the previous homepage, whose source remains in Git history.
    previous_homepage = "8e60422f086a8a0c2043c26d1ede18f56e09ad1798de9bccb8d8fb1440514dfd"
    for name in current.keys() | expected.keys():
        previous = current.get(name)
        if name == "index.html" and not current:
            previous = previous_homepage
        check_replacement(name, previous, expected.get(name))

    with TemporaryDirectory(prefix=".site-build-", dir=ROOT) as directory:
        stage = Path(directory)
        for name, content in files.items():
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
        for name in current.keys() - expected.keys():
            remove_output(name)
        manifest.replace(MANIFEST)
    print(f"Built {len(files)} files at {ROOT}")


def clean():
    current = recorded_outputs()
    for name, checksum in current.items():
        check_replacement(name, checksum, None)
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
    except (KeyError, OSError, ValueError, ET.ParseError) as error:
        raise SystemExit(str(error))
