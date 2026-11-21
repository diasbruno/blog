# Bruno Dias’s website

Run the build from the repository root. The Python build scripts are in
`scripts/`, with the site content in `content/`, assets in `static/`, and HTML
templates in `templates/`. The build writes HTML and public assets to the
root of the sibling `diasbruno.github.io` checkout. With the standard local
folder layout, that is `/Users/dias/Programming/diasbruno.github.io`.

The [previous Haskell version](legacy/) is archived under `legacy/`.

## Local development

Requires GNU Make, Python 3.9 or newer, `cmark-gfm` on the shell path, and the
publishing checkout with its legacy files. There are no Python packages to install.

- `make build` renders and validates pages before writing them to the publishing checkout.
- `make serve` builds and serves the publishing checkout at http://127.0.0.1:8000/.
  Use `make serve PORT=8080` to choose a different port. After an edit, run
  `make build` and refresh the browser.
- `make check` checks generated files, local links, and the preserved publishing inventory.
- `make clean` removes files listed in the publishing checkout’s
  `.site-generated.json` and removes the shared footer from the open-source archive.
  Run `make build` afterwards to restore the generated files and footer.

Set `DESTINATION` on a Make command to target a different publishing checkout,
for example `make build DESTINATION=/tmp/site-preview`.

Edit section content under `content/`, layouts under `templates/`,
and the styles and signature under `static/`. Fraunces is self-hosted under
the SIL Open Font License included with the font.

Edit the shared footer in `templates/footer.html`. The build includes it on
public section and article pages and inserts marked footer blocks into the
open-source directory.

Each page has a complete HTML document at its normal URL and a companion
`content.json` in the same directory. The JSON contains the page path, title,
and the rendered HTML. For example, `/posts/` and `/posts/content.json` come
from the same build. The new layouts use those companions for animated
navigation. Regular links load the full HTML when JavaScript is unavailable or
the JSON request fails. The original dated article URLs also serve the rebuilt
HTML and JSON. The open-source directory continues to open as a standalone page.

The landing uses the signature reveal from the original site, followed by
staggered menu links. Navigation between the new layouts fades out the current
page and fades in the next one. Reduced motion skips both effects.

When publishing from the branch, include the generated homepage, section
indexes, JSON companions, `assets/site/`, and `.site-generated.json` in the
publishing checkout.
Legacy styles and images and the open-source directory already live there.
Edit post metadata and bodies in the `posts` table in
`content/posts.sqlite3`. Set `status` to `publish`, `draft`, or `hidden`.
The build publishes only `publish` entries and uses the same selection for the
Posts index, Atom feed, legacy JSON indexes, and historical redirect pages.
Markdown bodies are rendered with `cmark-gfm`.
Imported Substack posts use their source dates and link back to the originals
from the article sidebar. Their images live in `static/substack/`, so local
builds and previews use the saved copies.
The build restores additional historical URLs through
`content/post-redirects.json`.

Edit the Projects index and organization links in `content/projects.json`.
Project pages are written in Markdown under `content/projects/`, with
repository artwork under `static/projects/`.

Edit the About bio in `content/about.md` and its introduction in
`content/sections.json`.

The Verona language section at `/vrn/` uses `content/vrn.md` for its body
and `content/sections.json` for its introduction and links.
