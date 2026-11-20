# Bruno Dias’s website

Run the build from this source checkout. Editable site files are under
`_site/`. The build writes HTML and public assets to the root of the sibling
`diasbruno.github.io` checkout. With the standard local folder layout, that
destination is `/Users/dias/Programming/diasbruno.github.io`.

## Local development

Requires GNU Make, Python 3.9 or newer, and the publishing checkout with its
legacy files. There are no Python packages to install.

- `make build` renders and validates pages before writing them to the publishing checkout.
- `make serve` builds and serves the publishing checkout at http://127.0.0.1:8000/.
  Use `make serve PORT=8080` to choose a different port. After an edit, run
  `make build` and refresh the browser.
- `make check` checks generated files, local links, and the preserved publishing inventory.
- `make clean` removes files listed in the publishing checkout’s
  `.site-generated.json`. Run `make build` afterwards to restore them.

Set `DESTINATION` on a Make command to target a different publishing checkout,
for example `make build DESTINATION=/tmp/site-preview`.

Edit section content under `_site/content/`, layouts under `_site/templates/`,
and the styles and signature under `_site/static/`. Fraunces is self-hosted under
the SIL Open Font License included with the font.

Each page has a complete HTML document at its normal URL and a companion
`content.json` in the same directory. The JSON contains the page path, title,
and the rendered HTML. For example, `/posts/` and `/posts/content.json` come
from the same build. The new layouts use those companions for animated
navigation. Regular links load the full HTML when JavaScript is unavailable or
the JSON request fails. Archived articles and the open-source directory also
have JSON companions and continue to open as standalone pages.

The landing uses the signature reveal from the original site, followed by
staggered menu links. Each navigation between the new layouts randomly picks
the original seven-color columns, alternating horizontal bands, or a diagonal
tile cascade. Reduced motion skips the landing reveal and the page transitions.

When publishing from the branch, include the generated homepage, section
indexes, JSON companions, `assets/site/`, and `.site-generated.json` in the
publishing checkout.
The archive, legacy styles and images, open-source directory, Atom feed, and
old JSON indexes already live there. The posts preview uses titles and dates
from the published archive and preserves the Substack links.
