"""Serve only the public site paths on a local development port."""

import argparse
from functools import partial
from http import HTTPStatus
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import unquote, urlsplit

from build import ROOT, legacy_paths, recorded_outputs


class SiteHandler(SimpleHTTPRequestHandler):
    def __init__(self, *args, legacy, **kwargs):
        self.legacy = legacy
        super().__init__(*args, **kwargs)

    def send_head(self):
        path = unquote(urlsplit(self.path).path).removeprefix("/")
        target = path + "index.html" if not path or path.endswith("/") else path
        public = self.legacy | recorded_outputs().keys()
        if target in public or (target + "/index.html") in public:
            return super().send_head()
        self.send_error(HTTPStatus.NOT_FOUND)
        return None

    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", default=8000, type=int)
    args = parser.parse_args()
    handler = partial(SiteHandler, directory=str(ROOT), legacy=set(legacy_paths()))
    with ThreadingHTTPServer(("127.0.0.1", args.port), handler) as server:
        print(f"Preview at http://127.0.0.1:{server.server_port}/", flush=True)
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()
