"""Validate the source, preserved paths, and current generated files."""

import fcntl
import sqlite3
import xml.etree.ElementTree as ET

from build import ROOT, SOURCE, digest, generate, owned_path, recorded_outputs


def main():
    with (SOURCE.parent / "Makefile").open() as lock:
        fcntl.flock(lock, fcntl.LOCK_SH)
        expected, archived, unavailable = generate()
        recorded = recorded_outputs()
        if recorded != {name: digest(content) for name, content in expected.items()}:
            raise ValueError("Generated output is out of date. Run make build.")
        for name, content in expected.items():
            path = owned_path(name)
            if not path.is_file() or path.read_bytes() != content:
                raise ValueError(f"Missing or stale output: {name}. Run make build.")
        for name, html in archived.items():
            if (ROOT / name).read_bytes() != html:
                raise ValueError(f"Stale footer: {name}. Run make build.")
        for name in sorted(unavailable):
            if owned_path(name).exists():
                raise ValueError(f"Unpublished post output exists: {name}. Run make build.")
    print(f"Checked {len(expected)} generated files, {len(archived)} archive pages, and the published file inventory")


if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValueError, sqlite3.Error, ET.ParseError) as error:
        raise SystemExit(str(error))
