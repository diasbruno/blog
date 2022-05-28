#!/usr/bin/env bash

set -eu

# expected 's/[^A-Za-z0-9\s]//g'
FILTER_SLUGIFY='s/[\!\-\@\:]//g'

function slugify {
    echo "${1}" | tr ' ' '-' | tr '[:upper:]' '[:lower:]' | sed -E $FILTER_SLUGIFY
}

function now {
    echo "`TZ=UTC date --iso-8601=s`"
}

function datefs {
    TZ=UTC date +%Y%m%d%H%M%S -d "${1}"
}

function log {
    echo "`now` | $1"
}
