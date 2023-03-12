#!/usr/bin/env bash

DATE="`now`"
TITLE="no title"
SLUG_TITLE="no-title"
SLUG="${SLUG_TITLE}"
STATUS="draft"
FILENAME=""

function set_date {
    DATE="${1}"
}

function set_title {
    TITLE="${1}"
    set_slug_title "`slugify "${TITLE}"`"
}

function set_slug_title {
    SLUG_TITLE="${1}"
    set_slug "$SLUG_TITLE"
}

function set_slug {
    SLUG="${1}"
}

function set_filename {
    FILENAME="${1}"
}

function set_status {
    STATUS="${1}"
}

function make_content_filepath {
    local fsdate=`datefs "${DATE}"`
    echo "${fsdate}-${SLUG_TITLE}"
}

function make_content_metadata {
    cat <<-EOF
DATE="${DATE}"
TITLE="${TITLE}"
SLUG_TITLE="${SLUG_TITLE}"
SLUG="${SLUG}"
FILENAME="${FILENAME}"
STATUS="${STATUS}"
EOF
}

function metadata_filename {
    echo "`in_contents_path "${1}/metadata.sh"`"
}

function content_filename {
    echo "`in_contents_path "${1}/content.md"`"
}

function load_metadata {
    source "`metadata_filename "${1}"`"
}

function write_content_metadata {
    local the_path="`metadata_filename "${1}"`"
    make_content_metadata > $the_path
}
