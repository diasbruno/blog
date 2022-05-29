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

function show_list {
    local list="${1}"
    local counter=1
    for x in $list; do
	echo "${counter}: $x";
	counter=$((counter+1))
    done
}

function select_option {
    local list="${1}"
    local counter=1
    local selected=""
    local choosed=""

    read -p "select option: " choosed

    counter=1
    for x in $list; do
	if [ "${choosed}" == "${counter}" ]; then
	    selected="$x"
	fi
	counter=$((counter+1))
    done

    echo "${selected}"
}
