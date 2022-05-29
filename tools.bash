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

function date_day {
    TZ=UTC date +%d -d "${1}" | cat
}

function date_month {
    TZ=UTC date +%m -d "${1}" | cat
}

function date_year {
    TZ=UTC date +%Y -d "${1}" | cat
}

function date_hour {
    TZ=UTC date +%H -d "${1}" | cat
}

function date_minutes {
    TZ=UTC date +%M -d "${1}" | cat
}

function date_seconds {
    TZ=UTC date +%S -d "${1}" | cat
}

function read_or_default {
    read -p "${1}" VAR
    [ -z "${VAR}" ] && echo "${2}" || echo "${VAR}" | cat
}

function change_date_from {
    local year=`date_year "${1}"`
    local month=`date_month "${1}"`
    local day=`date_day "${1}"`

    NEW_YEAR="`read_or_default "year(${year}): " "${year}"`"
    NEW_MONTH="`read_or_default "month(${month}): " "${month}"`"
    NEW_DAY="`read_or_default "day(${day}): " "${day}"`"

    HOURS="`date_hour "${1}"`"
    MINUTES="`date_minutes "${1}"`"
    SECONDS="`date_seconds "${1}"`"

    echo "${NEW_YEAR}-${NEW_MONTH}-${NEW_DAY}T${HOURS}:${MINUTES}:${SECONDS}+00:00"
}
