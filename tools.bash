#!/usr/bin/env bash

_DRY_RUN=1

function must_write {
    _DRY_RUN=0
}

function dry_run {
    _DRY_RUN=1
}

function is_dry_run {
    test $_DRY_RUN -eq 1
}

function list_contents {
    ls -r "${1}"
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
