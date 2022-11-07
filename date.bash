#!/usr/bin/env bash

function now {
    echo "`TZ=UTC date --iso-8601=s`"
}

function datefs {
    TZ=UTC date +%Y%m%d%H%M%S -d "${1}"
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
