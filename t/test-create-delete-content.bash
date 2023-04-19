#!/usr/bin/env bash

ECP=$ENGINE_CONTENT_PATH

export ENGINE_CONFIG="${PWD}/config"

source "./test-tools.bash"
source "${PWD}/../engine.bash"

disable_log

mkdir -p $ENGINE_CONTENT_PATH

T="test content!"
D="`now`"
S="test-content-"

create_content "${T}" "${D}"

CONTENT=`ls -r $ENGINE_CONTENT_PATH | head -n 1`

load_metadata $CONTENT

assert "${T}" "${TITLE}" \
       "TITLE must be defined."
assert "${D}" "${DATE}" \
       "DATE must be defined."
assert "${S}" "${SLUG}" \
       "SLUG must be defined."

delete_content $CONTENT

assert \
    "`ls -r $ENGINE_CONTENT_PATH`" \
    "" \
    "path must not exists"

delete_content ""

assert \
    "$?" \
    "1" \
    "must exit if content is empty."

rm -rf $ENGINE_CONTENT_PATH

export ENGINE_CONTENT_PATH="${ECP}"
