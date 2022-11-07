#!/usr/bin/env bash

_ENABLED_LOG=1

function disable_log {
    _ENABLED_LOG=0
}

function log {
    test $_ENABLED_LOG -eq 1 && echo "`now` | $1"
}
