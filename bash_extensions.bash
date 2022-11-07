#!/usr/bin/env bash

function read_or_default {
    read -p "${1}" VAR
    [ -z "${VAR}" ] && echo "${2}" || echo "${VAR}" | cat
}

function expect_args {
    if [ -z "${1}" ]; then
	usage
	exit 1
    fi
}
