#!/usr/bin/env bash

function assert {
    if [ "${1}" != "${2}" ]; then
	echo "failed|${3}"
	echo "  - ${1}"
	echo "  - ${2}"
	exit 1
    else
	echo "ok|${3}"
    fi
}
