#!/usr/bin/env bash



if [ -z "${ENGINE_CONTENT_PATH}" ]; then
    echo "ENGINE_CONTENT_PATH is not set."
    exit 1
fi

function in_contents_path {
    echo "${ENGINE_CONTENT_PATH}/${1}"
}

function trim {
    echo $1 | sed -e 's/[\\s\\n]*//g'
}

function delete_content {
    if test -z "${1}"; then
	log "missing content to delete."
	return 1
    fi

    local content_path=`in_contents_path "${1}"`
    log "${content_path} deleted."
    rm -rf "${content_path}"
}

function delete_if_create_content_fail {
    test "${1}" -ne "0" && \
	log "${3}" && \
	delete_content "${2}" && \
	exit 1
}

function create_content {
    local the_title="${1}"
    local the_date="${2}"

    set_title "${the_title}"
    set_date "${the_date}"
    set_slug `slugifier "${the_title}"`
    set_slug_title "${SLUG}"
    set_filename "`make_content_filepath`"
    set_status "hidden"

    local the_content_path="${FILENAME}"
    local result=0

    mkdir $(in_contents_path $the_content_path)

    write_content_metadata $the_content_path

    delete_if_create_content_fail \
	$? \
	$the_content_path \
	"failed to write the metadata."

    touch $(content_filename $the_content_path)

    delete_if_create_content_fail \
	$? \
	$the_content_path \
	"failed to write the content."

    log "wrote ${the_content_path}"
}

function copy_content_from {
    local content_from=content_filename $1
    local content_to=content_filename $2
    cp $content_from $content_to
}
