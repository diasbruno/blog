#!/usr/bin/env bash

set -eu

BASE_DIR="/"

function html_head {
    cat <<-EOF
<meta charset="UTF-8">
<title>diasbruno</title>
<base href="${BASE_DIR}">
<meta name="viewport" content="width=device-width" />
<link rel="stylesheet" type="text/css" href="./css/reset.css" />
<link rel="stylesheet" type="text/css" href="./css/style.css" />
<link rel="stylesheet" type="text/css" href="./css/highlight.min.css" />
EOF
}

function html_footer {
    cat <<-EOF
<script type="application/javascript" src="./js/highlight.min.js"></script>
EOF
}

function navigation {
    cat <<-EOF
<nav>
  <div><a href="${BASE_DIR}"><h1>diasbruno</h1></a></div>
  <div class="nav-links"><a href="${BASE_DIR}">articles</a></div>
</nav>
EOF
}

function render_html_wrapper {
    local hh=`html_head`
    local nav=`navigation`
    local pf=`html_footer`

    cat <<-EOF > ${1}
<!DOCTYPE html>
<html>
${hh}
<body><main>
${nav}
<section class="content">${2}</section>
${pf}
</main></body></html>
EOF
}

function render_date {
    date "+%b %d, %Y" -d "${1}"
}
