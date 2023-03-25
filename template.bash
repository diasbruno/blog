#!/usr/bin/env bash

set -eu

BASE_DIR="/"

function og_tag {
    if [ -z "$1" ]; then
	echo ""
    else
	cat <<-EOF
<meta name="author" content="Bruno Dias" />
<meta name="description" content="${OG_DESCRIPTION}" />
<meta name="robot" content="noindex,nofollow" />
<meta property="og:title" content="${TITLE}" />
<meta property="og:type" content="${OG_TYPE}" />
<meta property="og:image" content="https://diasbruno.github.io${OG_IMAGE}" />
<meta property="og:url" content="https://diasbruno.github.io/articles/${2}" />
EOF
    fi
}

function html_head {
    local og_tags_rendered="`og_tag "${USE_OG:-""}" "$1"`"

    cat <<-EOF
<meta charset="UTF-8">
<title>diasbruno</title>
<base href="${BASE_DIR}">
<meta name="viewport" content="width=device-width" />
<link rel="stylesheet" type="text/css" href="./css/style.css" />
<link rel="stylesheet" type="text/css" href="./css/milligram.css" />
<link rel="stylesheet" type="text/css" href="./css/highlight.min.css" />
${og_tags_rendered}
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
    local hh=`html_head "$2"`
    local nav=`navigation`
    local pf=`html_footer`

    cat <<-EOF > ${1}
<!DOCTYPE html>
<html prefix="og: http://ogp.me/ns#" >
<head>
${hh}
</head>
<body><main>
${nav}
<section class="content">${3}</section>
${pf}
</main></body></html>
EOF
}

function render_date {
    date "+%b %d, %Y" -d "${1}"
}
