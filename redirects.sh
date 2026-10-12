#!/usr/bin/env bash
# Post-render: write a redirect page for each chapter's old flat URL
# (narrative_<slug>.html) pointing at its new location
# (chapters/<slug>/narrative.html), so existing links keep working.

set -euo pipefail

BOOK_DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="${QUARTO_PROJECT_OUTPUT_DIR:-docs}"
case "$OUT" in /*) ;; *) OUT="$BOOK_DIR/$OUT" ;; esac

grep -oE 'chapters/[A-Za-z0-9_-]+/narrative\.qmd' "$BOOK_DIR/_quarto.yml" \
  | cut -d/ -f2 \
  | while read -r slug; do
      target="chapters/$slug/narrative.html"
      cat > "$OUT/narrative_$slug.html" <<HTML
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Moved</title>
<link rel="canonical" href="$target">
<meta http-equiv="refresh" content="0; url=$target">
</head>
<body><p>This chapter has moved to <a href="$target">$target</a>.</p></body>
</html>
HTML
    done
