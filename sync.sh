#!/usr/bin/env bash
# Copy each module's narrative into chapters/<slug>/ for the book.
#
# The chapters (and their order) come from _quarto.yml: every entry of the
# form chapters/<slug>/narrative.qmd is synced. SLUG_REPO below maps a slug
# to its module repo in ../Modules. Files are copied verbatim, with no path
# rewriting; knitr runs each chapter from its own folder, so relative
# media/ and data/ paths keep working.
#
# Usage:
#   ./sync.sh            sync every chapter listed in _quarto.yml
#   ./sync.sh aov joins  sync only these slugs
#
# Each module's commit is written to chapters.lock. A "+dirty" suffix means
# the module had uncommitted changes, so the copy doesn't match that commit.

set -euo pipefail

BOOK_DIR="$(cd "$(dirname "$0")" && pwd)"
MODULES_DIR="${MODULES_DIR:-$BOOK_DIR/../Modules}"
CHAPTERS_DIR="$BOOK_DIR/chapters"
LOCK="$BOOK_DIR/chapters.lock"

slug_repo() {
  case "$1" in
    markdown)    echo Markdown ;;
    datatypes)   echo Basic-Data-Types ;;
    containers)  echo Data-Containers ;;
    tidyverse)   echo Tidyverse ;;
    factors)     echo Factors ;;
    joins)       echo Joins ;;
    classic)     echo Data_Visualization ;;
    ggplot)      echo GGPlot2_Graphics ;;
    points)      echo Spatial-Points ;;
    vector)      echo Shapefiles ;;
    rasters)     echo Raster-Data ;;
    correlation) echo Correlation ;;
    regression)  echo Regression ;;
    aov)         echo Analysis-of-Variance ;;
    ordination)  echo Ordination ;;
    *)           return 1 ;;
  esac
}

if [[ $# -gt 0 ]]; then
  slugs=("$@")
else
  slugs=()
  while IFS= read -r s; do slugs+=("$s"); done < <(
    grep -oE 'chapters/[A-Za-z0-9_-]+/narrative\.qmd' "$BOOK_DIR/_quarto.yml" \
      | cut -d/ -f2
  )
fi

if [[ ${#slugs[@]} -eq 0 ]]; then
  echo "No chapters/<slug>/narrative.qmd entries found in _quarto.yml" >&2
  exit 1
fi

mkdir -p "$CHAPTERS_DIR"
touch "$LOCK"
errors=0

for slug in "${slugs[@]}"; do
  if ! repo="$(slug_repo "$slug")"; then
    echo "!! $slug: no module repo mapped in sync.sh" >&2
    errors=$((errors + 1)); continue
  fi
  src="$MODULES_DIR/$repo"
  if [[ ! -f "$src/narrative.qmd" ]]; then
    echo "!! $slug: $src/narrative.qmd not found" >&2
    errors=$((errors + 1)); continue
  fi

  dest="$CHAPTERS_DIR/$slug"
  rm -rf "$dest"
  mkdir -p "$dest"
  cp "$src/narrative.qmd" "$dest/"
  for d in media data; do
    [[ -d "$src/$d" ]] && cp -R "$src/$d" "$dest/"
  done

  commit="$(git -C "$src" rev-parse --short HEAD 2>/dev/null || echo unknown)"
  if [[ -n "$(git -C "$src" status --porcelain -- narrative.qmd media data 2>/dev/null)" ]]; then
    commit="$commit+dirty"
  fi

  # Replace this slug's line in the lock, keeping the others.
  grep -v "^$slug	" "$LOCK" > "$LOCK.tmp" || true
  printf '%s\t%s\t%s\n' "$slug" "$repo" "$commit" >> "$LOCK.tmp"
  sort -o "$LOCK" "$LOCK.tmp"
  rm -f "$LOCK.tmp"

  printf '   %-12s <- %-22s %s\n' "$slug" "$repo" "$commit"
done

if [[ $errors -gt 0 ]]; then
  echo "$errors chapter(s) failed to sync" >&2
  exit 1
fi
