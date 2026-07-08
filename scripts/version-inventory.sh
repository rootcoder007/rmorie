#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# version-inventory.sh -- regenerate VERSION_INVENTORY.csv deterministically.
#
# Single source of truth: the `Version:` field in DESCRIPTION (rmorie has no
# separate VERSION file; DESCRIPTION is canonical and inst/CITATION pulls
# meta$Version automatically). Anything else in the tree that mentions a
# version number is *derived*; this script scans the tree, compares each hit
# against the canonical version, and writes a categorised CSV.
#
# DRY contract:
#   - DESCRIPTION's Version is the only thing you hand-edit when bumping.
#     Keep the C++ User-Agent literal in src/ (MORIE_SIU_PARSER_VERSION) in
#     lockstep by hand.
#   - This script regenerates VERSION_INVENTORY.csv from current sources.
#   - .github/workflows/version-drift.yml re-runs it in CI and fails if the
#     committed CSV drifts from the regenerated output.
#
# Design notes:
#   - rg --vimgrep gives predictable `file:line:col:context`. ripgrep's
#     gitignore handling already excludes .git, etc.; the -g flags below add
#     the generated/self/data exclusions.
#   - DOI fragments (10.1080/...) match the semver regex, so the awk pass
#     skips any hit whose major segment is 4+ digits.

set -euo pipefail

# Run from repo root regardless of caller CWD.
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 1

INVENTORY_FILE="VERSION_INVENTORY.csv"

TMP_FILE="$(mktemp "./.${INVENTORY_FILE}.XXXXXX")"
trap 'rm -f "$TMP_FILE"' EXIT

if [[ ! -f DESCRIPTION ]]; then
    echo "Error: DESCRIPTION not found (must run from an rmorie checkout)." >&2
    exit 1
fi

CURRENT_VERSION=$(awk '/^Version:/{print $2; exit}' DESCRIPTION | tr -d '[:space:]')
if [[ -z "$CURRENT_VERSION" ]]; then
    echo "Error: could not read Version: from DESCRIPTION." >&2
    exit 1
fi

echo "==> Scanning for versions (canonical target: $CURRENT_VERSION)..."

REGEX='[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?'

echo "File,Line,Status,Version,Context" > "$TMP_FILE"

rg --vimgrep --sort path --color=never -e "$REGEX" \
  -g '!VERSION_INVENTORY.csv' \
  -g '!VERSION_INVENTORY.csv.committed' \
  -g '!inst/extdata/' \
  -g '!scripts/' \
  -g '!.git/' \
  . | awk -v curr="$CURRENT_VERSION" -F':' '{
    file = $1
    line = $2

    context = $4
    for (i = 5; i <= NF; i++) context = context ":" $i

    gsub(/"/, "", context)
    gsub(/,/, ";", context)
    gsub(/^[ \t]+|[ \t]+$/, "", context)

    if (match(context, /[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?/)) {
        matched_ver = substr(context, RSTART, RLENGTH)

        split(matched_ver, parts, ".")
        if (length(parts[1]) >= 4) next

        if (matched_ver == curr) {
            status = "CURRENT"
        } else {
            status = "STALE_OR_DEP"
        }

        printf "%s,%s,%s,%s,\"%s\"\n", file, line, status, matched_ver, context
    }
}' >> "$TMP_FILE"

mv "$TMP_FILE" "$INVENTORY_FILE"

TOTAL_ROWS=$(tail -n +2 "$INVENTORY_FILE" | wc -l | tr -d ' ')
STALE_ROWS=$(grep -c ",STALE_OR_DEP," "$INVENTORY_FILE" || true)

echo "==> Done. Generated $INVENTORY_FILE"
echo "    Found $TOTAL_ROWS version mentions ($STALE_ROWS stale or dependency)."
