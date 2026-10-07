#!/usr/bin/env bash
# Fast static guards that must hold before any push.
#
# Both of these exist because a real push broke CI in a way that only
# surfaced 20 minutes in, on a runner:
#   1. 2026-07-30 — an exported R alias forwarded to morie_esl_oob_632(),
#      a function a later whole-file overwrite had deleted. R CMD check
#      caught it only when running examples.
#   2. a citation written from memory (a bare "Author (year)", a venue and
#      pages with no title) shipped as if it were evidence.
#
# Usage: scripts/audit/prepush_guards.sh
set -uo pipefail
cd "$(dirname "$0")/../.." || exit 1
fail=0

run() {
  printf '\n[guard] %s\n' "$1"; shift
  "$@" || fail=1
}

if command -v Rscript >/dev/null 2>&1; then
  for pkg in r-package/morie .; do
    [ -d "$pkg/R" ] || continue
    run "R undefined symbols ($pkg)" Rscript scripts/audit/check_r_undefined.R "$pkg"
  done
else
  echo "[guard] Rscript not found — R symbol check SKIPPED" >&2
fi

if command -v Rscript >/dev/null 2>&1; then
  run "citations are checkable" Rscript scripts/audit/check_citations.R .
fi

if [ "$fail" -ne 0 ]; then
  echo
  echo "PUSH BLOCKED: fix the guards above, or push with --no-verify if you"
  echo "are certain (and say so in the commit message)."
fi
exit "$fail"
