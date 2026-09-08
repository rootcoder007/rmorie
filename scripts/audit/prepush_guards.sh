#!/usr/bin/env bash
# Fast static guards that must hold before any push.
#
# Both of these exist because a real push broke CI in a way that only
# surfaced 20 minutes in, on a runner:
#   1. 2026-07-30 — an exported R alias forwarded to morie_esl_oob_632(),
#      a function a later whole-file overwrite had deleted. R CMD check
#      caught it only when running examples.
#   2. earlier — a Python module was renamed without updating
#      _lazy_map.json, so a catalogue entry pointed at nothing.
#
# Usage: scripts/audit/prepush_guards.sh [--skip-catalogue]
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

if [ "${1:-}" != "--skip-catalogue" ] && [ -f src/morie/fn/_lazy_map.json ]; then
  printf '\n[guard] Python catalogue resolves\n'
  PYTHONPATH="$PWD/src" python3 - <<'PY' || fail=1
import importlib, json, sys
import morie.fn as F
here = __import__("os").getcwd()
assert here + "/src/" in F.__file__, (
    f"resolving against {F.__file__}, not this checkout — pin PYTHONPATH")
m = json.load(open("src/morie/fn/_lazy_map.json"))
bad = []
for name, mod in m.items():
    try:
        getattr(importlib.import_module(f"morie.fn.{mod}"), name)
    except Exception as e:
        bad.append(f"{name} -> {mod}: {type(e).__name__}")
print(f"catalogue: {len(m)} entries, {len(bad)} unresolved")
for b in bad[:20]:
    print("  ", b)
sys.exit(1 if bad else 0)
PY
fi

if [ "$fail" -ne 0 ]; then
  echo
  echo "PUSH BLOCKED: fix the guards above, or push with --no-verify if you"
  echo "are certain (and say so in the commit message)."
fi
exit "$fail"
