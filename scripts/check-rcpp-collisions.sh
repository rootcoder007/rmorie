#!/bin/sh
# check-rcpp-collisions.sh
#
# Guard against the Rcpp-export / hand-wrapper NAME COLLISION bug class.
#
# When a C++ function is tagged `// [[Rcpp::export]]` (no name=), Rcpp
# generates an R wrapper of the SAME name in R/RcppExports.R. If a
# hand-written wrapper of that name also exists in another R/ file (to add
# coercion/validation and call `.Call()` directly), the two bind the same
# symbol: the RcppExports wrapper (raw `_sxp` args) shadows the hand wrapper,
# and roxygen documents the hand wrapper's @params against the `_sxp` formals
# R CMD check sees -> "Codoc mismatches" + "Undocumented arguments" WARNING
# on every platform. The no-arg cases shadow silently (no codoc warning) but
# are still the wrong function.
#
# FIX / CONVENTION: any C++ export that has a hand-written R wrapper must use
#   // [[Rcpp::export(name = ".rmorie_<x>_impl")]]
# so RcppExports generates a dot-prefixed INTERNAL wrapper that cannot collide
# with (or shadow) the public hand wrapper. The .Call registration symbol is
# the C++ function name and is unaffected by name=, so the hand wrapper's
# `.Call("_rmorie_<cppname>", ...)` keeps working.
#
# This script fails if any PUBLIC (non-dot-prefixed) function defined in
# R/RcppExports.R is ALSO hand-defined in another R/ file.

set -eu
cd "$(dirname "$0")/.."

rcpp="R/RcppExports.R"
[ -f "$rcpp" ] || { echo "no $rcpp; nothing to check"; exit 0; }

# Public (non-dot) function names generated in RcppExports.R.
public_names=$(grep -oE '^[A-Za-z][A-Za-z0-9_.]* <- function' "$rcpp" | sed 's/ <- function//' | sort -u)

collisions=""
for n in $public_names; do
  # Is this name ALSO defined as `n <- function` in any OTHER R/ file?
  hits=$(grep -lE "^${n} <- function" R/*.R 2>/dev/null | grep -v "$rcpp" || true)
  if [ -n "$hits" ]; then
    collisions="${collisions}${n}  (also hand-defined in: $(echo $hits | tr '\n' ' '))\n"
  fi
done

if [ -n "$collisions" ]; then
  printf '\nERROR: Rcpp-export / hand-wrapper NAME COLLISION(S) detected:\n\n'
  printf "$collisions"
  printf '\nFix: give the C++ export an internal name so it stops shadowing the\n'
  printf 'hand wrapper, e.g. change  // [[Rcpp::export]]  to\n'
  printf '  // [[Rcpp::export(name = ".rmorie_<x>_impl")]]\n'
  printf 'then run Rcpp::compileAttributes(). See this script header.\n\n'
  exit 1
fi

echo "OK: no Rcpp-export / hand-wrapper name collisions."
