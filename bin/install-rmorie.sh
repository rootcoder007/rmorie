#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# install-rmorie.sh -- one-liner installer for rmorie.
#
# Usage:
#     curl -fsSL https://rootcoder007.github.io/rmorie/install.sh | bash
#
# What this does:
#   1. Verifies an R interpreter is on $PATH (errors clearly if not).
#   2. Installs rmorie from r-universe (binary; no source compile).
#   3. Optionally installs rmoriedata for the full fixture set.
#   4. Verifies the install with a smoke test (library(rmorie)).
#
# Why r-universe and not CRAN: rmorie is rOpenSci-review-pending and
# not yet on CRAN. r-universe ships a fresh binary daily for
# linux-x86_64, macos-aarch64, and windows.
#
# Safe to re-run; install.packages() will replace the existing library.

set -euo pipefail

REPOS="${RMORIE_REPOS:-https://rootcoder007.r-universe.dev}"
CRAN="${RMORIE_CRAN_FALLBACK:-https://cloud.r-project.org}"
INSTALL_DATA="${RMORIE_INSTALL_DATA:-yes}"

# 1. Verify R is available.
if ! command -v R >/dev/null 2>&1; then
  cat <<EOF >&2
rmorie installer: R is not on \$PATH.

Install R first, then re-run this installer:

  macOS:   brew install r          # or visit https://cran.r-project.org/bin/macosx
  Ubuntu:  sudo apt-get install r-base
  Other:   https://cran.r-project.org/

EOF
  exit 127
fi

R_VERSION=$(R --version | head -1 | awk '{print $3}')
echo "==> Found R $R_VERSION"

# 2. Install rmorie (+ optionally rmoriedata).
PKGS='c("rmorie")'
if [ "$INSTALL_DATA" = "yes" ]; then
  PKGS='c("rmorie", "rmoriedata")'
fi

echo "==> Installing rmorie from $REPOS"
R --vanilla --no-echo <<EOF
options(repos = c(rmorie = "${REPOS}", CRAN = "${CRAN}"))
install.packages(${PKGS}, dependencies = TRUE)
EOF

# 3. Smoke-load.
echo "==> Verifying install"
R --vanilla --no-echo <<EOF
library(rmorie)
cat("\nrmorie", as.character(packageVersion("rmorie")), "installed OK\n")
EOF

cat <<EOF

============================================================
rmorie is installed. Quick start:

    R
    library(rmorie)
    ?rmorie

Docs:        https://rootcoder007.github.io/rmorie/
Source:      https://github.com/rootcoder007/rmorie
Issues:      https://github.com/rootcoder007/rmorie/issues
============================================================
EOF
