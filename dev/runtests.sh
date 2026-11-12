#!/bin/sh
# runtests.sh <pkg> <testdir> [file...]: run testthat files against the build in ~/tmp/klib,
# with an empty HOME and no stored or environment key, as the CI gates do.
pkg=$1; dir=$2; shift 2
h=$(mktemp -d)
files="$*"
env -u MORIE_HOSTED_KEY -u MORIE_KEY -u MORIE_DATA_URL -u R_TESTS HOME=$h XDG_CONFIG_HOME=$h/cfg \
  R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null R_LIBS=$HOME/tmp/klib:$HOME/R/library \
  nice -n 10 Rscript -e '
a <- commandArgs(TRUE); pkg <- a[[1]]; dir <- a[[2]]; fs <- a[-(1:2)]
cat("loaded:", find.package(pkg), find.package("rmoriebricklayer"), format(packageVersion("rmoriebricklayer")), "\n")
library(testthat)
r <- if (length(fs)) {
  do.call(rbind, lapply(fs, function(f) as.data.frame(test_file(file.path(dir, f), package = pkg, load_package = "installed", reporter = "summary", stop_on_failure = FALSE))))
} else as.data.frame(test_dir(dir, package = pkg, load_package = "installed", reporter = "summary", stop_on_failure = FALSE))
cat(sprintf("\nTOTAL files=%d tests=%d failed=%d skipped=%d errors=%d warnings=%d\n", length(unique(r$file)), sum(r$nb), sum(r$failed), sum(r$skipped), sum(r$error), sum(r$warning)))
' "$pkg" "$dir" $files < /dev/null
rm -rf "$h"
