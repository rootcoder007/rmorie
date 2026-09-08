#!/usr/bin/env Rscript
# Anomaly-shelf R-vs-Python parity.
#
# Inputs AND expected outputs both come from the Python run (anchors/),
# because R cannot reproduce numpy's generator: exporting only the
# expected values would compare two different datasets and pass on
# nothing. See scripts/audit/README-parity.md.
#
# Usage: Rscript scripts/audit/verify_anomaly_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"

# .morie_ginv lives in the shared causal helper file; in the installed
# package it is always on hand, so source it here rather than duplicating it.
for (f in c("causal_shared_native.R", "anomaly_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to read the anchor file")
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
X <- as.matrix(utils::read.csv(file.path(anch, "X.csv"), header = FALSE))
y <- as.numeric(utils::read.csv(file.path(anch, "y.csv"), header = FALSE)[[1]])

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-9) {
  got <- as.numeric(got)
  want <- as.numeric(want)
  if (length(got) != length(want)) {
    cat(sprintf("FAIL %-24s length %d vs %d\n", label, length(got), length(want)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  # Tolerance scales with magnitude: an absolute 1e-9 is unmeetable on
  # values of order 1e4, and meaningless on values of order 1e-12.
  err <- max(abs(got - want) / pmax(1, abs(want)))
  if (is.finite(err) && err <= tol) {
    cat(sprintf("ok   %-24s max rel err %.3g\n", label, err))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-24s max rel err %.3g\n", label, err))
    fail <<- fail + 1L
  }
}

chk("abod exact", morie_abod(X)$abof, exp$abod_exact)
chk("abod k=8", morie_abod(X, k = 8)$abof, exp$abod_k8)
chk("ecod score", morie_ecod(X)$score, exp$ecod_score)
chk("ecod skewness", morie_ecod(X)$skewness, exp$ecod_skew)
chk("hbos static", morie_hbos(X, bins = 8)$score, exp$hbos_static)
chk("hbos dynamic", morie_hbos(X, bins = 8, mode = "dynamic")$score,
    exp$hbos_dynamic)
chk("lof k=10", morie_local_outlier_factor(X, k = 10)$lof, exp$lof_k10)
chk("lof lrd", morie_local_outlier_factor(X, k = 10)$lrd, exp$lof_lrd)
chk("ts robust z", morie_joseph_ts_outlier_detection(y, W = 12)$score,
    exp$ts_score)
chk("ts outlier flags",
    as.integer(morie_joseph_ts_outlier_detection(y, W = 12)$outlier),
    exp$ts_out)

# The two randomised members get invariants, not values: R's generator
# is not numpy's, so demanding equality would be demanding a bug.
inv <- function(label, ok) {
  if (isTRUE(ok)) {
    cat(sprintf("ok   %-24s (invariant)\n", label))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-24s (invariant)\n", label))
    fail <<- fail + 1L
  }
}
isf <- morie_isolation_forest(X, n_trees = 60, seed = 1)
inv("isoforest score range", all(isf$score > 0 & isf$score <= 1))
inv("isoforest finds planted", which.max(isf$score) %in% c(61L, 62L))
inv("isoforest path lengths", all(isf$path_length > 0))
mcd <- morie_mcd_outlier(X, seed = 1)
inv("mcd flags planted", all(mcd$outlier[c(61L, 62L)]))
inv("mcd beats classical",
    mcd$distance[61] > mcd$classical_distance[61])
inv("mcd covariance is p x p",
    identical(dim(mcd$covariance), c(ncol(X), ncol(X))))

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
