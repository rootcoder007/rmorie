# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native psychometrics vs psych (skip-if-not-installed).
# Usage: Rscript inst/benchmarks/bench-morie_vs_psych.R
suppressMessages(pkgload::load_all(quiet = TRUE))
set.seed(1)
n <- 5000L; k <- 12L
lambda <- runif(k, 0.4, 0.8)
f <- rnorm(n)
X <- sapply(lambda, function(l) l * f + sqrt(1 - l^2) * rnorm(n))
colnames(X) <- paste0("item", seq_len(k))
bench <- function(thunk) { t0 <- proc.time()[[3]]; v <- thunk(); c(t = proc.time()[[3]] - t0, est = v) }
rows <- list()
rows$alpha_native <- bench(function() morie_psymet_alpha(as.data.frame(X))$raw)
rows$omega_native <- bench(function() morie_psymet_omega(as.data.frame(X))$total)
if (requireNamespace("psych", quietly = TRUE)) {
  rows$alpha_psych <- bench(function() suppressWarnings(psych::alpha(X))$total$raw_alpha)
  rows$omega_psych <- bench(function() suppressMessages(suppressWarnings(psych::omega(X, plot = FALSE)))$omega.tot)
}
res <- do.call(rbind, rows)
print(round(res, 4))
dir.create("inst/benchmarks/results", showWarnings = FALSE)
write.csv(res, sprintf("inst/benchmarks/results/%s-psych.csv", Sys.Date()))
