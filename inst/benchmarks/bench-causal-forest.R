# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native R-learner causal forest vs grf::causal_forest.
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("grf", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  X <- matrix(rnorm(n * 4), n, 4)
  w <- rbinom(n, 1, plogis(0.5 * X[, 1]))
  y <- (1 + 0.5 * X[, 2]) * w + X[, 1] + rnorm(n)
  df <- data.frame(t = w, y = y, X)
  names(df)[3:6] <- paste0("x", 1:4)
  t_o <- system.time(morie_estimate_dr_forest(
    df, "t", "y", paste0("x", 1:4)))[["elapsed"]]
  t_m <- system.time({
    cf <- grf::causal_forest(X, y, w, seed = 1)
    grf::average_treatment_effect(cf, method = "AIPW")
  })[["elapsed"]]
  data.frame(n = n, forest_rmorie = t_o, forest_grf = t_m)
}
res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-causal-forest.csv",
                              format(Sys.Date())), row.names = FALSE)
