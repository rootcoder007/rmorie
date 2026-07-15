# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native DML (PLR) vs DoubleML + ranger.
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("DoubleML", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  X <- matrix(rnorm(n * 5), n, 5)
  d <- rbinom(n, 1, plogis(0.6 * X[, 1] - 0.4 * X[, 2]))
  y <- 0.5 * d + X[, 1] + 0.5 * X[, 2] + rnorm(n)
  df <- data.frame(y = y, d = d, X)
  names(df)[3:7] <- paste0("x", 1:5)
  t_o <- system.time(morie_estimate_double_ml(
    df, "y", "d", paste0("x", 1:5), n_folds = 5L))[["elapsed"]]
  t_m <- tryCatch({
    dml_data <- DoubleML::double_ml_data_from_data_frame(
      df = df, y_col = "y", d_cols = "d", x_cols = paste0("x", 1:5))
    ml <- mlr3::lrn("regr.ranger", num.trees = 100L, max.depth = 5L)
    .gst <- rmorie:::.morie_dml_guard_begin()
    on.exit(rmorie:::.morie_dml_guard_end(.gst), add = TRUE)
    st <- system.time({
      plr <- DoubleML::DoubleMLPLR$new(dml_data, ml_l = ml,
                                       ml_m = ml$clone(), n_folds = 5L)
      plr$fit()
    })
    st[["elapsed"]]
  }, error = function(e) NA)
  data.frame(n = n, dml_rmorie = t_o, dml_doubleml_ranger = t_m)
}
res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-dml.csv",
                              format(Sys.Date())), row.names = FALSE)
