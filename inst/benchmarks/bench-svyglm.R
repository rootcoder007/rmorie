# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native design-based weighted GLM vs survey::svyglm.
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("survey", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  x1 <- rnorm(n); x2 <- rnorm(n); w <- runif(n, 0.5, 3)
  y <- rbinom(n, 1, plogis(-0.4 + 0.6 * x1 - 0.3 * x2))
  df <- data.frame(x1, x2, y, w)
  t_o <- system.time(rmorie:::.morie_svyglm_native(
    y ~ x1 + x2, data = df, weights = df$w,
    family = stats::quasibinomial()))[["elapsed"]]
  t_m <- system.time({
    des <- survey::svydesign(ids = ~1, weights = ~w, data = df)
    survey::svyglm(y ~ x1 + x2, design = des,
                   family = stats::quasibinomial())
  })[["elapsed"]]
  data.frame(n = n, svyglm_rmorie = t_o, svyglm_survey = t_m)
}
res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-svyglm.csv",
                              format(Sys.Date())), row.names = FALSE)
