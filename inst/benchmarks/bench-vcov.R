# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native robust covariance vs sandwich (HC3 / HAC / CL).
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("sandwich", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  x1 <- rnorm(n); x2 <- runif(n)
  g  <- factor(sample.int(max(2L, n %/% 15L), n, replace = TRUE))
  y  <- 1 + 0.8 * x1 - 0.5 * x2 + rnorm(n, sd = 1 + abs(x1))
  df <- data.frame(x1, x2, g, y)
  m  <- stats::lm(y ~ x1 + x2, data = df)
  t_hc_m <- system.time(morie_vcov_hc(m, "HC3"))[["elapsed"]]
  t_hc_s <- system.time(sandwich::vcovHC(m, type = "HC3"))[["elapsed"]]
  t_cl_m <- system.time(morie_vcov_cl(m, df$g, "HC1"))[["elapsed"]]
  t_cl_s <- system.time(sandwich::vcovCL(m, cluster = df$g))[["elapsed"]]
  data.frame(n = n,
             HC3_rmorie = t_hc_m, HC3_sandwich = t_hc_s,
             CL_rmorie = t_cl_m, CL_sandwich = t_cl_s)
}
res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-vcov.csv",
                              format(Sys.Date())), row.names = FALSE)
