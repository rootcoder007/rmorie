# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native nonparametric bootstrap vs boot::boot.
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("boot", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  x <- rnorm(n)
  stat_i <- function(d, i) mean(d[i])
  R <- 1000L
  t_m <- system.time({
    set.seed(2)
    morie_boot(x, stat_i, R = R)
  })[["elapsed"]]
  t_b <- system.time({ set.seed(2); boot::boot(x, stat_i, R = R) })[["elapsed"]]
  data.frame(n = n, R = R, boot_rmorie = t_m, boot_boot = t_b)
}
res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-boot.csv",
                              format(Sys.Date())), row.names = FALSE)
