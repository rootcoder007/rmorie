# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native permutation tests vs coin (asymptotic reference).
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("coin", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  df <- data.frame(y = c(rnorm(n %/% 2), rnorm(n - n %/% 2, 0.3)),
                   g = factor(rep(c("a", "b"), c(n %/% 2, n - n %/% 2))))
  t_m <- system.time(morie_wilcox_test(y ~ g, df,
                                       distribution = "asymptotic"))[["elapsed"]]
  t_c <- system.time(coin::wilcox_test(y ~ g, data = df,
                                       distribution = "asymptotic"))[["elapsed"]]
  data.frame(n = n, wilcox_rmorie = t_m, wilcox_coin = t_c)
}
res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-coin.csv",
                              format(Sys.Date())), row.names = FALSE)
