# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native exact + CEM vs MatchIt at 1e3 / 1e4 / 1e5 rows.
# Release bar: <= 2x the reference at 100k. Run on L14:
#   LD_LIBRARY_PATH=/usr/local/lib64 Rscript inst/benchmarks/bench-cem_exact.R
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("MatchIt", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  g <- sample(letters[1:6], n, replace = TRUE)
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(-1.2 + 0.5 * x1 + 0.3 * (g %in% c("a", "b"))))
  df <- data.frame(g, x1, x2, d)
  t_ex_o <- system.time(morie_matching_exact(df, "d", "g"))[["elapsed"]]
  t_ex_m <- system.time(MatchIt::matchit(d ~ g, data = df,
                                         method = "exact"))[["elapsed"]]
  t_cem_o <- system.time(morie_matching_cem(df, "d", c("x1", "x2"),
                                            n_bins = 5L))[["elapsed"]]
  t_cem_m <- tryCatch(system.time(MatchIt::matchit(
    d ~ x1 + x2, data = df, method = "cem",
    cutpoints = list(x1 = 5L, x2 = 5L)))[["elapsed"]], error = function(e) NA)
  data.frame(n = n, exact_rmorie = t_ex_o, exact_matchit = t_ex_m,
             cem_rmorie = t_cem_o, cem_matchit = t_cem_m)
}

res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-cem-exact.csv",
                              format(Sys.Date())), row.names = FALSE)
