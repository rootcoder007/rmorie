# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native optimal pair matching vs MatchIt(method="optimal").
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("MatchIt", quietly = TRUE),
          requireNamespace("optmatch", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(-1.5 + 0.5 * x1 - 0.4 * x2))
  df <- data.frame(x1, x2, d)
  t_o <- system.time(morie_matching_optimal_pair(
    df, "d", c("x1", "x2")))[["elapsed"]]
  # optmatch materialises the dense nt x nc distance matrix; at n=1e5
  # that is ~12 GB and the process is OOM-killed (SIGKILL, not
  # catchable) -- reference capped at 1e4.
  t_m <- if (n > 1e4) NA_real_ else
    tryCatch(system.time(MatchIt::matchit(
      d ~ x1 + x2, data = df, method = "optimal",
      distance = "glm"))[["elapsed"]], error = function(e) NA)
  data.frame(n = n, optimal_rmorie = t_o, optimal_matchit = t_m)
}

res <- do.call(rbind, lapply(c(1e3, 1e4, 1e5), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-optimal.csv",
                              format(Sys.Date())), row.names = FALSE)
