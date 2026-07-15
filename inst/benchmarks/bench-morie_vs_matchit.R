# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native nearest-neighbour matcher vs MatchIt.
# Usage: Rscript inst/benchmarks/bench-morie_vs_matchit.R
# Writes inst/benchmarks/results/<date>-matchit.csv
suppressMessages(library(rmorie))
stopifnot(requireNamespace("MatchIt", quietly = TRUE))
rows <- list()
for (n in c(1e3, 1e4, 1e5)) {
  set.seed(1)
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.4 * x1 + 0.3 * x2 - 0.5))
  y <- 0.5 * d + 0.6 * x1 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
  t_m <- system.time(
    r_m <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2")))[3]
  t_r <- system.time(
    mi <- MatchIt::matchit(d ~ x1 + x2, data = df, method = "nearest",
                           distance = "glm", m.order = "largest"))[3]
  ate <- function(m) mean(m$y[m$d == 1]) - mean(m$y[m$d == 0])
  rows[[length(rows) + 1]] <- data.frame(
    n = n, morie_sec = t_m, matchit_sec = t_r,
    speedup = t_r / t_m,
    morie_ate = ate(r_m$matched_data),
    matchit_ate = ate(MatchIt::match.data(mi))
  )
  cat(sprintf("n=%g morie=%.2fs matchit=%.2fs speedup=%.1fx\n",
              n, t_m, t_r, t_r / t_m))
}
out <- do.call(rbind, rows)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
write.csv(out, sprintf("inst/benchmarks/results/%s-matchit.csv",
                       format(Sys.Date())), row.names = FALSE)
