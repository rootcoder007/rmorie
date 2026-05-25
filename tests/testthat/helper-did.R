# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared DiD synthetic-data generators. testthat auto-sources
# helper-*.R into every test session so test-did.R, test-did-extra.R,
# and any future DiD test can share these fixtures.

# Simple 2x2 DiD DGP with planted tau (default 0.5).
make_did_2x2 <- function(n = 400, tau = 0.5, seed = 1) {
  set.seed(seed)
  d <- stats::rbinom(n, 1, 0.5)
  p <- stats::rbinom(n, 1, 0.5)
  x <- stats::rnorm(n)
  y <- 1.0 + 0.3 * d + 0.4 * p + tau * d * p + 0.2 * x +
    stats::rnorm(n, sd = 0.5)
  data.frame(y = y, d = d, post = p, x = x,
             clust = sample.int(20, n, replace = TRUE))
}

# Balanced panel DiD DGP with planted tau (default 0.7).
# Half the units get treatment at period 4 (staggered when n_units > 2).
make_did_panel <- function(n_units = 30, n_periods = 6,
                           tau = 0.7, seed = 1) {
  set.seed(seed)
  treat_unit <- stats::rbinom(n_units, 1, 0.5)
  treat_time <- ifelse(treat_unit == 1L, 4L, Inf)
  rows <- list()
  for (u in seq_len(n_units)) {
    a_i <- stats::rnorm(1)
    for (t in seq_len(n_periods)) {
      d_it <- as.integer(treat_unit[u] == 1L && t >= 4L)
      y <- a_i + 0.1 * t + tau * d_it + stats::rnorm(1, sd = 0.4)
      rows[[length(rows) + 1L]] <- data.frame(
        unit = u, time = t, y = y, d = d_it,
        treat = treat_unit[u], post = as.integer(t >= 4L),
        treat_time = treat_time[u], x = stats::rnorm(1)
      )
    }
  }
  do.call(rbind, rows)
}
