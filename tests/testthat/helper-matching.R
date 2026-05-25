# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared matching-test synthetic-data generator (extracted from
# test-matching.R so any matching test can re-use it).

# Treatment d ~ logit(0.4 x1 + 0.3 x2). Outcome y = tau*d + 0.5 x1 +
# 0.3 x2 + N(0, 0.5). Includes a `region` factor + a `year` numeric
# for tests that exercise exact / CEM matching.
make_match_df <- function(n = 300, tau = 0.4, seed = 1) {
  set.seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  d  <- stats::rbinom(n, 1, stats::plogis(0.4 * x1 + 0.3 * x2))
  y  <- tau * d + 0.5 * x1 + 0.3 * x2 + stats::rnorm(n, sd = 0.5)
  data.frame(d = d, y = y, x1 = x1, x2 = x2,
             region = sample(c("A", "B", "C"), n, replace = TRUE),
             year   = sample(2018:2020, n, replace = TRUE))
}

# Exactly-balanced variant: n/2 controls + n/2 treated. Use on the
# happy-path MatchIt tests so we never trigger the "Fewer control
# units than treated" warning. Treatment assignment is independent
# of covariates (random A/B), so the no-confounding ATT is `tau`.
make_match_df_balanced <- function(n = 300L, tau = 0.4, seed = 1L) {
  set.seed(seed)
  n <- 2L * (as.integer(n) %/% 2L)
  x1 <- stats::rnorm(n); x2 <- stats::rnorm(n)
  d <- c(rep(0L, n %/% 2L), rep(1L, n %/% 2L))
  ord <- sample.int(n)
  d <- d[ord]; x1 <- x1[ord]; x2 <- x2[ord]
  y <- tau * d + 0.5 * x1 + 0.3 * x2 + stats::rnorm(n, sd = 0.5)
  data.frame(d = d, y = y, x1 = x1, x2 = x2,
             region = sample(c("A", "B", "C"), n, replace = TRUE),
             year   = sample(2018:2020, n, replace = TRUE))
}

# Skewed variant: ~80% treated, ~20% control -- guaranteed to trigger
# the MatchIt "Fewer control units" warning that
# morie_matching_doubly_robust / morie_matching_cardinality collapse
# into a single summary.
make_match_df_skewed <- function(n = 200L, tau = 0.4, seed = 1L) {
  set.seed(seed)
  x1 <- stats::rnorm(n); x2 <- stats::rnorm(n)
  d <- stats::rbinom(n, 1, 0.80)
  y <- tau * d + 0.5 * x1 + 0.3 * x2 + stats::rnorm(n, sd = 0.5)
  data.frame(d = d, y = y, x1 = x1, x2 = x2,
             region = sample(c("A", "B", "C"), n, replace = TRUE),
             year   = sample(2018:2020, n, replace = TRUE))
}
