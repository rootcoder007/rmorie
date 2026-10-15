# SPDX-License-Identifier: AGPL-3.0-or-later
# Native gbm propensities on macOS arm64 differed from x86 Linux in the third decimal: an
# exact tie in split gain (x1 and x2 both 0.05 at k = 10 of a 20-row node) was broken by the
# last bit of sum()/cumsum(), which accumulate in long double on x86 and in double on arm64.
# Gains equal within rounding now keep the first candidate on every platform, as gbm does.

test_that("double-only accumulation (arm64) and long double (x86) grow the same trees", {
  g <- get(".morie_matching_gbm_bernoulli", envir = environment(morie_matching_estimate_propensity))
  dsum <- function(x) { s <- 0; for (v in x) s <- s + v; s }
  dcum <- function(x) Reduce(`+`, x, accumulate = TRUE)
  g_dbl <- g
  body(g_dbl) <- eval(substitute(substitute(B, list(sum = quote(dsum), cumsum = quote(dcum))),
                                 list(B = body(g))))
  environment(g_dbl) <- list2env(list(dsum = dsum, dcum = dcum), parent = environment(g))
  for (sd in c(4, 19, 34)) {
    set.seed(sd)
    d <- data.frame(x1 = stats::rnorm(30), x2 = stats::runif(30))
    d$d <- stats::rbinom(30, 1, stats::plogis(d$x1))
    X <- as.matrix(d[, c("x1", "x2")])
    expect_equal(g_dbl(X, d$d), g(X, d$d), tolerance = 1e-12, info = sprintf("seed %d", sd))
  }
})
