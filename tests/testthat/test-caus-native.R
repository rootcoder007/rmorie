# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the instrumental-variables and
# modern-causal shelf. Anchors printed from the Python modules at
# full double precision -- testthat tolerances are RELATIVE, so a
# rounded anchor silently weakens the test.
#
# The fixture is an explicit linear congruential generator pushed
# through the normal quantile, so R and Python see identical numbers.
#
# One deliberate difference: the Sun-Abraham panel is indexed from 1
# in R and from 0 in Python, so its cohort arguments differ by one.
# The estimator is the same and the properties asserted are the same;
# only the calendar labels shift, and pretending otherwise by
# off-by-one-ing the R code would be worse than saying so.

caus_unif <- function(n, s = 777) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  u
}

caus_fixture <- function(n = 4000L, s = 777) stats::qnorm(caus_unif(n, s))

test_that("the fixture matches the one Python anchored against", {
  expect_equal(caus_fixture()[1:3],
               c(0.09337256867219569, 0.6915577231795447,
                 0.9857062862208894), tolerance = 1e-12)
})

iv_data <- function() {
  z <- caus_fixture()
  n <- 1000L
  Z <- matrix(z[1:n], ncol = 1L)
  u <- z[(n + 1):(2 * n)]
  D <- Z[, 1L] * 1.2 + u + z[(2 * n + 1):(3 * n)]
  y <- 2 * D + u + z[(3 * n + 1):(4 * n)]
  list(y = y, D = D, Z = Z, n = n)
}

test_that("morie_caus_iv_2sls matches morie.fn.causiv2sls", {
  d <- iv_data()
  o <- morie_caus_iv_2sls(d$y, d$D, d$Z)
  expect_equal(o$beta, c(0.038260045087011675, 2.057804233978858),
               tolerance = 1e-10)
  expect_equal(o$se, c(0.04299129395283615, 0.03506482124848771),
               tolerance = 1e-10)
  expect_equal(o$first_stage_F, 689.9479594582555, tolerance = 1e-8)
  expect_false(o$overidentified)
  expect_equal(o$n_overid_restrictions, 0L)
  expect_null(o$sargan)
  # residuals use the ORIGINAL X, not the first-stage fit
  X <- cbind(1, d$D)
  expect_equal(o$residuals, as.numeric(d$y - X %*% o$beta), tolerance = 1e-12)
})

test_that("the k-class specialises to least squares and 2SLS", {
  d <- iv_data()
  X <- cbind(1, d$D)
  Zf <- cbind(1, d$Z)
  ols <- as.numeric(qr.coef(qr(X), d$y))
  expect_equal(.caus_k_class(d$y, X, Zf, 0), ols, tolerance = 1e-10)
  tsls <- morie_caus_iv_2sls(d$y, d$D, d$Z)$beta
  expect_equal(.caus_k_class(d$y, X, Zf, 1), tsls, tolerance = 1e-10)
  # and least squares really is biased here, which is the point
  expect_gt(abs(ols[2L] - 2), abs(tsls[2L] - 2))
})

test_that("2SLS refuses an underidentified model", {
  # the order condition is arithmetic, so failing it must be an
  # error rather than a pseudo-inverse quietly returning something
  z <- caus_fixture()
  X <- matrix(z[1:600], ncol = 3L)
  expect_error(morie_caus_iv_2sls(z[601:800], X,
                                  matrix(z[801:1000], ncol = 1L)),
               "order condition")
})

test_that("morie_caus_iv_liml matches Python and equals 2SLS when just identified", {
  d <- iv_data()
  o <- morie_caus_iv_liml(d$y, d$D, d$Z)
  expect_equal(o$beta, c(0.038260045087011516, 2.0578042339788625),
               tolerance = 1e-10)
  expect_equal(o$kappa, 0.9999999999999918, tolerance = 1e-9)
  expect_true(o$just_identified)
  expect_true(o$equals_2sls)
  # kappa = 1 means the k-class collapses to 2SLS exactly
  expect_equal(o$beta, morie_caus_iv_2sls(d$y, d$D, d$Z)$beta,
               tolerance = 1e-10)
  # the constant is exogenous and must be kept out of the ratio: if
  # it leaked in, M_W would annihilate it exactly, kappa would be 0,
  # and the k-class estimator would silently become least squares
  expect_false(1L %in% o$endogenous_columns)
  expect_equal(o$endogenous_columns, 2L)
})

test_that("LIML kappa exceeds one when overidentified", {
  z <- caus_fixture(8000L)
  n <- 800L
  Z <- matrix(z[1:(4 * n)], ncol = 4L)
  u <- z[(4 * n + 1):(5 * n)]
  D <- as.numeric(Z %*% c(1.2, 0.9, 0.6, 0.4)) + u + z[(5 * n + 1):(6 * n)]
  y <- 2 * D + u + z[(6 * n + 1):(7 * n)]
  o <- morie_caus_iv_liml(y, D, Z)
  expect_gt(o$kappa, 1)
  expect_false(o$just_identified)
  expect_equal(o$n_overid_restrictions, 3L)
  expect_equal(o$beta[2L], 2, tolerance = 0.15)
  # Fuller shifts kappa down by exactly a/(n - m)
  f <- morie_caus_iv_liml(y, D, Z, fuller = 1)
  expect_equal(f$kappa, o$kappa - 1 / (n - 5L), tolerance = 1e-9)
  expect_error(morie_caus_iv_liml(y, D, Z, fuller = -1), "non-negative")
})

test_that("morie_caus_iv_late matches morie.fn.causivla", {
  u <- caus_unif(600L, 31)
  Zb <- as.numeric(u[1:200] < 0.5)
  tt <- u[201:400]
  Db <- ifelse(tt < 0.3, 1, ifelse(tt < 0.6, 0, Zb))
  yb <- Db * 3 + stats::qnorm(u[401:600])
  o <- morie_caus_iv_late(yb, Db, Zb)
  expect_equal(o$late, 3.2760489794765073, tolerance = 1e-10)
  expect_equal(o$se, 0.43531631029846946, tolerance = 1e-9)
  expect_equal(o$first_stage, 0.33253205128205127, tolerance = 1e-12)
  expect_equal(o$complier_share, o$first_stage)
  expect_true(grepl("COMPLIERS", o$estimand))
})

test_that("LATE validates its inputs and refuses a dead first stage", {
  u <- caus_unif(600L, 41)
  Z <- as.numeric(u[1:200] < 0.5)
  D <- as.numeric(u[201:400] < 0.5)   # independent of Z by construction
  y <- stats::qnorm(u[401:600])
  expect_error(morie_caus_iv_late(y, D * 2, Z), "binary")
  # a Wald ratio with literally no first stage is 0/0
  expect_error(morie_caus_iv_late(y, Z, Z * 0 + 1), "at least 2 observations")
})

test_that("the DAG estimator is arithmetically the LATE", {
  # same number, different assumption set; a discrepancy would mean
  # one of them is wrong
  u <- caus_unif(600L, 31)
  Zb <- as.numeric(u[1:200] < 0.5)
  tt <- u[201:400]
  Db <- ifelse(tt < 0.3, 1, ifelse(tt < 0.6, 0, Zb))
  yb <- Db * 3 + stats::qnorm(u[401:600])
  a <- morie_caus_iv_late(yb, Db, Zb)
  b <- morie_caus_iv_dag(yb, Db, Zb)
  expect_equal(b$beta, a$late, tolerance = 1e-14)
  expect_equal(b$se, a$se, tolerance = 1e-14)
  expect_false(b$homogeneous_asserted)
  expect_true(grepl("compliers", b$estimand))
  hom <- morie_caus_iv_dag(yb, Db, Zb, homogeneous = TRUE)
  expect_equal(hom$beta, b$beta, tolerance = 1e-14)
  expect_true(grepl("average treatment effect", hom$estimand))
  expect_equal(b$testable, "relevance")
  expect_true(all(c("exclusion", "exchangeability") %in% b$untestable))
})

test_that("morie_caus_aipw matches morie.fn.causaipw", {
  z <- caus_fixture()
  n <- 300L
  ps <- 1 / (1 + exp(-z[1:n]))
  T <- as.numeric(caus_unif(n, 55) < ps)
  m0 <- z[(n + 1):(2 * n)]
  m1 <- m0 + 2
  yy <- ifelse(T == 1, m1, m0) + z[(2 * n + 1):(3 * n)]
  o <- morie_caus_aipw(yy, T, ps, m1, m0)
  expect_equal(o$ate, 2.168431751084263, tolerance = 1e-10)
  expect_equal(o$se, 0.15427925116272248, tolerance = 1e-10)
  expect_equal(o$regression_component, 2, tolerance = 1e-12)
  expect_equal(o$ate, o$regression_component + o$augmentation_component,
               tolerance = 1e-12)
})

test_that("AIPW survives either nuisance being wrong but not both", {
  # the defining property, in all four combinations
  z <- caus_fixture(8000L)
  n <- 2000L
  X <- cbind(z[1:n], z[(n + 1):(2 * n)])
  lp <- as.numeric(X %*% c(0.8, -0.5))
  e <- 1 / (1 + exp(-lp))
  T <- as.numeric(caus_unif(n, 91) < e)
  m0 <- as.numeric(X %*% c(1, 0.5))
  m1 <- m0 + 2
  y <- ifelse(T == 1, m1, m0) + 0.5 * z[(2 * n + 1):(3 * n)]
  bad_e <- rep(0.5, n)
  bad_m <- rep(0, n)
  both <- morie_caus_aipw(y, T, e, m1, m0)$ate
  ps_bad <- morie_caus_aipw(y, T, bad_e, m1, m0)$ate
  out_bad <- morie_caus_aipw(y, T, e, bad_m, bad_m)$ate
  all_bad <- morie_caus_aipw(y, T, bad_e, bad_m, bad_m)$ate
  expect_equal(both, 2, tolerance = 0.1)
  expect_equal(ps_bad, 2, tolerance = 0.1)
  expect_equal(out_bad, 2, tolerance = 0.2)
  expect_gt(abs(all_bad - 2), max(abs(ps_bad - 2), abs(out_bad - 2)))
})

test_that("AIPW guards the propensity boundary", {
  z <- caus_fixture()
  n <- 300L
  e <- 1 / (1 + exp(-z[1:n]))
  T <- as.numeric(caus_unif(n, 55) < e)
  m0 <- z[(n + 1):(2 * n)]
  y <- ifelse(T == 1, m0 + 2, m0)
  expect_error(morie_caus_aipw(y, T, rep(0, n), m0 + 2, m0, trim = 0),
               "divides by zero")
  expect_error(morie_caus_aipw(y, T, e * 3, m0 + 2, m0), "\\[0, 1\\]")
  e2 <- e
  e2[1:20] <- 1e-6
  o <- morie_caus_aipw(y, T, e2, m0 + 2, m0, trim = 0.01)
  expect_gte(o$n_trimmed, 20L)
  expect_true(is.finite(o$ate))
})

test_that("morie_caus_dml_partial_lin recovers theta and cross-fits", {
  z <- caus_fixture(12000L)
  n <- 1000L
  p <- 8L
  X <- matrix(z[1:(n * p)], ncol = p)
  g <- as.numeric(X %*% rep(c(0.5, -0.3), length.out = p))
  D <- g + z[(n * p + 1):(n * p + n)]
  y <- 1.5 * D + g + z[(n * p + n + 1):(n * p + 2 * n)]
  o <- morie_caus_dml_partial_lin(y, D, X, n_folds = 5L, seed = 1)
  expect_equal(o$theta, 1.5, tolerance = 0.1)
  expect_true(o$cross_fitted)
  expect_equal(o$n_folds, 5L)
  expect_gt(o$ci[2L], o$ci[1L])
  # residualising only Y and regressing on raw D is NOT orthogonal
  half <- sum(o$y_residual * D) / sum(D^2)
  expect_lt(abs(o$theta - 1.5), abs(half - 1.5))
  expect_error(morie_caus_dml_partial_lin(y, D, X, n_folds = 1L),
               "n_folds must lie")
})

test_that("DML does not leak the global RNG stream", {
  z <- caus_fixture()
  X <- matrix(z[1:600], ncol = 3L)
  D <- z[601:800]
  y <- 1.5 * D + z[801:1000]
  set.seed(2024)
  before <- stats::runif(3L)
  set.seed(2024)
  invisible(morie_caus_dml_partial_lin(y, D, X, n_folds = 3L, seed = 1))
  expect_equal(stats::runif(3L), before, tolerance = 1e-12)
})

sa_panel <- function() {
  # periods indexed from 1 here; cohorts treated at period 4 and 7.
  # Every true effect is POSITIVE and grows with relative time.
  z <- caus_fixture()
  Tn <- 10L
  G <- c(rep(4, 150), rep(7, 150), rep(Inf, 200))
  n <- length(G)
  Y <- matrix(0, n, Tn)
  pt <- seq(0, 1, length.out = Tn)
  for (i in seq_len(n)) {
    Y[i, ] <- z[i] + pt + 0.1 * z[(500 + i):(500 + i + Tn - 1L)]
    if (is.finite(G[i])) {
      g <- G[i]
      for (t in g:Tn) {
        Y[i, t] <- Y[i, t] + (if (g == 4) 5 else 1) * (1 + 0.5 * (t - g))
      }
    }
  }
  list(Y = Y, G = G)
}

test_that("Sun-Abraham recovers the share-weighted cohort effect", {
  # equal cohorts, so the correct aggregate is the average of
  # 5(1 + l/2) and 1(1 + l/2): 3 at l = 0, 4.5 at l = 1, 6 at l = 2
  d <- sa_panel()
  o <- morie_caus_did_sun_abraham(d$Y, d$G, rel_periods = c(-2, -1, 0, 1, 2))
  mu <- stats::setNames(o$mu, as.character(o$rel_periods))
  expect_equal(unname(mu["0"]), 3, tolerance = 0.15)
  expect_equal(unname(mu["1"]), 4.5, tolerance = 0.2)
  expect_equal(unname(mu["2"]), 6, tolerance = 0.25)
  expect_equal(unname(mu["-1"]), 0, tolerance = 1e-12)
  expect_lt(abs(unname(mu["-2"])), 0.15)
})

test_that("the two-way fixed-effects event study gets the sign wrong", {
  # Sun and Abraham's central warning, reproduced: every true cohort
  # effect is positive and growing, and the TWFE coefficients come
  # out NEGATIVE. That is not a small bias, it is the wrong sign.
  d <- sa_panel()
  o <- morie_caus_did_sun_abraham(d$Y, d$G, rel_periods = c(0, 1, 2))
  expect_true(all(o$mu > 0))
  expect_true(all(o$naive_twfe < 0))
})

test_that("the interaction weights are shares", {
  # non-negative and summing to one at every relative time, which is
  # exactly what the TWFE weights are not and why this estimator
  # cannot invert a sign
  d <- sa_panel()
  o <- morie_caus_did_sun_abraham(d$Y, d$G, rel_periods = c(0, 1, 2, 3))
  expect_true(o$weights_nonnegative)
  expect_true(o$weights_sum_to_one)
  expect_true(all(o$weights >= 0))
  live <- colSums(o$weights) > 0
  expect_equal(colSums(o$weights)[live], rep(1, sum(live)),
               tolerance = 1e-9)
  expect_equal(o$cohorts, c(4, 7))
  expect_equal(o$n_never_treated, 200L)
})

test_that("Sun-Abraham validates its control group", {
  d <- sa_panel()
  allt <- ifelse(is.finite(d$G), d$G, 4)
  expect_error(morie_caus_did_sun_abraham(d$Y, allt, control = "never"),
               "no never-treated")
  o <- morie_caus_did_sun_abraham(d$Y, allt, rel_periods = c(0, 1),
                                  control = "notyet")
  expect_equal(o$control_group, "notyet")
  expect_true(all(is.finite(o$mu)))
  expect_error(morie_caus_did_sun_abraham(d$Y, d$G, control = "magic"),
               "control must")
})
