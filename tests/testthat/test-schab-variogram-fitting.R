# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spols  -- OLS semivariogram fitting  (Schabenberger & Gotway 2005, Sec 4.5.1)
# spwls  -- Cressie WLS                (Sec 4.5.1, eq 4.34)
# spreml -- REML                       (Sec 4.5.2, eq 4.39)
#
# spols and spwls fit by Gauss-Newton, the algorithm the text names for this
# problem (after 4.43 and after 4.44), with the derivatives of (4.42) taken
# analytically. No library optimiser is involved on either side, so the
# pinned values below are shared with the Python arm to the digits printed.
#
# The fixture is a semivariogram table built FROM the model with a 2 per cent
# deterministic wiggle: the answer is known exactly, there is no RNG, and both
# arms receive byte-identical input. A simulated field is the wrong fixture
# here -- a trended or strongly oscillating one has an unbounded or
# hole-effect variogram that no monotone model fits, and the fit then wanders
# in a degenerate limb of the model.

fit_table <- function() {
  lags <- (1:12) * 0.5
  gamma <- .sp_semivariogram(lags, 0.3, 2.0, 6.0, "exponential") *
    (1 + 0.02 * cos((1:12) * 1))
  list(lag = lags, gamma = gamma,
       n_pairs = c(40, 80, 120, 160, 200, 240, 240, 200, 160, 120, 80, 40))
}

test_that("the analytic Jacobian is the derivative it claims to be", {
  # (4.42) is written in terms of d gamma / d theta, so those derivatives
  # carry the fit. Check them rather than trusting the algebra.
  th <- c(0.3, 2.0, 6.0)
  h <- seq(0.5, 20, length.out = 7)
  for (model in c("exponential", "gaussian", "spherical")) {
    jac <- .schab_semivariogram_jacobian(h, th[1], th[2], th[3], model)
    num <- matrix(0, length(h), 3)
    for (i in 1:3) {
      e <- c(0, 0, 0); e[i] <- 1e-7
      num[, i] <- (.sp_semivariogram(h, th[1]+e[1], th[2]+e[2], th[3]+e[3], model) -
                   .sp_semivariogram(h, th[1]-e[1], th[2]-e[2], th[3]-e[3], model)) / 2e-7
    }
    expect_lt(max(abs(jac - num)), 1e-6)
  }
})

test_that("the OLS objective is the plain residual sum of squares", {
  ev <- fit_table()
  f <- .schab_objective("ols", ev$lag, ev$gamma, ev$n_pairs, "exponential")
  fitted <- .sp_semivariogram(ev$lag, 0.3, 2.0, 6.0, "exponential")
  expect_equal(f(c(0.3, 2.0, 6.0)), sum((ev$gamma - fitted)^2), tolerance = 1e-12)
})

test_that("the WLS objective is equation 4.34", {
  ev <- fit_table()
  f <- .schab_objective("wls", ev$lag, ev$gamma, ev$n_pairs, "exponential")
  fitted <- .sp_semivariogram(ev$lag, 0.3, 2.0, 6.0, "exponential")
  alt <- 0.5 * sum(ev$n_pairs * (ev$gamma / fitted - 1)^2)
  expect_equal(f(c(0.3, 2.0, 6.0)), alt, tolerance = 1e-12)
})

test_that("the fits recover the parameters the table was built from", {
  ev <- fit_table()
  for (r in list(spols(ev, "exponential"), spwls(ev, "exponential"))) {
    expect_equal(r$nugget, 0.3, tolerance = 0.2)
    expect_equal(r$partial_sill, 2.0, tolerance = 0.05)
    expect_equal(r$range, 6.0, tolerance = 0.10)
    expect_true(r$converged)
  }
})

test_that("the fit improves on its starting values", {
  # Regression guard: an earlier version stopped after one iteration and
  # reported the starting heuristic as the fit, with success TRUE.
  ev <- fit_table()
  sb <- .schab_start_and_bounds(ev$lag, ev$gamma)
  for (kind in c("ols", "wls")) {
    f <- .schab_objective(kind, ev$lag, ev$gamma, ev$n_pairs, "exponential")
    r <- if (kind == "ols") spols(ev, "exponential") else spwls(ev, "exponential")
    expect_false(isTRUE(all.equal(c(r$nugget, r$partial_sill, r$range), sb$start)))
    expect_lt(r$objective, f(sb$start))
  }
})

test_that("fitted parameters agree with the Python arm", {
  ev <- fit_table()
  o <- spols(ev, "exponential")
  w <- spwls(ev, "exponential")
  expect_equal(o$nugget, 0.300720377187438, tolerance = 1e-11)
  expect_equal(o$partial_sill, 2.00611785482791, tolerance = 1e-11)
  expect_equal(o$range, 6.0714734193828, tolerance = 1e-11)
  expect_equal(w$nugget, 0.292391714297037, tolerance = 1e-11)
  expect_equal(w$partial_sill, 2.00846165741921, tolerance = 1e-11)
  expect_equal(w$range, 5.98562346376273, tolerance = 1e-11)
})

test_that("parameters stay inside the valid space", {
  ev <- fit_table()
  for (r in list(spols(ev, "exponential"), spwls(ev, "exponential"))) {
    expect_gte(r$nugget, 0)
    expect_gte(r$partial_sill, 0)
    expect_gt(r$range, 0)
    expect_equal(r$sill, r$nugget + r$partial_sill)
  }
})

test_that("counts reach WLS and do not leak into OLS", {
  ev <- fit_table()
  skew <- ev
  half <- seq_len(length(ev$n_pairs) %/% 2)
  skew$n_pairs[half] <- skew$n_pairs[half] * 40
  expect_false(isTRUE(all.equal(spwls(ev, "exponential")$range,
                                spwls(skew, "exponential")$range)))
  expect_equal(spols(ev, "exponential")$range,
               spols(skew, "exponential")$range, tolerance = 1e-9)
})

# ---------------------------------------------------------------- spreml ---

reml_sites <- function() {
  g <- (0:8) / 1.2
  as.matrix(expand.grid(x = g, y = g))
}

test_that("the K-free form differs from the K form by a constant", {
  # Harville (1977), quoted in Sec 5.5.3: admissible choices of K change the
  # objective only by an amount free of theta and beta. So the gap between
  # the K-free objective and the explicit K form must be the SAME number at
  # every theta -- that is what licenses dropping K, and it is checkable.
  C <- reml_sites()
  n <- nrow(C)
  S <- .schab_covariance_matrix(C, 0.3, 2.0, 3.0, "exponential")
  z <- 5 + as.numeric(t(chol(S + 1e-10 * diag(n))) %*% (cos((1:n) * 1.7) * sqrt(2)))
  X <- matrix(1, nrow = n, ncol = 1)
  K <- .schab_error_contrasts(X)
  diffs <- numeric(0)
  for (p in list(c(0.10, 5), c(0.35, 9), c(0.02, 14))) {
    res <- .schab_profiled_reml(C, z, X, p[1], p[2], "exponential")
    Sig <- res$sigma2 * .schab_correlation_matrix(C, p[1], p[2], "exponential")$sigma
    M <- K %*% Sig %*% t(K); KZ <- as.numeric(K %*% z)
    kform <- 2 * sum(log(diag(chol(M)))) + nrow(K) * log(2 * pi) +
      sum(KZ * solve(M, KZ))
    diffs <- c(diffs, kform - res$value)
  }
  expect_lt(max(diffs) - min(diffs), 1e-8)
})

test_that("the REML gradient is the derivative it claims to be", {
  C <- reml_sites()
  n <- nrow(C)
  S <- .schab_covariance_matrix(C, 0.3, 2.0, 3.0, "exponential")
  z <- 5 + as.numeric(t(chol(S + 1e-10 * diag(n))) %*% (cos((1:n) * 1.7) * sqrt(2)))
  X <- matrix(1, nrow = n, ncol = 1)
  xi <- 0.2; a <- 7
  g <- .schab_profiled_reml(C, z, X, xi, a, "exponential")$gradient
  num <- numeric(2)
  for (j in 1:2) {
    d <- c(0, 0); d[j] <- 1e-6
    vp <- .schab_profiled_reml(C, z, X, xi + d[1], a + d[2], "exponential")$value
    vm <- .schab_profiled_reml(C, z, X, xi - d[1], a - d[2], "exponential")$value
    num[j] <- (vp - vm) / (2 * sum(d))
  }
  expect_lt(max(abs(g - num) / pmax(abs(num), 1e-12)), 1e-6)
})

test_that("REML agrees with the Python arm on a shared fixture", {
  # Parity only. The sequence driving this field is deterministic but NOT
  # white, so the fit has no reason to recover the generating parameters and
  # this must not be read as a recovery check.
  C <- reml_sites()
  n <- nrow(C)
  S <- .schab_covariance_matrix(C, 0.3, 2.0, 3.0, "exponential")
  z <- 5 + as.numeric(t(chol(S + 1e-10 * diag(n))) %*% (cos((1:n) * 1.7) * sqrt(2)))
  r <- spreml(C, z, NULL, "exponential")
  expect_equal(r$nugget, 0.940112416310311, tolerance = 1e-10)
  expect_equal(r$nugget_ratio, 0.816204550875122, tolerance = 1e-10)
  expect_equal(r$neg2_restricted_loglik, 82.7313713706666, tolerance = 1e-11)
  expect_equal(r$n_contrasts, n - 1L)
})

test_that("REML parameters stay inside the valid space", {
  C <- reml_sites()
  n <- nrow(C)
  S <- .schab_covariance_matrix(C, 0.3, 2.0, 3.0, "exponential")
  z <- 5 + as.numeric(t(chol(S + 1e-10 * diag(n))) %*% (cos((1:n) * 1.7) * sqrt(2)))
  r <- spreml(C, z, NULL, "exponential")
  expect_gte(r$nugget, 0)
  expect_gte(r$partial_sill, 0)
  expect_gt(r$range, 0)
  expect_gte(r$nugget_ratio, 0)
  expect_lte(r$nugget_ratio, 1)
})

test_that("the fitting family rejects bad input", {
  expect_error(spols(matrix(1, nrow = 2, ncol = 1), "exponential"))
  expect_error(spwls(list(lag = 1, gamma = 1, n_pairs = 1), "exponential"))
})
