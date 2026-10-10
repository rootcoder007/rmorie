# SPDX-License-Identifier: AGPL-3.0-or-later
# Native Markov-switching EM versus MSwM::msmFit(lm(y ~ 1), k = 2,
# sw = c(TRUE, TRUE)).
#
# MSwM starts EM from random smoothed probabilities, the native code from
# deterministic starts, so the two only meet at the EM fixed point. With
# MSwM run to convergence (maxiter = 2000, tol = 1e-12) the observed
# differences are ~1e-11 (log-likelihood) and ~1e-10 (parameters,
# smoothed probabilities); the tolerances 1e-4 (log-likelihood, as
# required) and 1e-5 (parameters) are EM-tolerance headroom.  Regimes
# are matched by ordering on the mean (label switching).

.sim_ms2 <- function(seed, n, mu, sd, P) {
  set.seed(seed)
  s <- integer(n)
  s[1] <- 1L
  for (t in 2:n) s[t] <- sample.int(2L, 1L, prob = P[s[t - 1], ])
  stats::rnorm(n, mu[s], sd[s])
}

test_that("morie_regime_switching reaches MSwM's EM optimum", {
  skip_if_not_installed("MSwM")
  Ptrue <- matrix(c(0.95, 0.05, 0.10, 0.90), 2, byrow = TRUE)
  specs <- list(list(250, c(0, 2), c(0.5, 1.5)),
                list(300, c(0, 1.5), c(0.6, 1.2)),
                list(200, c(-1, 2.5), c(1, 0.7)))
  for (i in seq_along(specs)) {
    sp <- specs[[i]]
    y <- .sim_ms2(10 + i, sp[[1]], sp[[2]], sp[[3]], Ptrue)
    r <- morie_regime_switching(y, k_regimes = 2)
    set.seed(99)
    f <- suppressWarnings(MSwM::msmFit(
      stats::lm(y ~ 1, data = data.frame(y = y)), k = 2, sw = c(TRUE, TRUE),
      control = list(parallelization = FALSE, maxiter = 2000, tol = 1e-12)
    ))
    o <- order(f@Coef[, 1])
    expect_equal(r$loglik, -f@Fit@logLikel, tolerance = 1e-4, scale = 1)
    expect_equal(r$mu, f@Coef[o, 1], tolerance = 1e-5, scale = 1)
    expect_equal(r$sigma, f@std[o], tolerance = 1e-5, scale = 1)
    expect_equal(r$transition, f@transMat[o, o], tolerance = 1e-5, scale = 1)
    expect_equal(r$smoothed_probabilities, f@Fit@smoProb[-1, o],
                 tolerance = 1e-5, scale = 1)
  }
})

test_that("morie_regime_switching matches MSwM's default-control fit", {
  skip_if_not_installed("MSwM")
  y <- .sim_ms2(3, 250, c(0, 2), c(0.5, 1.5),
                matrix(c(0.95, 0.05, 0.10, 0.90), 2, byrow = TRUE))
  set.seed(1)
  f <- suppressWarnings(MSwM::msmFit(
    stats::lm(y ~ 1, data = data.frame(y = y)), k = 2, sw = c(TRUE, TRUE),
    control = list(parallelization = FALSE)
  ))
  r <- morie_regime_switching(y)
  # MSwM's default tol = 1e-8 is relative, so its log-likelihood is
  # within ~1e-6 of the fixed point the native EM converges to.
  expect_equal(r$loglik, -f@Fit@logLikel, tolerance = 1e-4, scale = 1)
})

test_that("morie_regime_switching output is well formed without MSwM", {
  y <- .sim_ms2(4, 200, c(0, 3), c(1, 0.5),
                matrix(c(0.9, 0.1, 0.1, 0.9), 2, byrow = TRUE))
  r <- morie_regime_switching(y)
  expect_named(r, c("mu", "sigma", "transition", "smoothed_probabilities",
                    "loglik", "n", "k_regimes", "method"))
  expect_true(r$mu[1] <= r$mu[2])
  expect_equal(colSums(r$transition), c(1, 1), tolerance = 1e-12)
  expect_equal(dim(r$smoothed_probabilities), c(200L, 2L))
  expect_equal(rowSums(r$smoothed_probabilities), rep(1, 200), tolerance = 1e-10)
  expect_true(is.finite(r$loglik) && r$loglik < 0)
  r3 <- morie_regime_switching(c(stats::rnorm(60, -4), stats::rnorm(60),
                                 stats::rnorm(60, 6, 1.5)), k_regimes = 3)
  expect_length(r3$mu, 3L)
  expect_true(all(diff(r3$mu) > 0))
})
