# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Elements of Statistical Learning
# shelf. Anchors printed from the Python modules at full double
# precision -- testthat tolerances are RELATIVE, so a rounded anchor
# silently weakens the test.
#
# The fixture is an explicit linear congruential generator pushed
# through the normal quantile, so R and Python see bit-identical
# numbers. The two resampling estimators (bootstrap error, random
# forest) draw from the language's own RNG and therefore cannot be
# anchored exactly across languages; they are tested against the
# structural facts the book states about them, which is the stronger
# check in any case.

esl_fixture <- function(n = 400L, s = 2024) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  stats::qnorm(u)
}

test_that("the fixture matches the one Python anchored against", {
  z <- esl_fixture()
  expect_equal(z[1:3], c(-2.044054423671674, 1.0938408101620691,
                         0.4373658471384318), tolerance = 1e-12)
})

test_that("morie_esl_residual_variance matches morie.fn.eslsig", {
  z <- esl_fixture()
  # numpy reshape fills rows; matrix() fills columns
  X <- matrix(z[1:240], nrow = 80L, byrow = TRUE)
  y <- as.numeric(X %*% c(1, -2, 0.5)) + 3 + z[241:320]
  o <- morie_esl_residual_variance(X, y)
  expect_equal(o$value, 0.9111372771877517, tolerance = 1e-10)
  expect_equal(o$rss, 69.24643306626913, tolerance = 1e-10)
  expect_equal(o$mle_variance, 0.8655804133283642, tolerance = 1e-10)
  expect_equal(o$df, 76L)
  expect_equal(o$p, 3L)
  # (3.8): the denominator is N - p - 1, and the MLE differs from it
  # by exactly that ratio
  expect_equal(o$mle_variance / o$value, 76 / 80, tolerance = 1e-12)
  expect_equal(o$bias_factor, 76 / 80, tolerance = 1e-12)
  expect_false(o$intercept_in_X)
})

test_that("morie_esl_residual_variance counts the intercept once", {
  z <- esl_fixture()
  # numpy reshape fills rows; matrix() fills columns
  X <- matrix(z[1:240], nrow = 80L, byrow = TRUE)
  y <- as.numeric(X %*% c(1, -2, 0.5)) + 3 + z[241:320]
  bare <- morie_esl_residual_variance(X, y)
  withone <- morie_esl_residual_variance(cbind(1, X), y)
  expect_true(withone$intercept_in_X)
  expect_equal(bare$df, withone$df)
  expect_equal(bare$value, withone$value, tolerance = 1e-12)
})

test_that("morie_esl_residual_variance refuses an undefined estimate", {
  z <- esl_fixture(40L)
  # N = 5, p = 4 leaves zero residual degrees of freedom and (3.8)
  # divides by it; returning Inf would be worse than refusing
  expect_error(morie_esl_residual_variance(matrix(z[1:20], 5L), z[21:25]),
               "N > p \\+ 1")
  expect_equal(morie_esl_residual_variance(matrix(z[1:24], 6L), z[25:30])$df,
               1L)
})

test_that("morie_esl_kernel_density matches morie.fn.eslkrn", {
  z <- esl_fixture()
  g <- seq(-3, 3, length.out = 7L)
  o <- morie_esl_kernel_density(g, z[1:200], 0.5)
  expect_equal(o$density,
               c(0.004768100122862177, 0.058547794151972976,
                 0.25782224731878517, 0.3754535046987977,
                 0.23235172640669335, 0.05837072539526031,
                 0.011212784881818993), tolerance = 1e-10)
  expect_equal(o$normaliser, 250.66282746310003, tolerance = 1e-12)
  expect_equal(morie_esl_kernel_density(g, z[1:200])$lambda,
               0.343491445045094, tolerance = 1e-12)
  expect_true(o$is_convolution)
  expect_equal(o$p, 1L)
})

test_that("the kernel density really is the convolution (6.23) claims", {
  z <- esl_fixture()
  g <- seq(-4, 4, length.out = 300L)
  o <- morie_esl_kernel_density(g, z[1:200], 0.4)
  direct <- vapply(g, function(t) {
    mean(stats::dnorm(t - z[1:200], sd = 0.4))
  }, numeric(1))
  expect_equal(o$density, direct, tolerance = 1e-12)
  expect_true(all(o$density >= 0))
  expect_equal(o$mass, 1, tolerance = 1e-3)
  expect_error(morie_esl_kernel_density(g, z[1:50], -1), "positive")
})

test_that("the normaliser exponent is p/2, not p", {
  # (6.24). In two dimensions an exponent of p rather than p/2 leaves
  # the estimate short by a factor of 2 lambda^2 pi, which only a
  # mass check catches
  z <- esl_fixture()
  D <- cbind(z[1:200], z[201:400])
  lam <- 0.6
  ax <- seq(-4, 4, length.out = 60L)
  G <- as.matrix(expand.grid(ax, ax))
  o <- morie_esl_kernel_density(G, D, lam)
  expect_equal(o$p, 2L)
  expect_equal(o$normaliser, 200 * (2 * lam^2 * pi)^1, tolerance = 1e-12)
  step <- ax[2L] - ax[1L]
  expect_equal(sum(o$density) * step^2, 1, tolerance = 0.05)
  expect_null(o$mass)   # only defined on an ordered 1-D grid
})

test_that("morie_esl_oob_632 matches morie.fn.eslo63", {
  o <- morie_esl_oob_632(0.2, 0.5, gamma = 0.9)
  expect_equal(o$err_632, 0.3896, tolerance = 1e-12)
  expect_equal(o$err_632_plus, 0.4251017639077341, tolerance = 1e-12)
  expect_equal(o$weight, 0.7503392130257802, tolerance = 1e-12)
  expect_equal(o$relative_overfitting_rate, 0.4285714285714286,
               tolerance = 1e-12)
  expect_true(o$uses_leave_one_out)
})

test_that("the .632 estimator reproduces the book's worked failure", {
  # ESL p.252 verbatim: a 1-nearest-neighbour rule on two equal
  # classes with labels independent of the inputs gives err_bar = 0
  # and Err^(1) = 0.5, so Err^(.632) = .632 x 0.5 = 0.316 while the
  # true error rate is 0.5. With gamma = 0.5 the relative
  # overfitting rate is 1, the weight is 1, and .632+ returns 0.5.
  o <- morie_esl_oob_632(0, 0.5, gamma = 0.5)
  expect_equal(o$err_632, 0.316, tolerance = 1e-12)
  expect_equal(o$relative_overfitting_rate, 1, tolerance = 1e-12)
  expect_equal(o$weight, 1, tolerance = 1e-12)
  expect_equal(o$err_632_plus, 0.5, tolerance = 1e-12)
})

test_that("the .632+ weight runs from .632 to 1 and brackets the estimate", {
  et <- 0.2
  e1 <- 0.5
  plain <- morie_esl_oob_632(et, e1)$err_632
  expect_null(morie_esl_oob_632(et, e1)$err_632_plus)
  for (gam in c(0.5, 0.7, 1, 3)) {
    o <- morie_esl_oob_632(et, e1, gamma = gam)
    expect_gte(o$weight, 0.632 - 1e-12)
    expect_lte(o$weight, 1 + 1e-12)
    expect_gte(o$err_632_plus, min(plain, e1) - 1e-12)
    expect_lte(o$err_632_plus, max(plain, e1) + 1e-12)
  }
  # with no overfitting at all the weight collapses to .632 exactly
  expect_equal(morie_esl_oob_632(et, e1, gamma = 1e9)$weight, 0.632,
               tolerance = 1e-6)
})

test_that("gamma from the double sum and the dichotomous formula agree", {
  # (7.58) and (7.59) are the same quantity
  z <- esl_fixture()
  y <- as.numeric(z[1:200] > 0.5)
  yhat <- as.numeric(z[201:400] > 0.1)
  p1 <- mean(y)
  q1 <- mean(yhat)
  a <- morie_esl_oob_632(0.1, 0.2, y = y, y_pred = yhat)$gamma
  b <- morie_esl_oob_632(0.1, 0.2, p1 = p1, q1 = q1)$gamma
  expect_equal(a, b, tolerance = 1e-12)
  expect_equal(a, p1 * (1 - q1) + (1 - p1) * q1, tolerance = 1e-12)
})

test_that("morie_esl_bootstrap_err puts Err_boot below the honest estimate", {
  # (7.54) tests on points it also trained on, so it must land below
  # (7.56), which does not -- and both above the training error.
  # Language RNGs differ, so this is the structural claim rather than
  # an anchor.
  z <- esl_fixture()
  # numpy reshape fills rows; matrix() fills columns
  X <- matrix(z[1:240], nrow = 80L, byrow = TRUE)
  y <- as.numeric(X %*% c(1, -1, 0.5)) + 0.5 * z[241:320]
  o <- morie_esl_bootstrap_err(X, y, B = 80, seed = 5)
  expect_lt(o$err_train, o$err_boot)
  expect_lt(o$err_boot, o$err_loo_boot)
  expect_true(o$optimistic)
  expect_equal(o$n_dropped, 0L)
  expect_equal(o$inclusion_probability, 1 - (1 - 1 / 80)^80,
               tolerance = 1e-12)
  expect_equal(o$B, 80L)
})

test_that("feeding Err_boot to the .632 estimator makes it worse", {
  # the distinction the section is about: err_boot is already biased
  # downward, so correcting it downward again compounds the error
  z <- esl_fixture()
  # numpy reshape fills rows; matrix() fills columns
  X <- matrix(z[1:240], nrow = 80L, byrow = TRUE)
  y <- as.numeric(X %*% c(1, -1, 0.5)) + 0.5 * z[241:320]
  b <- morie_esl_bootstrap_err(X, y, B = 80, seed = 5)
  right <- morie_esl_oob_632(b$err_train, b$err_loo_boot)$err_632
  wrong <- morie_esl_oob_632(b$err_train, b$err_boot)$err_632
  expect_lt(wrong, right)
})

test_that("morie_esl_random_forest uses the regression mtry rule", {
  # floor(p/3) for regression, floor(sqrt(p)) for classification.
  # They cross at p = 9, where both give 3; above it the regression
  # rule is the larger of the two.
  expect_equal(.esl_mtry(9L), 3L)
  expect_equal(.esl_mtry(9L, "classification"), 3L)
  for (p in c(12L, 30L, 100L)) {
    expect_gt(.esl_mtry(p), .esl_mtry(p, "classification"))
  }
  for (p in c(2L, 4L, 8L)) {
    expect_gte(.esl_mtry(p, "classification"), .esl_mtry(p))
  }
  expect_equal(.esl_mtry(100L), 33L)
  expect_equal(.esl_mtry(100L, "classification"), 10L)
  expect_error(.esl_mtry(5L, "clustering"), "regression")
  z <- esl_fixture()
  X <- matrix(z[1:360], ncol = 6L)
  expect_equal(morie_esl_random_forest(X, z[1:60], B = 8L)$mtry, 2L)
})

test_that("out-of-bag error exceeds training error and still beats the mean", {
  # OOB predictions come only from trees that never saw the
  # observation, so they must be worse than the in-bag fit. If they
  # are not, the out-of-bag bookkeeping is wrong.
  z <- esl_fixture(1200L)
  X <- matrix(z[1:750], ncol = 5L)
  y <- sin(2 * X[, 1L]) + X[, 2L]^2 - X[, 3L] + 0.3 * z[751:900]
  o <- morie_esl_random_forest(X, y, B = 40L, seed = 1)
  expect_gt(o$oob_mse, o$train_mse)
  expect_lt(o$oob_mse, stats::var(y))
  expect_equal(o$n_oob_missing, 0L)
  expect_equal(o$subset_drawn_per, "node")
})

test_that("averaging more trees reduces prediction variance", {
  # Ch. 15's thesis: bagged trees are identically distributed, so
  # averaging cannot move the bias -- the only gain is variance
  # reduction, and it must show as a more stable prediction.
  z <- esl_fixture(1000L)
  X <- matrix(z[1:600], ncol = 4L)
  y <- 2 * X[, 1L] - X[, 2L] + 0.5 * z[601:750]
  grid <- matrix(z[751:830], ncol = 4L)
  spread <- vapply(c(1L, 30L), function(B) {
    preds <- vapply(1:5, function(s) {
      morie_esl_random_forest(X, y, B = B, newdata = grid, seed = s)$prediction
    }, numeric(nrow(grid)))
    mean(apply(preds, 1L, stats::var))
  }, numeric(1))
  expect_lt(spread[2L], spread[1L] / 3)
})

test_that("morie_esl_random_forest is reproducible and validates inputs", {
  z <- esl_fixture(600L)
  X <- matrix(z[1:240], ncol = 4L)
  y <- X[, 1L] + 0.4 * z[241:300]
  a <- morie_esl_random_forest(X, y, B = 10L, seed = 7)$prediction
  b <- morie_esl_random_forest(X, y, B = 10L, seed = 7)$prediction
  expect_equal(a, b)
  expect_false(isTRUE(all.equal(
    a, morie_esl_random_forest(X, y, B = 10L, seed = 8)$prediction)))
  expect_error(morie_esl_random_forest(X, y, B = 5L, mtry = 99L),
               "mtry must lie")
  expect_error(morie_esl_random_forest(X, y, B = 0L), "at least one tree")
  expect_error(morie_esl_random_forest(X, y, B = 5L,
                                       newdata = matrix(z[1:6], ncol = 2L)),
               "columns")
})

test_that("the forest does not leak the global RNG stream", {
  # set.seed inside a modelling function is a trap: it silently
  # reseeds the caller's stream. This one restores it.
  z <- esl_fixture(400L)
  X <- matrix(z[1:120], ncol = 3L)
  y <- X[, 1L] + 0.3 * z[121:160]
  set.seed(99)
  before <- stats::runif(3L)
  set.seed(99)
  invisible(morie_esl_random_forest(X, y, B = 5L, seed = 2))
  expect_equal(stats::runif(3L), before, tolerance = 1e-12)
})
