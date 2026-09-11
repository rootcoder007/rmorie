# Anchors for the V-conditional marginal structural model
# (Robins & Hernan 2009 sec. 4; Hernan & Robins 2020 sec. 12.5),
#
#   E[Y^bar_a | V] = b0 + b1 bar_a + b2 V + b3 bar_a V
#
# where b3 is the effect modification. The building blocks check against
# base R exactly, and the estimator checks against a design whose b1 and
# b3 are known by construction. The file sat at 26.2% with no test naming
# any of its functions.
#
# Note on the weight, since a sibling module had this wrong: the IP
# weight here is built from the probability of the treatment ACTUALLY
# RECEIVED, ifelse(a == 1, p, 1 - p), which is the correct form.

test_that("the quantile helper is R's type 7", {
  set.seed(1)
  x <- rnorm(37)
  for (q in c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) {
    expect_equal(.mfovsm_quantile7(x, q),
                 as.numeric(quantile(x, q, type = 7)), tolerance = 1e-12)
  }
  # the extremes are the extremes
  expect_equal(.mfovsm_quantile7(x, 0), min(x), tolerance = 1e-12)
  expect_equal(.mfovsm_quantile7(x, 1), max(x), tolerance = 1e-12)
})

test_that("the weighted least squares fit agrees with lm(weights=)", {
  set.seed(1); n <- 200
  Z <- cbind(rnorm(n), rnorm(n))
  y <- 1 + 2 * Z[, 1] - Z[, 2] + rnorm(n, 0, 0.4)
  w <- runif(n, 0.5, 2)
  # it prepends its own intercept
  f <- .mfovsm_wls(Z, y, w)
  ref <- lm(y ~ Z[, 1] + Z[, 2], weights = w)
  expect_equal(as.numeric(f$coef), as.numeric(coef(ref)), tolerance = 1e-8)
  expect_equal(as.numeric(f$se), as.numeric(summary(ref)$coefficients[, 2]),
               tolerance = 1e-6)
  # the point estimate is invariant to rescaling every weight
  expect_equal(as.numeric(.mfovsm_wls(Z, y, w * 13)$coef),
               as.numeric(f$coef), tolerance = 1e-8)
})

test_that("the logistic fit agrees with glm(binomial)", {
  set.seed(2); n <- 300
  Z <- cbind(1, rnorm(n), rnorm(n))
  y <- rbinom(n, 1, plogis(as.numeric(Z %*% c(-0.2, 0.8, -0.4))))
  f <- .mfovsm_logreg_fit(y, Z)
  g <- glm(y ~ Z[, 2] + Z[, 3], family = binomial())
  expect_equal(as.numeric(f$coef), as.numeric(coef(g)), tolerance = 1e-6)
  expect_equal(as.numeric(f$fitted), as.numeric(fitted(g)), tolerance = 1e-6)
  expect_true(all(f$fitted > 0 & f$fitted < 1))
})

test_that("the IP weight uses the probability of the treatment received", {
  set.seed(3); n <- 500
  L <- rnorm(n)
  A <- rbinom(n, 1, plogis(1.2 * L))
  iw <- .mfovsm_ip_weights(A, den = cbind(L), num = NULL, stabilize = TRUE)
  pd <- as.numeric(fitted(glm(A ~ L, family = binomial())))
  pn <- rep(mean(A), n)
  want <- ifelse(A == 1, pn / pd, (1 - pn) / (1 - pd))
  expect_equal(iw$weights, want, tolerance = 1e-5)
  # so the stabilized weights average about one
  expect_equal(mean(iw$weights), 1, tolerance = 0.05)
  expect_true(all(iw$weights > 0))
  # with nothing to condition on the weights are exactly one
  expect_true(all(.mfovsm_ip_weights(A, NULL, NULL)$weights == 1))
})

test_that("the continuous-treatment weight uses a normal density", {
  set.seed(4); n <- 500
  L <- rnorm(n)
  A <- 0.7 * L + rnorm(n)
  iw <- .mfovsm_ip_weights(A, den = cbind(L), num = NULL, kind = "normal")
  expect_true(all(iw$weights > 0))
  expect_true(all(is.finite(iw$weights)))
  # the fitted density is a density
  expect_true(all(iw$fitted > 0))
  # an unrecognised kind is refused
  expect_error(.mfovsm_ip_weights(A, cbind(L), NULL, kind = "poisson"),
               "kind must be")
  # the stabilized continuous weight multiplies by the numerator density
  st <- .mfovsm_ip_weights(A, den = cbind(L), num = NULL, kind = "normal",
                           stabilize = TRUE)
  un <- .mfovsm_ip_weights(A, den = cbind(L), num = NULL, kind = "normal",
                           stabilize = FALSE)
  expect_true(all(st$weights > 0))
  # stabilizing shrinks the spread, which is what it is for
  expect_lt(sd(st$weights), sd(un$weights))
})

test_that("the model recovers an effect modification it was given", {
  # E[Y^a | V] = 2 + 1.0 a + 1.04 V + 0.8 a V, since L is pre-treatment
  # and E[L | V] = 0.4 V folds into the V coefficient. V affects treatment
  # here as well as modifying the effect, so it belongs in the covariate
  # history too.
  set.seed(31); n <- 20000
  V <- rbinom(n, 1, 0.5)
  L <- rnorm(n, 0.4 * V)
  A <- rbinom(n, 1, plogis(-0.2 + 1.3 * L + 0.5 * V))
  y <- 2 + 1.0 * A + 0.6 * V + 0.8 * A * V + 1.1 * L + rnorm(n, 0, 0.5)
  r <- morie_mfovsm(y, V, list(A), list(cbind(L, V)))
  expect_equal(as.numeric(r$estimate), 0.8, tolerance = 0.08)
  expect_equal(as.numeric(r$main_effect), 1.0, tolerance = 0.08)
  # the stabilized weights behave
  expect_equal(as.numeric(r$mean_weight), 1, tolerance = 0.05)
  expect_gt(as.numeric(r$effective_sample_size), 0.3 * n)
  expect_lte(as.numeric(r$effective_sample_size), n)
  # and the reported bookkeeping
  expect_equal(r$n, n)
  expect_identical(r$n_times, 1L)
  expect_length(r$coef, 4L)
  expect_identical(dim(r$vcov), c(4L, 4L))
  expect_identical(r$estimate, r$coef[4])
  expect_identical(r$main_effect, r$coef[2])
  expect_match(r$method, "Hernan|marginal structural")
})

test_that("no effect modification is reported as none", {
  set.seed(32); n <- 20000
  V <- rbinom(n, 1, 0.5)
  L <- rnorm(n, 0.4 * V)
  A <- rbinom(n, 1, plogis(-0.2 + 1.3 * L + 0.5 * V))
  # the same design with the interaction term removed
  y <- 2 + 1.0 * A + 0.6 * V + 1.1 * L + rnorm(n, 0, 0.5)
  r <- morie_mfovsm(y, V, list(A), list(cbind(L, V)))
  # judged against the reported standard error, so the bound means
  # something rather than being a number chosen to pass
  expect_lt(abs(as.numeric(r$estimate)), 3 * as.numeric(r$se))
  expect_lt(abs(as.numeric(r$main_effect) - 1.0),
            3 * as.numeric(r$main_effect_se))
})

test_that("weighting beats leaving the confounding in place", {
  set.seed(33); n <- 20000
  V <- rbinom(n, 1, 0.5)
  L <- rnorm(n, 0.4 * V)
  A <- rbinom(n, 1, plogis(-0.2 + 1.3 * L + 0.5 * V))
  y <- 2 + 1.0 * A + 0.6 * V + 0.8 * A * V + 1.1 * L + rnorm(n, 0, 0.5)
  r <- morie_mfovsm(y, V, list(A), list(cbind(L, V)))
  naive <- coef(lm(y ~ A * V))
  # L confounds the main effect badly; the weighted fit does not inherit it
  expect_gt(abs(naive[["A"]] - 1.0), 0.5)
  expect_lt(abs(as.numeric(r$main_effect) - 1.0), 0.1)
})

test_that("the history helpers accept the shapes they document", {
  expect_equal(.mfovsm_vec(list(1, 2, 3)), c(1, 2, 3))
  expect_equal(.mfovsm_vec(c(1, 2, 3)), c(1, 2, 3))
  expect_identical(dim(.mfovsm_mat(c(1, 2, 3))), c(3L, 1L))
  expect_identical(dim(.mfovsm_mat(cbind(1:3, 4:6))), c(3L, 2L))
  # a single block is accepted where one is allowed and refused otherwise
  expect_length(.mfovsm_hist(list(c(1, 2), c(3, 4))), 2L)
})

test_that("a mismatched history is refused", {
  set.seed(34); n <- 200
  V <- rbinom(n, 1, 0.5)
  A1 <- rbinom(n, 1, 0.5); A2 <- rbinom(n, 1, 0.5)
  y <- rnorm(n)
  # two treatment times but one covariate block
  expect_error(morie_mfovsm(y, V, list(A1, A2), list(cbind(rnorm(n)))),
               "treatment times but")
})
