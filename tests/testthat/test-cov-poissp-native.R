# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/poissp_native.R (Poisson areal regression with a CAR
# random effect, Banerjee, Carlin & Gelfand 2014). The CAR precision
# tau(D - rho W) and the propriety interval are built from the weight
# matrix; with the random effect pinned the fit must agree with
# glm(poisson); the score X'(y - m) is zero at the mode.

.ps_W <- rbind(c(0, 1, 0, 1, 0, 0), c(1, 0, 1, 0, 1, 0), c(0, 1, 0, 0, 0, 1),
               c(1, 0, 0, 0, 1, 0), c(0, 1, 0, 1, 0, 1), c(0, 0, 1, 0, 1, 0))
.ps_y <- c(3, 7, 2, 9, 5, 4)
.ps_E <- c(2, 5, 1.5, 6, 4, 3)
.ps_X <- cbind(z = c(0.2, -0.4, 1, 0.5, -1, 0.1))

test_that("car_precision is tau (D_w - rho W) and validates W", {
  d <- rowSums(.ps_W)
  expect_equal(morie_poissp_car_precision(.ps_W, 2, 0.5), 2 * (diag(d) - 0.5 * .ps_W))
  Q1 <- morie_poissp_car_precision(.ps_W)
  # the intrinsic CAR has Q 1 = 0
  expect_equal(as.numeric(Q1 %*% rep(1, 6)), rep(0, 6), tolerance = 1e-12)
  expect_error(morie_poissp_car_precision(.ps_W, tau = 0), "tau must be positive")
  expect_error(morie_poissp_car_precision(diag(3)), "zero diagonal")
  W2 <- .ps_W
  W2[1, 2] <- 5
  expect_error(morie_poissp_car_precision(W2), "must be symmetric")
  W3 <- .ps_W
  W3[1, 2] <- W3[2, 1] <- -1
  expect_error(morie_poissp_car_precision(W3), "non-negative")
  expect_error(morie_poissp_car_precision(matrix(0, 2, 3)), "square weight matrix")
})

test_that("rho_bounds inverts the extreme eigenvalues of the scaled adjacency", {
  b <- morie_poissp_rho_bounds(.ps_W)
  d <- rowSums(.ps_W)
  S <- .ps_W / sqrt(outer(d, d))
  ev <- sort(eigen(S, symmetric = TRUE)$values)
  expect_equal(b$eigenvalues, ev, tolerance = 1e-10)
  expect_equal(b$lower, 1 / min(ev), tolerance = 1e-10)
  expect_equal(b$upper, 1 / max(ev), tolerance = 1e-10)
  # a row-normalisable W has largest scaled eigenvalue 1, so rho < 1
  expect_lte(b$upper, 1 + 1e-8)
  expect_error(morie_poissp_rho_bounds(matrix(0, 2, 2)), "at least one neighbour")
})

test_that("with no CAR term the fit is glm(poisson) with a log offset", {
  r <- morie_poissp(.ps_y, .ps_X, offset = .ps_E)
  g <- glm(.ps_y ~ .ps_X, family = poisson(), offset = log(.ps_E),
           control = list(epsilon = 1e-14, maxit = 200))
  expect_false(r$spatial)
  expect_equal(r$beta, unname(coef(g)), tolerance = 1e-7)
  expect_equal(r$se, unname(sqrt(diag(vcov(g)))), tolerance = 1e-6)
  expect_equal(r$fitted, unname(fitted(g)), tolerance = 1e-6)
  expect_equal(r$loglik, sum(dpois(.ps_y, fitted(g), log = TRUE)), tolerance = 1e-7)
  expect_equal(r$deviance, deviance(g), tolerance = 1e-7)
  expect_equal(r$score_beta, rep(0, 2), tolerance = 1e-6)
  expect_equal(r$lower, r$beta - qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_equal(r$relative_risk, exp(r$eta), tolerance = 1e-12)
  # an intercept forces the fitted total to match the observed one
  expect_equal(sum(r$fitted), sum(.ps_y), tolerance = 1e-6)
  expect_true(all(abs(r$u) < 1e-6))
})

test_that("a CAR effect is fitted at its penalised mode and respects propriety", {
  s <- morie_poissp(.ps_y, .ps_X, offset = .ps_E, W = .ps_W, tau = 1)
  expect_true(s$spatial)
  expect_true(s$constrained)
  expect_equal(sum(s$u), 0, tolerance = 1e-6)
  expect_equal(s$score_beta, rep(0, 2), tolerance = 1e-6)
  expect_equal(s$fitted, .ps_E * exp(s$eta), tolerance = 1e-12)
  expect_equal(s$eta, as.numeric(cbind(1, .ps_X) %*% s$beta) + s$u, tolerance = 1e-12)
  # a huge precision shrinks u to zero, recovering the non-spatial fit
  tight <- morie_poissp(.ps_y, .ps_X, offset = .ps_E, W = .ps_W, tau = 1e9)
  plain <- morie_poissp(.ps_y, .ps_X, offset = .ps_E)
  expect_equal(tight$beta, plain$beta, tolerance = 1e-5)
  # a loose precision fits each area: the deviance falls
  loose <- morie_poissp(.ps_y, .ps_X, offset = .ps_E, W = .ps_W, tau = 0.01)
  expect_lt(loose$deviance, plain$deviance)
  # proper CAR: rho inside the interval, and the effect need not be centred
  pr <- morie_poissp(.ps_y, .ps_X, offset = .ps_E, W = .ps_W, tau = 1, rho = 0.5)
  expect_false(pr$constrained)
  expect_equal(pr$rho, 0.5)
  expect_error(morie_poissp(.ps_y, W = .ps_W, rho = 5, tau = 1), "outside the propriety interval")
  expect_error(morie_poissp(.ps_y, W = .ps_W, tau = -1), "tau must be positive")
  # tau selected from a grid by the Laplace approximation
  sel <- morie_poissp(.ps_y, offset = .ps_E, W = .ps_W, tau_grid = c(0.5, 10))
  expect_true(sel$tau %in% c(0.5, 10))
})

test_that("morie_poissp validates its inputs; the alias agrees", {
  expect_equal(morie_poisson_spatial_glm(.ps_y, .ps_X, offset = .ps_E)$beta,
               morie_poissp(.ps_y, .ps_X, offset = .ps_E)$beta)
  expect_error(morie_poissp(numeric(0)), "no observations")
  expect_error(morie_poissp(c(1, -1)), "non-negative")
  expect_error(morie_poissp(c(1, 1.5)), "must be integers")
  expect_error(morie_poissp(.ps_y, offset = c(1, 2)), "6 counts but 2 offsets")
  expect_error(morie_poissp(.ps_y, offset = c(0, .ps_E[-1])), "offsets must be positive")
  expect_error(morie_poissp(.ps_y, .ps_X[-1, , drop = FALSE]), "covariate rows")
})

test_that("morie_poissp_cheatsheet names the intrinsic CAR", {
  expect_match(morie_poissp_cheatsheet(), "INTRINSIC CAR", fixed = TRUE)
})
