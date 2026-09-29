# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tlhal_native.R (highly adaptive lasso, van der Laan &
# Rose 2018, Ch. 6). The indicator basis 1{x_S >= knot_S} is built by
# hand, the L1-ball projection is checked against the soft-threshold
# level solved with uniroot, and leave-one-out CV (which makes the
# fold shuffle irrelevant) is recomputed from hal_fit / hal_predict.

.hl_X <- cbind(c(0.1, 0.5, 0.3, 0.9, 0.7, 0.2), c(1, 0, 1, 1, 0, 0))
.hl_y <- c(0.5, 1.2, 0.8, 2.5, 1.9, 0.4)

test_that("indicator_basis has one column per (subset, knot)", {
  X <- .hl_X[1:3, ]
  B <- indicator_basis(X)
  expect_equal(B$n_basis, 9L)
  expect_equal(B$design[, 1], as.numeric(X[, 1] >= X[1, 1]))
  expect_equal(B$design[, 5], as.numeric(X[, 2] >= X[2, 2]))
  expect_equal(B$design[, 9], as.numeric(X[, 1] >= X[3, 1] & X[, 2] >= X[3, 2]))
  expect_identical(B$columns[[9]]$S, 1:2)
  expect_equal(indicator_basis(X, max_order = 1)$n_basis, 6L)
  expect_equal(indicator_basis(cbind(X, 1), max_order = 3)$n_basis, 3 * 7)
  k <- indicator_basis(c(0.2, 0.6, 0.4), knots = c(0.5))
  expect_equal(k$design[, 1], c(0, 1, 0))
})

test_that("variation_norm is the L1 norm; the projection soft-thresholds onto the ball", {
  expect_equal(variation_norm(c(1, -2, 0.5)), 3.5)
  v <- c(3, -1, 0.5, -2.5)
  p <- .tlhal_project_l1(v, 2)
  th <- uniroot(function(t) sum(pmax(abs(v) - t, 0)) - 2, c(0, 3), tol = 1e-14)$root
  expect_equal(p, sign(v) * pmax(abs(v) - th, 0), tolerance = 1e-10)
  expect_equal(sum(abs(p)), 2, tolerance = 1e-12)
  expect_identical(.tlhal_project_l1(c(0.5, -0.5), 2), c(0.5, -0.5))
})

test_that("hal_fit stays in the L1 ball and hal_predict reproduces its fit", {
  f <- hal_fit(.hl_X, .hl_y, lam = 1.5, iters = 400)
  expect_lte(f$variation_norm, 1.5 + 1e-12)
  D <- indicator_basis(.hl_X)$design
  pr <- hal_predict(f, .hl_X)
  expect_equal(pr, as.numeric(f$intercept + D %*% f$beta), tolerance = 1e-12)
  expect_equal(f$mse, mean((pr - .hl_y)^2), tolerance = 1e-12)
  expect_lt(f$mse, mean((.hl_y - mean(.hl_y))^2))
  # a tight bound is active: the fit uses the whole budget
  t <- hal_fit(.hl_X, .hl_y, lam = 0.3, iters = 400)
  expect_equal(t$variation_norm, 0.3, tolerance = 1e-9)
  expect_lt(f$mse, t$mse)
  n0 <- hal_fit(.hl_X, .hl_y, lam = 5, iters = 50, intercept = FALSE)
  expect_equal(n0$intercept, 0)
  expect_error(hal_fit(.hl_X, .hl_y[-1]), "6 rows but 5 outcomes")
  expect_error(hal_fit(.hl_X, .hl_y, lam = 0), "lambda must be positive")
})

test_that("cv_select_lambda with V = n is leave-one-out", {
  lams <- c(0.2, 2)
  cv <- cv_select_lambda(.hl_X, .hl_y, lams, V = 6, iters = 150)
  loo <- vapply(lams, function(l) {
    mean(vapply(1:6, function(i) {
      fit <- hal_fit(.hl_X[-i, ], .hl_y[-i], lam = l, knots = .hl_X[-i, ], iters = 150)
      (hal_predict(fit, .hl_X[i, , drop = FALSE]) - .hl_y[i])^2
    }, 0))
  }, 0)
  expect_equal(unname(unlist(cv$cv_risks)), loo, tolerance = 1e-12)
  expect_equal(cv$lambda, lams[which.min(loo)])
})

test_that("morie_tlhal dispatches on mode", {
  f <- morie_tlhal(.hl_X, .hl_y, lam = 1, iters = 100)
  expect_equal(f$beta, hal_fit(.hl_X, .hl_y, lam = 1, iters = 100)$beta)
  expect_equal(morie_tlhal(f, .hl_X, mode = "predict"), hal_predict(f, .hl_X))
  expect_equal(morie_tlhal(c(1, -1), mode = "norm"), 2)
  cv <- morie_tlhal(.hl_X, .hl_y, lambdas = c(0.5, 1), V = 2, iters = 50, mode = "cv")
  expect_true(cv$lambda %in% c(0.5, 1))
  expect_error(morie_tlhal(.hl_X, .hl_y, mode = "boot"))
})
