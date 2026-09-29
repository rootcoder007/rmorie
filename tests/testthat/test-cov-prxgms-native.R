# Proximal gradient / FISTA (Beck & Teboulle 2009): soft thresholding,
# the lasso KKT conditions at the FISTA solution, least squares through
# the identity prox, and backtracking on L.

px_data <- function() {
  set.seed(19)
  A <- matrix(stats::rnorm(30 * 5), 30)
  b <- as.numeric(A %*% c(2, 0, -1, 0, 0.5)) + stats::rnorm(30, sd = 0.2)
  list(A = A, b = b)
}

test_that("soft thresholding shrinks towards zero by tau", {
  v <- c(-3, -0.5, 0, 0.2, 1.7)
  ref <- sign(v) * pmax(abs(v) - 0.6, 0)
  expect_equal(morie_prxgms_soft_threshold(v, 0.6), ref)
  expect_equal(soft_threshold(v, 0.6), ref)
  expect_identical(morie_prxgms(v, 0.6), morie_prxgms_soft_threshold(v, 0.6))
})

test_that("FISTA and ISTA reach the lasso KKT point", {
  d <- px_data()
  lam <- 3
  for (acc in c(TRUE, FALSE)) {
    for (f in list(function() morie_prxgms_lasso_fista(d$A, d$b, lam, max.iter = 20000,
                                                        tol = 1e-13, accelerate = acc),
                   function() lasso_fista(d$A, d$b, lam, max_iter = 20000, tol = 1e-13,
                                          accelerate = acc))) {
      r <- f()
      g <- as.numeric(crossprod(d$A, d$b - d$A %*% r$x))
      act <- abs(r$x) > 1e-10
      expect_equal(g[act], lam * sign(r$x[act]), tolerance = 1e-7)
      expect_true(all(abs(g[!act]) <= lam + 1e-7))
      expect_equal(r$L, max(eigen(crossprod(d$A))$values), tolerance = 1e-8)
      expect_equal(r$objective[length(r$objective)],
                   0.5 * sum((d$A %*% r$x - d$b)^2) + lam * sum(abs(r$x)), tolerance = 1e-12)
    }
  }
})

test_that("with the identity prox the method solves least squares; backtracking finds L", {
  d <- px_data()
  f <- function(x) 0.5 * sum((d$A %*% x - d$b)^2)
  g <- function(x) as.numeric(crossprod(d$A, d$A %*% x - d$b))
  ols <- qr.solve(d$A, d$b)
  L <- max(eigen(crossprod(d$A))$values)
  r <- morie_prxgms_prox_gradient(f, g, function(v, t) v, rep(0, 5), L = L,
                                  max.iter = 5000, tol = 1e-13)
  expect_equal(r$x, as.numeric(ols), tolerance = 1e-8)
  bt <- morie_prxgms_prox_gradient(f, g, function(v, t) v, rep(0, 5), L = 1,
                                   max.iter = 5000, tol = 1e-13, backtrack = TRUE)
  expect_equal(bt$x, as.numeric(ols), tolerance = 1e-8)
  expect_gte(bt$L, 1)
  pg <- prox_gradient(f, g, function(v, t) v, rep(0, 5), L = L, max_iter = 5000, tol = 1e-13)
  expect_equal(pg$x, as.numeric(ols), tolerance = 1e-8)
  expect_identical(proximal_gradient_method, prox_gradient)
  expect_error(morie_prxgms_prox_gradient(f, g, function(v, t) v, rep(0, 5), L = 0), "L must be positive")
  expect_error(prox_gradient(f, g, function(v, t) v, rep(0, 5), L = -1), "got -1")
  expect_match(prxgms_cheatsheet(), "soft threshold", fixed = TRUE)
})
