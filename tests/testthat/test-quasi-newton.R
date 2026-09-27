rb <- function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
rbg <- function(x) c(-2 * (1 - x[1]) - 400 * x[1] * (x[2] - x[1]^2), 200 * (x[2] - x[1]^2))
ext <- function(x) {
  n <- length(x)
  sum(100 * (x[-1] - x[-n]^2)^2 + (1 - x[-n])^2)
}
extg <- function(x) {
  n <- length(x)
  g <- numeric(n)
  for (i in 1:(n - 1)) {
    g[i] <- g[i] - 400 * x[i] * (x[i + 1] - x[i]^2) - 2 * (1 - x[i])
    g[i + 1] <- g[i + 1] + 200 * (x[i + 1] - x[i]^2)
  }
  g
}
x10 <- rep(c(-1.2, 1), 5)

test_that("BfgsMinimize and NelderMead reach the minima and match the Python arm", {
  r <- BfgsMinimize(rb, c(-1.2, 1), rbg)
  expect_true(r$converged)
  expect_equal(r$x, c(1, 1), tolerance = 1e-9)
  r <- BfgsMinimize(ext, x10, extg)
  expect_identical(r$n_iter, 77)
  expect_lte(max(abs(extg(r$x))), 1e-8)
  q <- NelderMead(function(x) (x[1] - 1)^2 + 2 * (x[2] + 2)^2 + 3 * (x[3] - 0.5)^2 + x[1] * x[2], c(0, 0, 0))
  expect_equal(q$x, c(16 / 7, -18 / 7, 0.5), tolerance = 1e-7)
  expect_identical(c(q$n_iter, q$n_fev), c(136, 272))
  r <- NelderMead(rb, c(-1.2, 1))
  expect_identical(c(r$n_iter, r$n_fev), c(150, 287))
})

test_that("LbfgsbMinimize matches optim L-BFGS-B", {
  r <- LbfgsbMinimize(rb, c(-1.2, 1), rbg, c(-2, -2), c(0.5, 2))
  expect_equal(r$x, c(0.5, 0.25), tolerance = 1e-9)
  expect_identical(c(r$n_iter, r$n_fev), c(24, 54))
  r <- LbfgsbMinimize(ext, x10, extg, rep(-2, 10), rep(0.8, 10))
  o <- optim(x10, ext, extg, method = "L-BFGS-B", lower = rep(-2, 10), upper = rep(0.8, 10))
  expect_equal(r$fun, o$value, tolerance = 1e-6)
  expect_equal(r$x, o$par, tolerance = 5e-5)
  expect_identical(r$n_iter, 34)
  r <- LbfgsbMinimize(function(x) (x[1] - 3)^2 + (x[2] + 1)^2, c(10, 10), NULL, c(-Inf, 0), c(2, Inf))
  expect_equal(r$x, c(2, 0), tolerance = 1e-12)
})

test_that("the generalized Cauchy point matches the Python arm", {
  S <- list(c(0.3, -0.2, 0.1, 0.4), c(-0.1, 0.5, 0.2, -0.3), c(0.2, 0.1, -0.4, 0.1))
  Y <- list(c(1.1, -0.3, 0.4, 0.9), c(-0.2, 1.4, 0.3, -0.8), c(0.5, 0.2, -1.2, 0.3))
  cm <- .lb_compact(S, Y, 1.7)
  x <- c(0.2, -0.4, 0.9, 0.1)
  lo <- c(-0.3, -1, 0, -0.2)
  hi <- c(0.5, 0.3, 1, 0.6)
  expect_equal(.lb_cauchy(x, c(2.0, -1.5, 3.0, 0.7), lo, hi, 1.7, cm)$xc, c(-0.3, 0.16065831318190105, 0.0, -0.1616405461515538), tolerance = 1e-12)
  expect_equal(.lb_cauchy(x, c(-0.4, 2.5, -0.3, 1.9), lo, hi, 1.7, cm)$xc, c(0.42710925951379863, -1.0, 1.0, -0.2), tolerance = 1e-12)
  expect_equal(.lb_cauchy(x, c(0.9, 0.2, 2.2, -2.8), lo, hi, 1.7, cm)$xc, c(-0.09295395726054151, -0.46510087939123146, 0.183890326696454, 0.6), tolerance = 1e-12)
})
