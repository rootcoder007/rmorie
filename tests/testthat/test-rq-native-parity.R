# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity of morie_rq (native Frisch-Newton quantile regression) with
# quantreg::rq / summary.rq, used here only as the reference.
#
# Tolerances: morie's "fn" repeats quantreg's lpfnb iteration step for
# step, so on a problem with a unique solution the two agree to rounding
# (observed ~1e-14); 1e-8 leaves room for BLAS differences.  "br" returns
# the exact optimal vertex, which is the unique solution when there is
# one.  Where the solution is NOT unique (quantreg warns "Solution may be
# nonunique") the interior point method stops inside the optimal face and
# its position there is not determined by the objective, so only the
# check-loss value is compared.

.rqn_rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-300))

.rqn_sim <- function(n, seed) {
  set.seed(seed)
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::runif(n),
                  g = factor(sample(letters[1:3], n, TRUE)))
  d$y <- 1 + d$x1 - 2 * d$x2 + as.integer(d$g) + stats::rnorm(n) * (1 + d$x2)
  d
}

test_that("coefficients and nid/iid/ker standard errors match quantreg", {
  testthat::skip_if_not_installed("quantreg")
  d <- .rqn_sim(2000, 42)
  fo <- y ~ x1 + x2 + g
  for (tau in c(0.1, 0.5)) for (m in c("fn", "br")) {
    q <- quantreg::rq(fo, tau = tau, data = d, method = m)
    for (s in c("nid", "iid", "ker")) {
      f <- rmorie::morie_rq(fo, d, tau = tau, method = m, se = s)
      expect_lt(.rqn_rel(coef(f), coef(q)), 1e-8)
      sq <- quantreg::summary.rq(q, se = s)$coefficients
      expect_lt(.rqn_rel(f$coef_table[[1]][, 2], sq[, 2]), 1e-8)
      expect_lt(.rqn_rel(f$coef_table[[1]][, 4], sq[, 4]), 1e-6)
    }
  }
})

test_that("fn and br agree when the solution is unique", {
  testthat::skip_if_not_installed("quantreg")
  d <- .rqn_sim(2000, 42)
  a <- rmorie::morie_rq(y ~ x1 + x2 + g, d, tau = 0.25, method = "fn")
  b <- rmorie::morie_rq(y ~ x1 + x2 + g, d, tau = 0.25, method = "br")
  # The interior point solution is within its duality gap (1e-6) of the vertex.
  expect_lt(.rqn_rel(coef(a), coef(b)), 1e-7)
})

test_that("a non-unique problem: br reaches an optimal vertex", {
  testthat::skip_if_not_installed("quantreg")
  d <- .rqn_sim(2000, 42)
  q <- suppressWarnings(quantreg::rq(y ~ x1 + x2 + g, tau = 0.9, data = d))
  f <- rmorie::morie_rq(y ~ x1 + x2 + g, d, tau = 0.9, method = "br")
  expect_lt(abs(f$rho - q$rho) / q$rho, 1e-12)
})

test_that("engel data, several quantiles at once", {
  testthat::skip_if_not_installed("quantreg")
  e <- new.env()
  utils::data("engel", package = "quantreg", envir = e)
  taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  for (m in c("fn", "br")) {
    q <- quantreg::rq(foodexp ~ income, tau = taus, data = e$engel, method = m)
    f <- rmorie::morie_rq(foodexp ~ income, e$engel, tau = taus, method = m)
    expect_lt(.rqn_rel(coef(f), coef(q)), 1e-8)
  }
})

test_that("tied, discrete data: br matches quantreg's vertex", {
  testthat::skip_if_not_installed("quantreg")
  set.seed(5)
  n <- 500
  d <- data.frame(g = factor(sample(1:4, n, TRUE)), x = sample(0:5, n, TRUE))
  d$y <- stats::rpois(n, 2 + as.integer(d$g) + d$x)
  for (tau in c(0.25, 0.5, 0.75)) {
    q <- suppressWarnings(quantreg::rq(y ~ g + x, tau = tau, data = d))
    f <- rmorie::morie_rq(y ~ g + x, d, tau = tau, method = "br")
    expect_lt(abs(f$rho - q$rho) / q$rho, 1e-12)
  }
})

test_that("case weights match rq.wfit", {
  testthat::skip_if_not_installed("quantreg")
  set.seed(3)
  n <- 300
  d <- data.frame(x = stats::runif(n), z = stats::rnorm(n))
  d$y <- 1 + 2 * d$x - d$z + stats::rnorm(n) * (1 + d$x)
  w <- stats::runif(n, 0.5, 2)
  q <- quantreg::rq(y ~ x + z, tau = 0.3, data = d, weights = w, method = "fn")
  f <- rmorie::morie_rq(y ~ x + z, d, tau = 0.3, weights = w)
  expect_lt(.rqn_rel(coef(f), coef(q)), 1e-8)
  expect_lt(.rqn_rel(f$coef_table[[1]][, 2],
                     quantreg::summary.rq(q, se = "nid")$coefficients[, 2]), 1e-8)
})

test_that("xy-pair bootstrap reproduces summary.rq(se = 'boot') draw for draw", {
  testthat::skip_if_not_installed("quantreg")
  set.seed(3)
  n <- 300
  d <- data.frame(x = stats::runif(n), z = stats::rnorm(n))
  d$y <- 1 + 2 * d$x - d$z + stats::rnorm(n) * (1 + d$x)
  q <- quantreg::rq(y ~ x + z, tau = 0.5, data = d)
  set.seed(11)
  sq <- quantreg::summary.rq(q, se = "boot", R = 100)
  f <- rmorie::morie_rq(y ~ x + z, d, tau = 0.5, method = "br", se = "boot", R = 100, seed = 11)
  expect_lt(.rqn_rel(f$coef_table[[1]][, 2], sq$coefficients[, 2]), 1e-8)
})

test_that("predict, logLik and the methods", {
  testthat::skip_if_not_installed("quantreg")
  d <- .rqn_sim(400, 9)
  q <- quantreg::rq(y ~ x1 + x2, tau = c(0.25, 0.75), data = d)
  f <- rmorie::morie_rq(y ~ x1 + x2, d, tau = c(0.25, 0.75), method = "br")
  nd <- d[1:5, ]
  expect_lt(.rqn_rel(predict(f, nd), predict(q, nd)), 1e-8)
  f1 <- rmorie::morie_rq(y ~ x1 + x2, d, tau = 0.5, method = "br")
  q1 <- quantreg::rq(y ~ x1 + x2, tau = 0.5, data = d)
  expect_equal(as.numeric(logLik(f1)), as.numeric(logLik(q1)), tolerance = 1e-10)
  expect_equal(dim(vcov(f1)), c(3L, 3L))
  expect_output(print(f1), "Quantile regression")
  expect_output(print(summary(f1)), "Std. Error")
  expect_equal(unname(fitted(f1) + residuals(f1)), d$y)
  expect_error(rmorie::morie_rq(y ~ x1, d, tau = 1), "between 0 and 1")
})

test_that("n = 100,000, p = 10 fits with nid standard errors in under 5 s", {
  testthat::skip_on_cran()
  set.seed(7)
  n <- 1e5
  Z <- matrix(stats::rnorm(n * 9), n)
  colnames(Z) <- paste0("x", 1:9)
  d <- data.frame(Z)
  d$y <- drop(Z %*% (1:9 / 3)) + stats::rt(n, 3)
  fo <- stats::reformulate(colnames(Z), "y")
  tm <- system.time(f <- rmorie::morie_rq(fo, d, tau = 0.5, method = "fn", se = "nid"))[["elapsed"]]
  expect_lt(tm, 5)
  if (requireNamespace("quantreg", quietly = TRUE)) {
    q <- quantreg::rq(fo, tau = 0.5, data = d, method = "fn")
    expect_lt(.rqn_rel(coef(f), coef(q)), 1e-8)
  }
})
