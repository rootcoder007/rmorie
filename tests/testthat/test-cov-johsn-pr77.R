# R/johsn.R, the lines PR #77 added that no other test reached: an unnamed input, the refusals, and
# more than eleven series (no Osterwald-Lenum rows), against urca::ca.jo and a hand reduced-rank
# regression.

.jn <- function() get(".morie_johansen_native", envir = environment(morie_johansen_cointegration))

test_that("an unnamed matrix gets y1, y2 names and urca's eigenvalues and statistics", {
  skip_if_not_installed("urca")
  set.seed(2)
  e <- matrix(stats::rnorm(240), 120, 2)
  x <- cbind(cumsum(e[, 1]), cumsum(e[, 1]) + e[, 2])
  r <- .jn()(x, K = 2L)
  xu <- x
  colnames(xu) <- c("y1", "y2")  # urca::ca.jo itself needs column names
  u <- urca::ca.jo(xu, K = 2)
  expect_identical(rownames(r$V), c("y1.l2", "y2.l2"))
  expect_equal(r$lambda, unname(u@lambda), tolerance = 1e-10)
  expect_equal(r$trace, unname(urca::ca.jo(xu, K = 2, type = "trace")@teststat), tolerance = 1e-10)
  expect_equal(r$maxeig, unname(u@teststat), tolerance = 1e-10)
})

test_that("K below 2, missing values and too few rows are refused as urca refuses them", {
  skip_if_not_installed("urca")
  set.seed(3)
  x <- apply(matrix(stats::rnorm(40), 20, 2), 2, cumsum)
  expect_error(.jn()(x, K = 1L), "K must be at least 2.", fixed = TRUE)
  expect_error(urca::ca.jo(x, K = 1), "K must be at least K=2")
  xn <- x
  xn[4, 2] <- NA
  expect_error(.jn()(xn), "x contains missing values.", fixed = TRUE)
  expect_error(.jn()(x[1:3, ], K = 2L), "Insufficient degrees of freedom.", fixed = TRUE)
  expect_error(urca::ca.jo(x[1:3, ], K = 2), "Insufficient degrees of freedom")
})

test_that("twelve series have no critical values; eigenvalues are the reduced-rank regression's", {
  set.seed(1)
  x <- apply(matrix(stats::rnorm(200 * 12), 200, 12), 2, cumsum)
  r <- .jn()(x, K = 2L)
  expect_null(r$cval_trace)
  expect_null(r$cval_eigen)
  # Johansen by hand: residuals of dx_t and x_{t-2} on (1, dx_{t-1}), then the eigenproblem
  dx <- diff(x)
  Z0 <- dx[-1, ]
  Z1 <- cbind(1, dx[-nrow(dx), ])
  ZK <- x[seq_len(nrow(x) - 2L), ]
  R0 <- stats::lm.fit(Z1, Z0)$residuals
  R1 <- stats::lm.fit(Z1, ZK)$residuals
  N <- nrow(R0)
  S00 <- crossprod(R0) / N
  S01 <- crossprod(R0, R1) / N
  S11 <- crossprod(R1) / N
  lam <- sort(Re(eigen(solve(S11, t(S01) %*% solve(S00, S01)), only.values = TRUE)$values), decreasing = TRUE)
  expect_equal(r$lambda, lam, tolerance = 1e-8)
  expect_equal(r$trace[12], -N * sum(log(1 - lam)), tolerance = 1e-8)
})
