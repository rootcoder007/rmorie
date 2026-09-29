n <- 12
W <- 1 * (abs(outer(1:n, 1:n, "-")) %in% c(1, 4))
dim(W) <- c(n, n)
y <- 1 + sin(0.6 * (0:11)) + 0.3 * cos(2.1 * (0:11))
mor <- function(e) {
  d <- e - mean(e)
  n / sum(W) * sum(W * outer(d, d)) / sum(d^2)
}
E <- MoranEigenvectors(W)$vectors

test_that("filters recompute", {
  r <- spatial_filter(y, W, method = "aic")
  f <- stats::lm.fit(cbind(1, E[, r$selected + 1, drop = FALSE]), y)
  expect_equal(r$residual_moran, mor(f$residuals), tolerance = 1e-10)
  X <- matrix(0.1 * (0:11) + cos(0:11))
  s <- sfloc(y, X, W, i = 5, criterion = "aic")
  b <- stats::lm.fit(cbind(1, X, E[, s$selected + 1, drop = FALSE]), y)$coefficients
  expect_equal(s$value, sum(E[6, s$selected + 1] * b[-(1:2)]), tolerance = 1e-10)
  u <- sfredu(y, W)
  expect_equal(u$moran_before, mor(y), tolerance = 1e-12)
  expect_equal(sfmi(y, W)$statistic, mor(y), tolerance = 1e-12)
  expect_lt(sforth(E)$statistic, 1e-12)
  g <- sfgetis(c(2, 4, 6, 3, 5, 1.5), 1 * (abs(outer(1:6, 1:6, "-")) == 1))
  expect_equal(g$filtered[1], 2 * (1 / 5) / (4 / (21.5 - 2)), tolerance = 1e-12)
})

test_that("sfmemb recomputes the t tests", {
  r <- sfmemb(y, W, alpha = 0.1)
  pos <- MoranEigenvectors(W)$positive
  yc <- y - mean(y)
  rr <- as.vector(crossprod(E[, pos], yc)) / sqrt(sum(yc^2))
  tt <- rr * sqrt((n - 2) / (1 - rr^2))
  expect_equal(r$t, tt, tolerance = 1e-10)
  expect_equal(r$selected, pos[2 * stats::pt(-abs(tt), n - 2) < 0.1 / length(pos)] - 1L)
})
