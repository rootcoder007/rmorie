test_that("effect modification recomputes", {
  zay <- function(p, tau) {
    L <- length(p)
    w <- prod(p[p <= tau])
    tot <- 0
    for (k in seq_len(L)) {
      inner <- if (w <= tau^k) w * sum((k * log(tau) - log(w))^(0:(k - 1)) / factorial(0:(k - 1))) else tau^k
      tot <- tot + choose(L, k) * (1 - tau)^(L - k) * inner
    }
    tot
  }
  p <- c(0.01, 0.03, 0.2, 0.04, 0.5)
  expect_equal(TruncatedProductPvalue(p)$pvalue, zay(p, 0.05), tolerance = 1e-12)
  m <- WilcoxonSensitivityMoments(8)
  expect_equal(c(m$mu, m$nu), c(8 * 9 / 4, 8 * 9 * 17 / 24), tolerance = 1e-14)
  r <- SubmaxTest(c(40, 30), rep(27.5, 2), rep(96.25, 2), SubmaxComparisons(1), nsim = 20000)
  expect_equal(r$D[2], (40 - 27.5) / sqrt(96.25), tolerance = 1e-12)
  expect_lt(abs(r$kappa - 2.03), 0.03)
})
