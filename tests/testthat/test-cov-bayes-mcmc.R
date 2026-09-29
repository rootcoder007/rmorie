# Coverage for the MCMC diagnostics: Gelman-Rubin R-hat (between/within
# variance formula), the autocorrelation ESS truncated at the first
# negative lag (via stats::acf), and the Geweke z with spectral-density
# (AR) variances of the early and late segment means, as in coda.

test_that("R-hat follows sqrt(((n-1)/n W + B/n) / W)", {
  set.seed(1)
  ch <- list(matrix(stats::rnorm(200), 100, dimnames = list(NULL, c("a", "b"))),
             matrix(stats::rnorm(200, 0.3), 100, dimnames = list(NULL, c("a", "b"))),
             matrix(stats::rnorm(200), 100, dimnames = list(NULL, c("a", "b"))))
  r <- morie_bayes_rhat(ch)
  xs <- sapply(ch, function(c) c[, "a"])
  B <- 100 / 2 * sum((colMeans(xs) - mean(colMeans(xs)))^2)
  W <- mean(apply(xs, 2, stats::var))
  expect_equal(r[["a"]], sqrt((99 / 100 * W + B / 100) / W), tolerance = 1e-12)
  expect_identical(names(r), c("a", "b"))
  expect_identical(unname(morie_bayes_rhat(list(matrix(1, 5, 1), matrix(1, 5, 1)))), 1)
})

test_that("ESS truncates the autocorrelation sum at the first negative lag", {
  set.seed(2)
  x <- as.numeric(stats::arima.sim(list(ar = 0.7), 300))
  a <- stats::acf(x, lag.max = 50, plot = FALSE)$acf[-1]
  k <- which(a < 0)[1]
  e <- morie_bayes_ess(list(matrix(x, dimnames = list(NULL, "th"))))
  expect_equal(e[["th"]], 300 / (1 + 2 * sum(a[seq_len(k - 1)])), tolerance = 1e-12)
  expect_lt(e[["th"]], 300)
})

test_that("Geweke z uses spectral variances of the segment means", {
  set.seed(3)
  x <- as.numeric(stats::arima.sim(list(ar = 0.8), 1000))
  z <- morie_bayes_geweke(list(matrix(x, dimnames = list(NULL, "p"))))
  s0 <- function(v) {
    f <- stats::ar(v, aic = TRUE)
    f$var.pred / (1 - sum(f$ar))^2
  }
  xa <- x[1:100]
  xb <- x[501:1000]
  expect_equal(z[["p"]], (mean(xa) - mean(xb)) / sqrt(s0(xa) / 100 + s0(xb) / 500), tolerance = 1e-12)
  # a stationary chain passes; a drifting one does not
  expect_lt(abs(z[["p"]]), 3)
  drift <- x + seq(0, 20, length.out = 1000)
  expect_gt(abs(morie_bayes_geweke(list(matrix(drift)))), 3)
})
