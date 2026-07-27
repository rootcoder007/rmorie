# Checked against Hosking (1980) eq. (9) as restated in Mahdi (2020),
# and against portes::Hosking where that package is available.

.whtnse_white <- function(n = 400, k = 3, seed = 0) {
  set.seed(seed); matrix(rnorm(n * k), n, k)
}

.whtnse_var1 <- function(n = 400, k = 3, seed = 0, a = 0.6) {
  set.seed(seed)
  e <- matrix(rnorm(n * k), n, k)
  x <- matrix(NA_real_, n, k)
  x[1, ] <- e[1, ]
  for (t in 2:n) x[t, ] <- a * x[t - 1, ] + e[t, ]
  x
}

test_that("white noise is not rejected", {
  expect_gt(morie_portmanteau_hosking(.whtnse_white(seed = 1), lags = 10)$p_value, 0.05)
})

test_that("a serially dependent series is rejected", {
  expect_lt(morie_portmanteau_hosking(.whtnse_var1(seed = 2), lags = 10)$p_value, 0.01)
})

test_that("degrees of freedom are k^2 (m - fitdf)", {
  expect_equal(morie_portmanteau_hosking(.whtnse_white(k = 3, seed = 3), lags = 7)$df, 63)
  expect_equal(morie_portmanteau_hosking(.whtnse_white(k = 2, seed = 4),
                                         lags = 10, fitdf = 3)$df, 28)
})

test_that("the statistic is non-negative and grows with more lags", {
  X <- .whtnse_white(seed = 6)
  v <- vapply(c(2, 5, 10, 20),
              function(m) morie_portmanteau_hosking(X, lags = m)$statistic,
              numeric(1))
  expect_true(all(v >= 0))
  expect_true(all(diff(v) >= 0))
})

test_that("the modified weighting exceeds the unmodified one", {
  # n^2 / (n - l) > n for every l >= 1.
  X <- .whtnse_white(seed = 7)
  expect_gt(morie_portmanteau_hosking(X, lags = 10, modified = TRUE)$statistic,
            morie_portmanteau_hosking(X, lags = 10, modified = FALSE)$statistic)
})

test_that("the univariate case reduces to the Ljung-Box form", {
  # With k = 1 the trace term is (gamma_l / gamma_0)^2, so the statistic
  # is n^2 sum r_l^2 / (n - l). This pins the algebra without a
  # reference implementation.
  set.seed(8)
  x <- matrix(rnorm(300), 300, 1)
  m <- 6L
  xc <- x - mean(x)
  g0 <- sum(xc * xc) / 300
  expected <- 300^2 * sum(vapply(seq_len(m), function(l) {
    gl <- sum(xc[(l + 1):300] * xc[1:(300 - l)]) / 300
    (gl / g0)^2 / (300 - l)
  }, numeric(1)))
  expect_equal(morie_portmanteau_hosking(x, lags = m)$statistic, expected,
               tolerance = 1e-12)
})

test_that("a transposed panel is detected", {
  expect_equal(morie_portmanteau_hosking(t(.whtnse_white(seed = 11)), lags = 5)$k, 3L)
})

test_that("morie_portmanteau_hosking agrees with portes::Hosking", {
  skip_if_not_installed("portes")
  set.seed(2027)
  X <- matrix(rnorm(250 * 3), 250, 3)
  X <- sweep(X, 2, colMeans(X), "-")
  ref <- portes::Hosking(X, lags = c(5, 10, 15), fitdf = 0)
  for (i in seq_len(nrow(ref))) {
    got <- morie_portmanteau_hosking(X, lags = ref[i, "lags"])
    expect_equal(got$statistic, unname(ref[i, "statistic"]), tolerance = 1e-8)
    expect_equal(got$df, unname(ref[i, "df"]))
    expect_equal(got$p_value, unname(ref[i, "p-value"]), tolerance = 1e-8)
  }
})

test_that("morie_portmanteau_hosking validates its inputs", {
  X <- .whtnse_white(seed = 12)
  expect_error(morie_portmanteau_hosking(X, lags = 0), "at least 1")
  expect_error(morie_portmanteau_hosking(X, lags = 400), "smaller than the series length")
  expect_error(morie_portmanteau_hosking(X, lags = 5, fitdf = -1), "must not be negative")
  expect_error(morie_portmanteau_hosking(X, lags = 3, fitdf = 3), "must exceed fitdf")
  expect_error(morie_portmanteau_hosking(cbind(X[, 1], X[, 1]), lags = 5), "singular")
})
