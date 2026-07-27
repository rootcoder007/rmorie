# W_1 is checked against a direct transcription of integral |F - G|, and
# MMD against a brute-force transcription of Gretton et al. (2012).

test_that("W_1 of a sample with itself is zero", {
  set.seed(1); x <- rnorm(40)
  expect_equal(.w1_distance(x, x), 0, tolerance = 1e-14)
})

test_that("W_1 of a pure shift is the shift", {
  set.seed(2); x <- rnorm(200)
  expect_equal(.w1_distance(x, x + 3), 3, tolerance = 1e-10)
})

test_that("W_1 matches a brute-force integral of |F - G|", {
  set.seed(3)
  a <- rnorm(40); b <- rnorm(30, 0.5, 2)
  grid <- sort(c(a, b))
  left <- grid[-length(grid)]
  brute <- sum(abs(stats::ecdf(a)(left) - stats::ecdf(b)(left)) * diff(grid))
  expect_equal(.w1_distance(a, b), brute, tolerance = 1e-12)
})

test_that("the Wasserstein test holds its size and finds a shift", {
  set.seed(4)
  expect_gt(morie_wasserstein_test(rnorm(80), rnorm(80), B = 199)$p_value, 0.05)
  set.seed(5)
  expect_lte(morie_wasserstein_test(rnorm(80), rnorm(80, 1.5), B = 199)$p_value, 0.01)
})

test_that("the Wasserstein test sees a scale difference at equal means", {
  set.seed(6)
  expect_lte(morie_wasserstein_test(rnorm(150), rnorm(150, 0, 4), B = 199)$p_value, 0.01)
})

test_that("MMD matches the paper's formulas", {
  set.seed(7)
  A <- matrix(rnorm(24), 12, 2)
  Bm <- matrix(rnorm(18, 0.4), 9, 2)
  for (unb in c(FALSE, TRUE)) {
    kxx <- 0; kyy <- 0; kxy <- 0
    for (i in 1:12) for (j in 1:12) if (!(unb && i == j)) kxx <- kxx + exp(-0.5 * sum((A[i, ] - A[j, ])^2))
    for (i in 1:9) for (j in 1:9) if (!(unb && i == j)) kyy <- kyy + exp(-0.5 * sum((Bm[i, ] - Bm[j, ])^2))
    for (i in 1:12) for (j in 1:9) kxy <- kxy + exp(-0.5 * sum((A[i, ] - Bm[j, ])^2))
    want <- if (unb) kxx / (12 * 11) + kyy / (9 * 8) - 2 * kxy / 108 else kxx / 144 + kyy / 81 - 2 * kxy / 108
    got <- morie_mmd_test(A, Bm, gamma = 0.5, B = 1L, unbiased = unb)$statistic
    expect_equal(got, want, tolerance = 1e-10)
  }
})

test_that("the biased MMD estimate is non-negative", {
  set.seed(8)
  for (i in 1:5) {
    expect_gte(morie_mmd_test(rnorm(30), rnorm(30), B = 1L)$statistic, -1e-12)
  }
})

test_that("the MMD test holds its size and finds a shift", {
  set.seed(9)
  expect_gt(morie_mmd_test(rnorm(60), rnorm(60), B = 199)$p_value, 0.05)
  set.seed(10)
  expect_lte(morie_mmd_test(rnorm(60), rnorm(60, 2), B = 199)$p_value, 0.01)
})

test_that("a linear kernel is blind to a mean-preserving difference", {
  # This is what "characteristic kernel" buys: rbf detects any
  # difference in distribution, linear compares means only.
  set.seed(11)
  a <- rnorm(200); b <- rnorm(200, 0, 5)
  expect_lte(morie_mmd_test(a, b, kernel = "rbf", B = 199)$p_value, 0.01)
  set.seed(12)
  expect_gt(morie_mmd_test(a, b, kernel = "linear", B = 199)$p_value, 0.05)
})

test_that("the two-sample tests validate their inputs", {
  set.seed(13); a <- rnorm(30)
  expect_error(morie_wasserstein_test(matrix(a, ncol = 1), a), "plain numeric vector")
  expect_error(morie_wasserstein_test(a[1], a), "at least 2 observations")
  expect_error(morie_wasserstein_test(c(1, NA, 2), a), "must be finite")
  expect_error(morie_wasserstein_test(a, a, B = 0), "B must be at least 1")
  A <- matrix(rnorm(40), 20, 2)
  expect_error(morie_mmd_test(A, matrix(rnorm(60), 20, 3)), "share a feature dimension")
  expect_error(morie_mmd_test(A, A, kernel = "cosine"), "kernel must be one of")
  expect_error(morie_mmd_test(A, A, gamma = 0), "gamma must be positive")
  expect_error(morie_mmd_test(A, A, B = 0), "B must be at least 1")
})
