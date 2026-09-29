# Unclear-attribution helpers: DFT amplitudes, the normal density and
# cdf, additive log-ratios (Aitchison), the lrEM / lrDA imputation of
# values below detection limits (Palarea-Albaladejo & Martin-Fernandez),
# and the symmetric GCN normalisation.

test_that("DFT amplitudes, phi and Phi", {
  x <- c(1, 3, -2, 0.5, 4, -1)
  n <- length(x)
  ref <- vapply(0:(n - 1), function(k) {
    Mod(sum(x * exp(-2i * pi * k * (0:(n - 1)) / n)))
  }, numeric(1))
  expect_equal(morie_unclr_dft_amp(x), ref, tolerance = 1e-12)
  z <- c(-2, -0.3, 0, 1.7)
  expect_equal(morie_unclr_phi(z), stats::dnorm(z), tolerance = 1e-15)
  expect_equal(morie_unclr_Phi(z), stats::pnorm(z), tolerance = 1e-15)
})

test_that("alr and its inverse round-trip at a given total", {
  x <- c(0.2, 0.5, 0.1, 0.2)
  z <- morie_unclr_alr(x)
  expect_equal(z, log(x[1:3] / x[4]), tolerance = 1e-15)
  expect_equal(morie_unclr_alr_inv(z, 1), x, tolerance = 1e-15)
  expect_equal(morie_unclr_alr_inv(z, 7), 7 * x, tolerance = 1e-14)
})

test_that("log-ratio imputation keeps totals and observed ratios", {
  set.seed(3)
  n <- 12
  X <- cbind(stats::runif(n, 0.01, 0.2), stats::runif(n, 0.3, 0.5), stats::runif(n, 0.2, 0.4))
  X <- X / rowSums(X) * 100
  dl <- c(sort(X[, 1])[4] * 1.0001, 0, 0)
  cens <- X[, 1] < dl[1]
  expect_identical(sum(cens), 4L)
  for (r in list(morie_unclr_lr_impute(X, dl, 10),
                 morie_unclr_lr_impute(X, dl, 10, draw = matrix(stats::rnorm(40), 4)))) {
    expect_equal(rowSums(r$X), rowSums(X), tolerance = 1e-12)
    expect_equal(r$X[, 2] / r$X[, 3], X[, 2] / X[, 3], tolerance = 1e-12)
    expect_equal(r$X[!cens, ], X[!cens, ], tolerance = 1e-12)
    expect_identical(r$n_censored, sum(cens))
    expect_true(all(r$X[cens, 1] > 0))
  }
  expect_equal(Lrem(X, dl, 5)$X, morie_unclr_lr_impute(X, dl, 5)$X)
  expect_error(morie_unclr_lr_impute(X, c(1, 2), 1), "one detection limit")
  expect_error(Lrda(X, dl, NULL), "standard normal")
})

test_that("the symmetric normalisation is D^-1/2 (A + I) D^-1/2", {
  A <- matrix(c(0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0), 4)
  M <- A + diag(4)
  D <- diag(1 / sqrt(rowSums(M)))
  expect_equal(morie_unclr_sym_norm(A, TRUE), D %*% M %*% D, tolerance = 1e-15)
  D0 <- diag(1 / sqrt(rowSums(A)))
  expect_equal(morie_unclr_sym_norm(A, FALSE), D0 %*% A %*% D0, tolerance = 1e-15)
  expect_error(morie_unclr_sym_norm(matrix(0, 2, 2), FALSE), "non-positive degree")
})
