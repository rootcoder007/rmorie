# Parity of the R spatial estimators against morie Python and against
# the R reference packages (spdep, spatialreg) they mirror.

rook_lattice <- function(k = 5) {
  n <- k * k
  idx <- function(r, c) (r - 1) * k + c
  W <- matrix(0, n, n)
  for (r in 1:k) for (cc in 1:k)
    for (d in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
      rr <- r + d[1]
      c2 <- cc + d[2]
      if (rr >= 1 && rr <= k && c2 >= 1 && c2 <= k)
        W[idx(r, cc), idx(rr, c2)] <- 1
    }
  W / rowSums(W)
}

W25 <- rook_lattice(5)
X25 <- sapply(0:24, function(i) ((i * 7) %% 11) + 0.5 * ((i * 3) %% 5))

test_that("weights constants match spdep Szero and friends", {
  tt <- morie_weights_totals(W25)
  expect_equal(tt$S0, 25)
  expect_equal(tt$S1, 16.194444444444443, tolerance = 1e-12)
  expect_equal(tt$S2, 100.66666666666664, tolerance = 1e-12)
  expect_error(morie_weights_totals(matrix(1, 2, 3)), "square")
})

test_that("Moran's I and its test match spdep::moran.test", {
  expect_equal(morie_morans_i(X25, W25), -0.15340012143290832,
               tolerance = 1e-12)
  r <- morie_morans_i_test(X25, W25, randomisation = TRUE)
  expect_equal(r$estimate, -0.15340012143290832, tolerance = 1e-12)
  expect_equal(r$expectation, -1/24, tolerance = 1e-14)
  expect_equal(r$variance, 0.023435730140245057, tolerance = 1e-12)
  expect_equal(r$statistic, -0.72986742846571095, tolerance = 1e-12)
  expect_equal(r$p_value, 0.76726438824390431, tolerance = 1e-12)

  nn <- morie_morans_i_test(X25, W25, randomisation = FALSE)
  expect_equal(nn$variance, 0.022571225071225071, tolerance = 1e-12)
  expect_equal(nn$statistic, -0.74371349417329535, tolerance = 1e-12)
  expect_equal(nn$p_value, 0.77147508801808429, tolerance = 1e-12)
})

test_that("Moran expectation is -1/(n-1) on any lattice", {
  for (k in c(4, 5, 6)) {
    W <- rook_lattice(k)
    x <- sapply(0:(k * k - 1), function(i) (i * 3) %% 7)
    expect_equal(morie_morans_i_test(x, W)$expectation,
                 -1 / (k * k - 1), tolerance = 1e-12)
  }
})

sar_fixture <- function() {
  n <- 25
  x1 <- sapply(0:(n - 1), function(i) ((i * 7) %% 11) + 0.5 * ((i * 3) %% 5))
  x2 <- sapply(0:(n - 1), function(i) ((i * 5) %% 7) - 0.25 * ((i * 2) %% 3))
  e  <- sapply(0:(n - 1), function(i) ((i * 13) %% 17) / 17 - 0.5)
  y  <- as.numeric(solve(diag(n) - 0.4 * W25) %*% (2 + 1.5 * x1 - 0.8 * x2 + e))
  list(y = y, X = cbind(x1, x2))
}

test_that("spatial 2SLS matches spatialreg::stsls", {
  f <- sar_fixture()
  s <- morie_spatial_2sls(f$y, f$X, W25)
  expect_equal(s$rho, 0.392173355844, tolerance = 1e-11)
  expect_equal(s$beta[1], 1.930187859839, tolerance = 1e-11)
  expect_equal(s$beta[2], 1.506892839923, tolerance = 1e-11)
  expect_equal(s$beta[3], -0.762612153824, tolerance = 1e-11)
  # and it recovers the rho the fixture was built with
  expect_lt(abs(s$rho - 0.4), 0.05)
})

test_that("GM error model matches spatialreg::GMerrorsar", {
  f <- sar_fixture()
  g <- morie_gm_error_sar(f$y, f$X, W25)
  expect_equal(g$lambda, 0.513267505390, tolerance = 1e-7)
  expect_equal(g$beta[1], 7.630934971425, tolerance = 1e-7)
  expect_equal(g$beta[2], 1.378029513797, tolerance = 1e-7)
  expect_equal(g$beta[3], -0.495944119576, tolerance = 1e-7)
  expect_lte(g$criterion, 2.686597e-03)
})

test_that("spatial estimators reject mismatched shapes", {
  f <- sar_fixture()
  expect_error(morie_spatial_2sls(f$y[1:5], f$X, W25))
  expect_error(morie_gm_error_sar(f$y[1:5], f$X, W25))
  expect_error(morie_morans_i(c(1, 2), W25), "match")
})
