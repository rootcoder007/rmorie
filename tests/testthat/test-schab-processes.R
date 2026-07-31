# SPDX-License-Identifier: AGPL-3.0-or-later
# Point-process models, Ch 3. Assertions are the book's properties.
# Schabenberger & Gotway (2005), Secs 3.2, 3.3, 3.7.2.

REG <- c(0, 0, 10, 10)

test_that("HPP count has mean equal to variance", {
  set.seed(1)
  lam <- 2
  counts <- vapply(seq_len(400), function(i) sppois(lam, REG)$n, numeric(1))
  expect_equal(mean(counts), lam * 100, tolerance = 0.05)
  expect_equal(stats::var(counts), lam * 100, tolerance = 0.20)
})

test_that("HPP reports the theoretical moments and they are equal", {
  r <- sppois(3, REG, seed = 1)
  expect_equal(r$expected_n, 300)
  expect_equal(r$var_n, r$expected_n)      # Poisson: mean = variance
})

test_that("binomial count is fixed, not random", {
  for (s in 1:5) expect_identical(spbino(150, REG, seed = s)$n, 150L)
})

test_that("binomial sub-region variance is below its mean", {
  r <- spbino(200, REG, seed = 1)
  expect_lt(r$binomial_var_half, r$binomial_mean_half)
  expect_equal(r$binomial_var_half, 200 * 0.5 * 0.5)
  f <- r$counts_in_fraction(0.25)
  expect_equal(unname(f[1]), 50)
  expect_equal(unname(f[2]), 200 * 0.25 * 0.75)
})

test_that("binomial points lie inside the region", {
  p <- spbino(300, REG, seed = 2)$points
  expect_true(all(p[, 1] >= 0 & p[, 1] <= 10))
  expect_true(all(p[, 2] >= 0 & p[, 2] <= 10))
})

test_that("CSR diagnostics sit near one on a CSR pattern", {
  set.seed(0)
  pts <- matrix(stats::runif(1600), 800, 2) * 10
  r <- spcsr(pts, REG)
  expect_lt(abs(r$index_of_dispersion - 1), 0.35)
  expect_lt(abs(r$clark_evans - 1), 0.10)
})

test_that("clustering raises dispersion and lowers Clark-Evans", {
  set.seed(2)
  par <- matrix(stats::runif(80), 40, 2) * 10
  cl <- pmin(pmax(par[rep(1:40, each = 20), ] +
                    matrix(stats::rnorm(1600, 0, 0.15), 800, 2), 0), 10)
  r <- spcsr(cl, REG)
  expect_gt(r$index_of_dispersion, 2)
  expect_lt(r$clark_evans, 0.8)
})

test_that("regularity moves the diagnostics the other way", {
  set.seed(1)
  g <- seq(0.5, 9.5, length.out = 28)
  pts <- as.matrix(expand.grid(g, g))
  pts <- pmin(pmax(pts + matrix(stats::rnorm(nrow(pts) * 2, 0, 0.03),
                                nrow(pts), 2), 0), 10)
  r <- spcsr(pts, REG)
  expect_lt(r$index_of_dispersion, 1)
  expect_gt(r$clark_evans, 1)
})

test_that("Neyman-Scott K exceeds the Poisson K everywhere", {
  r <- seq(0.05, 2, length.out = 20)
  out <- spnscl(r, rho = 10, mu = 5, sigma = 0.1)
  expect_true(all(out$k > out$k_csr))
  expect_true(all(out$excess > 0))
})

test_that("Neyman-Scott excess matches its closed form", {
  r <- c(0, 0.25, 1)
  rho <- 7; sigma <- 0.2
  out <- spnscl(r, rho = rho, mu = 3, sigma = sigma)
  expect_equal(out$excess, (1 - exp(-(r^2) / (4 * sigma^2))) / rho,
               tolerance = 1e-12)
})

test_that("Neyman-Scott excess vanishes as parents get dense", {
  expect_lt(spnscl(0.5, rho = 1e6, mu = 5, sigma = 0.1)$excess, 1e-5)
})

test_that("Neyman-Scott intensity is rho times mu", {
  expect_equal(spnscl(1, rho = 4, mu = 6, sigma = 0.1)$lambda, 24)
})

test_that("Thomas is the Gaussian case of Neyman-Scott", {
  r <- seq(0, 1.5, length.out = 10)
  expect_equal(spthom(r, 10, 5, 0.1)$k, spnscl(r, 10, 5, 0.1)$k,
               tolerance = 1e-15)
  expect_false(is.null(spthom(r, 10, 5, 0.1)$k_function))
})

test_that("process input validation", {
  expect_error(sppois(0, REG), "`lam` must be")
  expect_error(spbino(-1, REG), "`n` must be")
  expect_error(spnscl(1, rho = 0), "must all be")
  expect_error(spnscl(1, mu = 0), "must all be")
  expect_error(spnscl(1, sigma = 0), "must all be")
  expect_error(spnscl(-1), "non-negative")
})
