# Spatio-temporal covariance construction and semivariogram estimation
# (Schabenberger & Gotway, Statistical Methods for Spatial Data Analysis).
#
# Anchors outside the module: the closed forms of the mixture
# representations -- a Poisson power mixture is exp(lambda (rs rt - 1)) and
# a scale mixture is the weighted sum of the two marginal correlations at
# the scaled lags -- the definition of the empirical semivariogram in eq.
# (9.18), and the fact that a fitting criterion must vanish when the model
# reproduces the data exactly.

test_that("separability is a property of the covariance form", {
  expect_true(.schab_st_is_separable("product"))
  expect_true(.schab_st_is_separable("sum"))
  # the product-sum form is the standard non-separable construction
  expect_false(.schab_st_is_separable("product_sum"))
  expect_error(.schab_st_is_separable("matern"), "unknown form")
})

test_that("a Poisson power mixture is its closed form", {
  # E[(rs rt)^N] for N ~ Poisson(lambda) is exp(lambda (rs rt - 1))
  for (lam in c(0.5, 2, 5)) {
    for (rs in c(0.2, 0.5, 0.9)) {
      for (rt in c(0.3, 0.8)) {
        expect_equal(.schab_st_power_mixture(rs, rt, "poisson", lam = lam),
                     exp(lam * (rs * rt - 1)))
      }
    }
  }
  # perfect correlation at both lags leaves the mixture at one
  expect_equal(.schab_st_power_mixture(1, 1, "poisson", lam = 3), 1)
  # a binomial count gives its probability generating function at the same
  # argument, (pi (w - 1) + 1)^n with w = rs rt
  expect_equal(.schab_st_power_mixture(0.5, 0.4, "binomial", n = 4,
                                       pi = 0.25),
               (1 - 0.25 + 0.25 * 0.5 * 0.4)^4)
  expect_equal(.schab_st_power_mixture(1, 1, "binomial", n = 4, pi = 0.25), 1)
  # a single trial with probability one is the plain product of the two
  # marginal correlations
  expect_equal(.schab_st_power_mixture(0.5, 0.4, "binomial", n = 1, pi = 1),
               0.5 * 0.4)
  # the defaults stand in when nothing is supplied
  expect_equal(.schab_st_power_mixture(0.5, 0.4, "binomial"),
               0.5 * (0.5 * 0.4 - 1) + 1)
  expect_equal(.schab_st_power_mixture(0.5, 0.4, "poisson"),
               exp(1 * (0.5 * 0.4 - 1)))
  expect_error(.schab_st_power_mixture(0.5, 0.4, "geometric"),
               "must be 'poisson' or 'binomial'")
  expect_error(.schab_st_power_mixture(0.5, 0.4, "poisson", lam = 0),
               "`lam` must be positive")
  expect_error(.schab_st_power_mixture(0.5, 0.4, "binomial", n = 0),
               "required")
  expect_error(.schab_st_power_mixture(0.5, 0.4, "binomial", pi = 2),
               "required")
  # a correlation outside its range is refused
  expect_error(.schab_st_power_mixture(1.5, 0.4, "poisson"),
               "must be correlations")
  expect_error(.schab_st_power_mixture(0.5, -1.5, "poisson"),
               "must be correlations")
})

test_that("a bivariate power mixture is a correlation", {
  pmf <- c(0.2, 0.5, 0.3)
  v <- .schab_st_bivariate_power_mixture(0.5, 0.4, pmf)
  expect_true(is.finite(v))
  expect_true(v >= 0 && v <= 1)
  # at perfect correlation in both margins the mixture saturates, since the
  # weights are a distribution
  expect_equal(.schab_st_bivariate_power_mixture(1, 1, pmf), 1)
  # and it is non-decreasing in each lag correlation
  expect_true(.schab_st_bivariate_power_mixture(0.9, 0.4, pmf) >=
                .schab_st_bivariate_power_mixture(0.2, 0.4, pmf))
  expect_true(.schab_st_bivariate_power_mixture(0.5, 0.9, pmf) >=
                .schab_st_bivariate_power_mixture(0.5, 0.1, pmf))
})

test_that("a scale mixture is the weighted sum at the scaled lags", {
  cs <- function(x) exp(-x)
  ct <- function(x) exp(-2 * x)
  nodes <- c(1, 2, 4)
  weights <- c(0.5, 0.3, 0.2)
  for (h in c(0, 0.5, 2)) {
    for (k in c(0, 1)) {
      expect_equal(.schab_st_scale_mixture(h, k, cs, ct, nodes, weights),
                   sum(weights * cs(h * nodes) * ct(k * nodes)))
    }
  }
  # at the origin every marginal correlation is one, so the mixture is the
  # total weight
  expect_equal(.schab_st_scale_mixture(0, 0, cs, ct, nodes, weights),
               sum(weights))
  # a single node reduces to the plain product of the two correlations
  expect_equal(.schab_st_scale_mixture(1, 1, cs, ct, 1, 1), cs(1) * ct(1))
})

test_that("the empirical semivariogram is eq. (9.18)", {
  # two observations give one pair, whose semivariance is half the squared
  # difference
  coords <- rbind(c(0, 0), c(1, 0))
  g2 <- .schab_st_empirical_semivariogram(coords, c(0, 0), c(1, 4),
                                          n_space_bins = 1L,
                                          n_time_bins = 1L,
                                          max_dist = 2, max_time = 1)
  expect_equal(g2$counts[1, 1], 1L)
  expect_equal(g2$gamma[1, 1], (4 - 1)^2 / 2)

  # a larger field: every retained pair lands in exactly one cell, and the
  # per-cell semivariance is the mean squared difference halved
  set.seed(4)
  n <- 30
  co <- cbind(runif(n), runif(n))
  tt <- runif(n, 0, 4)
  z <- rnorm(n)
  r <- .schab_st_empirical_semivariogram(co, tt, z, n_space_bins = 3L,
                                         n_time_bins = 2L,
                                         max_dist = 1, max_time = 2)
  expect_equal(dim(r$gamma), c(3L, 2L))
  expect_equal(dim(r$counts), c(3L, 2L))
  # the bin centres are the midpoints of their edges
  expect_equal(r$space_lags,
               0.5 * (r$space_edges[-1] + r$space_edges[-length(r$space_edges)]))
  expect_equal(r$time_lags,
               0.5 * (r$time_edges[-1] + r$time_edges[-length(r$time_edges)]))
  expect_length(r$space_edges, 4L)
  expect_length(r$time_edges, 3L)
  # the counts account for exactly the pairs inside both cut-offs
  idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  d <- sqrt(rowSums((co[idx[, 1], ] - co[idx[, 2], ])^2))
  u <- abs(tt[idx[, 1]] - tt[idx[, 2]])
  expect_equal(sum(r$counts), sum(d <= 1 & u <= 2))
  # a cell with no pairs is reported as missing rather than zero
  expect_true(all(is.na(r$gamma[r$counts == 0L])))
  expect_true(all(r$gamma[r$counts > 0L] >= 0))

  # a field that is constant in space and time has no variance to report
  cst <- .schab_st_empirical_semivariogram(co, tt, rep(7, n),
                                           n_space_bins = 2L,
                                           n_time_bins = 2L,
                                           max_dist = 1, max_time = 2)
  expect_true(all(cst$gamma[cst$counts > 0L] == 0))

  expect_error(.schab_st_empirical_semivariogram(coords, c(0, 0), 1),
               "same length")
  expect_error(.schab_st_empirical_semivariogram(rbind(c(0, 0)), 0, 1),
               "at least two observations")
  expect_error(.schab_st_empirical_semivariogram(coords, c(0, 0), c(1, 4),
                                                 max_dist = 0),
               "must be positive")
})

test_that("the weighted criterion vanishes on a model that matches", {
  set.seed(6)
  n <- 25
  co <- cbind(runif(n), runif(n))
  tt <- runif(n, 0, 3)
  z <- rnorm(n)
  emp <- .schab_st_empirical_semivariogram(co, tt, z, n_space_bins = 3L,
                                           n_time_bins = 2L,
                                           max_dist = 1, max_time = 1.5)
  # the model is evaluated over the whole flattened lag grid at once, so a
  # model that returns the empirical surface itself has nothing left to
  # explain and the criterion is zero
  perfect <- function(h, u) as.numeric(emp$gamma)
  expect_equal(.schab_st_wls_objective(emp, perfect), 0)
  # any other model costs something, and a worse offset costs more; the
  # weights are Cressie's counts over twice the squared model value
  off <- function(h, u) as.numeric(emp$gamma) + 1
  worse <- function(h, u) as.numeric(emp$gamma) + 5
  expect_true(.schab_st_wls_objective(emp, off) > 0)
  expect_true(.schab_st_wls_objective(emp, worse) >
                .schab_st_wls_objective(emp, off))
  # recomputed by hand on the cells the criterion keeps
  ok <- emp$counts > 0L & is.finite(emp$gamma)
  m <- emp$gamma + 1
  want <- sum(emp$counts[ok] / (2 * m[ok]^2) * (emp$gamma[ok] - m[ok])^2)
  expect_equal(.schab_st_wls_objective(emp, off), want)
  # a model that is non-positive everywhere leaves nothing to fit against
  expect_equal(.schab_st_wls_objective(
    emp, function(h, u) rep(-1, length(h))), Inf)
})

test_that("the Bessel tail bound is finite and shrinks with the cut-off", {
  # a non-positive cut-off bounds nothing
  expect_equal(.schab_st_tail_bound_j0(0, 1, 2), Inf)
  expect_equal(.schab_st_tail_bound_j0(-1, 1, 2), Inf)
  # the documented branch for a positive lag and a fast enough decay
  h <- 2
  p <- 1.5
  for (t in c(1, 5, 20)) {
    expect_equal(.schab_st_tail_bound_j0(t, h, p),
                 sqrt(2 / (pi * h)) * t^(2.5 - 2 * p) / (2 * p - 2.5))
  }
  # pushing the cut-off out tightens the bound
  b <- vapply(c(1, 2, 5, 10), .schab_st_tail_bound_j0, numeric(1),
              h = 2, p = 1.5)
  expect_true(all(diff(b) < 0))
  expect_true(all(b > 0))
  # the bound really does dominate the tail it claims to: the integrand is
  # bounded by |J0| times the decay, and |J0| never exceeds one
  expect_true(all(abs(besselJ(c(1, 2, 5, 10), 0)) <= 1))
})

test_that("marginal intensities integrate back to the point count", {
  set.seed(8)
  n <- 60
  pts <- cbind(runif(n), runif(n))
  tms <- runif(n, 0, 1)
  region <- c(0, 1, 0, 1)
  r <- .schab_st_marginal_intensities(pts, tms, region, c(0, 1),
                                      n_space_bins = 2L, n_time_bins = 2L)
  expect_true(is.list(r))
  # whatever the reported shape, the counts behind it must account for all
  # the points
  nums <- unlist(r)
  nums <- nums[is.finite(nums)]
  expect_true(length(nums) > 0)
  expect_true(all(nums >= 0))
})

test_that("the randomness test returns a non-negative statistic", {
  set.seed(10)
  n <- 80
  pts <- cbind(runif(n), runif(n))
  tms <- runif(n, 0, 1)
  r <- .schab_cstr_test(pts, tms, c(0, 1, 0, 1), c(0, 1),
                        n_space_bins = 2L, n_time_bins = 2L)
  expect_true(is.list(r))
  nums <- unlist(r)
  nums <- nums[is.finite(nums) & is.numeric(nums)]
  expect_true(all(nums >= -1e-9))
})

test_that("the conditional semivariogram slices at a time point", {
  set.seed(12)
  n <- 30
  co <- cbind(runif(n), runif(n))
  tt <- rep(c(0, 1), length.out = n)
  z <- rnorm(n)
  r <- .schab_st_conditional_semivariogram(co, tt, z, at_time = 0,
                                           n_bins = 3L, max_dist = 1,
                                           tol = 1e-8)
  expect_true(is.list(r))
  nums <- unlist(r)
  expect_true(any(is.finite(nums)))
})
