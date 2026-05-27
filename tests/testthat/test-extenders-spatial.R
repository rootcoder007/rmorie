# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 1.m tests for the spatial / multivariate / meta-analysis
# wrapper-as-extender entry points in R/extenders_spatial.R:
#   gstat / copula / kernlab / metafor / mvtnorm.
# Each block guards with skip_if_not_installed() and checks the
# happy-path shape (a 2-element list with $method and $raw).

# ---------------------------------------------------------------------------
# gstat::variogram
# ---------------------------------------------------------------------------

test_that("morie_geostat_variogram wraps gstat::variogram", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  set.seed(1L)
  n <- 60L
  df <- data.frame(
    x = stats::runif(n, 0, 10),
    y = stats::runif(n, 0, 10),
    z = stats::rnorm(n)
  )
  sp::coordinates(df) <- ~ x + y
  out <- morie_geostat_variogram(z ~ 1, data = df)
  expect_type(out, "list")
  expect_identical(out$method, "gstat::variogram")
  expect_false(is.null(out$raw))
  expect_s3_class(out$raw, "gstatVariogram")
})


# ---------------------------------------------------------------------------
# gstat::krige
# ---------------------------------------------------------------------------

test_that("morie_geostat_krige wraps gstat::krige", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  set.seed(1L)
  n <- 80L
  df <- data.frame(
    x = stats::runif(n, 0, 10),
    y = stats::runif(n, 0, 10),
    z = stats::rnorm(n)
  )
  sp::coordinates(df) <- ~ x + y
  grid <- expand.grid(
    x = seq(0, 10, length.out = 5L),
    y = seq(0, 10, length.out = 5L)
  )
  sp::coordinates(grid) <- ~ x + y
  vg <- gstat::variogram(z ~ 1, data = df)
  mod <- gstat::fit.variogram(vg, gstat::vgm(1, "Sph", 5, 1))
  out <- morie_geostat_krige(z ~ 1, df, grid, mod)
  expect_type(out, "list")
  expect_identical(out$method, "gstat::krige")
  expect_false(is.null(out$raw))
})


# ---------------------------------------------------------------------------
# copula::fitCopula
# ---------------------------------------------------------------------------

test_that("morie_copula_fit wraps copula::fitCopula", {
  skip_if_not_installed("copula")
  set.seed(1L)
  true_cop <- copula::normalCopula(0.5, dim = 2L)
  u <- copula::rCopula(200L, true_cop)
  out <- morie_copula_fit(copula::normalCopula(dim = 2L), data = u)
  expect_type(out, "list")
  expect_identical(out$method, "copula::fitCopula")
  expect_false(is.null(out$raw))
  expect_s4_class(out$raw, "fitCopula")
})


# ---------------------------------------------------------------------------
# copula::rCopula
# ---------------------------------------------------------------------------

test_that("morie_copula_sample wraps copula::rCopula", {
  skip_if_not_installed("copula")
  set.seed(1L)
  out <- morie_copula_sample(150L, copula::claytonCopula(2, dim = 3L))
  expect_type(out, "list")
  expect_identical(out$method, "copula::rCopula")
  expect_false(is.null(out$raw))
  expect_true(is.matrix(out$raw))
  expect_identical(dim(out$raw), c(150L, 3L))
})


# ---------------------------------------------------------------------------
# kernlab::kpca
# ---------------------------------------------------------------------------

test_that("morie_kernel_pca wraps kernlab::kpca", {
  skip_if_not_installed("kernlab")
  set.seed(1L)
  x <- matrix(stats::rnorm(200L), ncol = 4L)
  out <- morie_kernel_pca(x, kernel = "rbfdot", features = 2L)
  expect_type(out, "list")
  expect_identical(out$method, "kernlab::kpca")
  expect_false(is.null(out$raw))
  expect_s4_class(out$raw, "kpca")
})


# ---------------------------------------------------------------------------
# kernlab::specc
# ---------------------------------------------------------------------------

test_that("morie_spectral_cluster wraps kernlab::specc", {
  skip_if_not_installed("kernlab")
  set.seed(1L)
  x <- rbind(
    matrix(stats::rnorm(80L, mean = -3), ncol = 2L),
    matrix(stats::rnorm(80L, mean =  3), ncol = 2L)
  )
  out <- morie_spectral_cluster(x, centers = 2L)
  expect_type(out, "list")
  expect_identical(out$method, "kernlab::specc")
  expect_false(is.null(out$raw))
  expect_s4_class(out$raw, "specc")
})


# ---------------------------------------------------------------------------
# metafor::rma
# ---------------------------------------------------------------------------

test_that("morie_meta_rma wraps metafor::rma", {
  skip_if_not_installed("metafor")
  set.seed(1L)
  k <- 12L
  vi <- stats::runif(k, 0.02, 0.10)
  yi <- stats::rnorm(k, mean = 0.3, sd = sqrt(vi))
  out <- morie_meta_rma(yi = yi, vi = vi)
  expect_type(out, "list")
  expect_identical(out$method, "metafor::rma")
  expect_false(is.null(out$raw))
  expect_s3_class(out$raw, "rma.uni")
})


# ---------------------------------------------------------------------------
# mvtnorm::rmvnorm
# ---------------------------------------------------------------------------

test_that("morie_mvnorm_sample wraps mvtnorm::rmvnorm", {
  skip_if_not_installed("mvtnorm")
  set.seed(1L)
  sigma <- matrix(c(1, 0.4, 0.4, 1), 2L, 2L)
  out <- morie_mvnorm_sample(100L, mean = c(0, 0), sigma = sigma)
  expect_type(out, "list")
  expect_identical(out$method, "mvtnorm::rmvnorm")
  expect_false(is.null(out$raw))
  expect_true(is.matrix(out$raw))
  expect_identical(dim(out$raw), c(100L, 2L))
})


# ---------------------------------------------------------------------------
# mvtnorm::pmvnorm
# ---------------------------------------------------------------------------

test_that("morie_mvnorm_pmv wraps mvtnorm::pmvnorm", {
  skip_if_not_installed("mvtnorm")
  set.seed(1L)
  sigma <- matrix(c(1, 0.4, 0.4, 1), 2L, 2L)
  out <- morie_mvnorm_pmv(
    lower = c(-1, -1), upper = c(1, 1),
    mean = c(0, 0), sigma = sigma
  )
  expect_type(out, "list")
  expect_identical(out$method, "mvtnorm::pmvnorm")
  expect_false(is.null(out$raw))
  expect_true(is.numeric(as.numeric(out$raw)))
})
