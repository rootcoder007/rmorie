# Functional clustering: a K-component Gaussian mixture on B-spline
# coefficients fitted by EM (James & Sugar 2003; de Boor 1978; Ramsay &
# Silverman 2005).
#
# Anchors outside the module: the partition-of-unity property every
# B-spline basis must satisfy, EM's guarantee that the log-likelihood never
# decreases, the exact closed form of the one-component fit, every reported
# summary recomputed from the returned quantities, and a design of two
# clearly separated curve shapes whose grouping is known in advance.

# two well-separated shapes: a rising line and a falling one
sim_curves <- function(n = 60, m = 25, seed = 1, noise = 0.05) {
  set.seed(seed)
  tv <- seq(0, 1, length.out = m)
  grp <- rep(c(0, 1), length.out = n)
  Y <- t(vapply(seq_len(n), function(i) {
    base <- if (grp[i] == 0) tv else 1 - tv
    base + rnorm(m, 0, noise)
  }, numeric(m)))
  list(Y = Y, grp = grp, t = tv)
}

test_that("the B-spline basis is a partition of unity", {
  d <- sim_curves(20, 30, 2)
  r <- morie_funmix_functional_mixture(d$Y, 2L, t = d$t, n_basis = 6L)
  B <- r$basis
  expect_equal(dim(B), c(30L, 6L))
  # every B-spline basis sums to one at each site, and is non-negative
  expect_equal(rowSums(B), rep(1, 30), tolerance = 1e-9)
  expect_true(all(B >= -1e-12))
  # the basis is local: each row has at most degree + 1 non-zero entries
  expect_true(all(apply(B, 1, function(z) sum(abs(z) > 1e-12)) <= 4))
  # knots span the grid
  expect_true(min(r$knots) <= min(d$t) + 1e-12)
  expect_true(max(r$knots) >= max(d$t) - 1e-12)
  expect_equal(r$grid, d$t)
  expect_equal(r$n_basis, 6L)
  expect_equal(r$degree, 3L)
  # a lower degree is honoured and stays a partition of unity
  r1 <- morie_funmix_functional_mixture(d$Y, 2L, t = d$t, n_basis = 6L,
                                        degree = 1L)
  expect_equal(rowSums(r1$basis), rep(1, 30), tolerance = 1e-9)
  expect_equal(r1$degree, 1L)
})

test_that("the fitted coefficients reproduce smooth curves", {
  # a cubic basis with enough functions represents a smooth curve closely
  m <- 40
  tv <- seq(0, 1, length.out = m)
  Y <- rbind(sin(2 * pi * tv), tv^2, rep(1, m))
  r <- morie_funmix_functional_mixture(Y, 1L, t = tv, n_basis = 10L)
  recon <- r$basis %*% t(r$curve_coefficients)
  expect_equal(dim(r$curve_coefficients), c(3L, 10L))
  for (i in 1:3) {
    expect_equal(as.numeric(recon[, i]), Y[i, ], tolerance = 0.02)
  }
  # a constant curve is represented exactly, since the basis sums to one
  expect_equal(as.numeric(recon[, 3]), rep(1, m), tolerance = 1e-6)
  # the reported mean curve is the basis applied to the mean coefficients
  expect_equal(as.numeric(r$mean_curves[1, ]),
               as.numeric(r$basis %*% r$coefficients[1, ]),
               tolerance = 1e-9)
})

test_that("the EM log-likelihood never decreases", {
  d <- sim_curves(60, 25, 3)
  for (K in 1:3) {
    r <- morie_funmix_functional_mixture(d$Y, K, t = d$t, n_basis = 5L,
                                          max_iter = 300L)
    expect_true(all(diff(r$loglik_path) >= -1e-8))
    expect_equal(r$loglik, tail(r$loglik_path, 1))
    expect_equal(length(r$loglik_path), r$iterations)
  }
  # more components cannot fit worse
  lls <- vapply(1:3, function(K) {
    morie_funmix_functional_mixture(d$Y, K, t = d$t, n_basis = 5L)$loglik
  }, numeric(1))
  expect_true(all(diff(lls) >= -1e-6))
})

test_that("one component reduces to the pooled Gaussian fit", {
  d <- sim_curves(40, 25, 5)
  r <- morie_funmix_functional_mixture(d$Y, 1L, t = d$t, n_basis = 5L)
  expect_equal(r$proportions, 1)
  expect_equal(as.numeric(r$posterior), rep(1, 40))
  # with a single component the M-step returns the plain moments of the
  # coefficient matrix
  C <- r$curve_coefficients
  expect_equal(as.numeric(r$coefficients), colMeans(C), tolerance = 1e-8)
  expect_equal(as.numeric(r$variances),
               apply(C, 2, function(z) mean((z - mean(z))^2)),
               tolerance = 1e-8)
  # and the log-likelihood is the independent-Gaussian one
  mu <- colMeans(C)
  sg <- apply(C, 2, function(z) mean((z - mean(z))^2))
  want <- sum(vapply(seq_len(40), function(i) {
    sum(-0.5 * log(2 * pi * sg) - 0.5 * (C[i, ] - mu)^2 / sg)
  }, numeric(1)))
  expect_equal(r$loglik, want, tolerance = 1e-7)
  expect_equal(r$labels, rep(0, 40))
  expect_equal(r$entropy, 0, tolerance = 1e-9)
  expect_equal(r$n_parameters, 2L * 5L)
})

test_that("two separated shapes are recovered", {
  d <- sim_curves(80, 30, 7, noise = 0.03)
  r <- morie_funmix_functional_mixture(d$Y, 2L, t = d$t, n_basis = 6L)
  expect_equal(length(r$proportions), 2L)
  expect_equal(sum(r$proportions), 1)
  # the two groups are equally sized in the simulation
  expect_equal(r$proportions, c(0.5, 0.5), tolerance = 0.1)
  # the clustering agrees with the truth up to the label convention
  agree <- mean(r$labels == d$grp)
  expect_true(max(agree, 1 - agree) > 0.95)
  # the two mean curves are the rising and falling lines
  mc <- r$mean_curves
  expect_equal(dim(mc), c(2L, 30L))
  # components are ordered by the integral of their mean curve, so the
  # first has the smaller area
  expect_true(sum(mc[1, ]) <= sum(mc[2, ]))
  # one curve rises across the grid and the other falls
  slopes <- c(mc[1, 30] - mc[1, 1], mc[2, 30] - mc[2, 1])
  expect_true(min(slopes) < -0.5)
  expect_true(max(slopes) > 0.5)
  # the assignment is confident, so the classification entropy is small
  expect_true(r$entropy < 0.05 * 80)
  expect_equal(rowSums(r$posterior), rep(1, 80), tolerance = 1e-10)
})

test_that("the reported summaries are recomputable", {
  d <- sim_curves(50, 25, 11)
  r <- morie_funmix_functional_mixture(d$Y, 2L, t = d$t, n_basis = 5L)
  expect_equal(r$n_parameters, 2L - 1L + 2L * 5L + 2L * 5L)
  expect_equal(r$bic, -2 * r$loglik + r$n_parameters * log(50))
  expect_equal(r$aic, -2 * r$loglik + 2 * r$n_parameters)
  expect_equal(r$entropy, -sum(r$posterior * log(pmax(r$posterior, 1e-300))))
  expect_equal(r$labels, apply(r$posterior, 1, which.max) - 1)
  expect_equal(r$estimate, r$labels)
  expect_equal(r$K, 2L)
  expect_equal(r$n, 50L)
  expect_true(all(r$variances > 0))
  expect_match(r$method, "James & Sugar")
  # both criteria penalise the same free-parameter count, so their gap is
  # fixed by n alone
  expect_equal(r$bic - r$aic, r$n_parameters * (log(50) - 2))
})

test_that("the default grid is the unit interval", {
  d <- sim_curves(30, 21, 13)
  a <- morie_funmix_functional_mixture(d$Y, 2L, n_basis = 5L)
  b <- morie_funmix_functional_mixture(d$Y, 2L, t = seq(0, 1, length.out = 21),
                                        n_basis = 5L)
  expect_equal(a$grid, seq(0, 1, length.out = 21))
  expect_equal(a$loglik, b$loglik, tolerance = 1e-9)
  # a rescaled grid is an affine reparametrisation, so the fit is unchanged
  cc <- morie_funmix_functional_mixture(d$Y, 2L, t = seq(0, 10, length.out = 21),
                                         n_basis = 5L)
  expect_equal(cc$labels, a$labels)
})

test_that("the variance floor keeps a degenerate component finite", {
  # identical curves give a component with no spread at all
  m <- 20
  Y <- matrix(rep(seq(0, 1, length.out = m), each = 6), nrow = 6, byrow = TRUE)
  r <- morie_funmix_functional_mixture(Y, 1L, n_basis = 5L)
  expect_true(all(is.finite(r$variances)))
  expect_true(all(r$variances > 0))
  expect_true(is.finite(r$loglik))
  # a larger floor raises the variances it clamps
  r2 <- morie_funmix_functional_mixture(Y, 1L, n_basis = 5L, var_floor = 1e-3)
  expect_true(all(r2$variances >= r$variances - 1e-12))
})

test_that("funmix rejects input it cannot fit", {
  d <- sim_curves(20, 25, 17)
  expect_error(morie_funmix_functional_mixture(matrix(0, 0, 5), 1L),
               "no curves")
  expect_error(morie_funmix_functional_mixture(d$Y, 0L), "K must be at least 1")
  expect_error(morie_funmix_functional_mixture(d$Y, 99L),
               "components for .* curves")
  expect_error(morie_funmix_functional_mixture(d$Y, 2L, t = c(0, 1)),
               "grid points but curves of length")
  # more basis functions than time points is unidentified
  expect_error(morie_funmix_functional_mixture(d$Y, 2L, n_basis = 99L),
               "not identified")
  # a grid with no extent cannot carry a basis
  expect_error(morie_funmix_functional_mixture(d$Y, 2L, t = rep(1, 25)),
               "no extent")
})
