# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/schN_native.R (SchNet continuous-filter convolutions,
# Schutt et al. 2017): the Gaussian distance expansion, the cosine
# cutoff, the interaction sum, numerical forces (checked against an
# analytic gradient) and the invariance / equivariance diagnostics.

.sn_R <- rbind(c(0, 0, 0), c(1.2, 0, 0), c(0, 1.5, 0.3))
.sn_X <- rbind(c(1, 0.5), c(-1, 2), c(0.25, -0.5))
.sn_Q <- local({
  th <- 0.4
  rbind(c(cos(th), -sin(th), 0), c(sin(th), cos(th), 0), c(0, 0, 1))
})
# a rotation-invariant energy: sum over pairs of exp(-r^2)
.sn_E <- function(P) {
  s <- 0
  for (i in 1:(nrow(P) - 1)) for (j in (i + 1):nrow(P)) {
    s <- s + exp(-sum((P[i, ] - P[j, ])^2))
  }
  s
}

test_that("gaussian_expansion places n centres and decays as exp(-gamma (r - mu)^2)", {
  g <- gaussian_expansion(2.5, 0, 6, 25)
  step <- 6 / 24
  mus <- 0 + step * (0:24)
  expect_equal(g, exp(-(2.5 - mus)^2 / (2 * step^2)), tolerance = 1e-12)
  expect_length(g, 25L)
  expect_equal(max(g), 1, tolerance = 1e-3)
  expect_equal(gaussian_expansion(1, 0, 2, 3, gamma = 1), exp(-c(1, 0, 1)), tolerance = 1e-12)
  expect_error(gaussian_expansion(1, n_gaussians = 1), "at least 2 Gaussians")
  expect_error(gaussian_expansion(1, 6, 0), "mu_max must exceed mu_min")
})

test_that("cosine_cutoff falls smoothly to zero at the cutoff", {
  expect_equal(cosine_cutoff(0, 5), 1)
  expect_equal(cosine_cutoff(2.5, 5), 0.5, tolerance = 1e-12)
  expect_equal(cosine_cutoff(5, 5), 0)
  expect_equal(cosine_cutoff(7, 5), 0)
  expect_equal(cosine_cutoff(1, 5), 0.5 * (cos(pi / 5) + 1), tolerance = 1e-12)
  expect_error(cosine_cutoff(1, 0), "cutoff must be positive")
})

test_that("cfconv sums neighbour features times the filter and the cutoff", {
  fnet <- function(gexp) c(sum(gexp), sum(gexp^2))
  out <- cfconv(.sn_X, .sn_R, fnet, cutoff = 4)
  ex <- matrix(0, 3, 2)
  for (i in 1:3) for (j in 1:3) {
    if (i == j) next
    r <- sqrt(sum((.sn_R[i, ] - .sn_R[j, ])^2))
    w <- fnet(gaussian_expansion(r))
    ex[i, ] <- ex[i, ] + .sn_X[j, ] * w * cosine_cutoff(r, 4)
  }
  expect_equal(out, ex, tolerance = 1e-12)
  # beyond the cutoff a neighbour contributes nothing
  far <- cfconv(.sn_X, .sn_R, fnet, cutoff = 0.5)
  expect_equal(far, matrix(0, 3, 2))
  # the convolution is rotation invariant: only distances enter
  expect_equal(cfconv(.sn_X, .sn_R %*% t(.sn_Q), fnet, cutoff = 4), out, tolerance = 1e-12)
  expect_equal(schnet(.sn_X, .sn_R, fnet, cutoff = 4), out, tolerance = 1e-12)
  expect_equal(morie_schN(.sn_X, .sn_R, fnet, cutoff = 4), out, tolerance = 1e-12)
  expect_error(cfconv(.sn_X, .sn_R[1:2, ], fnet), "3 feature rows but 2 positions")
  expect_error(cfconv(.sn_X, .sn_R, function(g) 1), "1-dimensional but the features are 2")
})

test_that("forces_from_energy is the central difference of minus the energy", {
  f <- forces_from_energy(.sn_E, .sn_R)
  # analytic gradient of sum exp(-r^2): dE/dx_i = sum_j -2 exp(-r^2) (x_i - x_j)
  an <- matrix(0, 3, 3)
  for (i in 1:3) for (j in 1:3) {
    if (i == j) next
    d <- .sn_R[i, ] - .sn_R[j, ]
    an[i, ] <- an[i, ] + 2 * exp(-sum(d^2)) * d
  }
  expect_equal(f$forces, an, tolerance = 1e-6)
  # a conservative field of internal forces sums to zero
  expect_equal(f$net_force, rep(0, 3), tolerance = 1e-9)
  expect_identical(f$estimate, f$forces)
  # the step size is honoured
  big <- forces_from_energy(.sn_E, .sn_R, h = 1e-2)
  expect_equal(big$forces, an, tolerance = 1e-3)
})

test_that("invariance_error passes an invariant energy and fails a coordinate-reading one", {
  r <- invariance_error(.sn_E, .sn_R, .sn_Q)
  expect_lt(r$energy_error, 1e-8)
  expect_true(r$energy_invariant)
  expect_true(r$forces_equivariant)
  # a translation as well as the rotation
  rt <- invariance_error(.sn_E, .sn_R, .sn_Q, g = c(1, -2, 0.5))
  expect_true(rt$energy_invariant)
  expect_true(rt$forces_equivariant)
  bad <- function(P) sum(P[, 1]^2)
  b <- invariance_error(bad, .sn_R, .sn_Q)
  expect_gt(b$energy_error, 1e-6)
  expect_false(b$energy_invariant)
  expect_false(b$forces_equivariant)
})
