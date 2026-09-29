# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/painn_native.R (PaiNN equivariant message passing,
# Schutt, Unke & Gastegger 2021). The message and update are
# recomputed with matrix algebra, the dipole from its definition, and
# the equivariance check is exercised on an equivariant model (error 0)
# and on one that breaks it.

.pn_V <- rbind(c(1, 0, 2), c(-1, 2, 0), c(0.5, 1, -1))
.pn_s <- c(0.3, -0.2, 1)
.pn_Q <- local({
  th <- 0.7
  rbind(c(cos(th), -sin(th), 0), c(sin(th), cos(th), 0), c(0, 0, 1))
})

test_that("vector_norm takes the norm down the spatial axis", {
  expect_equal(vector_norm(.pn_V), sqrt(colSums(.pn_V^2)), tolerance = 1e-12)
  expect_equal(vector_norm(matrix(c(3, 4), 2, 1)), 5)
  expect_equal(vector_norm(list(c(0, 0), c(0, 0))), c(0, 0))
})

test_that("scalar_vector_message scales the vectors and adds s * r_hat", {
  r <- c(1, 2, 2)
  hat <- r / 3
  W <- function(d) c(d, d^2)
  ps <- function(s, w) s * w[1]
  pv <- function(s, w) c(s * w[2], rev(s))
  m <- scalar_vector_message(.pn_s, .pn_V, r, ps, pv, W)
  expect_equal(m$ds, .pn_s * 3, tolerance = 1e-12)
  sc <- .pn_s * 9
  ad <- rev(.pn_s)
  expect_equal(m$dv, .pn_V * rep(sc, each = 3) + outer(hat, ad), tolerance = 1e-12)
  expect_error(scalar_vector_message(.pn_s, .pn_V, c(0, 0, 0), ps, pv, W), "same position")
  expect_error(scalar_vector_message(.pn_s, .pn_V, r, function(s, w) 1, pv, W), "mis-sized")
})

test_that("gated_update mixes types only through the invariant inner product", {
  U <- rbind(c(1, 0, 0.5), c(0, 2, -1), c(1, 1, 0))
  V <- rbind(c(0, 1, 0), c(1, 0, 1), c(-1, 0, 2))
  phi <- function(s, dot, nrm) list(ds = s + dot, gate = nrm)
  g <- gated_update(.pn_s, .pn_V, U, V, phi)
  Uv <- .pn_V %*% t(U)
  Vw <- .pn_V %*% t(V)
  dot <- colSums(Uv * Vw)
  expect_equal(g$scalar_from_vectors, dot, tolerance = 1e-12)
  expect_equal(g$ds, .pn_s + dot, tolerance = 1e-12)
  expect_equal(g$dv, Uv * rep(sqrt(colSums(Vw^2)), each = 3), tolerance = 1e-12)
  # the inner product is invariant: rotating the vectors leaves it alone
  rot <- .pn_Q %*% .pn_V
  expect_equal(gated_update(.pn_s, rot, U, V, phi)$scalar_from_vectors, dot, tolerance = 1e-12)
  expect_error(gated_update(.pn_s, .pn_V, U, V, function(s, d, n) list(ds = 1, gate = n)), "mis-sized")
})

test_that("dipole_moment is sum q (r - centre)", {
  q <- c(1, -1, 0.5)
  R <- rbind(c(1, 0, 0), c(0, 2, 0), c(0, 0, 4))
  cen <- colMeans(R)
  d <- dipole_moment(q, R)
  expect_equal(d$dipole, as.numeric(colSums(q * sweep(R, 2, cen))), tolerance = 1e-12)
  expect_equal(d$magnitude, sqrt(sum(d$dipole^2)), tolerance = 1e-12)
  d0 <- dipole_moment(q, R, centre = c(0, 0, 0))
  expect_equal(d0$dipole, as.numeric(colSums(q * R)), tolerance = 1e-12)
  # a neutral molecule's dipole does not depend on the centre
  qn <- c(1, -1, 0)
  expect_equal(dipole_moment(qn, R)$dipole, dipole_moment(qn, R, centre = c(5, 5, 5))$dipole,
               tolerance = 1e-12)
  # rotating the molecule rotates the dipole
  expect_equal(dipole_moment(q, R %*% t(.pn_Q), centre = as.numeric(.pn_Q %*% cen))$dipole,
               as.numeric(.pn_Q %*% d$dipole), tolerance = 1e-12)
  expect_error(dipole_moment(c(1, 2), R), "2 charges but 3 positions")
})

test_that("equivariance_error is zero for an equivariant model and positive otherwise", {
  R <- rbind(c(0, 0, 0), c(1, 0.5, -1), c(-0.5, 1, 0.25))
  good <- function(s, v, pos) {
    list(s = s + vector_norm(v), v = 2 * v)
  }
  r <- morie_painn_equivariance_error(good, .pn_s, .pn_V, R, .pn_Q)
  expect_lt(r$scalar_error, 1e-12)
  expect_lt(r$vector_error, 1e-12)
  expect_true(r$scalars_invariant)
  expect_true(r$vectors_equivariant)
  # a model that reads a raw coordinate is not invariant
  bad_s <- function(s, v, pos) list(s = s + pos[2, 1], v = v)
  b1 <- morie_painn_equivariance_error(bad_s, .pn_s, .pn_V, R, .pn_Q)
  expect_gt(b1$scalar_error, 1e-6)
  expect_false(b1$scalars_invariant)
  # a model that mixes the spatial axes is not equivariant
  bad_v <- function(s, v, pos) list(s = s, v = v[c(2, 3, 1), , drop = FALSE])
  b2 <- morie_painn_equivariance_error(bad_v, .pn_s, .pn_V, R, .pn_Q)
  expect_true(b2$scalars_invariant)
  expect_gt(b2$vector_error, 1e-6)
  expect_false(b2$vectors_equivariant)
  # a model that drops its vectors keeps the scalars invariant, which is
  # exactly the failure the check exists to catch
  zero <- function(s, v, pos) list(s = s, v = 0 * v)
  z <- morie_painn_equivariance_error(zero, .pn_s, .pn_V, R, .pn_Q)
  expect_true(z$scalars_invariant)
  expect_true(z$vectors_equivariant)
})
