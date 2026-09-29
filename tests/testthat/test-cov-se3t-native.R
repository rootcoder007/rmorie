# SE(3)-equivariant attention (Fuchs et al. 2020): Rodrigues rotations,
# invariant distances, Gaussian radial kernels, attention built from
# invariants, and the equivariance check.

s3_P <- rbind(c(0, 0, 0), c(1, 0.2, -0.3), c(-0.5, 1.1, 0.4), c(0.3, -0.8, 1.2))
s3_S <- c(0.5, -0.2, 1, 0.1)
s3_V <- rbind(c(1, 0, 0), c(0, 1, 0.5), c(-0.3, 0.2, 1), c(0.4, 0.4, -0.4))

test_that("Rodrigues' formula gives a rotation about the axis", {
  k <- c(1, 2, 2) / 3
  th <- 0.7
  K <- matrix(c(0, k[3], -k[2], -k[3], 0, k[1], k[2], -k[1], 0), 3)
  ref <- diag(3) + sin(th) * K + (1 - cos(th)) * K %*% K
  R <- morie_se3T_rotation_matrix(c(1, 2, 2), th)
  expect_equal(R, ref, tolerance = 1e-14)
  expect_equal(crossprod(R), diag(3), tolerance = 1e-14)
  expect_equal(det(R), 1, tolerance = 1e-14)
  expect_equal(as.numeric(R %*% k), k, tolerance = 1e-14)
  expect_identical(morie_se3T(c(1, 2, 2), th), R)
  expect_error(morie_se3T_rotation_matrix(c(0, 0, 0), 1), "axis is zero")
})

test_that("invariant features: distance and unit direction", {
  f <- morie_se3T_invariant_features(s3_P, 1, 2)
  d <- s3_P[2, ] - s3_P[1, ]
  expect_equal(f$distance, sqrt(sum(d^2)), tolerance = 1e-15)
  expect_equal(f$direction, d / sqrt(sum(d^2)), tolerance = 1e-15)
  expect_identical(morie_se3T_invariant_features(s3_P, 3, 3)$direction, c(0, 0, 0))
})

test_that("the radial kernel is a Gaussian mixture in the distance", {
  w <- c(0.5, -1, 2)
  r <- 1.3
  expect_equal(morie_se3T_radial_kernel(r, w, sigma = 0.7),
               sum(w * exp(-(r - 0:2)^2 / (2 * 0.49))), tolerance = 1e-15)
  expect_equal(morie_se3T_radial_kernel(0.4), exp(-0.08), tolerance = 1e-15)
  expect_error(morie_se3T_radial_kernel(-1), "negative")
  expect_error(morie_se3T_radial_kernel(1, sigma = 0), "sigma")
})

test_that("SE(3) attention: softmax of (s_i s_j + k(r_ij)) / T over values", {
  w <- c(0.3, 0.8)
  r <- morie_se3T_se3_attention(s3_P, s3_S, s3_V, weights = w, sigma = 0.9, temperature = 1.5)
  D <- as.matrix(stats::dist(s3_P))
  K <- matrix(vapply(D, function(d) sum(w * exp(-(d - 0:1)^2 / (2 * 0.81))), 1), 4)
  L <- (outer(s3_S, s3_S) + K) / 1.5
  A <- exp(L - apply(L, 1, max))
  A <- A / rowSums(A)
  expect_equal(r$weights, A, tolerance = 1e-14)
  expect_equal(r$type1, A %*% s3_V, tolerance = 1e-14)
  expect_equal(r$type0, as.numeric(A %*% s3_S), tolerance = 1e-14)
  expect_identical(morie_se3T_se3_transformer(s3_P, s3_S, s3_V)$type0,
                   morie_se3T_se3_attention(s3_P, s3_S, s3_V)$type0)
  expect_error(morie_se3T_se3_attention(s3_P, s3_S[-1], s3_V), "positions")
  expect_error(morie_se3T_se3_attention(s3_P, s3_S, s3_V[, 1:2]), "3-vectors")
  expect_error(morie_se3T_se3_attention(s3_P, s3_S, s3_V, temperature = 0), "temperature")
})

test_that("the equivariance check passes for the layer and fails for a non-equivariant one", {
  ok <- morie_se3T_check_equivariance(s3_P, s3_S, s3_V)
  expect_true(ok$equivariant)
  expect_true(ok$weights_invariant)
  expect_lt(ok$type1_deviation, 1e-12)
  # a layer that adds absolute position to the values breaks equivariance
  bad <- function(P, s, V) {
    r <- morie_se3T_se3_attention(P, s, V)
    r$type1 <- r$type1 + P
    r
  }
  nb <- morie_se3T_check_equivariance(s3_P, s3_S, s3_V, layer = bad)
  expect_false(nb$equivariant)
  expect_gt(nb$type1_deviation, 0.1)
  expect_match(morie_se3T_cheatsheet(), "INVARIANT", fixed = TRUE)
})
