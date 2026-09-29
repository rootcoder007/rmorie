# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/manfd_native.R (functional manifold learning: L2
# curve distances, k-NN geodesics, Isomap / classical scaling). The
# Jacobi eigensolver is checked against eigen(), classical scaling
# against cmdscale(), and geodesics against a hand-worked graph.

.mf_Y <- rbind(sin(seq(0, 3, length.out = 6)), cos(seq(0, 3, length.out = 6)),
               seq(0, 1, length.out = 6), (seq(0, 1, length.out = 6))^2,
               rep(0.5, 6), c(1, 0, 1, 0, 1, 0))
.mf_sign <- function(v) apply(v, 2, function(x) x * sign(x[which.max(abs(x))]))

test_that("l2 is the trapezoid L2 distance between curves", {
  g <- c(0, 0.5, 1, 2, 2.5, 4)
  D <- morie_manfd_l2(.mf_Y, g)
  for (i in 1:3) for (j in 4:6) {
    d2 <- (.mf_Y[i, ] - .mf_Y[j, ])^2
    expect_equal(D[i, j], sqrt(sum(0.5 * (head(d2, -1) + tail(d2, -1)) * diff(g))), tolerance = 1e-12)
  }
  expect_equal(D, t(D))
  expect_equal(morie_manfd_l2(.mf_Y[, 1:2])[1, 2], sqrt(0.5 * sum((.mf_Y[1, 1:2] - .mf_Y[2, 1:2])^2)),
               tolerance = 1e-12)
  expect_error(morie_manfd_l2(.mf_Y, 1:3), "match the curve length")
  expect_error(morie_manfd_l2(.mf_Y, c(0, 1, 1, 2, 3, 4)), "strictly increasing")
})

test_that("knn keeps the k nearest and symmetrises; paths run Floyd-Warshall", {
  D <- matrix(c(0, 1, 4, 9,
                1, 0, 2, 7,
                4, 2, 0, 3,
                9, 7, 3, 0), 4)
  A <- morie_manfd_knn(D, 1)
  expect_equal(A[1, 2], 1)
  expect_equal(A[3, 4], 3)
  expect_equal(A[2, 3], 2)
  expect_true(is.infinite(A[1, 3]))
  expect_equal(A, pmin(A, t(A)))
  An <- morie_manfd_knn(D, 1, symmetric = FALSE)
  expect_true(is.infinite(An[2, 3]))
  sp <- morie_manfd_paths(A)
  expect_equal(sp$G[1, 4], 1 + 2 + 3)
  expect_equal(sp$G[1, 3], 3)
  expect_equal(sp$components, 1L)
  B <- matrix(Inf, 4, 4)
  diag(B) <- 0
  B[1, 2] <- B[2, 1] <- 1
  expect_equal(morie_manfd_paths(B)$components, 3L)
  expect_error(morie_manfd_knn(D, 0), "at least one neighbour")
  expect_error(morie_manfd_knn(D, 4), "smaller than the sample size")
})

test_that("jacobi agrees with eigen() up to the stated sign convention", {
  S <- crossprod(matrix(c(2, -1, 0.5, 1, 3, -2, 0.3, 1, 4, 2, -1, 0.7), 4))
  j <- morie_manfd_jacobi(S)
  e <- eigen(S, symmetric = TRUE)
  expect_equal(j$values, e$values, tolerance = 1e-10)
  expect_equal(j$vectors, .mf_sign(e$vectors), tolerance = 1e-9)
  expect_equal(morie_manfd_jacobi(diag(c(1, 3, 2)))$values, c(3, 2, 1))
})

test_that("scaling is classical MDS (cmdscale) on the double-centred squares", {
  D <- morie_manfd_l2(.mf_Y)
  sc <- morie_manfd_scaling(D, 2)
  cm <- cmdscale(D, k = 2, eig = TRUE)
  expect_equal(sc$values[1:2], cm$eig[1:2], tolerance = 1e-10)
  expect_equal(sc$coords, .mf_sign(cm$points) * 1, tolerance = 1e-8)
  J <- diag(6) - 1 / 6
  expect_equal(sc$B, -0.5 * J %*% (D^2) %*% J, tolerance = 1e-12)
  expect_error(morie_manfd_scaling(D, 7), "1..n")
})

test_that("morie_manfd: Isomap on a complete graph is classical scaling", {
  D <- morie_manfd_l2(.mf_Y)
  iso <- morie_manfd(.mf_Y, k = 5)
  mds <- morie_manfd(.mf_Y, k = 5, method = "mds")
  expect_equal(iso$geodesic, D, tolerance = 1e-12)
  expect_equal(iso$coords, mds$coords, tolerance = 1e-10)
  a <- D[upper.tri(D)]
  b <- as.matrix(dist(iso$coords))[upper.tri(D)]
  expect_equal(iso$residual_variance, 1 - cor(a, b)^2, tolerance = 1e-10)
  g <- morie_manfd(.mf_Y, k = 2, method = "geodesic_only")
  expect_equal(dim(g$coords), c(0L, 0L))
  expect_equal(g$geodesic, morie_manfd_paths(morie_manfd_knn(D, 2))$G, tolerance = 1e-12)
  expect_equal(g$geodesic_max, max(g$geodesic[is.finite(g$geodesic)]))
  expect_error(morie_manfd(.mf_Y, method = "lle"), "method must be one of")
  expect_error(morie_manfd(.mf_Y[1:2, ]), "at least three curves")
})

test_that("morie_manfd_cheatsheet lists the methods", {
  expect_match(morie_manfd_cheatsheet(), "isomap, mds, geodesic_only", fixed = TRUE)
})
