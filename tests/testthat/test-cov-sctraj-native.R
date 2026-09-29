# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sctraj_native.R (Slingshot, Street et al. 2018). The
# eq. (1) distance is recomputed with cov() and solve(), the MST and
# its lineages on a distance matrix whose tree is known, the cosine
# kernel CDF with integrate(), and the eq. (2) average on straight
# curves whose arc-length parametrisation is exact.

.st_X <- rbind(c(0, 0), c(0.5, 0.2), c(1, -0.1), c(0.3, 0.4),
               c(3, 0.1), c(3.4, -0.3), c(2.8, 0.5), c(3.1, 0.2),
               c(5, 2), c(5.5, 2.6), c(4.8, 2.3),
               c(5, -2), c(5.6, -2.4), c(4.7, -2.2))
.st_lab <- c(rep("A", 4), rep("B", 4), rep("C", 3), rep("D", 3))

test_that("cluster_distances implements eq. (1) for all three covariance choices", {
  r <- morie_sctraj_cluster_distances(.st_X, .st_lab)
  expect_identical(r$clusters, c("A", "B", "C", "D"))
  ia <- .st_lab == "A"
  ib <- .st_lab == "B"
  d <- colMeans(.st_X[ia, ]) - colMeans(.st_X[ib, ])
  S <- cov(.st_X[ia, ]) + cov(.st_X[ib, ])
  expect_equal(r$distances["A", "B"], sqrt(sum(d * solve(S, d))), tolerance = 1e-12)
  expect_equal(r$covariances$C, cov(.st_X[.st_lab == "C", ]), tolerance = 1e-12)
  expect_equal(r$centers$D, colMeans(.st_X[.st_lab == "D", ]), tolerance = 1e-12)
  dg <- morie_sctraj_cluster_distances(.st_X, .st_lab, cov = "diagonal")
  expect_equal(dg$distances["A", "B"], sqrt(sum(d^2 / diag(S))), tolerance = 1e-12)
  eu <- morie_sctraj_cluster_distances(.st_X, .st_lab, cov = "euclidean")
  expect_equal(eu$distances["A", "B"], sqrt(sum(d^2)), tolerance = 1e-12)
  expect_equal(eu$distances, t(eu$distances))
  # weights give weighted centres and the unbiased weighted covariance
  w <- seq(0.5, 2, length.out = 14)
  rw <- morie_sctraj_cluster_distances(.st_X, .st_lab, weights = w)
  wa <- w[ia]
  expect_equal(rw$centers$A, colSums(.st_X[ia, ] * wa) / sum(wa), tolerance = 1e-12)
  expect_equal(rw$covariances$A, cov.wt(.st_X[ia, ], wa)$cov, tolerance = 1e-12)
  expect_error(morie_sctraj_cluster_distances(.st_X, .st_lab, cov = "robust"), "cov must be one of")
  expect_error(morie_sctraj_cluster_distances(.st_X, .st_lab[-1]), "one label per cell")
  expect_error(morie_sctraj_cluster_distances(.st_X, rep("A", 14)), "two clusters")
  expect_error(morie_sctraj_cluster_distances(.st_X, .st_lab, weights = -w), "non-negative")
  expect_error(morie_sctraj_cluster_distances(rbind(c(0, 0), c(1, 1), c(2, 2), c(3, 3)), c("a", "a", "b", "b")),
               "singular")
  expect_error(morie_sctraj_cluster_distances(matrix(c(1, NA), 1), "a"), "non-finite")
})

test_that("minimum_spanning_tree and lineages_from_tree recover a known tree", {
  nm <- c("A", "B", "C", "D")
  D <- matrix(c(0, 1, 5, 6,
                1, 0, 2, 3,
                5, 2, 0, 4,
                6, 3, 4, 0), 4, dimnames = list(nm, nm))
  t1 <- morie_sctraj_minimum_spanning_tree(D, nm)
  w <- vapply(t1$edges, function(e) e$weight, 0)
  expect_equal(sum(w), 1 + 2 + 3)
  expect_setequal(t1$adjacency$B, c("A", "C", "D"))
  lin <- morie_sctraj_lineages_from_tree(t1, "A")
  expect_identical(lin, list(c("A", "B", "C"), c("A", "B", "D")))
  # C forced terminal: MST on A, B, D then C hangs off its nearest inner node (B)
  t2 <- morie_sctraj_minimum_spanning_tree(D, nm, ends = "C")
  expect_identical(t2$adjacency$C, "B")
  expect_identical(morie_sctraj_lineages_from_tree(t2, "D"), list(c("D", "B", "A"), c("D", "B", "C")))
  expect_error(morie_sctraj_minimum_spanning_tree(D, "A"), "two clusters")
  expect_error(morie_sctraj_minimum_spanning_tree(D, nm, ends = "E"), "not a cluster")
  expect_error(morie_sctraj_minimum_spanning_tree(D, nm, ends = nm), "every cluster")
  expect_error(morie_sctraj_lineages_from_tree(t1, "Z"), "root is not")
})

test_that("principal_curve returns arc-length pseudotime on collinear cells", {
  X <- cbind(c(0.5, 1, 2, 2.5, 3.5, 4), 1)
  f <- morie_sctraj_principal_curve(X, rbind(c(0, 1), c(5, 1)))
  expect_equal(f$pseudotime, X[, 1] - 0.5, tolerance = 1e-12)
  expect_equal(f$distance, rep(0, 6), tolerance = 1e-12)
  expect_equal(f$sse, 0, tolerance = 1e-12)
  # off-line cells: distance is the perpendicular offset to the fitted line
  Y <- cbind(1:6, c(1, -1, 1, -1, 1, -1))
  g <- morie_sctraj_principal_curve(Y, rbind(c(0, 0), c(7, 0)), max_iter = 1)
  expect_equal(g$pseudotime, 0:5, tolerance = 1e-12)
  expect_equal(g$distance, rep(1, 6), tolerance = 1e-12)
  expect_error(morie_sctraj_principal_curve(X, rbind(c(0, 1))), "two points")
  expect_error(morie_sctraj_principal_curve(X, rbind(c(0, 1), c(1, 1)), weights = 1), "one weight")
  expect_error(morie_sctraj_principal_curve(X, rbind(c(0, 1), c(1, 1)), max_iter = 0), "at least 1")
})

test_that("average_curve averages curves at equal arc length (eq. 2)", {
  c1 <- rbind(c(0, 0), c(2, 0))
  c2 <- rbind(c(0, 1), c(0, 3), c(0, 5))
  a <- morie_sctraj_average_curve(list(c1, c2), n_points = 3, return_grid = TRUE)
  u <- c(0, 1, 2)
  expect_equal(a$grid, u)
  expect_equal(a$curve, cbind(u / 2, (1 + u) / 2), tolerance = 1e-12)
  expect_equal(morie_sctraj_average_curve(list(c1), n_points = 2), c1)
  expect_error(morie_sctraj_average_curve(list()), "nothing to average")
  expect_error(morie_sctraj_average_curve(list(c1), n_points = 1), "at least 2")
})

test_that("cosine_cdf integrates 1 + cos(2 pi u); shrinkage_weight is 1 - F (eq. 4)", {
  for (u in c(-0.3, 0, 0.17, 0.45)) {
    expect_equal(morie_sctraj_cosine_cdf(u),
                 integrate(function(x) 1 + cos(2 * pi * x), -0.5, u, rel.tol = 1e-12)$value,
                 tolerance = 1e-10)
  }
  expect_identical(morie_sctraj_cosine_cdf(-1), 0)
  expect_identical(morie_sctraj_cosine_cdf(0.7), 1)
  expect_equal(morie_sctraj_shrinkage_weight(3, 2, 6), 1 - morie_sctraj_cosine_cdf(1 / 4 - 0.5), tolerance = 1e-12)
  expect_equal(morie_sctraj_shrinkage_weight(3, 2, 6, "as_printed"), 1 - morie_sctraj_cosine_cdf(3 / 4 - 0.5),
               tolerance = 1e-12)
  expect_equal(morie_sctraj_shrinkage_weight(2, 2, 6), 1)
  expect_equal(morie_sctraj_shrinkage_weight(6, 2, 6), 0, tolerance = 1e-12)
  expect_identical(morie_sctraj_shrinkage_weight(1, 2, 6), 1)
  expect_identical(morie_sctraj_shrinkage_weight(7, 2, 6), 0)
  expect_identical(morie_sctraj_shrinkage_weight(2, 2, 2), 1)
  expect_identical(morie_sctraj_shrinkage_weight(3, 2, 2), 0)
  expect_error(morie_sctraj_shrinkage_weight(3, 2, 6, "literal"), "arg must be")
})

test_that("morie_sctraj chains tree, lineages and principal curves", {
  f <- morie_sctraj(.st_X, .st_lab, root = "A", cov = "euclidean", shrink = FALSE)
  info <- morie_sctraj_cluster_distances(.st_X, .st_lab, "euclidean")
  tr <- morie_sctraj_minimum_spanning_tree(info$distances, info$clusters)
  lins <- morie_sctraj_lineages_from_tree(tr, "A")
  expect_identical(f$lineages, lins)
  expect_identical(f$lineages, list(c("A", "B", "C"), c("A", "B", "D")))
  for (m in 1:2) {
    init <- do.call(rbind, info$centers[lins[[m]]])
    pc <- morie_sctraj_principal_curve(.st_X, init, as.numeric(.st_lab %in% lins[[m]]))
    expect_equal(f$pseudotime[[m]], pc$pseudotime, tolerance = 1e-12)
    expect_equal(f$distance[[m]], pc$distance, tolerance = 1e-12)
  }
  best <- pmin(f$distance[[1]], f$distance[[2]])
  expect_equal(f$weights[[1]], ifelse(f$distance[[1]] <= best + 1e-12, 1, best / f$distance[[1]]), tolerance = 1e-12)
  s <- morie_sctraj_pseudotime_trajectory(.st_X, .st_lab, root = "A", cov = "euclidean")
  expect_true(s$shrink)
  expect_equal(s$n_lineages, 2L)
  for (m in 1:2) expect_equal(min(s$pseudotime[[m]]), 0)
  expect_identical(morie_sctraj_scrnaseq_trajectory(.st_X, .st_lab, "A", cov = "euclidean")$pseudotime, s$pseudotime)
  expect_error(morie_sctraj(.st_X, .st_lab[-1], "A"), "one label per cell")
})

test_that("morie_sctraj_cheatsheet states the eq. (1) distance", {
  s <- morie_sctraj_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "(xi-xj)'(Si+Sj)^-1(xi-xj)", fixed = TRUE)
})
