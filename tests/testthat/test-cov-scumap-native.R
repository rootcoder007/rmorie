# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/scumap_native.R (UMAP, McInnes, Healy & Melville 2018).
# sigma is checked through the Algorithm 3 cardinality equation, the
# graph through exp(-(d - rho)/sigma) and the t-conorm, the spectral
# layout through the eigen-equation of the normalised Laplacian, and
# (a, b) against umap-learn's published fit for min_dist = 0.1.

.um_X <- rbind(cbind(seq(0, 1, length.out = 6), c(0, 0.2, 0.1, 0.3, 0.15, 0.05)),
               cbind(seq(0, 1, length.out = 6) + 8, c(5, 5.2, 5.1, 4.9, 5.05, 5.3)))

test_that("smooth_knn_dist solves sum exp(-(d - rho)/sigma) = log2(k)", {
  d <- c(0.5, 0.9, 1.3, 2, 2.2)
  r <- morie_scumap_smooth_knn_dist(d, 5)
  expect_equal(r$rho, 0.5)
  expect_lt(abs(sum(exp(-pmax(d - r$rho, 0) / r$sigma)) - log2(5)), 1e-5)
  r2 <- morie_scumap_smooth_knn_dist(c(0, 0, 1, 3), 4)
  expect_equal(r2$rho, 1)
  r3 <- morie_scumap_smooth_knn_dist(d, 5, rho = 0.2)
  expect_lt(abs(sum(exp(-(d - 0.2) / r3$sigma)) - log2(5)), 1e-5)
  # equal distances cannot reach the target: sigma floors at 1e-3 * mean(d)
  expect_equal(morie_scumap_smooth_knn_dist(rep(2, 3), 4)$sigma, 2e-3)
  expect_error(morie_scumap_smooth_knn_dist(numeric(0), 3), "no distances")
  expect_error(morie_scumap_smooth_knn_dist(d, 1), "at least 2")
})

test_that("fuzzy_simplicial_set gives memberships and the t-conorm", {
  g <- morie_scumap_fuzzy_simplicial_set(.um_X, n_neighbors = 3)
  D <- as.matrix(dist(.um_X))
  for (i in c(1, 7, 12)) {
    o <- order(replace(D[i, ], i, Inf))[1:3]
    expect_setequal(g$neighbours[[i]], o)
    sk <- morie_scumap_smooth_knn_dist(D[i, o], 3)
    expect_equal(g$A[i, o], unname(exp(-pmax(D[i, o] - sk$rho, 0) / sk$sigma)), tolerance = 1e-12)
    expect_equal(sum(g$A[i, ] > 0), 3L)
  }
  expect_equal(g$B, g$A + t(g$A) - g$A * t(g$A), tolerance = 1e-12)
  expect_equal(g$B, t(g$B))
  expect_identical(morie_scumap_fuzzy_simplicial_set(.um_X, 3, symmetrize = FALSE)$B, g$A)
  expect_error(morie_scumap_fuzzy_simplicial_set(.um_X, 1), "at least 2")
  expect_error(morie_scumap_fuzzy_simplicial_set(.um_X, 12), "smaller than")
  expect_error(morie_scumap_fuzzy_simplicial_set(matrix(NA, 3, 2), 2), "non-finite")
})

test_that("spectral_layout uses the second and third Laplacian eigenvectors", {
  B <- morie_scumap_fuzzy_simplicial_set(.um_X, 4)$B
  dg <- rowSums(B)
  L <- diag(12) - diag(1 / sqrt(dg)) %*% B %*% diag(1 / sqrt(dg))
  ev <- eigen(L, symmetric = TRUE)
  Y <- morie_scumap_spectral_layout(B, 2)
  for (cc in 1:2) {
    lam <- rev(ev$values)[cc + 1]
    expect_equal(as.numeric(L %*% Y[, cc]), lam * Y[, cc], tolerance = 1e-9)
  }
  expect_equal(max(apply(Y, 2, function(v) diff(range(v)))), 10, tolerance = 1e-12)
  Lp <- diag(sqrt(dg)) %*% (diag(dg) - B) %*% diag(sqrt(dg))
  Yp <- morie_scumap_spectral_layout(B, 1, "as_printed")
  lamp <- rev(eigen(Lp, symmetric = TRUE)$values)[2]
  expect_equal(as.numeric(Lp %*% Yp[, 1]), lamp * Yp[, 1], tolerance = 1e-9)
  expect_error(morie_scumap_spectral_layout(B, 2, "random"), "laplacian must be")
})

test_that("fit_ab reproduces umap-learn's a, b and is a local least-squares minimum", {
  ab <- morie_scumap_fit_ab(0.1, 1)
  # umap-learn find_ab_params(1.0, 0.1): a = 1.5769, b = 0.8951
  expect_equal(ab$a, 1.576943, tolerance = 1e-3)
  expect_equal(ab$b, 0.895061, tolerance = 1e-3)
  xs <- 3 * (0:299) / 299
  ys <- ifelse(xs < 0.1, 1, exp(-(xs - 0.1)))
  loss <- function(a, b) sum((1 / (1 + a * xs[-1]^(2 * b)) - ys[-1])^2)
  l0 <- loss(ab$a, ab$b)
  for (da in c(-1e-3, 1e-3)) for (db in c(-1e-3, 0, 1e-3)) expect_gte(loss(ab$a + da, ab$b + db), l0)
  expect_error(morie_scumap_fit_ab(-1), "non-negative")
  expect_error(morie_scumap_fit_ab(0.1, 0), "spread must be positive")
})

test_that("umap_singlecell keeps the two clusters apart", {
  for (fn in list(morie_scumap_umap_singlecell, morie_scumap, morie_scumap_umapsinglecell)) {
    r <- fn(.um_X, n_neighbors = 4, n_epochs = 30, a = 1.58, b = 0.9)
    Y <- r$embedding
    cen1 <- colMeans(Y[1:6, ])
    cen2 <- colMeans(Y[7:12, ])
    within <- max(sqrt(rowSums(sweep(Y[1:6, ], 2, cen1)^2)), sqrt(rowSums(sweep(Y[7:12, ], 2, cen2)^2)))
    expect_gt(sqrt(sum((cen1 - cen2)^2)), within)
    expect_equal(r$graph, morie_scumap_fuzzy_simplicial_set(.um_X, 4)$B)
    expect_equal(dim(Y), c(12L, 2L))
  }
  rr <- morie_scumap_umap_singlecell(.um_X, n_neighbors = 4, n_epochs = 5, init = "random", n_components = 3)
  expect_equal(dim(rr$embedding), c(12L, 3L))
  expect_true(all(is.finite(rr$embedding)))
  expect_error(morie_scumap_umap_singlecell(.um_X, init = "pca"), "init must be")
  expect_error(morie_scumap_umap_singlecell(.um_X, n_components = 0), "n_components")
  expect_error(morie_scumap_umap_singlecell(.um_X, learning_rate = 0), "learning_rate")
  expect_error(morie_scumap_umap_singlecell(.um_X, n_epochs = 0), "n_epochs")
})

test_that("morie_scumap_cheatsheet states the membership", {
  expect_match(morie_scumap_cheatsheet(), "exp(-max(0, d - rho)/sigma)", fixed = TRUE)
})
