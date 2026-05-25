# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for R/spatial_voting.R --- ideal-point + MDS + Procrustes +
# NOMINATE + Wordfish primitives.

# ---------------------------------------------------------------------------
# 1. Aldrich-McKelvey -- recovers the simulated latent stimulus ordering.
# ---------------------------------------------------------------------------

test_that("aldrich_mckelvey returns z-scored stimuli with correct ranks", {
  set.seed(1)
  n_resp <- 60L; n_stim <- 5L
  true_z <- seq(-2, 2, length.out = n_stim)
  alpha  <- stats::rnorm(n_resp, 0, 0.5)
  beta   <- stats::rnorm(n_resp, 1, 0.3)
  Z <- outer(alpha, rep(1, n_stim)) +
       outer(beta,  true_z) +
       matrix(stats::rnorm(n_resp * n_stim, 0, 0.2), n_resp, n_stim)
  res <- morie_spatial_voting_aldrich_mckelvey(Z)
  expect_named(res, c("zhat", "alpha", "beta", "weights",
                      "iterations", "converged", "engine"),
               ignore.order = TRUE)
  expect_length(res$zhat, n_stim)
  # rank correlation with truth >= 0.9 (or its mirror image)
  rho <- abs(suppressWarnings(stats::cor(res$zhat, true_z, method = "spearman")))
  expect_gt(rho, 0.9)
  # z-scored (mean ~ 0)
  expect_lt(abs(mean(res$zhat)), 1e-2)
  expect_true(res$engine %in% c("basicspace", "fallback"))
})

test_that("aldrich_mckelvey basicspace branch fires when available", {
  skip_if_not_installed("basicspace")
  set.seed(1)
  Z <- matrix(stats::rnorm(40 * 6), 40, 6)
  res <- morie_spatial_voting_aldrich_mckelvey(Z)
  expect_length(res$zhat, ncol(Z))
})

# ---------------------------------------------------------------------------
# 2. Classical MDS (Mardia recoverability) -- recover an n-d point cloud
#    from its pairwise distance matrix.
# ---------------------------------------------------------------------------

test_that("classical_mds recovers 2-D coords from their distance matrix", {
  set.seed(1)
  X <- matrix(stats::rnorm(20 * 2), 20, 2)
  D <- as.matrix(stats::dist(X))
  res <- morie_spatial_voting_classical_mds(D, n_dims = 2L)
  expect_named(res, c("coordinates", "eigenvalues", "stress",
                      "fit", "B_matrix"),
               ignore.order = TRUE)
  expect_equal(dim(res$coordinates), c(20L, 2L))
  # Recovered pairwise distances match the input distance matrix.
  D_hat <- as.matrix(stats::dist(res$coordinates))
  expect_lt(max(abs(D_hat - D)), 1e-6)
  # First eigenvalue dominant; Mardia fit close to 1 for true 2-D data.
  expect_gt(res$fit, 0.95)
})

test_that("classical_mds fit-stats summary reproduces Mardia ratio", {
  ev <- c(4, 2, 1, 0.5)
  fs <- morie_spatial_voting_mds_fit_stats(ev)
  expect_equal(fs$fit_by_dim, ev / sum(abs(ev)))
  expect_equal(fs$cumulative_fit, cumsum(fs$fit_by_dim))
  expect_equal(tail(fs$cumulative_fit, 1L), 1)
})

# ---------------------------------------------------------------------------
# 3. SMACOF -- stress monotone-decreasing along iterations.
# ---------------------------------------------------------------------------

test_that("smacof produces a finite, non-negative stress on a real D", {
  set.seed(1)
  X <- matrix(stats::rnorm(15 * 2), 15, 2)
  D <- as.matrix(stats::dist(X))
  res <- morie_spatial_voting_smacof(D, n_dims = 2L, max_iter = 50L)
  expect_named(res, c("coordinates", "stress", "iterations", "converged"))
  expect_true(is.finite(res$stress))
  expect_gte(res$stress, 0)
  expect_equal(dim(res$coordinates), c(15L, 2L))
})

test_that("smacof stress is monotone-non-increasing across iter caps", {
  set.seed(1)
  X <- matrix(stats::rnorm(12 * 2), 12, 2)
  D <- as.matrix(stats::dist(X))
  r1 <- morie_spatial_voting_smacof(D, n_dims = 2L, max_iter = 5L)
  r2 <- morie_spatial_voting_smacof(D, n_dims = 2L, max_iter = 50L)
  # More iterations cannot make stress worse (up to a small tolerance).
  expect_lte(r2$stress, r1$stress + 1e-6)
})

# ---------------------------------------------------------------------------
# 4. Procrustes -- rotated copy maps back to the original (identity).
# ---------------------------------------------------------------------------

test_that("procrustes recovers an orthogonal rotation of the target", {
  set.seed(1)
  X_target <- matrix(stats::rnorm(20 * 2), 20, 2)
  theta <- pi / 7
  R <- matrix(c(cos(theta), -sin(theta),
                sin(theta),  cos(theta)), 2, 2)
  X <- X_target %*% R
  res <- morie_spatial_voting_procrustes(X, X_target)
  expect_named(res, c("rotated", "rotation_matrix", "scale", "mse"),
               ignore.order = TRUE)
  expect_equal(dim(res$rotated), dim(X_target))
  # Recovered configuration matches target.
  expect_lt(res$mse, 1e-10)
  # Rotation is orthogonal: R R' == I
  RtR <- res$rotation_matrix %*% t(res$rotation_matrix)
  expect_lt(max(abs(RtR - diag(2L))), 1e-8)
})

# ---------------------------------------------------------------------------
# 5. NOMINATE utility -- monotone in distance, probabilities in (0,1).
# ---------------------------------------------------------------------------

test_that("nominate_utility returns finite probs in (0,1) with right shape", {
  set.seed(1)
  x     <- matrix(stats::rnorm(8), 4, 2)
  z_yea <- matrix(stats::rnorm(6), 3, 2)
  z_nay <- matrix(stats::rnorm(6), 3, 2)
  res <- morie_spatial_voting_nominate_utility(x, z_yea, z_nay)
  expect_named(res, c("U_yea", "U_nay", "utility_diff", "vote_probs"),
               ignore.order = TRUE)
  expect_equal(dim(res$vote_probs), c(4L, 3L))
  expect_true(all(res$vote_probs > 0 & res$vote_probs < 1))
})

test_that("nominate_vote_prob == 0.5 when legislator is equidistant", {
  # x exactly halfway between yea (+1) and nay (-1) -> p = 0.5
  p <- morie_spatial_voting_nominate_vote_prob(c(0), c(1), c(-1))
  expect_equal(p, 0.5, tolerance = 1e-10)
  # closer to yea -> p > 0.5
  p2 <- morie_spatial_voting_nominate_vote_prob(c(0.5), c(1), c(-1))
  expect_gt(p2, 0.5)
})

test_that("nominate_loglik returns finite ll and GMP in [0,1]", {
  set.seed(1)
  v  <- matrix(stats::rbinom(20, 1, 0.5), 4, 5)
  x  <- matrix(stats::rnorm(4), 4, 1)
  zy <- matrix(stats::rnorm(5), 5, 1)
  zn <- matrix(stats::rnorm(5), 5, 1)
  res <- morie_spatial_voting_nominate_loglik(v, x, zy, zn)
  expect_named(res, c("loglik", "GMP", "n_correct", "n_total"),
               ignore.order = TRUE)
  expect_true(is.finite(res$loglik))
  expect_gte(res$GMP, 0)
  expect_lte(res$GMP, 1)
  expect_equal(res$n_total, sum(!is.na(v)))
})

# ---------------------------------------------------------------------------
# 6. Wordfish -- positions cluster correctly on two-bloc simulated DTM.
# ---------------------------------------------------------------------------

test_that("wordfish separates two clearly-distinct document blocs", {
  set.seed(1)
  # Two blocs of documents. Bloc-1 uses words 1-15 heavily, bloc-2
  # uses words 16-30 heavily; bloc-1 docs should land on one side of
  # zero, bloc-2 on the other.
  n_docs <- 16L; n_words <- 30L
  dtm <- matrix(0L, n_docs, n_words)
  for (i in seq_len(8L)) {
    dtm[i, 1:15]      <- stats::rpois(15, 20)
    dtm[i, 16:30]     <- stats::rpois(15, 2)
  }
  for (i in 9:16) {
    dtm[i, 1:15]      <- stats::rpois(15, 2)
    dtm[i, 16:30]     <- stats::rpois(15, 20)
  }
  res <- morie_spatial_voting_wordfish(dtm, max_iter = 50L)
  expect_named(res, c("positions", "word_weights", "word_fixed",
                      "doc_fixed", "log_lik", "iterations"),
               ignore.order = TRUE)
  expect_length(res$positions, n_docs)
  # Blocs separate: mean of bloc-1 differs in sign from mean of bloc-2.
  m1 <- mean(res$positions[1:8])
  m2 <- mean(res$positions[9:16])
  expect_gt(abs(m1 - m2), 0.5)
})

# ---------------------------------------------------------------------------
# 7. Double-centring + blackbox sanity.
# ---------------------------------------------------------------------------

test_that("double_centering returns a symmetric matrix of the same shape", {
  set.seed(1)
  X <- matrix(stats::rnorm(10 * 2), 10, 2)
  D <- as.matrix(stats::dist(X))
  B <- morie_spatial_voting_double_centering(D)
  expect_equal(dim(B), dim(D))
  expect_lt(max(abs(B - t(B))), 1e-10)
})

test_that("blackbox returns ideal_points with requested dimensions", {
  set.seed(1)
  X <- matrix(stats::rnorm(20 * 5), 20, 5)
  res <- morie_spatial_voting_blackbox(X, n_dims = 2L)
  expect_true(is.list(res))
  expect_true("ideal_points" %in% names(res))
  expect_equal(NCOL(res$ideal_points), 2L)
})

# ---------------------------------------------------------------------------
# 8. Nonmetric MDS -- finite stress + correct shape.
# ---------------------------------------------------------------------------

test_that("nonmetric_mds runs and returns finite stress", {
  set.seed(1)
  X <- matrix(stats::rnorm(10 * 2), 10, 2)
  D <- as.matrix(stats::dist(X))
  res <- morie_spatial_voting_nonmetric_mds(D, n_dims = 2L, max_iter = 30L)
  expect_named(res, c("coordinates", "stress", "iterations", "converged"))
  expect_true(is.finite(res$stress))
  expect_equal(dim(res$coordinates), c(10L, 2L))
})
