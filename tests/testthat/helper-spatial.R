# SPDX-License-Identifier: AGPL-3.0-or-later
# Synthetic spatial-voting fixtures (Phase 2E).
#
# Each generator has a known-truth property so the recovery tests in
# test-spatial_voting-extra.R can assert mathematically, not via
# snapshots.

# n_units points in n_dims R, then Euclidean distances. classical_mds /
# smacof should recover the coords up to rotation+reflection (within
# ~1e-3 for n_units=15).
make_synthetic_distance_matrix <- function(n_units = 15L,
                                            n_dims = 2L,
                                            seed = 1L) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n_units * n_dims), n_units, n_dims)
  D <- as.matrix(stats::dist(X))
  attr(D, "true_coords") <- X
  D
}

# Rectangular dissimilarity matrix for unfolding: n_r respondents
# rate n_s stimuli on an n_dims-dim issue space, dissimilarity =
# Euclidean distance.
make_synthetic_unfolding_matrix <- function(n_r = 30L, n_s = 10L,
                                             n_dims = 2L, seed = 1L) {
  set.seed(seed)
  X_r <- matrix(stats::rnorm(n_r * n_dims), n_r, n_dims)
  X_s <- matrix(stats::rnorm(n_s * n_dims), n_s, n_dims)
  D <- matrix(0, n_r, n_s)
  for (i in seq_len(n_r))
    for (j in seq_len(n_s))
      D[i, j] <- sqrt(sum((X_r[i, ] - X_s[j, ])^2))
  attr(D, "true_X_r") <- X_r
  attr(D, "true_X_s") <- X_s
  D
}

# Binary roll-call matrix (n_legis x n_votes) from an n_dims-dim ideal-
# point model. yea/nay positions are random; legislator chooses the
# closer one. With moderate noise the recovery algorithms should
# converge.
make_synthetic_vote_matrix <- function(n_legis = 40L, n_votes = 30L,
                                        n_dims = 1L, noise_p = 0.1,
                                        seed = 1L) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n_legis * n_dims), n_legis, n_dims)
  Z_yea <- matrix(stats::rnorm(n_votes * n_dims), n_votes, n_dims)
  Z_nay <- matrix(stats::rnorm(n_votes * n_dims), n_votes, n_dims)
  votes <- matrix(0L, n_legis, n_votes)
  for (i in seq_len(n_legis)) {
    for (j in seq_len(n_votes)) {
      d_yea <- sqrt(sum((X[i, ] - Z_yea[j, ])^2))
      d_nay <- sqrt(sum((X[i, ] - Z_nay[j, ])^2))
      ideal <- as.integer(d_yea < d_nay)
      votes[i, j] <- if (stats::runif(1) < noise_p) 1L - ideal else ideal
    }
  }
  attr(votes, "true_ideal_points") <- X
  attr(votes, "true_z_yea") <- Z_yea
  attr(votes, "true_z_nay") <- Z_nay
  votes
}

# n_units x n_dims matrix of ideal points — directly used by
# normal_vectors() + cutting_lines() + ideal_point_recovery().
make_synthetic_ideal_points <- function(n_units = 20L, n_dims = 2L,
                                         seed = 1L) {
  set.seed(seed)
  matrix(stats::rnorm(n_units * n_dims), n_units, n_dims)
}

# Indscal needs a STACK of dissimilarity matrices (n_subjects of them).
make_synthetic_indscal_dissims <- function(n_subj = 3L, n_units = 8L,
                                            n_dims = 2L, seed = 1L) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n_units * n_dims), n_units, n_dims)
  stack <- lapply(seq_len(n_subj), function(s) {
    # Subject-specific weight per dimension (Indscal model)
    w <- stats::runif(n_dims, 0.5, 1.5)
    Xw <- sweep(X, 2, sqrt(w), "*")
    as.matrix(stats::dist(Xw))
  })
  attr(stack, "true_group_coords") <- X
  stack
}

# Anchoring-vignettes: ordinal self-rating Y + matrix V of vignette
# ratings on the same ordinal scale.
make_synthetic_anchoring <- function(n_resp = 40L, n_vig = 5L,
                                      n_cat = 5L, seed = 1L) {
  set.seed(seed)
  Y <- sample.int(n_cat, n_resp, replace = TRUE)
  V <- matrix(sample.int(n_cat, n_resp * n_vig, replace = TRUE),
              n_resp, n_vig)
  list(Y = Y, V = V)
}
