# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Aldrich-McKelvey and blackbox scaling are native; basicspace is only a
# cross-validation reference here.

sv_scale_data <- function(seed, n = 120, p = 10, n_miss = 0) {
  set.seed(seed)
  X <- round(matrix(stats::rnorm(n * 2), n, 2) %*%
               t(matrix(stats::rnorm(p * 2), p, 2)) +
               matrix(stats::rnorm(n * p, 0, 0.5), n, p) + 4)
  if (n_miss > 0) X[sample(length(X), n_miss)] <- NA
  dimnames(X) <- list(paste0("r", seq_len(n)), paste0("q", seq_len(p)))
  X
}

test_that("aldrich_mckelvey is native-only and matches basicspace::aldmck", {
  expect_false(any(grepl("basicspace::|requireNamespace",
                         deparse(morie_spatial_voting_aldrich_mckelvey))))
  skip_if_not_installed("basicspace")
  for (s in 1:4) {
    set.seed(s)
    truth <- c(-1.2, -0.4, 0.1, 0.9, 1.5, -0.8)
    Z <- t(sapply(1:80, function(i) 4 + stats::rnorm(1, 0, 0.3) +
                    stats::runif(1, 0.6, 1.4) * truth + stats::rnorm(6, 0, 0.5)))
    # basicspace truncates placements to integers, so compare on a 1-7 scale
    Z <- pmin(pmax(round(Z), 1), 7)
    storage.mode(Z) <- "double"
    if (s > 2) Z[sample(length(Z), 10)] <- NA
    fit <- morie_spatial_voting_aldrich_mckelvey(Z)
    expect_identical(fit$engine, "native")
    ref <- as.numeric(basicspace::aldmck(Z, respondent = 0, polarity = 1)$stimuli)
    # same closed form (smallest eigenvector): observed max diff ~7e-16
    expect_equal(fit$zhat, (ref - mean(ref)) / stats::sd(ref), tolerance = 1e-10)
  }
})

test_that("blackbox reproduces basicspace::blackbox (complete and missing)", {
  expect_false(any(grepl("basicspace::|requireNamespace",
                         deparse(morie_spatial_voting_blackbox))))
  skip_if_not_installed("basicspace")
  for (s in 1:4) {
    for (nd in 1:3) {
      X <- sv_scale_data(s, n_miss = if (s > 1) 30 * s else 0)
      ref <- basicspace::blackbox(X, dims = nd, minscale = 8, verbose = FALSE)
      f <- morie_spatial_voting_blackbox(X, n_dims = nd, minscale = 8)
      expect_identical(f$engine, "native")
      # singular values are reported unrounded by basicspace: same port of
      # the same arithmetic, observed max diff ~1e-13
      expect_equal(f$singular_values, ref$fits$singular, tolerance = 1e-10)
      # coordinates, weights and intercepts are rounded to 3 decimals by
      # basicspace, so the bound is half a unit in the third decimal
      ip <- as.matrix(ref$individuals[[nd]][, paste0("c", seq_len(nd))])
      sw <- as.matrix(ref$stimuli[[nd]][, paste0("w", seq_len(nd))])
      sg <- sign(colSums(sw * f$stimuli_weights))  # LAPACK sign freedom
      expect_lt(max(abs(sw - sweep(f$stimuli_weights, 2L, sg, "*"))), 5.01e-4)
      expect_lt(max(abs(ip - sweep(f$ideal_points, 2L, sg, "*")), na.rm = TRUE),
                5.01e-4)
      expect_lt(max(abs(ref$stimuli[[nd]]$c - f$col_means)), 5.01e-4)
      expect_equal(f$explained_variance, sum(ref$fits$percent) / 100,
                   tolerance = 1e-8)
    }
  }
})

test_that("blackbox leaves respondents below minscale unscaled", {
  X <- sv_scale_data(9, n_miss = 0)
  X[1:3, 1:4] <- NA
  f <- morie_spatial_voting_blackbox(X, n_dims = 2L, minscale = 8L)
  expect_true(all(is.na(f$ideal_points[1:3, ])))
  expect_true(all(is.finite(f$ideal_points[-(1:3), ])))
})
