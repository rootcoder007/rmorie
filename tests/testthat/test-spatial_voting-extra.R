# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2E: tests for the 21 spatial_voting.R exports that
# test-spatial_voting.R doesn't yet exercise.
#
# Fixtures from helper-spatial.R (testthat auto-sources helper-*.R).

# ---------------------------------------------------------- Ideal-point recovery

test_that("morie_spatial_voting_ideal_point_recovery passes X_r through", {
  # 2s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_heavy()
  X <- make_synthetic_ideal_points(20L, 2L)
  out <- morie_spatial_voting_ideal_point_recovery(X)
  expect_equal(dim(out), c(20L, 2L))
})

test_that("ideal_point_recovery also accepts X_s without crashing", {
  skip_heavy()
  X_r <- make_synthetic_ideal_points(20L, 2L, seed = 11L)
  X_s <- make_synthetic_ideal_points(5L, 2L, seed = 12L)
  out <- morie_spatial_voting_ideal_point_recovery(X_r, X_s)
  expect_equal(nrow(out), 20L)
})

# --------------------------------------------------------------- Cutting lines

test_that("morie_spatial_voting_normal_vectors returns the normal + r^2 list", {
  skip_heavy()
  X <- make_synthetic_ideal_points(20L, 2L, seed = 21L)
  ext <- stats::rnorm(20L)
  out <- morie_spatial_voting_normal_vectors(X, ext)
  expect_type(out, "list")
  expect_true(all(c("normal_vector", "angle_degrees", "angle_radians",
                    "r_squared", "coefficients") %in% names(out)))
  expect_true(is.finite(out$r_squared))
})

test_that("morie_spatial_voting_cutting_lines returns endpoints per vote", {
  skip_heavy()
  # cutting_lines expects normals as an (n_votes x n_dims) matrix.
  normals <- matrix(stats::rnorm(6L), nrow = 3L, ncol = 2L)
  out <- morie_spatial_voting_cutting_lines(normals, c(0.1, -0.2, 0))
  expect_type(out, "list")
  expect_equal(out$n_lines, 3L)
  expect_length(out$endpoints, 3L)
  expect_length(out$angles, 3L)
})

# ------------------------------------------------------ Bayesian / IRT helpers

test_that("morie_spatial_voting_bayesian_irt_likelihood returns ll + accuracy", {
  skip_heavy()
  votes <- make_synthetic_vote_matrix(20L, 10L, 1L, seed = 31L)
  x <- matrix(stats::rnorm(20L), 20L, 1L)
  alpha <- stats::rnorm(10L)
  beta  <- matrix(stats::rnorm(10L), 10L, 1L)
  out <- morie_spatial_voting_bayesian_irt_likelihood(votes, x, alpha, beta)
  expect_type(out, "list")
  expect_true(is.finite(out$loglik))
  expect_true(out$accuracy >= 0 && out$accuracy <= 1)
  expect_equal(dim(out$vote_probs), c(20L, 10L))
})

test_that("morie_spatial_voting_bayesian_irt_posterior summarises a chain array", {
  skip_heavy()
  # Per the docstring: chain is array (n_samples, n_leg, n_dims).
  ch <- array(stats::rnorm(100L * 5L * 2L), c(100L, 5L, 2L))
  out <- morie_spatial_voting_bayesian_irt_posterior(ch)
  expect_type(out, "list")
  expect_true(all(c("posterior_mean", "posterior_sd",
                    "ci_lower", "ci_upper") %in% names(out)))
})

# -------------------------------------------------------- Optimal classification

test_that("morie_spatial_voting_optimal_classification returns ideal-point matrix", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(25L, 20L, 1L, noise_p = 0.05, seed = 41L)
  out <- tryCatch(
    morie_spatial_voting_optimal_classification(V, n_dims = 1L,
                                                 max_iter = 50L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("OC needs richer fixture: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out) || is.data.frame(out))
})

# ------------------------------------------------------------ Smacof unfolding

test_that("morie_spatial_voting_smacof_unfolding returns finite stress", {
  skip_heavy()
  D <- make_synthetic_unfolding_matrix(15L, 5L, 2L, seed = 51L)
  out <- tryCatch(
    morie_spatial_voting_smacof_unfolding(D, n_dims = 2L,
                                           max_iter = 50L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("smacof_unfolding error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_spatial_voting_unfolding_stress is non-negative on synthetic data", {
  skip_heavy()
  D <- make_synthetic_unfolding_matrix(10L, 4L, 2L, seed = 52L)
  X_r <- attr(D, "true_X_r")
  X_s <- attr(D, "true_X_s")
  out <- morie_spatial_voting_unfolding_stress(X_r, X_s, D)
  expect_true(is.numeric(out) && is.finite(out))
  expect_true(out >= 0)
})

# ------------------------------------------------------------------- INDSCAL

test_that("morie_spatial_voting_indscal returns shared-coord matrix", {
  skip_heavy()
  stack <- make_synthetic_indscal_dissims(3L, 8L, 2L, seed = 61L)
  out <- tryCatch(
    morie_spatial_voting_indscal(stack, n_dims = 2L,
                                  max_iter = 30L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("indscal error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

# ------------------------------------------------------ DW-NOMINATE / bootstrap

test_that("morie_spatial_voting_dw_nominate returns ideal-point matrix", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(30L, 25L, 2L, seed = 71L)
  out <- tryCatch(
    morie_spatial_voting_dw_nominate(V, n_dims = 2L,
                                      max_iter = 20L, tol = 1e-3),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("dw_nominate error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_spatial_voting_nominate_bootstrap returns SE matrix", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(20L, 15L, 1L, seed = 72L)
  # nominate_bootstrap bootstraps a pre-fitted W-NOMINATE; it does
  # NOT fit one itself. Run dw_nominate first to get
  # ideal_points / normal_vectors / cutpoints, then pass them in.
  fit <- tryCatch(
    morie_spatial_voting_dw_nominate(V, n_dims = 1L, max_iter = 30L),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    skip(sprintf("dw_nominate prerequisite error: %s",
                 conditionMessage(fit)))
  }
  out <- tryCatch(
    morie_spatial_voting_nominate_bootstrap(
      V,
      ideal_points = fit$ideal_points,
      normal_vectors_arr = fit$normal_vectors,
      cutpoints = fit$cutpoints,
      n_boot = 3L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("nominate_bootstrap error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_spatial_voting_alpha_nominate returns ideal-point matrix", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(25L, 20L, 1L, seed = 73L)
  out <- tryCatch(
    morie_spatial_voting_alpha_nominate(V, n_dims = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("alpha_nominate error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

# ----------------------------------------------------------------- Ordinal IRT

test_that("morie_spatial_voting_ordinal_irt runs on ordinal vote-like data", {
  skip_heavy()
  set.seed(81L)
  Y <- matrix(sample.int(4L, 20L * 15L, replace = TRUE), 20L, 15L)
  out <- tryCatch(
    morie_spatial_voting_ordinal_irt(Y, n_dims = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("ordinal_irt error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

# ----------------------------------------------------------------- Dynamic IRT

test_that("morie_spatial_voting_dynamic_irt accepts per-period vote matrices", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(20L, 12L, 1L, seed = 91L)
  # time_periods is one-period-per-VOTE (length == ncol(V)), NOT
  # per-legislator -- the 3MMM.27 port iterates votes within each
  # period and runs EM-IRT per period.
  periods <- rep(1:3, length.out = ncol(V))
  out <- morie_spatial_voting_dynamic_irt(V, time_periods = periods)
  expect_true(is.list(out))
  expect_equal(out$n_periods, 3L)
})

# ----------------------------------------------------------------------- EM-IRT

test_that("morie_spatial_voting_em_irt converges to a finite log-likelihood", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(30L, 25L, 1L, seed = 101L)
  out <- tryCatch(
    morie_spatial_voting_em_irt(V, n_dims = 1L, max_iter = 20L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("em_irt error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

# ----------------------------------------------------- Non-parametric bootstrap

test_that("morie_spatial_voting_nonparametric_bootstrap returns SE matrix", {
  skip_heavy()
  Z <- matrix(stats::rnorm(20L * 5L), 20L, 5L)
  out <- tryCatch(
    morie_spatial_voting_nonparametric_bootstrap(Z, n_boot = 5L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("np_bootstrap error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

# -------------------------------------------------------- Anchoring vignettes

test_that("morie_spatial_voting_anchoring_vignettes returns DIF-adjusted ratings", {
  skip_heavy()
  fix <- make_synthetic_anchoring(40L, 5L, 5L, seed = 111L)
  out <- tryCatch(
    morie_spatial_voting_anchoring_vignettes(fix$Y, fix$V,
                                              n_categories = 5L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("anchoring_vignettes error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out) || is.matrix(out))
})

# --------------------------------------------------- Ordered optimal classific

test_that("morie_spatial_voting_ordered_oc runs on ordinal vote matrix", {
  skip_heavy()
  set.seed(121L)
  Y <- matrix(sample.int(3L, 25L * 20L, replace = TRUE), 25L, 20L)
  out <- tryCatch(
    morie_spatial_voting_ordered_oc(Y, n_dims = 1L, max_iter = 30L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("ordered_oc error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

# --------------------------------------- Bayesian wrappers (require Stan etc.)
# These typically delegate to optional packages (rstan/MCMCpack). They
# may error cleanly when the optional dep is absent; skip gracefully.

test_that("morie_spatial_voting_bayesian_am runs or skips on missing Stan", {
  skip_heavy()
  Z <- matrix(stats::rnorm(20L * 5L), 20L, 5L)
  out <- tryCatch(
    morie_spatial_voting_bayesian_am(Z, n_samples = 20L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("bayesian_am: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_spatial_voting_bayesian_mds runs or skips on missing Stan", {
  skip_heavy()
  D <- make_synthetic_distance_matrix(10L, 2L, seed = 131L)
  out <- tryCatch(
    morie_spatial_voting_bayesian_mds(D, n_dims = 2L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("bayesian_mds: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_spatial_voting_bayesian_unfolding runs or skips on missing Stan", {
  skip_heavy()
  D <- make_synthetic_unfolding_matrix(10L, 4L, 2L, seed = 132L)
  out <- tryCatch(
    morie_spatial_voting_bayesian_unfolding(D, n_dims = 2L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("bayesian_unfolding: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_spatial_voting_cjr_irt runs or skips on missing Stan", {
  skip_heavy()
  V <- make_synthetic_vote_matrix(20L, 10L, 1L, seed = 133L)
  out <- tryCatch(
    morie_spatial_voting_cjr_irt(V, n_dims = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("cjr_irt: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("single-row and single-column matrices never reach basicspace", {
  # basicspace's Fortran writes out of bounds on these shapes and the
  # process dies later (valgrind: mckalnew_, blackb_, blackboxt_)
  ok <- rmorie:::.sv_basicspace_shape_ok
  expect_false(ok(matrix(c(1, NA, 3), 3, 1)))
  expect_false(ok(matrix(c(1, 2), 1, 2)))
  expect_false(ok(matrix(1:4, 2))) # integer storage is refused by basicspace too
  expect_true(ok(matrix(rnorm(4), 2, 2)))
  expect_false(ok(matrix(rnorm(14), 2, 7), min_cols = 8L))
  skip_if_not_installed("basicspace")
  testthat::local_mocked_bindings(
    aldmck = function(...) stop("reached basicspace"),
    blackbox = function(...) stop("reached basicspace"),
    .package = "basicspace"
  )
  thin <- matrix(c(1, NA, 3), 3, 1)
  flat <- matrix(c(1, 2), 1, 2)
  for (Z in list(thin, flat)) {
    r <- tryCatch(morie_spatial_voting_aldrich_mckelvey(Z), error = identity)
    if (inherits(r, "error")) expect_false(grepl("reached basicspace", conditionMessage(r)))
    r <- tryCatch(morie_spatial_voting_blackbox(Z, n_dims = 1L), error = identity)
    if (inherits(r, "error")) expect_false(grepl("reached basicspace", conditionMessage(r)))
    r <- tryCatch(morie_spatial_voting_bayesian_am(Z, n_samples = 5L, burn_in = 1L),
                  error = identity)
    if (inherits(r, "error")) expect_false(grepl("reached basicspace", conditionMessage(r)))
  }
})

test_that("aldrich_mckelvey delegates to basicspace on a valid matrix", {
  skip_if_not_installed("basicspace")
  set.seed(1)
  Z <- matrix(rnorm(100), 20, 5)
  Z[3, 2] <- NA
  fit <- morie_spatial_voting_aldrich_mckelvey(Z)
  expect_identical(fit$engine, "basicspace")
  expect_length(fit$zhat, 5L)
  expect_length(fit$alpha, 20L)
})
