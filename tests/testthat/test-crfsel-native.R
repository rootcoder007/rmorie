# Depth-weighted split-frequency variable importance for a CATE forest
# (Athey, Tibshirani & Wager 2019; Wager & Athey 2018).
#
# Anchors outside the module: split counts on a tree built by hand, so the
# tallies are known exactly; the normalisation the importance measure must
# satisfy; the degenerate cases where every split is on one variable or
# there are no splits at all; and a data-generating process whose treatment
# effect is driven by a single covariate, which must therefore rank first.

# a tree split on feature 1 at the root and feature 2 on the left
leaf <- function() list(leaf = TRUE)
TREE <- list(
  leaf = FALSE, feature = 1L,
  left = list(leaf = FALSE, feature = 2L, left = leaf(), right = leaf()),
  right = leaf()
)

test_that("split counts are tallied by depth and feature", {
  cc <- .depth_counts(TREE, max_depth = 3L, d = 3L)
  expect_length(cc, 3L)
  # one split on feature 1 at the root
  expect_equal(cc[[1]], c(1, 0, 0))
  # one split on feature 2 at the second level
  expect_equal(cc[[2]], c(0, 1, 0))
  # nothing deeper
  expect_equal(cc[[3]], c(0, 0, 0))
  # a depth budget shorter than the tree truncates rather than erroring
  expect_length(.depth_counts(TREE, max_depth = 1L, d = 3L), 1L)
  expect_equal(.depth_counts(TREE, max_depth = 1L, d = 3L)[[1]], c(1, 0, 0))
  # a bare leaf has no splits at all
  expect_equal(.depth_counts(leaf(), max_depth = 2L, d = 3L)[[1]],
               c(0, 0, 0))
  # the width follows the covariate count
  expect_length(.depth_counts(TREE, max_depth = 2L, d = 5L)[[1]], 5L)
})

test_that("importance is a distribution over the covariates", {
  imp <- .split_frequency_importance(list(TREE), d = 3L, max_depth = 3L,
                                     decay = 2)
  expect_length(imp, 3L)
  expect_equal(sum(imp), 1)
  expect_true(all(imp >= 0))
  # feature 3 is never split on, so it earns nothing
  expect_equal(imp[3], 0)
  # the root split is weighted more heavily than the deeper one at decay 2
  expect_true(imp[1] > imp[2])

  # a forest that only ever splits on one variable gives it everything
  only1 <- list(list(leaf = FALSE, feature = 1L, left = leaf(),
                     right = leaf()))
  expect_equal(.split_frequency_importance(only1, 3L, 3L, 2), c(1, 0, 0))

  # with no splits anywhere there is nothing to distinguish, so the measure
  # falls back on the uniform distribution rather than dividing by zero
  expect_equal(.split_frequency_importance(list(leaf()), 4L, 3L, 2),
               rep(0.25, 4))

  # decay controls how much depth matters: at zero every level counts the
  # same, so the two features split once each come out level
  flat <- .split_frequency_importance(list(TREE), 3L, 3L, decay = 0)
  expect_equal(flat[1], flat[2])
  # and a large decay concentrates on the root
  steep <- .split_frequency_importance(list(TREE), 3L, 3L, decay = 6)
  expect_true(steep[1] > 0.9)

  expect_error(.split_frequency_importance(list(TREE), 3L, 0L, 2),
               "max_depth must be at least 1")
  expect_error(.split_frequency_importance(list(TREE), 3L, 3L, -1),
               "decay must be non-negative")
})

test_that("permutation importance rises for a variable the forest uses", {
  set.seed(5)
  n <- 60
  X <- cbind(runif(n), runif(n))
  # the outcome depends on the first covariate only
  y <- 3 * X[, 1] + rnorm(n, 0, 0.1)
  fo <- grow_forest(X, y, n_trees = 20L, min_leaf = 5L, seed = 1L)
  pi <- .permutation_importance(fo$trees, X, y, features = NULL, seed = 2L,
                                n_repeats = 2L)
  expect_length(pi$importance, 2L)
  expect_true(pi$baseline_error >= 0)
  # scrambling the informative covariate hurts more than scrambling noise
  expect_true(pi$importance[1] > pi$importance[2])
  expect_true(pi$importance[1] > 0)
  # asking for one feature leaves the other at zero
  one <- .permutation_importance(fo$trees, X, y, features = 2L, seed = 2L,
                                 n_repeats = 1L)
  expect_equal(one$importance[1], 0)
  # the baseline is the unpermuted error, so it does not depend on the seed
  expect_equal(one$baseline_error, pi$baseline_error)
})

test_that("the covariate driving the treatment effect ranks first", {
  set.seed(9)
  n <- 80
  X <- cbind(runif(n), runif(n), runif(n))
  W <- rbinom(n, 1, 0.5)
  # the treatment effect is driven by the first covariate alone
  y <- 0.5 * X[, 2] + W * (2 * X[, 1]) + rnorm(n, 0, 0.1)
  r <- morie_crfsel(y, W, X, n_trees = 20L, min_leaf = 5L, max_depth = 3L,
                    seed = 1L)
  expect_equal(r$d, 3L)
  expect_equal(r$n, 80L)
  expect_length(r$importance, 3L)
  # the measure is a distribution
  expect_equal(sum(r$importance), 1)
  expect_true(all(r$importance >= 0))
  expect_equal(r$estimate, r$importance)
  # the ranking is sorted by importance and the top name agrees with it
  ranks <- vapply(r$ranking, function(z) z$rank, numeric(1))
  imps <- vapply(r$ranking, function(z) z$importance, numeric(1))
  expect_equal(ranks, seq_len(3))
  expect_true(all(diff(imps) <= 1e-12))
  expect_equal(r$top, r$ranking[[1]]$variable)
  expect_equal(r$top, paste0("X", which.max(r$importance)))
  # the default names are X1..Xd, and the keyed list agrees with the vector
  expect_named(r$importance_by_name, c("X1", "X2", "X3"))
  expect_equal(unlist(r$importance_by_name), r$importance,
               ignore_attr = TRUE)
  # the covariate the effect was built on comes first
  expect_equal(r$top, "X1")
  expect_match(r$method, "Athey, Tibshirani & Wager")
  # no permutation pass unless asked for
  expect_null(r$permutation)
})

test_that("names and the permutation pass are honoured", {
  set.seed(11)
  n <- 60
  X <- cbind(runif(n), runif(n))
  W <- rbinom(n, 1, 0.5)
  y <- W * (2 * X[, 1]) + rnorm(n, 0, 0.1)
  r <- morie_crfsel(y, W, X, n_trees = 12L, min_leaf = 5L, max_depth = 2L,
                    seed = 1L, names = c("dose", "noise"),
                    permutation = TRUE)
  expect_named(r$importance_by_name, c("dose", "noise"))
  expect_true(r$top %in% c("dose", "noise"))
  # the permutation pass reports one number per covariate
  expect_false(is.null(r$permutation))
  expect_length(r$permutation, 2L)
  expect_true(r$baseline_error >= 0)
  # the ranking carries the permutation figure alongside the frequency one
  expect_false(is.null(r$ranking[[1]]$permutation))
})

test_that("crfsel validates its arguments", {
  set.seed(13)
  n <- 60
  X <- cbind(runif(n), runif(n))
  W <- rbinom(n, 1, 0.5)
  y <- rnorm(n)
  expect_error(morie_crfsel(y, W[1:5], X), "outcomes but .* treatments")
  expect_error(morie_crfsel(y, W, X[1:5, , drop = FALSE]),
               "covariate rows for")
  expect_error(morie_crfsel(y, W, matrix(0, n, 0)), "no covariates")
  expect_error(morie_crfsel(y, W, X, names = "only one"),
               "names for .* covariates")
  # a forest needs enough data to be worth growing, and it says so
  expect_error(morie_crfsel(y[1:40], W[1:40], X[1:40, , drop = FALSE]),
               "at least 60 observations")
})
