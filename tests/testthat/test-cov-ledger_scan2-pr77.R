# R/ledger_scan2.R, the lines PR #77 added that no other test reached: the budget rule's stop at a
# non-positive effect, and every refusal of the second scan's functions.

test_that("the budget rule funds only positive effects, however much budget is left", {
  # an LP with budget to spare: every positive-effect unit is funded in full, the rest not at all
  r <- morie_budget_rule(m = c(1, 2, 1), tau = c(3, -1, 0), kappa = 10)
  expect_identical(r$rule, c(1, 0, 0))
  expect_identical(r$threshold, 3)
  expect_identical(r$value, 3)
  # the best 0/1 plan (base enumeration of the 8 plans) is the same, so no randomisation is needed
  plans <- as.matrix(expand.grid(0:1, 0:1, 0:1))
  ok <- plans %*% c(1, 2, 1) <= 10
  expect_identical(r$best_deterministic, max((plans %*% (c(1, 2, 1) * c(3, -1, 0)))[ok]))
  expect_false(r$stochastic_needed)
})

test_that("the second scan's functions refuse what their help pages rule out", {
  expect_error(morie_gamma_bound(2, 10, Gamma = c(1, -0.5)), "`Gamma` must be non-negative", fixed = TRUE)
  expect_error(morie_adjustment_variances(1, 0, -1, 1, 0.5), "noise variances must be non-negative", fixed = TRUE)
  expect_error(morie_adjustment_variances(1, 0, 1, -1, 0.5), "noise variances must be non-negative", fixed = TRUE)
  expect_error(morie_spatial_omission(0.5, c(1, 2, 3), diag(2)), "`Sigma` must be square with `length(x)` rows",
               fixed = TRUE)
  expect_error(morie_spatial_omission(0.5, c(1, 2), matrix(1, 2, 3)), "`Sigma` must be square", fixed = TRUE)
  expect_error(morie_exposure_positivity(0.5, c(1, -1)), "`degrees` must be non-negative", fixed = TRUE)
  expect_error(morie_permutation_valid(c(1, 2), alpha = c(0.05, 1.5)), "`alpha` must lie in \\[0, 1\\]",
               fixed = TRUE)
  expect_error(morie_thinning_identification(0, 0.5), "`lambda` must be positive", fixed = TRUE)
  expect_error(morie_random_walk_exposure(matrix(1, 2, 3)), "`adj` must be a square non-negative matrix", fixed = TRUE)
  expect_error(morie_random_walk_exposure(matrix(c(0, -1, 1, 0), 2)), "`adj` must be a square non-negative matrix",
               fixed = TRUE)
  p <- array(0.5, c(2, 2, 2))
  expect_error(morie_bellman_value(matrix(0, 2, 2), p[, , 1], 0.5), "`p` must be states x actions x states",
               fixed = TRUE)
  expect_error(morie_bellman_value(matrix(0, 2, 3), p, 0.5), "`p` must be states x actions x states", fixed = TRUE)
  expect_error(morie_bellman_value(matrix(0, 2, 2), array(0.5, c(2, 2, 3)), 0.5),
               "`p` must be states x actions x states", fixed = TRUE)
  expect_error(morie_myopic_example(c(0.5, 1)), "`gamma` must lie in [0, 1)", fixed = TRUE)
})
