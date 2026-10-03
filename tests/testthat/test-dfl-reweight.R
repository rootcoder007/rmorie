# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P15 (continued): DFL reweighting must agree with research/lean/P15Reweight.lean.

test_that("reweighting_matches and reweighted_mass: the reweighted group-0 covariate distribution is group 1's, exactly", {
  set.seed(15)
  for (rep in 1:40) {
    n <- sample(20:80, 1)
    g <- sample(c(TRUE, FALSE), n, replace = TRUE)
    x <- sample(letters[1:4], n, replace = TRUE)
    x[!g] <- sample(letters[1:4], sum(!g), replace = TRUE)   # group 0 covers every value: common support
    if (!all(unique(x[g]) %in% x[!g]) || sum(g) == 0 || sum(!g) == 0) next
    y <- rnorm(n, 10 + 2 * (x == "b") + 5 * g)
    w <- runif(n, 0.5, 2)
    r <- morie_dfl_reweight(g, x, y, w)
    psi <- setNames(r$psi$psi, r$psi$x)
    expect_equal(r$reweighted_mass, sum(w[g]), tolerance = 1e-10)                       # reweighted_mass
    for (draw in 1:3) {
      h <- setNames(rnorm(4), letters[1:4])
      expect_equal(sum(psi[x[!g]] * w[!g] * h[x[!g]]), sum(w[g] * h[x[g]]), tolerance = 1e-9)   # reweighting_matches
    }
    expect_lt(r$max_composition_gap, 1e-12)
    expect_equal(r$mean_1 - r$mean_0, r$structure + r$composition, tolerance = 1e-12)   # decomposition
    expect_equal(r$mean_1, weighted.mean(y[g], w[g]), tolerance = 1e-12)
    expect_equal(r$mean_0, weighted.mean(y[!g], w[!g]), tolerance = 1e-12)
    expect_equal(r$counterfactual, sum(psi[x[!g]] * w[!g] * y[!g]) / sum(psi[x[!g]] * w[!g]), tolerance = 1e-12)
  }
})

test_that("counterfactual_outcome: group-0 outcomes a function of x give group 1's composition at group 0's structure", {
  g <- c(rep(TRUE, 6), rep(FALSE, 6))
  x <- c("a", "a", "a", "a", "b", "b",  "a", "a", "b", "b", "b", "b")
  mu0 <- c(a = 8, b = 15)
  y <- c(10, 12, 11, 13, 20, 22, mu0[x[7:12]])
  r <- morie_dfl_reweight(g, x, y)
  share1 <- c(a = 4, b = 2) / 6
  expect_equal(r$counterfactual, sum(share1 * mu0), tolerance = 1e-12)
  expect_equal(r$psi$psi, c(2, 0.5))
  expect_equal(r$psi$mass_1, c(4, 2)); expect_equal(r$psi$mass_0, c(2, 4))
})

test_that("common support and input checks", {
  expect_error(morie_dfl_reweight(c(TRUE, TRUE, FALSE), c("a", "b", "a"), 1:3), "common support fails at x = b")
  expect_error(morie_dfl_reweight(c(TRUE, FALSE), c("a"), 1:2), "equal length")
  expect_error(morie_dfl_reweight(c(TRUE, NA), c("a", "a"), 1:2), "missing")
  expect_error(morie_dfl_reweight(c("x", "y"), c("a", "a"), 1:2), "logical")
  expect_error(morie_dfl_reweight(c(TRUE, FALSE), c("a", "a"), 1:2, weights = c(1, -1)), "weights")
  expect_error(morie_dfl_reweight(c(TRUE, TRUE), c("a", "a"), 1:2), "positive total")
})
