# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P15: the decompositions must agree with research/lean/P15Decomposition.lean.

test_that("the three decompositions are exact, reference dependence equals the interaction, and recentring moves the attribution", {
  set.seed(16)
  for (k in 1:25) {
    n <- 300
    g <- rep(c("A", "B"), each = n / 2)
    score <- rnorm(n, ifelse(g == "A", 5, 4)); priors <- rpois(n, ifelse(g == "A", 2, 1))
    sentence <- 2 + 0.8 * score + 0.5 * priors + ifelse(g == "A", runif(1, 0, 2), 0) + rnorm(n)
    d <- data.frame(sentence, score, priors, g)
    r <- morie_disparity_decomposition(sentence ~ score + priors, d, group = "g", reference = "B", shift = 3)
    expect_equal(unname(r$identity_checks), rep(0, 4), tolerance = 1e-10)             # twofold_B/A, threefold, reference_dependence
    expect_equal(r$gap, mean(sentence[g == "A"]) - mean(sentence[g == "B"]), tolerance = 1e-12)
    expect_equal(r$twofold_A[["explained"]] - r$twofold_B[["explained"]], r$threefold[["interaction"]], tolerance = 1e-10)
    # attribution_shift: recentre score by 3 and the per-variable unexplained lines move by 3 (bA - bB)
    d2 <- d; d2$score <- d2$score + 3
    r2 <- morie_disparity_decomposition(sentence ~ score + priors, d2, group = "g", reference = "B")
    expect_equal(r2$twofold_B[["unexplained"]], r$twofold_B[["unexplained"]], tolerance = 1e-10)       # total unchanged
    bv <- r$by_variable; bv2 <- r2$by_variable
    expect_equal(bv2$unexplained_A[bv2$variable == "score"] - bv$unexplained_A[bv$variable == "score"],
                 bv$unexplained_shift[bv$variable == "score"], tolerance = 1e-10)
    expect_equal(sum(bv$unexplained_A), r$twofold_B[["unexplained"]], tolerance = 1e-10)
    expect_equal(sum(bv$explained_B), r$twofold_B[["explained"]], tolerance = 1e-10)
  }
  # explained_eq_iff: identical coefficients in the two groups make the interaction zero and the references agree
  n <- 200; g <- rep(c("A", "B"), each = n / 2); x <- rnorm(n, ifelse(g == "A", 1, 0))
  y <- 1 + 2 * x
  r0 <- morie_disparity_decomposition(y ~ x, data.frame(y, x, g), group = "g", reference = "B")
  expect_equal(r0$threefold[["interaction"]], 0, tolerance = 1e-10)
  expect_equal(r0$twofold_A[["explained"]], r0$twofold_B[["explained"]], tolerance = 1e-10)
  expect_error(morie_disparity_decomposition(y ~ x, data.frame(y, x, g = "A"), group = "g", reference = "A"), "two values")
  expect_error(morie_disparity_decomposition(y ~ x, data.frame(y, x, g), group = "g", reference = "C"), "reference")
  expect_error(morie_disparity_decomposition(y ~ x + I(2 * x), data.frame(y, x, g), group = "g", reference = "B"), "collinear")
})
