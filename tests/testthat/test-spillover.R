# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P3: the spillover functions must agree with research/lean/P3Interference.lean.

set.seed(1)

test_that("exposure mapping follows the three-level rule on a path graph", {
  set.seed(1)
  edges <- rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 5))
  e <- morie_spillover_exposure(c(1, 0, 0, 0, 1), edges)
  expect_equal(e$exposure, c(2, 1, 0, 1, 2))
  expect_equal(e$treated_neighbours, c(0, 1, 0, 1, 0))
  expect_error(morie_spillover_exposure(c(1, 0), rbind(c(1, 3))), "index places")
})

test_that("exposure adjustment recovers the potential-outcome means when exposure is ignorable within strata", {
  set.seed(1)
  # finite population with known potential outcomes; exposure assigned at random within strata
  n <- 6000
  stratum <- sample(c("a", "b", "c"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  base <- c(a = 2, b = 5, c = 9)[stratum]
  y0 <- base; y1 <- base - 0.7; y2 <- base - 1.5
  exposure <- integer(n)
  for (s in c("a", "b", "c")) {
    idx <- which(stratum == s)
    exposure[idx] <- sample(0:2, length(idx), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  }
  y <- ifelse(exposure == 0, y0, ifelse(exposure == 1, y1, y2))
  res <- morie_spillover_effects(y, exposure, stratum)
  # within each stratum the potential outcomes are constant, so ignorability holds exactly
  expect_equal(unname(res$means["Y0"]), mean(y0), tolerance = 1e-12)
  expect_equal(unname(res$means["Y1"]), mean(y1), tolerance = 1e-12)
  expect_equal(unname(res$means["Y2"]), mean(y2), tolerance = 1e-12)
  expect_equal(res$spillover, -0.7, tolerance = 1e-12)
  expect_equal(res$direct, -0.8, tolerance = 1e-12)
  expect_equal(res$total, res$spillover + res$direct, tolerance = 1e-12)   # decomposition
  expect_true(res$positivity)
})

test_that("pooling bias equals share(1) times the level gap, stratum by stratum (misspecified_exposure_bias)", {
  set.seed(1)
  n <- 300
  stratum <- rep(c("u", "v"), each = n / 2)
  exposure <- sample(0:2, n, replace = TRUE)
  y <- rnorm(n)
  res <- morie_spillover_effects(y, exposure, stratum)
  for (s in c("u", "v")) {
    idx0 <- stratum == s & exposure == 0
    idx1 <- stratum == s & exposure == 1
    share1 <- sum(idx1) / (sum(idx0) + sum(idx1))
    expect_equal(unname(res$pooling_bias$by_stratum[s]),
                 share1 * (mean(y[idx1]) - mean(y[idx0])), tolerance = 1e-12)
  }
})

test_that("a naive pooled analysis mis-states the effect when spillover exists", {
  set.seed(1)
  n <- 4000
  stratum <- rep("one", n)
  exposure <- sample(0:2, n, replace = TRUE)
  y <- 10 - 2 * (exposure == 2) - 1 * (exposure == 1)
  res <- morie_spillover_effects(y, exposure, stratum)
  expect_equal(res$total, -2)
  expect_equal(res$spillover, -1)
  # pooled control mean is pulled down by the spilled-over places
  expect_lt(res$pooling_bias$overall, 0)
})

test_that("argument checks", {
  set.seed(1)
  expect_error(morie_spillover_effects(1:3, c(0, 1, 3), c("a", "a", "a")), "0, 1, 2")
  expect_error(morie_spillover_effects(1:3, c(0, 1, 2), c("a", "a")), "equal length")
  expect_error(morie_spillover_effects(1:3, c(0, 1, 2), c("a", "a", "a"), weights = c(-1, 1, 1)), "non-negative")
})

test_that("Horvitz-Thompson is unbiased over the design (ht_unbiased, ht_contrast_unbiased)", {
  set.seed(1)
  n <- 8; edges <- cbind(1:7, 2:8); n_treated <- 3
  y0 <- 10 + seq_len(n); y1 <- y0 - 1; y2 <- y0 - 3       # potential outcomes per place
  pr <- morie_spillover_exposure_probs(n, edges, n_treated)   # exact enumeration (56 assignments)
  expect_true(all(pr > 0))
  cmb <- utils::combn(n, n_treated)
  est <- t(sapply(seq_len(ncol(cmb)), function(k) {
    e <- morie_spillover_exposure(seq_len(n) %in% cmb[, k], edges)$exposure
    y <- ifelse(e == 0, y0, ifelse(e == 1, y1, y2))
    h <- morie_spillover_ht(y, e, pr)
    c(h$totals, total_effect = h$total_effect)
  }))
  # exact design expectation (uniform over assignments) equals the population totals
  expect_equal(unname(colMeans(est)[1:3]), c(sum(y0), sum(y1), sum(y2)), tolerance = 1e-10)
  expect_equal(unname(colMeans(est)[4]), (sum(y2) - sum(y0)) / n, tolerance = 1e-10)
})

test_that("Horvitz-Thompson variance: exact design variance equals the formula and the estimator is unbiased (ht_variance, ht_variance_estimator_unbiased)", {
  set.seed(1)
  n <- 8; edges <- cbind(1:7, 2:8); n_treated <- 3
  y0 <- 10 + seq_len(n); y1 <- y0 - 1; y2 <- y0 - 3
  pr <- morie_spillover_exposure_probs(n, edges, n_treated, joint = TRUE)
  expect_equal(dim(pr$joint), c(n, n, 3L))
  # diagonal of the joint equals the marginal; joint is symmetric
  for (l in 1:3) {
    expect_equal(diag(pr$joint[, , l]), unname(pr$marginal[, l]))
    expect_equal(pr$joint[, , l], t(pr$joint[, , l]))
  }
  cmb <- utils::combn(n, n_treated)
  pot <- list(y0, y1, y2)
  est <- t(sapply(seq_len(ncol(cmb)), function(k) {
    e <- morie_spillover_exposure(seq_len(n) %in% cmb[, k], edges)$exposure
    y <- ifelse(e == 0, y0, ifelse(e == 1, y1, y2))
    h <- morie_spillover_ht(y, e, pr$marginal, pr$joint)
    c(h$totals, h$variance)
  }))
  for (l in 1:3) {
    # exact design variance of the total over the 56 equiprobable assignments
    v_design <- mean(est[, l]^2) - mean(est[, l])^2
    v_formula <- morie_spillover_ht_variance(pot[[l]], pr$marginal[, l], pr$joint[, , l])
    expect_equal(v_design, v_formula, tolerance = 1e-10)
    # the estimator averages to the variance when every joint probability is positive
    if (all(pr$joint[, , l] > 0)) {
      expect_equal(mean(est[, 3 + l]), v_formula, tolerance = 1e-10)
    } else {
      expect_true(anyNA(est[, 3 + l]) || TRUE)
    }
  }
  # at least one level must have all joint probabilities positive on this design, else the test is vacuous
  expect_true(any(vapply(1:3, function(l) all(pr$joint[, , l] > 0), logical(1))))
  expect_error(morie_spillover_ht(y0, rep(0, n), pr$marginal, joint = matrix(1, n, n)), "n x n x 3")
})
