# Polya urn predictive (Blackwell & MacQueen 1973; Muller & Quintana
# 2004): urn weights, a replayed urn, the DP-mixture predictive density,
# the expected number of clusters and the tie probability.

test_that("urn weights are n_k / (alpha + n) and alpha / (alpha + n)", {
  w <- urn_weights(c(3, 1, 2), 1.5)
  expect_equal(w$existing, c(3, 1, 2) / 7.5, tolerance = 1e-15)
  expect_equal(w$new, 1.5 / 7.5, tolerance = 1e-15)
  expect_equal(sum(w$existing) + w$new, 1, tolerance = 1e-15)
  expect_equal(urn_weights(numeric(0), 2)$new, 1)
  expect_error(urn_weights(1, 0), "concentration must be positive")
  expect_error(urn_weights(c(1, 0), 1), "positive count")
})

test_that("the urn replays: join cluster j with prob n_j/(a+n), else open a new one", {
  r <- morie_posspr(30, 1.2, seed = 6)
  e <- .ghc_rng(6)
  cnt <- 1
  lab <- 0L
  for (k in 2:30) {
    u <- .ghc_unif(e, 1L)
    cum <- cumsum(cnt / (1.2 + sum(cnt)))
    j <- which(u <= cum)[1]
    if (is.na(j)) {
      cnt <- c(cnt, 1)
      lab <- c(lab, length(cnt) - 1L)
    } else {
      cnt[j] <- cnt[j] + 1
      lab <- c(lab, j - 1L)
    }
  }
  expect_identical(r$labels, lab)
  expect_equal(r$counts, cnt)
  expect_identical(r$n_clusters, length(cnt))
  expect_identical(sample_urn(30, 1.2, seed = 6)$labels, lab)
  expect_error(morie_posspr(0, 1), "at least 1")
})

test_that("the predictive density mixes occupied kernels with the prior predictive", {
  grid <- c(-1, 0, 0.5, 2)
  params <- list(c(0, 1), c(1.5, 0.5))
  kern <- function(y, p) stats::dnorm(y, p[1], p[2])
  base <- function(y) stats::dnorm(y, 0, 3)
  r <- predictive_density(grid, params, c(4, 2), 1, kern, base)
  ref <- (4 * stats::dnorm(grid, 0, 1) + 2 * stats::dnorm(grid, 1.5, 0.5) + stats::dnorm(grid, 0, 3)) / 7
  expect_equal(r$density, ref, tolerance = 1e-15)
  expect_equal(r$new_cluster_weight, 1 / 7)
  # the predictive integrates to one
  tot <- stats::integrate(function(y) predictive_density(y, params, c(4, 2), 1, kern, base)$density,
                          -Inf, Inf)$value
  expect_equal(tot, 1, tolerance = 1e-6)
  e0 <- predictive_density(grid, list(), numeric(0), 2, kern, base)
  expect_equal(e0$density, base(grid), tolerance = 1e-15)
})

test_that("expected clusters sum alpha/(alpha + i - 1); ties have prob 1/(1 + alpha)", {
  ec <- expected_clusters(50, 2)
  expect_equal(ec$expected, sum(2 / (2 + 0:49)), tolerance = 1e-14)
  expect_equal(ec$log_approximation, 2 * log(1 + 25), tolerance = 1e-15)
  # Monte Carlo check against the urn itself: 400 replicate urns
  k <- vapply(1:400, function(s) morie_posspr(50, 2, seed = s)$n_clusters, 1L)
  expect_lt(abs(mean(k) - ec$expected), 4 * stats::sd(k) / sqrt(400))
  expect_error(expected_clusters(0, 1), "n >= 1")
  tp <- tie_probability(3)
  expect_equal(c(tp$tie, tp$new), c(0.25, 0.75))
  expect_error(tie_probability(-1), "positive")
})
