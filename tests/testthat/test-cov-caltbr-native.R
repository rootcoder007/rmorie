# Coverage tests for R/caltbR_native.R (Steck 2018, calibrated
# recommendations): genre distributions, calibration metrics, the
# diversity prior and greedy calibrated re-ranking.

cb_P <- rbind(c(1, 0, 0), c(0.5, 0.5, 0), c(0, 1, 0), c(0, 0.2, 0.8), c(0.9, 0.1, 0), c(0, 0, 1))

test_that("genre distribution of a list is the weighted average of p(g|i)", {
  expect_equal(genre_distribution(c(1, 3, 4), cb_P), colMeans(cb_P[c(1, 3, 4), ]), tolerance = 1e-12)
  w <- c(3, 1, 1)
  expect_equal(genre_distribution(c(1, 3, 4), cb_P, w), colSums(cb_P[c(1, 3, 4), ] * w) / 5, tolerance = 1e-12)
  expect_equal(morie_caltbR(2, as.data.frame(cb_P)), cb_P[2, ])
  expect_equal(genre_distribution(1:2, lapply(1:6, function(i) cb_P[i, ])), colMeans(cb_P[1:2, ]))
  expect_error(genre_distribution(7, cb_P), "out of range")
  expect_error(genre_distribution(1:2, cb_P, c(0, 0)), "sum to zero")
})

test_that("calibration KL and Hellinger", {
  p <- c(0.7, 0.3, 0)
  q <- c(0.2, 0.5, 0.3)
  qt <- 0.99 * q + 0.01 * p
  expect_equal(calibration_kl(p, q), sum(p[1:2] * log(p[1:2] / qt[1:2])), tolerance = 1e-12)
  expect_equal(calibration_kl(p, p), 0, tolerance = 1e-12)
  expect_equal(calibration_kl(c(7, 3, 0), c(2, 5, 3), alpha = 0.1), sum(p[1:2] * log(p[1:2] / (0.9 * q + 0.1 * p)[1:2])), tolerance = 1e-12)
  expect_error(calibration_kl(p, q, alpha = 1), "alpha")
  expect_error(calibration_kl(p, q[1:2]), "3 genres in p but 2")
  expect_equal(calibration_hellinger(p, q), sqrt(sum((sqrt(p) - sqrt(q))^2)) / sqrt(2), tolerance = 1e-12)
  expect_error(calibration_hellinger(c(0, 0), q), "no mass")
  expect_equal(diversity_prior(p, c(1, 1, 1) / 3, 0.25), 0.25 / 3 + 0.75 * p, tolerance = 1e-12)
  expect_error(diversity_prior(p, q, 2), "beta")
})

test_that("greedy calibrated re-ranking maximises the MMR objective step by step", {
  s <- c(0.9, 0.85, 0.8, 0.4, 0.7, 0.3)
  pt <- c(0.5, 0.3, 0.2)
  r <- calibrated_rerank(s, cb_P, pt, N = 3, lam = 0.5)
  obj <- function(sel) 0.5 * sum(s[sel]) - 0.5 * calibration_kl(pt, genre_distribution(sel, cb_P))
  chosen <- integer(0)
  for (k in 1:3) {
    cand <- setdiff(1:6, chosen)
    v <- vapply(cand, function(i) obj(c(chosen, i)), 0)
    chosen <- c(chosen, cand[which.max(v)])
    expect_equal(r$objective_path[k], max(v), tolerance = 1e-12)
  }
  expect_equal(r$ranking, chosen)
  expect_equal(r$score_uncalibrated, sum(sort(s, decreasing = TRUE)[1:3]))
  expect_lte(r$calibration, r$calibration_uncalibrated + 1e-12)
  # lambda = 0 is pure relevance ranking
  expect_equal(calibrated_rerank(s, cb_P, pt, N = 4, lam = 0)$ranking, order(-s)[1:4])
  h <- calibrated_rerank(s, cb_P, pt, N = 2, metric = "hellinger")
  expect_equal(h$calibration, calibration_hellinger(pt, genre_distribution(h$ranking, cb_P)), tolerance = 1e-12)
  rw <- calibrated_rerank(s, cb_P, pt, N = 3, rank_weights = c(1, 0.5, 0.25))
  expect_equal(rw$q, genre_distribution(rw$ranking, cb_P, c(1, 0.5, 0.25)), tolerance = 1e-12)
  expect_identical(calibrated_rec, calibrated_rerank)
  expect_identical(calibratedrec, calibrated_rerank)
  expect_identical(calibratedrecommendations, calibrated_rerank)
  expect_error(calibrated_rerank(s, cb_P, pt, metric = "js"), "metric must be")
  expect_error(calibrated_rerank(s, cb_P, pt, lam = 2), "lambda")
  expect_error(calibrated_rerank(s[-1], cb_P, pt), "6 genre rows for 5 scores")
})
