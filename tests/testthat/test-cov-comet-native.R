# Coverage tests for R/comet_native.R (Rei et al. 2020): pooled features,
# the estimator and reference-free heads, the triplet margin loss and
# segment-level Kendall tau.

cm_h <- c(0.2, -0.5, 1)
cm_s <- c(0.1, 0.3, 0.8)
cm_r <- c(0.4, -0.2, 0.9)

test_that("pooled features and the estimator head", {
  f <- pooled_features(cm_h, cm_s, cm_r)
  expect_equal(f$features, c(cm_h, cm_r, cm_h * cm_r, abs(cm_h - cm_r), cm_h * cm_s, abs(cm_h - cm_s)))
  expect_equal(f$dim, 18L)
  expect_error(pooled_features(cm_h, cm_s[-1], cm_r), "differ in length")
  W <- matrix(seq(-0.9, 0.8, by = 0.1), 1)
  expect_equal(estimator_score(cm_h, cm_s, cm_r, W, 0.3)$score, 0.3 + sum(W * f$features), tolerance = 1e-12)
  W2 <- rbind(W, rev(W))
  expect_equal(estimator_score(cm_h, cm_s, cm_r, W2)$estimate, as.numeric(W2 %*% f$features), tolerance = 1e-12)
  expect_equal(morie_comet(cm_h, cm_s, cm_r, W, 1)$score, estimator_score(cm_h, cm_s, cm_r, W, 1)$score)
  expect_identical(comet, estimator_score)
  expect_identical(cometmetric, estimator_score)
  expect_error(estimator_score(cm_h, cm_s, cm_r, W[, -1, drop = FALSE]), "expects 17 features but got 18")
})

test_that("reference-free head uses only hypothesis and source", {
  W <- matrix(1:12 / 10, 1)
  r <- reference_free(cm_h, cm_s, W, -1)
  expect_equal(r$score, -1 + sum(W * c(cm_h, cm_s, cm_h * cm_s, abs(cm_h - cm_s))), tolerance = 1e-12)
  expect_false(r$reference_used)
  expect_length(reference_free(cm_h, cm_s, rbind(W, W))$score, 2L)
  expect_error(reference_free(cm_h, cm_s[-1], W), "differ in length")
  expect_error(reference_free(cm_h, cm_s, W[, 1:4, drop = FALSE]), "expects 4 features but got 12")
  expect_match(.comet_cheatsheet(), "Kendall")
})

test_that("triplet margin loss on source and reference", {
  better <- c(0.3, -0.3, 0.9)
  worse <- c(-1, 1, 0)
  d <- function(a, b) sqrt(sum((a - b)^2))
  t1 <- triplet_loss(better, worse, cm_s, cm_r, margin = 0.5)
  expect_equal(t1$source_term, max(0, d(better, cm_s) - d(worse, cm_s) + 0.5), tolerance = 1e-12)
  expect_equal(t1$reference_term, max(0, d(better, cm_r) - d(worse, cm_r) + 0.5), tolerance = 1e-12)
  expect_true(t1$satisfied)
  t2 <- triplet_loss(worse, better, cm_s, cm_r)
  expect_equal(t2$loss, d(worse, cm_s) - d(better, cm_s) + 1 + d(worse, cm_r) - d(better, cm_r) + 1, tolerance = 1e-12)
  expect_false(t2$satisfied)
  expect_error(triplet_loss(better, worse, cm_s, cm_r, margin = 0), "margin must be positive")
  expect_error(triplet_loss(better, worse[-1], cm_s, cm_r), "differ in length")
})

test_that("segment-level Kendall tau drops tied pairs", {
  a <- c(0.1, 0.7, 0.3, 0.9, 0.5)
  b <- c(1, 3, 4, 5, 2)
  k <- kendall_tau(a, b)
  expect_equal(k$tau, cor(a, b, method = "kendall"), tolerance = 1e-12)
  expect_equal(k$concordant + k$discordant, 10)
  tt <- kendall_tau(c(1, 1, 2, 3), c(1, 2, 2, 1))
  # pairs tied on either side count neither way
  pr <- combn(4, 2)
  sa <- sign(c(1, 1, 2, 3)[pr[1, ]] - c(1, 1, 2, 3)[pr[2, ]])
  sb <- sign(c(1, 2, 2, 1)[pr[1, ]] - c(1, 2, 2, 1)[pr[2, ]])
  ok <- sa != 0 & sb != 0
  expect_equal(tt$tau, sum(sa[ok] * sb[ok]) / sum(ok))
  expect_equal(kendall_tau(c(1, 1), c(2, 3))$tau, 0)
  expect_error(kendall_tau(1, 2), "at least 2")
  expect_error(kendall_tau(1:3, 1:2), "3 scores but 2")
})
