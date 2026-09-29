# Coverage tests for R/ibpfa_native.R (Griffiths and Ghahramani 2011,
# Indian buffet process): sampling, expected feature counts,
# left-ordered form, the matrix log-probability and the Gibbs update.

test_that("buffet sampling is reproducible and internally consistent", {
  s <- sample_ibp(8, 2, seed = 5)
  expect_identical(sample_ibp(8, 2, seed = 5), s)
  expect_equal(s$K, ncol(s$Z))
  expect_equal(s$counts, as.integer(colSums(s$Z)))
  expect_equal(s$features_per_object, as.integer(rowSums(s$Z)))
  expect_true(all(s$Z %in% c(0L, 1L)))
  # a dish first appears in the row that introduced it
  first <- apply(s$Z, 2, function(z) which(z == 1)[1])
  expect_false(is.unsorted(first))
  expect_identical(morie_ibpfa(8, 2, seed = 5), s)
  expect_error(sample_ibp(0, 1), "n >= 1")
})

test_that("expected feature counts", {
  e <- expected_features(10, 1.5)
  expect_equal(e$expected_total_features, 1.5 * sum(1 / (1:10)), tolerance = 1e-12)
  expect_equal(e$expected_nonzeros, 15)
  expect_error(expected_features(5, 0), "alpha > 0")
})

test_that("left-ordered form sorts columns by their binary history", {
  Z <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 1), c(0, 0, 1, 1))
  lf <- left_ordered_form(Z)
  hist <- apply(Z, 2, function(z) sum(z * 2^(rev(seq_along(z)) - 1)))
  expect_equal(lf$order, order(-hist, seq_along(hist)))
  expect_equal(lf$Z, Z[, order(-hist)])
  expect_error(left_ordered_form(matrix(0L, 0, 0)), "empty")
})

test_that("log-probability of a feature matrix and the Gibbs update", {
  Z <- rbind(c(1, 1, 0), c(1, 0, 1), c(0, 1, 0), c(1, 0, 0))
  a <- 1.3
  m <- colSums(Z)
  ref <- 3 * log(a) - a * sum(1 / (1:4)) + sum(lgamma(4 - m + 1) + lgamma(m) - lgamma(5))
  expect_equal(ibp_log_probability(Z, a), ref, tolerance = 1e-12)
  expect_error(ibp_log_probability(Z, 0), "positive")
  lik <- function(M) -sum((M - 0.5)^2 * c(1, 2, 3, 4))
  g <- gibbs_feature_update(Z, 1, 1, lik, a)
  pr <- 2 / 4
  on <- Z
  on[1, 1] <- 1
  off <- Z
  off[1, 1] <- 0
  l1 <- lik(on) + log(pr)
  l0 <- lik(off) + log(1 - pr)
  expect_equal(g$p_on, exp(l1) / (exp(l1) + exp(l0)), tolerance = 1e-12)
  expect_equal(g$z, as.integer(g$p_on > 0.5))
  expect_equal(gibbs_feature_update(Z, 2, 3, lik, a)$z, 0L)
})
