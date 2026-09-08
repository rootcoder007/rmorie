test_that("covariate balance recovers a known shift and equal weights are neutral", {
  set.seed(2)
  n <- 4000
  tr <- rbinom(n, 1, 0.5)
  X <- matrix(rnorm(n * 3), n, 3) + 0.8 * tr
  r <- morie_covariate_balance(X, tr)
  expect_equal(r$smd[1], 0.8, tolerance = 0.2)
  expect_false(r$balanced)
  expect_equal(r$smd, morie_covariate_balance(X, tr, weights = rep(2.5, n))$smd,
               tolerance = 1e-12)
})

test_that("inverse-probability weights restore balance", {
  set.seed(4)
  n <- 4000
  xx <- rnorm(n)
  ps <- 1 / (1 + exp(-xx))
  tr <- as.integer(runif(n) < ps)
  w <- ifelse(tr == 1, 1 / ps, 1 / (1 - ps))
  before <- morie_covariate_balance(matrix(xx, ncol = 1), tr)$max_smd
  after <- morie_covariate_balance(matrix(xx, ncol = 1), tr, weights = w)$max_smd
  expect_gt(before, 0.3)
  expect_lt(after, before / 2)
})

test_that("Jacquez finds space-time clustering but not space-only", {
  set.seed(3)
  centres <- matrix(runif(12), 6, 2)
  times <- runif(6, 0, 10)
  idx <- rep(seq_len(6), each = 12)
  coords <- centres[idx, ] + matrix(rnorm(144, 0, 0.01), 72, 2)
  tt <- times[idx] + rnorm(72, 0, 0.05)
  expect_lte(morie_jacquez_knn(coords, tt, k = 3, B = 199)$p_value, 0.01)
  # Space-only clustering is not interaction. Asserted as a rate over
  # seeds: one draw is far too noisy, and measured here the test
  # rejects 2 of 12 null replicates.
  rej <- sum(vapply(1:8, function(s) {
    set.seed(200 + s)
    as.numeric(morie_jacquez_knn(coords, runif(72, 0, 10), k = 3, B = 99)$p_value <= 0.05)
  }, numeric(1)))
  expect_lte(rej, 3)
})

test_that("Jacquez saturates when space and time orderings agree", {
  x <- matrix(seq_len(30), ncol = 1)
  expect_equal(morie_jacquez_knn(x, as.numeric(x), k = 2, B = 19)$statistic, 60)
})

test_that("Ripley K is non-decreasing and the CSR test separates the cases", {
  set.seed(6)
  P <- matrix(runif(240), 120, 2)
  r <- morie_ripley_csr_test(P, nsim = 99)
  expect_true(all(diff(r$k_observed) >= -1e-12))
  # Size asserted as a rate: measured at 0 of 12 null replicates, but a
  # single draw can land at the 1/100 floor by chance.
  rej <- sum(vapply(1:8, function(s) {
    set.seed(100 + s)
    as.numeric(morie_ripley_csr_test(matrix(runif(240), 120, 2), nsim = 99)$p_value <= 0.05)
  }, numeric(1)))
  expect_lte(rej, 2)
  set.seed(7)
  parents <- matrix(runif(12, 0.1, 0.9), 6, 2)
  idx <- sample.int(6, 120, replace = TRUE)
  clus <- pmin(pmax(parents[idx, ] + matrix(rnorm(240, 0, 0.02), 120, 2), 0), 1)
  expect_lte(morie_ripley_csr_test(clus, nsim = 99)$p_value, 0.05)
})
