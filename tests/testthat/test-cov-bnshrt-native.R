# Coverage for Honore & Tamer (2006) short-panel bounds: sequence
# probabilities of the dynamic binary-choice model (products of
# F(x'b + g y_{t-1} + a) terms, summing to one), empirical sequence
# frequencies, the mixture-feasibility test over (alpha, y0) (a target
# built as a mixture is feasible, a distorted one is not) and the
# identified set over a (beta, gamma) grid.

.x <- matrix(c(0.5, -1, 1.5), ncol = 1)

test_that("sequence probabilities multiply per-period choice probabilities", {
  p <- morie_sequence_probabilities(0.8, -0.5, .x, 0.2, 1)
  expect_equal(sum(unlist(p)), 1, tolerance = 1e-12)
  seq_ <- c(1, 0, 1)
  prev <- 1
  pr <- 1
  for (t in 1:3) {
    q <- stats::plogis(0.8 * .x[t] - 0.5 * prev + 0.2)
    pr <- pr * if (seq_[t] == 1) q else 1 - q
    prev <- seq_[t]
  }
  expect_equal(p[["101"]], pr, tolerance = 1e-12)
  pp <- morie_sequence_probabilities(0.8, -0.5, .x, 0.2, 0, link = "probit")
  expect_equal(pp[["000"]], prod(1 - stats::pnorm(0.8 * .x[, 1] + 0.2 + c(0, 0, 0) * -0.5)), tolerance = 1e-12)
  expect_error(morie_sequence_probabilities(c(1, 2), 0, .x, 0, 0), "2 entries for 1 covariates")
  expect_error(morie_sequence_probabilities(1, 0, .x, 0, 0, link = "cloglog"), "logit or probit")
  expect_error(morie_sequence_probabilities(1, 0, matrix(0, 0, 1), 0, 0), "at least one period")
})

test_that("sequence frequencies", {
  Y <- rbind(c(1, 0, 1), c(1, 0, 1), c(0, 0, 0), c(0, 1, 1))
  f <- morie_sequence_frequencies(Y)
  expect_length(f, 8L)
  expect_equal(f[["101"]], 0.5)
  expect_equal(f[["011"]], 0.25)
  expect_equal(f[["110"]], 0)
  expect_error(morie_sequence_frequencies(matrix(2, 1, 2)), "0/1")
  expect_error(morie_sequence_frequencies(matrix(0, 0, 2)), "no observations")
})

test_that("mixtures over (alpha, y0) are feasible; distortions are not", {
  ag <- c(-1, 0, 1)
  cols <- lapply(ag, function(a) morie_sequence_probabilities(0.8, -0.5, .x, a, 0))
  keys <- sort(names(cols[[1]]))
  mix <- 0.3 * unlist(cols[[1]][keys]) + 0.7 * unlist(cols[[3]][keys])
  freq <- as.list(stats::setNames(mix, keys))
  r <- morie_in_identified_set(freq, 0.8, -0.5, .x, ag, tol = 1e-4)
  expect_true(r$feasible)
  expect_lt(r$discrepancy, 1e-4)
  expect_equal(sum(r$weights), 1, tolerance = 1e-12)
  A <- do.call(cbind, lapply(ag, function(a) sapply(c(0, 1), function(y0) unlist(morie_sequence_probabilities(0.8, -0.5, .x, a, y0)[keys]))))
  expect_equal(r$fitted, as.numeric(A %*% r$weights), tolerance = 1e-12)
  bad <- freq
  bad[["000"]] <- bad[["000"]] + 0.2
  bad[["111"]] <- bad[["111"]] - 0.2
  expect_false(morie_in_identified_set(bad, 0.8, -0.5, .x, ag)$feasible)
  expect_error(morie_in_identified_set(freq, 0.8, -0.5, .x, numeric(0)), "alpha grid is empty")
})

test_that("the identified set contains the true parameter", {
  set.seed(4)
  n <- 400
  al <- sample(c(-1, 1), n, replace = TRUE)
  Y <- t(vapply(al, function(a) {
    prev <- 0
    y <- numeric(3)
    for (t in 1:3) {
      y[t] <- stats::rbinom(1, 1, stats::plogis(0.8 * .x[t] - 0.5 * prev + a))
      prev <- y[t]
    }
    y
  }, numeric(3)))
  r <- morie_identified_set(Y, .x, beta_grid = c(0.8, 3), gamma_grid = c(-0.5, 2), alpha_grid = seq(-2, 2, 0.5), tol = 0.05)
  expect_true(list(c(0.8, -0.5)) %in% r$set)
  expect_identical(morie_bnshrt, morie_identified_set)
  expect_identical(morie_shortpanelbound, morie_identified_set)
  expect_identical(morie_bound_short_panel, morie_identified_set)
  n0 <- morie_identified_set(Y, .x, 3, 2, 0, tol = 1e-8)
  expect_identical(n0$n_feasible, 0L)
})
