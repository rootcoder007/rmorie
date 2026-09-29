# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/pate_native.R (PATE, Papernot et al. 2017). Votes are
# counted by hand, the noisy argmax re-drawn from the package stream
# with inverse-CDF Laplace noise, and the privacy accounting
# recomputed from eq. (1), Lemma 4, Theorem 3 and the data-independent
# bound (which gives the paper's 5.80 for T = 100, gamma = 0.05).

.pt_teachers <- list(
  function(rows) lapply(rows, function(x) if (x > 0) 1L else 0L),
  function(rows) lapply(rows, function(x) if (x > 1) 1L else 0L),
  function(rows) lapply(rows, function(x) if (x > -1) c(0.2, 0.8) else c(0.9, 0.1)),
  function(rows) lapply(rows, function(x) 2L))

test_that("teacher_votes builds the per-record vote histogram", {
  v <- teacher_votes(.pt_teachers, list(-2, 0.5, 3))
  expect_equal(unname(v), list(c(3, 0, 1), c(1, 2, 1), c(0, 3, 1)))
  expect_equal(unname(teacher_votes(.pt_teachers[1:2], list(3), n_classes = 4)), list(c(0, 2, 0, 0)))
  expect_error(teacher_votes(list(), list(1)), "at least one teacher")
})

test_that("noisy_argmax adds Lap(1/gamma) draws on the package stream", {
  cnt <- c(3, 5, 4)
  for (g in c(0.05, 2)) {
    e <- .ghc_rng(9)
    u <- .ghc_unif(e, 3L) - 0.5
    lap <- -sign(u) * log(1 - 2 * abs(u)) / g
    expect_identical(noisy_argmax(cnt, g, seed = 9), which.max(cnt + lap) - 1L)
  }
  expect_identical(noisy_argmax(c(0, 100, 0), 10), 1L)
  expect_error(noisy_argmax(cnt, 0), "gamma must be positive")
})

test_that("the accounting follows the paper's bounds", {
  expect_equal(epsilon_data_independent(100, 0.05, 1e-5), 1 + 0.1 * sqrt(200 * log(1e5)), tolerance = 1e-12)
  expect_equal(round(epsilon_data_independent(100, 0.05, 1e-5), 2), 5.80)
  expect_error(epsilon_data_independent(-1, 0.05, 1e-5), "T >= 0")
  expect_error(epsilon_data_independent(10, 0.05, 1), "delta must lie")
  n <- c(10, 2, 1)
  g <- 0.5
  q <- (2 + g * 8) / (4 * exp(g * 8)) + (2 + g * 9) / (4 * exp(g * 9))
  expect_equal(lemma4_bound(n, g), c(min(q, 1), q), tolerance = 1e-12)
  expect_equal(lemma4_bound(c(1, 1, 1, 1), 0.3), c(1, 1.5))
  expect_error(lemma4_bound(numeric(0), 1), "empty vote")
  l <- 3
  th <- log((1 - q) * ((1 - q) / (1 - exp(2 * g) * q))^l + q * exp(2 * g * l))
  expect_equal(theorem3_moment(q, g, l), th, tolerance = 1e-12)
  expect_null(theorem3_moment(0.9, g, l))
  expect_equal(theorem3_moment(0, g, l), 0)
  expect_error(theorem3_moment(-0.1, g, l), "non-negative")
  votes <- list(c(10, 2, 1), c(4, 4, 5))
  acc <- moments_accountant(votes, g, 1e-5, lambdas = 1:3)
  alpha <- vapply(1:3, function(l) {
    sum(vapply(votes, function(v) {
      ind <- 2 * g^2 * l * (l + 1)
      dep <- theorem3_moment(lemma4_bound(v, g)[1], g, l)
      if (!is.null(dep) && dep < ind) dep else ind
    }, 0))
  }, 0)
  eps <- (alpha + log(1e5)) / (1:3)
  expect_equal(acc$epsilon, min(eps), tolerance = 1e-12)
  expect_equal(acc$lambda, which.min(eps))
  ind_only <- moments_accountant(votes, g, 1e-5, lambdas = 2, data_dependent = FALSE)
  expect_equal(ind_only$epsilon, (2 * 2 * g^2 * 6 + log(1e5)) / 2, tolerance = 1e-12)
  expect_equal(ind_only$used$data_independent, 2L)
  expect_error(moments_accountant(votes, g, 2), "delta must lie")
  expect_error(moments_accountant(votes, g, 1e-5, lambdas = 0), "lambdas must be positive")
})

test_that("pate labels queries with the noisy plurality and reports epsilon", {
  q <- list(-2, 0.5, 3, 5)
  for (fn in list(pate, private_aggregation, pate_aggregate, morie_pate)) {
    r <- fn(.pt_teachers, q, gamma = 0.5, seed = 2, lambdas = 1:4)
    v <- teacher_votes(.pt_teachers, q)
    e <- .ghc_rng(2)
    lab <- vapply(v, function(n) {
      u <- .ghc_unif(e, 3L) - 0.5
      which.max(n - sign(u) * log(1 - 2 * abs(u)) / 0.5) - 1L
    }, 0L)
    expect_identical(r$labels, unname(lab))
    expect_identical(r$clean_labels, unname(vapply(v, which.max, 0L) - 1L))
    expect_equal(r$agreement, mean(r$labels == r$clean_labels))
    expect_equal(r$epsilon, min(moments_accountant(v, 0.5, 1e-5, 1:4)$epsilon,
                                epsilon_data_independent(4, 0.5, 1e-5)), tolerance = 1e-12)
    expect_equal(r$n_teachers, 4L)
  }
  st <- pate(.pt_teachers, q, student_train_fn = function(X, y) list(n = length(X), y = y))
  expect_equal(st$student$n, 4L)
  expect_error(pate(.pt_teachers, q, student_train_fn = identity, student_features = 1:2), "one per query")
  expect_error(pate(.pt_teachers, list()), "no queries")
})
