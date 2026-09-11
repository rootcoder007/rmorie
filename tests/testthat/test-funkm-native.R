# Anchors for regularised matrix factorisation.
#
# The point of a training loop is that the parameters move. This module
# reported an identical RMSE for every epoch and handed back its random
# initialisation, because the epoch returned only the RMSE while R gave it
# copies of bu, bi, P and Q. So the assertions below are about learning,
# not about the call completing.

toy <- function(n_users = 24L, n_items = 18L, n = 500L, seed = 1) {
  set.seed(seed)
  # a rank-2 signal so a factor model can actually fit it
  U <- matrix(rnorm(n_users * 2, 0, 0.6), nrow = n_users)
  V <- matrix(rnorm(n_items * 2, 0, 0.6), nrow = n_items)
  u <- sample.int(n_users, n, TRUE) - 1L
  i <- sample.int(n_items, n, TRUE) - 1L
  r <- 3 + rowSums(U[u + 1L, ] * V[i + 1L, ]) + rnorm(n, 0, 0.05)
  list(ratings = data.frame(u = u, i = i, r = r),
       n_users = n_users, n_items = n_items)
}

test_that("the epoch returns the parameters it updated", {
  d <- toy()
  R <- .funkM_as_ratings(d$ratings)
  mu <- .funkM_global_mean(R)
  bu <- rep(0, d$n_users); bi <- rep(0, d$n_items)
  P <- matrix(0.05, d$n_users, 3L); Q <- matrix(0.05, d$n_items, 3L)
  st <- .funkM_sgd_epoch(R, mu, bu, bi, P, Q, lr = 0.01, reg = 0.02)
  expect_named(st, c("rmse", "bu", "bi", "P", "Q"))
  expect_true(is.numeric(st$rmse) && length(st$rmse) == 1L)
  # the updates have to come back out: returning the RMSE alone discarded
  # every one of them
  expect_false(isTRUE(all.equal(st$P, P)))
  expect_false(isTRUE(all.equal(st$Q, Q)))
  expect_false(isTRUE(all.equal(st$bu, bu)))
  expect_identical(dim(st$P), dim(P))
  expect_identical(dim(st$Q), dim(Q))
  expect_length(st$bu, length(bu))
  expect_length(st$bi, length(bi))
})

test_that("training reduces the error, epoch after epoch", {
  d <- toy()
  fit <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                     epochs = 40, lr = 0.02, reg = 0.02, seed = 1)
  h <- fit$rmse_history
  expect_length(h, 40L)
  # an identical RMSE for every epoch was the signature of the dead loop
  expect_gt(length(unique(round(h, 10))), 1L)
  expect_lt(h[length(h)], h[1])
  # and it descends rather than wandering
  expect_true(mean(diff(h) < 0) > 0.9)
  expect_identical(fit$rmse, h[length(h)])
  expect_identical(fit$estimate, h[length(h)])
})

test_that("the fitted factors beat the bias-only baseline", {
  d <- toy()
  fit <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                     epochs = 60, lr = 0.02, reg = 0.02, seed = 1)
  # scoring with the fitted parameters
  got <- morie_funkM_rmse(d$ratings, fit$mu, fit$b_user, fit$b_item,
                          fit$P, fit$Q)
  # the same scorer with the factors zeroed: mu and biases only
  flat <- morie_funkM_rmse(d$ratings, fit$mu, fit$b_user, fit$b_item,
                           matrix(0, nrow(fit$P), ncol(fit$P)),
                           matrix(0, nrow(fit$Q), ncol(fit$Q)))
  expect_lt(got, flat)
  # The history records the error measured as the epoch ran, using each
  # rating's parameters before its own update, so re-scoring with the
  # final parameters is at least as good. It should still be close.
  expect_lte(got, fit$rmse + 1e-12)
  expect_equal(got, fit$rmse, tolerance = 0.2)
})

test_that("the returned factors are not the initialisation", {
  d <- toy()
  one <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                     epochs = 1, lr = 0.02, seed = 1)
  many <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                      epochs = 50, lr = 0.02, seed = 1)
  expect_false(isTRUE(all.equal(one$P, many$P)))
  expect_lt(many$rmse, one$rmse)
})

test_that("the same seed reproduces the fit and a different one does not", {
  d <- toy()
  a <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                   epochs = 20, lr = 0.02, seed = 5)
  b <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                   epochs = 20, lr = 0.02, seed = 5)
  c2 <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                    epochs = 20, lr = 0.02, seed = 6)
  expect_equal(a$P, b$P, tolerance = 1e-15)
  expect_equal(a$rmse_history, b$rmse_history, tolerance = 1e-15)
  expect_false(isTRUE(all.equal(a$P, c2$P)))
})

test_that("the incremental route trains one factor at a time and learns", {
  d <- toy()
  fit <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                     lr = 0.02, reg = 0.02, seed = 1,
                     incremental = TRUE, epochs_per_factor = 15)
  h <- fit$rmse_history
  expect_length(h, 3L * 15L)
  expect_true(fit$incremental)
  expect_gt(length(unique(round(h, 10))), 1L)
  expect_lt(h[length(h)], h[1])
})

test_that("a single factor still fits the marginal structure", {
  d <- toy()
  f1 <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 1,
                    epochs = 40, lr = 0.02, seed = 1)
  f3 <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                    epochs = 40, lr = 0.02, seed = 1)
  expect_lt(f1$rmse, f1$rmse_history[1])
  # the rank-2 signal needs more than one factor
  expect_lt(f3$rmse, f1$rmse)
})

test_that("regularisation shrinks the factors", {
  d <- toy()
  loose <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                       epochs = 40, lr = 0.02, reg = 0.0, seed = 1)
  tight <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 3,
                       epochs = 40, lr = 0.02, reg = 0.5, seed = 1)
  expect_lt(sum(tight$P^2), sum(loose$P^2))
})

test_that("prediction is the bias model plus the factor dot product", {
  expect_equal(
    .funkM_predict(3, 0.5, -0.25, matrix(c(1, 2), nrow = 1),
                   matrix(c(0.5, -1), nrow = 1)),
    3 + 0.5 - 0.25 + (1 * 0.5 + 2 * -1),
    tolerance = 1e-12
  )
  # no factors at all leaves the bias model
  expect_equal(.funkM_predict(3, 0.5, -0.25, matrix(numeric(0), nrow = 1),
                              matrix(numeric(0), nrow = 1)),
               3.25, tolerance = 1e-12)
})

test_that("the global mean is the mean of the observed ratings only", {
  R <- .funkM_as_ratings(data.frame(u = c(0L, 1L, 2L), i = c(0L, 1L, 2L),
                                    r = c(1, 2, 6)))
  expect_equal(.funkM_global_mean(R), 3, tolerance = 1e-12)
})

test_that("the inputs are validated", {
  d <- toy()
  expect_error(morie_funkM(d$ratings[0, ], 5L, 5L), "no ratings given")
  expect_error(morie_funkM(d$ratings, 0L, 5L), "counts must be positive")
  expect_error(morie_funkM(d$ratings, 5L, 5L, factors = 0), "counts must be positive")
  expect_error(morie_funkM(d$ratings, d$n_users, d$n_items, reg = -1),
               "cannot be negative")
  expect_error(morie_funkM_rmse(d$ratings[0, ], 0, 0, 0, matrix(0), matrix(0)),
               "no ratings to score")
})

test_that("the reported density and observation count describe the input", {
  d <- toy(n = 300L)
  fit <- morie_funkM(d$ratings, d$n_users, d$n_items, factors = 2,
                     epochs = 5, seed = 1)
  expect_identical(fit$observed, 300L)
  expect_equal(fit$density, 300 / (d$n_users * d$n_items), tolerance = 1e-12)
  expect_s3_class(fit, "RichResult")
})
