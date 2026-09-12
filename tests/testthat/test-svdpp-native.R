# SVD++ (Koren 2008, eq. 15): which items a user rated is itself a signal.
#
# Anchors outside the module: every closed form written out longhand -- the
# baseline, the implicit term's scaling, the prediction, and each of the
# five gradient updates -- plus the property a descent step must have, that
# it reduces the squared error of the rating it was taken on.

test_that("the baseline is the sum of the three biases", {
  expect_equal(.svdpp_baseline(3.5, 0.2, -0.1), 3.5 + 0.2 - 0.1)
  expect_equal(.svdpp_baseline(0, 0, 0), 0)
  # it is additive in each argument
  expect_equal(.svdpp_baseline(1, 2, 3) - .svdpp_baseline(1, 2, 0), 3)
})

test_that("the implicit term is the scaled sum of the rated items' factors", {
  y <- list("0" = c(1, 0), "1" = c(0, 2), "2" = c(3, 3))
  r <- .svdpp_implicit_term(list(0, 1), y)
  # |N(u)|^-1/2 times the sum over the rated items
  expect_equal(r$raw_sum, c(1, 2))
  expect_equal(r$scale, 2^-0.5)
  expect_equal(r$term, 2^-0.5 * c(1, 2))
  expect_equal(r$n_rated, 2L)
  expect_equal(r$exponent, -0.5)
  # a different exponent rescales it
  r2 <- .svdpp_implicit_term(list(0, 1), y, exponent = -1)
  expect_equal(r2$scale, 0.5)
  expect_equal(r2$term, 0.5 * c(1, 2))
  # an exponent of zero is the plain sum
  expect_equal(.svdpp_implicit_term(list(0, 1, 2), y, exponent = 0)$term,
               c(4, 5))
  # a user who rated nothing gets no signal, at the right width
  none <- .svdpp_implicit_term(list(), y)
  expect_equal(none$term, c(0, 0))
  expect_equal(none$n_rated, 0L)
  # and with no factor table at all there is no width to report
  expect_length(.svdpp_implicit_term(list(), list())$term, 0L)
})

test_that("the prediction puts the implicit term inside the inner product", {
  mu <- 3.5
  bu <- 0.2
  bi <- -0.1
  p <- c(0.5, -0.2)
  q <- c(1, 2)
  y <- list("0" = c(1, 0), "1" = c(0, 2))
  pr <- .svdpp_predict(mu, bu, bi, p, q, list(0, 1), y)
  imp <- 2^-0.5 * c(1, 2)
  expect_equal(pr$implicit, imp)
  expect_equal(pr$effective_user_factor, p + imp)
  expect_equal(pr$prediction, mu + bu + bi + sum(q * (p + imp)))
  expect_equal(pr$n_rated, 2L)
  # with no implicit information the prediction is the plain latent model
  plain <- .svdpp_predict(mu, bu, bi, p, q)
  expect_equal(plain$prediction, mu + bu + bi + sum(q * p))
  expect_equal(plain$implicit, c(0, 0))
  expect_equal(plain$effective_user_factor, p)
  expect_equal(plain$n_rated, 0L)
  # the implicit term shifts taste, not level: it enters through q
  expect_false(isTRUE(all.equal(pr$prediction, plain$prediction)))
  expect_equal(pr$prediction - plain$prediction, sum(q * imp))
  expect_error(.svdpp_predict(mu, bu, bi, c(1, 2, 3), q),
               "differ in width")
})

test_that("each gradient update is its closed form", {
  mu <- 3.0
  bu <- 0.1
  bi <- -0.2
  p <- c(0.3, -0.4)
  q <- c(0.5, 0.6)
  y <- list("0" = c(0.1, 0.2), "1" = c(-0.1, 0.3))
  lr <- 0.01
  reg <- 0.02
  rating <- 4.0
  st <- .svdpp_sgd_step(rating, mu, bu, bi, p, q, list(0, 1), y, lr, reg)

  pr <- .svdpp_predict(mu, bu, bi, p, q, list(0, 1), y)
  e <- rating - pr$prediction
  expect_equal(st$error, e)
  # the two biases take the same shape of step
  expect_equal(st$b_user, bu + lr * (e - reg * bu))
  expect_equal(st$b_item, bi + lr * (e - reg * bi))
  # the item factor moves along the effective user factor, which includes
  # the implicit term; the user factor moves along the item factor
  expect_equal(st$q_i, q + lr * (e * pr$effective_user_factor - reg * q))
  expect_equal(st$p_u, p + lr * (e * q - reg * p))
  # each rated item's factor carries the same scaling the forward pass used
  scale <- 2^-0.5
  for (j in c("0", "1")) {
    expect_equal(st$y[[j]], y[[j]] + lr * (e * scale * q - reg * y[[j]]))
  }
  expect_match(st$note, "forward pass")

  # a step reduces the squared error on the rating it was taken on
  after <- .svdpp_predict(mu, st$b_user, st$b_item, st$p_u, st$q_i,
                          list(0, 1), st$y)
  expect_true(abs(rating - after$prediction) < abs(e))
  # with no implicit part the y update is empty
  ni <- .svdpp_sgd_step(rating, mu, bu, bi, p, q, list(), list(), lr, reg)
  expect_length(ni$y, 0L)
  expect_equal(ni$p_u, p + lr * ((rating - (mu + bu + bi + sum(q * p))) * q -
                                   reg * p))
})

test_that("training drives the error down", {
  # a small rating matrix with clear structure: two users who agree and two
  # items that differ
  ratings <- list(
    list(0L, 0L, 5), list(0L, 1L, 1),
    list(1L, 0L, 5), list(1L, 1L, 1),
    list(2L, 0L, 4), list(2L, 1L, 2)
  )
  fit <- .svdpp_fit(ratings, n_users = 3L, n_items = 2L, factors = 2L,
                    epochs = 200L, lr = 0.02, reg = 0.001, seed = 1L)
  expect_length(fit$rmse_history, 200L)
  expect_equal(fit$rmse, tail(fit$rmse_history, 1))
  expect_equal(fit$estimate, fit$rmse)
  # the error at the end is well below the error at the start
  expect_true(fit$rmse < fit$rmse_history[1])
  expect_true(fit$rmse < 0.5)
  # the learned pieces have the shapes the model calls for
  expect_length(fit$b_user, 3L)
  expect_length(fit$b_item, 2L)
  expect_length(fit$P, 3L)
  expect_length(fit$Q, 2L)
  expect_length(fit$P[[1]], 2L)
  expect_true(fit$implicit)
  expect_false(is.null(fit$Y))
  expect_match(fit$method, "Koren")
  # the fitted model reproduces the ratings it was trained on
  for (r in ratings) {
    u <- r[[1]] + 1L
    i <- r[[2]] + 1L
    pr <- .svdpp_predict(fit$mu, fit$b_user[u], fit$b_item[i],
                         fit$P[[u]], fit$Q[[i]],
                         as.list(c(0L, 1L)), fit$Y)
    expect_equal(pr$prediction, r[[3]], tolerance = 0.6)
  }
  # a fixed seed reproduces the run exactly
  again <- .svdpp_fit(ratings, 3L, 2L, factors = 2L, epochs = 20L,
                      lr = 0.02, reg = 0.001, seed = 1L)
  once <- .svdpp_fit(ratings, 3L, 2L, factors = 2L, epochs = 20L,
                     lr = 0.02, reg = 0.001, seed = 1L)
  expect_equal(again$rmse_history, once$rmse_history)
})

test_that("the implicit signal can be switched off", {
  ratings <- list(
    list(0L, 0L, 5), list(0L, 1L, 1),
    list(1L, 0L, 5), list(1L, 1L, 1)
  )
  with_ <- .svdpp_fit(ratings, 2L, 2L, factors = 2L, epochs = 50L, seed = 2L,
                      implicit = TRUE)
  without <- .svdpp_fit(ratings, 2L, 2L, factors = 2L, epochs = 50L, seed = 2L,
                        implicit = FALSE)
  expect_true(with_$implicit)
  expect_false(without$implicit)
  # the plain model keeps no item-implicit factors at all
  expect_null(without$Y)
  expect_false(is.null(with_$Y))
  # both learn something
  expect_true(with_$rmse < with_$rmse_history[1])
  expect_true(without$rmse < without$rmse_history[1])
  # and the two routes are genuinely different models
  expect_false(isTRUE(all.equal(with_$rmse, without$rmse)))
})

test_that("the public entry point matches the fitter", {
  ratings <- list(list(0L, 0L, 4), list(0L, 1L, 2), list(1L, 0L, 3))
  a <- morie_svdpp(ratings, 2L, 2L, factors = 2L, epochs = 10L, seed = 3L)
  b <- .svdpp_fit(ratings, 2L, 2L, factors = 2L, epochs = 10L, seed = 3L)
  expect_equal(a$rmse_history, b$rmse_history)
  expect_equal(a$mu, b$mu)
  # the global mean is the mean of the observed ratings
  expect_equal(a$mu, mean(c(4, 2, 3)))
  expect_type(.svdpp_cheatsheet(), "character")
})
