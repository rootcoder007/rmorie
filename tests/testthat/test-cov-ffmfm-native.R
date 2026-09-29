# Coverage tests for R/ffmFM_native.R (Juan et al. 2016): the FFM
# interaction, logistic loss, parameter counts, initialisation on the
# .ghc stream and one AdaGrad step checked by hand.

ff_fields <- c(0, 0, 1, 2)

test_that("interaction uses each vector indexed by the other feature's field", {
  W <- lapply(1:4, function(j) lapply(1:3, function(f) c(j, f) / 10))
  x <- list(list(0, 1), list(2, 0.5), list(3, 2), list(1, 0))
  ref <- sum(W[[1]][[2]] * W[[3]][[1]]) * 0.5 + sum(W[[1]][[3]] * W[[4]][[1]]) * 2 + sum(W[[3]][[3]] * W[[4]][[2]]) * 1
  expect_equal(phi(x, ff_fields, W), ref, tolerance = 1e-12)
  expect_equal(.ffmFM_phi(x, ff_fields, W), ref, tolerance = 1e-12)
  expect_equal(phi(list(list(0, 1)), ff_fields, W), 0)
})

test_that("logistic loss and parameter counts", {
  expect_equal(logistic_loss(1, 0.7), log1p(exp(-0.7)), tolerance = 1e-12)
  expect_equal(logistic_loss(-1, 0.7), log1p(exp(0.7)), tolerance = 1e-12)
  expect_equal(logistic_loss(-1, 800), 800)
  expect_error(logistic_loss(0, 1), "must be -1 or 1")
  expect_error(.logistic_loss(2, 1), "must be -1 or 1")
  expect_equal(n_parameters(10, 3, 4), 120L)
  expect_equal(n_parameters(10, 3, 4, "fm"), 40L)
  expect_error(n_parameters(1, 1, 1, "ffm2"), "ffm or fm")
  expect_error(.n_parameters(1, 1, 1, "x"), "ffm or fm")
})

test_that("initialisation and one AdaGrad step", {
  z <- fit_ffm(list(), numeric(0), ff_fields, 4, 3, k_dim = 2, epochs = 0, seed = 3)
  u <- .ghc_unif(.ghc_rng(3), 24) / sqrt(2)
  expect_equal(unlist(z$W), u, tolerance = 1e-12)
  row <- list(list(0, 1), list(2, 0.5))
  f1 <- fit_ffm(list(row), 1, ff_fields, 4, 3, k_dim = 2, eta = 0.2, lam = 0.01, epochs = 1, seed = 3)
  W0 <- z$W
  p <- sum(W0[[1]][[2]] * W0[[3]][[1]]) * 0.5
  g0 <- -1 / (1 + exp(p))
  # g0 is the derivative of the logistic loss in phi
  expect_equal(g0, (logistic_loss(1, p + 1e-6) - logistic_loss(1, p - 1e-6)) / 2e-6, tolerance = 1e-8)
  a <- W0[[1]][[2]]
  b <- W0[[3]][[1]]
  g1 <- 0.01 * a + g0 * b * 0.5
  g2 <- 0.01 * b + g0 * a * 0.5
  expect_equal(f1$W[[1]][[2]], a - 0.2 / sqrt(1 + g1^2) * g1, tolerance = 1e-12)
  expect_equal(f1$W[[3]][[1]], b - 0.2 / sqrt(1 + g2^2) * g2, tolerance = 1e-12)
  expect_equal(f1$W[[2]], W0[[2]])
  expect_equal(f1$loss_history, logistic_loss(1, p), tolerance = 1e-12)
  expect_equal(f1$n_parameters, 24L)
  expect_equal(f1$n_parameters_fm, 8L)
  expect_error(fit_ffm(list(), numeric(0), ff_fields, 0, 3), "at least 1")
  expect_error(fit_ffm(list(row), c(1, -1), ff_fields, 4, 3), "1 rows but 2 labels")
  expect_match(.ffmFM_cheatsheet(), "OTHER feature's field")
})

test_that("training lowers the loss; the public aliases match the helper", {
  rows <- list(list(list(0, 1), list(2, 1)), list(list(1, 1), list(3, 1)), list(list(0, 1), list(3, 1)), list(list(1, 1), list(2, 1)))
  y <- c(1, 1, -1, -1)
  f <- fit_ffm(rows, y, ff_fields, 4, 3, k_dim = 2, eta = 0.5, lam = 0, epochs = 30, seed = 1)
  expect_lt(f$final_loss, f$loss_history[1])
  g <- .fit_ffm(rows, y, ff_fields, 4, 3, k_dim = 2, eta = 0.5, lam = 0, epochs = 30, seed = 1)
  expect_equal(g$W, f$W, tolerance = 1e-12)
  expect_equal(morie_ffmFM(rows, y, ff_fields, 4, 3, 2, 0.5, 0, 3, 1), fit_ffm(rows, y, ff_fields, 4, 3, 2, 0.5, 0, 3, 1))
  expect_equal(fieldawarefm(rows, y, ff_fields, 4, 3, 2, 0.5, 0, 2, 1)$W, fit_ffm(rows, y, ff_fields, 4, 3, 2, 0.5, 0, 2, 1)$W)
  expect_equal(field_aware_fm(rows, y, ff_fields, 4, 3, 2, 0.5, 0, 2, 1)$W, fit_ffm(rows, y, ff_fields, 4, 3, 2, 0.5, 0, 2, 1)$W)
})
