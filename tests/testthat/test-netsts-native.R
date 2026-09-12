# LSTM time-series forecasting, and what the gates are for.
#
# Anchors outside the module: the LSTM recurrence written out longhand,
# which with zero weights collapses to arithmetic that can be checked by
# inspection; the closed form of the gradient retained through a chain of
# identical forget gates; base R's solve for the ridge readout; and a
# constant series, whose forecast must be that constant exactly.

test_that("the cell is the textbook LSTM recurrence", {
  d <- 2L
  # zero weights and biases put every pre-activation at zero, so the input
  # and output gates sit at a half, the candidate is zero, and the cell
  # simply halves
  W <- matrix(0, 1 + d, 4 * d)
  b <- rep(0, 4 * d)
  h0 <- c(0, 0)
  c0 <- c(2, -4)
  out <- .netsts_lstm_cell(1, h0, c0, W, b)
  expect_equal(out$gates$i, rep(0.5, d))
  expect_equal(out$gates$f, rep(0.5, d))
  expect_equal(out$gates$o, rep(0.5, d))
  expect_equal(out$gates$g, rep(0, d))
  # c' = f * c + i * g
  expect_equal(out$c, 0.5 * c0)
  # h = o * tanh(c')
  expect_equal(out$h, 0.5 * tanh(0.5 * c0))

  # the forget bias shifts the forget gate and nothing else
  fb <- .netsts_lstm_cell(1, h0, c0, W, b, forget_bias = 100)
  expect_equal(fb$gates$f, rep(1, d))
  expect_equal(fb$gates$i, rep(0.5, d))
  # so with the gate open the cell is retained rather than halved
  expect_equal(fb$c, c0)
  # and a large negative bias closes it, erasing the cell
  cl <- .netsts_lstm_cell(1, h0, c0, W, b, forget_bias = -100)
  expect_equal(cl$gates$f, rep(0, d))
  expect_equal(cl$c, rep(0, d))

  # a non-zero weight reaches the gates through the concatenated input
  W2 <- matrix(0, 1 + d, 4 * d)
  W2[1, 1] <- 10          # input gate of the first unit, from x
  g2 <- .netsts_lstm_cell(1, h0, c0, W2, b)
  expect_equal(g2$gates$i[1], 1 / (1 + exp(-10)))
  expect_equal(g2$gates$i[2], 0.5)

  expect_error(.netsts_lstm_cell(1, c(0, 0), c(0, 0, 0), W, b),
               "hidden and cell sizes differ")
  expect_error(.netsts_lstm_cell(1, h0, c0, matrix(0, 2, 4 * d), b),
               "wrong number of rows")
  expect_error(.netsts_lstm_cell(1, h0, c0, W, rep(0, 3)),
               "bias needs 4[*]hidden entries")
})

test_that("the run threads the state through the sequence", {
  d <- 2L
  W <- matrix(0, 1 + d, 4 * d)
  b <- rep(0, 4 * d)
  X <- list(1, 1, 1)
  r <- .netsts_lstm_run(X, W, b, d)
  expect_length(r$hs, 3L)
  expect_length(r$cs, 3L)
  expect_length(r$gates, 3L)
  # from a zero cell with zero weights the candidate is zero, so the cell
  # stays at zero and so does the hidden state
  for (k in 1:3) {
    expect_equal(r$cs[[k]], rep(0, d))
    expect_equal(r$hs[[k]], rep(0, d))
  }
  # with the forget gate wide open a non-zero candidate accumulates
  W2 <- matrix(0, 1 + d, 4 * d)
  W2[1, 2 * d + 1] <- 10          # candidate of the first unit
  r2 <- .netsts_lstm_run(X, W2, b, d, forget_bias = 100)
  g <- tanh(10)
  # c_t = c_{t-1} + i * g with f = 1 and i = 1/2
  expect_equal(r2$cs[[1]][1], 0.5 * g)
  expect_equal(r2$cs[[2]][1], 1.0 * g)
  expect_equal(r2$cs[[3]][1], 1.5 * g)
  # each step records its own gates
  expect_named(r2$gates[[1]], c("i", "f", "g", "o"))
})

test_that("retained gradient is the forget gate raised to the lag", {
  # the derivative of the cell state through k identical forget gates is
  # the product of those gates
  for (f in c(0, 0.25, 0.5, 0.9, 1)) {
    for (k in c(1L, 5L, 20L)) {
      expect_equal(.netsts_gradient_retention(f, k), f^k)
    }
  }
  # a fully open gate retains everything however long the lag
  expect_equal(.netsts_gradient_retention(1, 1000L), 1)
  # a closed one retains nothing after a single step
  expect_equal(.netsts_gradient_retention(0, 1L), 0)
  # and a half-open gate halves the gradient each step, which is the
  # vanishing the gates exist to avoid
  expect_equal(.netsts_gradient_retention(0.5, 10L), 2^-10)
  expect_true(.netsts_gradient_retention(0.9, 50L) < 0.01)
  expect_error(.netsts_gradient_retention(-0.1, 5L), "must be in \\[0, 1\\]")
  expect_error(.netsts_gradient_retention(1.1, 5L), "must be in \\[0, 1\\]")
})

test_that("the ridge readout agrees with base R", {
  set.seed(3)
  X <- cbind(1, matrix(rnorm(40), 20, 2))
  y <- X %*% c(1, 2, -1) + rnorm(20, 0, 0.1)
  for (ridge in c(0, 1e-6, 0.5)) {
    XtX <- crossprod(X)
    diag(XtX) <- diag(XtX) + ridge
    want <- as.numeric(solve(XtX, crossprod(X, y)))
    expect_equal(as.numeric(.netsts_lstsq(X, y, ridge)), want,
                 tolerance = 1e-10)
  }
  # with no penalty it is ordinary least squares
  expect_equal(as.numeric(.netsts_lstsq(X, y, 0)),
               unname(coef(lm(y ~ X[, 2] + X[, 3]))), tolerance = 1e-8)
  # a list of rows is accepted as the design
  rows <- lapply(seq_len(nrow(X)), function(i) X[i, ])
  expect_equal(as.numeric(.netsts_lstsq(rows, y, 1e-6)),
               as.numeric(.netsts_lstsq(X, y, 1e-6)))
  # a larger penalty shrinks the coefficients toward zero
  small <- .netsts_lstsq(X, y, 1e-6)
  big <- .netsts_lstsq(X, y, 1e4)
  expect_true(sum(big^2) < sum(small^2))
})

test_that("the normal draws are standard and reproducible", {
  rng <- .ghc_rng(1)
  z <- .netsts_standard_normal(rng, 4000L)
  expect_length(z, 4000L)
  expect_equal(mean(z), 0, tolerance = 0.05)
  expect_equal(stats::sd(z), 1, tolerance = 0.05)
  expect_true(all(is.finite(z)))
  # the stream is deterministic given its seed
  expect_equal(.netsts_standard_normal(.ghc_rng(5), 10L),
               .netsts_standard_normal(.ghc_rng(5), 10L))
  expect_false(isTRUE(all.equal(.netsts_standard_normal(.ghc_rng(5), 10L),
                                .netsts_standard_normal(.ghc_rng(9), 10L))))
})

test_that("standardising is reversible and survives a constant series", {
  y <- c(3, 5, 9, 11)
  s <- .netsts_standardize(y)
  expect_equal(s$mu, mean(y))
  expect_equal(s$sd, stats::sd(y))
  expect_equal(s$z, (y - mean(y)) / stats::sd(y))
  # the transform inverts
  expect_equal(s$z * s$sd + s$mu, y)
  # a constant series has no spread, so the scale falls back on one rather
  # than dividing by zero
  cs <- .netsts_standardize(rep(5, 10))
  expect_equal(cs$mu, 5)
  expect_equal(cs$sd, 1)
  expect_equal(cs$z, rep(0, 10))
  expect_true(all(is.finite(cs$z)))
})

test_that("a constant series forecasts that constant", {
  r <- morie_netsts(rep(5, 40), horizon = 3L, hidden = 4L, n_lags = 3L,
                    seed = 1L)
  expect_length(r$forecast, 3L)
  expect_equal(r$forecast, rep(5, 3))
  expect_equal(r$estimate, r$forecast)
  expect_equal(r$mean, 5)
  expect_equal(r$sd, 1)
  expect_equal(r$hidden, 4L)
  expect_equal(r$n_lags, 3L)
  expect_equal(r$strategy, "recursive")
  # the reported retention is the forget gate's ten-step product
  expect_equal(r$retention_10,
               .netsts_gradient_retention(
                 1 / (1 + exp(-r$forget_bias)), 10L),
               tolerance = 1e-8)
  expect_match(r$method, "LSTM")
})

test_that("a trend is continued, and both strategies run", {
  y <- as.numeric(1:60)
  rec <- morie_netsts(y, horizon = 4L, hidden = 4L, n_lags = 3L, seed = 1L)
  expect_length(rec$forecast, 4L)
  # the series rises by one per step, so the forecast must rise too and
  # start above the last observation's neighbourhood
  expect_true(all(diff(rec$forecast) > 0))
  expect_true(rec$forecast[1] > 55 && rec$forecast[1] < 65)
  expect_equal(rec$strategy, "recursive")
  # the run reproduces from its seed
  expect_equal(morie_netsts(y, horizon = 4L, hidden = 4L, n_lags = 3L,
                            seed = 1L)$forecast, rec$forecast)
  # a different seed initialises different weights
  expect_false(isTRUE(all.equal(
    morie_netsts(y, horizon = 4L, hidden = 4L, n_lags = 3L,
                 seed = 9L)$forecast, rec$forecast)))
  # the direct strategy fits one readout per horizon step
  dir <- morie_netsts(y, horizon = 4L, hidden = 4L, n_lags = 3L, seed = 1L,
                      strategy = "direct")
  expect_equal(dir$strategy, "direct")
  expect_length(dir$forecast, 4L)
  expect_true(all(diff(dir$forecast) > 0))
  expect_equal(dir$n_models, 4L)
  # the recursive route fits a single one-step model
  expect_equal(rec$n_models, 1L)
})

test_that("netsts validates its arguments", {
  y <- as.numeric(1:40)
  expect_error(morie_netsts(y, horizon = 2L, strategy = "ensemble"),
               "strategy must be recursive or direct")
  # too short a series for the lags and horizon asked for
  expect_error(morie_netsts(y[1:6], horizon = 3L, n_lags = 4L),
               "too few for 4 lags")
})
