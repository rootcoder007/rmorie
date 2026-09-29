# Coverage tests for R/elmo_native.R (Peters et al. 2018, ELMo): softmax
# layer weights, the LSTM cell, the bidirectional LM and the eq. (1) mix.

el_mk <- function(r, c, s) matrix(sin(s * seq_len(r * c)) / 2, r, c)
el_layer <- function(s) list(Wxf = el_mk(2, 8, s), Whf = el_mk(2, 8, s + 0.3), bf = sin(s * (1:8)) / 5,
  Wxb = el_mk(2, 8, s + 0.7), Whb = el_mk(2, 8, s + 1.1), bb = cos(s * (1:8)) / 5)
el_X <- rbind(c(0.5, -0.2), c(0.1, 0.8), c(-0.6, 0.3))

el_cell <- function(x, h, c, Wx, Wh, b) {
  z <- as.numeric(x %*% Wx + h %*% Wh + b)
  i <- plogis(z[1:2])
  f <- plogis(z[3:4])
  g <- tanh(z[5:6])
  o <- plogis(z[7:8])
  cn <- f * c + i * g
  list(h = o * tanh(cn), c = cn)
}

test_that("layer weights and one LSTM step", {
  expect_equal(layer_weights(c(0, 1, 2)), exp(0:2) / sum(exp(0:2)), tolerance = 1e-12)
  expect_equal(layer_weights(c(1000, 1000)), c(0.5, 0.5))
  expect_error(layer_weights(numeric(0)), "no layer weights")
  L <- el_layer(1.3)
  r <- lstm_step(c(0.5, -0.2), c(0.1, 0.3), c(-0.2, 0.4), L$Wxf, L$Whf, L$bf)
  ref <- el_cell(c(0.5, -0.2), c(0.1, 0.3), c(-0.2, 0.4), L$Wxf, L$Whf, L$bf)
  expect_equal(r, ref, tolerance = 1e-12)
  expect_error(lstm_step(1:2, c(0, 0), 0, L$Wxf, L$Whf, L$bf), "sizes differ")
})

test_that("biLM: duplicated token layer, forward and re-aligned backward passes", {
  L1 <- el_layer(0.9)
  reps <- bilm_forward(el_X, list(L1))
  expect_equal(reps[[1]], lapply(1:3, function(t) rep(el_X[t, ], 2)))
  h <- c <- c(0, 0)
  fw <- list()
  for (t in 1:3) {
    s <- el_cell(el_X[t, ], h, c, L1$Wxf, L1$Whf, L1$bf)
    h <- s$h
    c <- s$c
    fw[[t]] <- h
  }
  h <- c <- c(0, 0)
  bw <- list()
  for (t in 3:1) {
    s <- el_cell(el_X[t, ], h, c, L1$Wxb, L1$Whb, L1$bb)
    h <- s$h
    c <- s$c
    bw[[t]] <- h
  }
  expect_equal(reps[[2]], lapply(1:3, function(t) c(fw[[t]], bw[[t]])), tolerance = 1e-12)
  two <- bilm_forward(el_X, list(L1, list(Wxf = el_mk(4, 8, 2), Whf = el_mk(2, 8, 2.2), bf = rep(0, 8),
    Wxb = el_mk(4, 8, 2.5), Whb = el_mk(2, 8, 2.7), bb = rep(0, 8))))
  expect_length(two, 3L)
  expect_error(bilm_forward(cbind(el_X, 1), list(L1)), "token dimension 3")
  expect_error(bilm_forward(matrix(0, 0, 2), list(L1)), "empty sequence")
})

test_that("ELMo mixture gamma * sum_j s_j h_kj and its entry points", {
  reps <- bilm_forward(el_X, list(el_layer(0.9)))
  raw <- c(0.3, -0.5)
  s <- exp(raw) / sum(exp(raw))
  mix <- elmo_mix(reps, raw, gamma = 1.7)
  ref <- lapply(1:3, function(t) 1.7 * (s[1] * reps[[1]][[t]] + s[2] * reps[[2]][[t]]))
  expect_equal(mix, ref, tolerance = 1e-12)
  expect_equal(elmo_mix(reps, raw, gamma = 1.7, position = 2), ref[[2]], tolerance = 1e-12)
  expect_error(elmo_mix(reps, 1), "1 weights for 2 layers")
  e <- elmo_representation(el_X, list(el_layer(0.9)), raw_weights = raw, gamma = 1.7)
  expect_equal(e$elmo, ref, tolerance = 1e-12)
  expect_equal(e$weights, s, tolerance = 1e-12)
  expect_equal(e$top_layer, reps[[2]])
  eq <- elmo_representation(el_X, list(el_layer(0.9)))
  expect_equal(eq$weights, c(0.5, 0.5))
  for (f in list(elmo, elmorepresentation, morie_elmo)) {
    expect_equal(f(el_X, list(el_layer(0.9)), raw, 1.7)$elmo, ref, tolerance = 1e-12)
  }
})
