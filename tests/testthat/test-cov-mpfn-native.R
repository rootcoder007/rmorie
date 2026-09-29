# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/mpfn_native.R (Gilmer et al. 2017 message passing).
# With the default message M = e_vw h_w and the residual update, T
# steps are H_T = (I + E)^T H_0 for the weighted adjacency E, which the
# tests use as the independent recomputation; the GRU update is
# recomputed with plogis and the readouts with colSums.

.mp_H <- matrix(c(1, 2, 3, 0.5, -1, 2), 3, 2)
.mp_E <- matrix(c(0, 2, 0.5, 2, 0, 0, 0.5, 0, 0), 3, 3)
.mp_adj0 <- list("0" = c(2L, 1L), "1" = 0L, "2" = 0L)
.mp_ef0 <- list("0_1" = 2, "2_0" = 0.5)
.mp_pow <- function(M, k) Reduce(`%*%`, rep(list(M), k), diag(nrow(M)))

test_that("mpfn_message scales by the edge weight or maps through A(e)", {
  A <- function(e) matrix(c(e, 1, 0, 2 * e), 2, 2)
  for (fn in list(morie_mpfn_message, mpfn_message)) {
    expect_equal(fn(c(9, 9), c(1, -2), 3), c(3, -6))
    expect_equal(fn(c(9, 9), c(1, -2), c(0.5, 7)), c(0.5, -1))
    expect_equal(fn(c(9, 9), c(1, -2), 3, A), as.numeric(A(3) %*% c(1, -2)))
  }
})

test_that("sigmoid and GRU update recompute z, r and the candidate state", {
  expect_equal(mpfn_sig(c(-2, 0, 3)), plogis(c(-2, 0, 3)), tolerance = 1e-12)
  expect_equal(mpfn_sig(-800), 1 / (1 + exp(700)))
  set.seed(11)
  W <- replicate(6, matrix(stats::rnorm(4), 2, 2), simplify = FALSE)
  h <- c(0.3, -0.7)
  m <- c(1.2, 0.4)
  z <- plogis(W[[1]] %*% m + W[[2]] %*% h)
  r <- plogis(W[[3]] %*% m + W[[4]] %*% h)
  hh <- tanh(W[[5]] %*% m + W[[6]] %*% (r * h))
  expected <- as.numeric((1 - z) * h + z * hh)
  expect_equal(morie_mpfn_update_gru(h, m, W[[1]], W[[2]], W[[3]], W[[4]], W[[5]], W[[6]]),
               expected, tolerance = 1e-12)
  expect_equal(mpfn_update_gru(h, m, W[[1]], W[[2]], W[[3]], W[[4]], W[[5]], W[[6]]),
               expected, tolerance = 1e-12)
})

test_that("message_passing with residual update is (I + E)^T H0", {
  for (T in 1:3) {
    expected <- .mp_pow(diag(3) + .mp_E, T) %*% .mp_H
    got <- morie_mpfn_message_passing(.mp_H, .mp_adj0, .mp_ef0, T)
    expect_equal(unname(got), unname(expected), tolerance = 1e-12)
    expect_equal(unname(morie_mpfn(.mp_H, .mp_adj0, .mp_ef0, T)), unname(expected), tolerance = 1e-12)
    # the list-based arm uses 1-based labels and "v,w" keys
    gl <- mpfn_message_passing(list(.mp_H[1, ], .mp_H[2, ], .mp_H[3, ]),
                               list("1" = c(2L, 3L), "2" = 1L, "3" = 1L),
                               list("1,2" = 2, "3,1" = 0.5), T)
    expect_equal(do.call(rbind, gl), unname(expected), tolerance = 1e-12)
  }
  # missing edge features default to weight 1; a custom update replaces h + m
  got <- morie_mpfn_message_passing(.mp_H, .mp_adj0, list(), 1L, update = function(h, m) m)
  E1 <- (.mp_E > 0) * 1
  expect_equal(unname(got), E1 %*% .mp_H, tolerance = 1e-12)
  gl <- mpfn_message_passing(list(1, 2, 3), list("1" = c(2L, 3L), "2" = 1L, "3" = 1L),
                             list(), 1, update = function(h, m) m)
  expect_equal(unlist(gl), c(5, 1, 1))
  expect_error(morie_mpfn_message_passing(.mp_H, .mp_adj0, .mp_ef0, 0L), "at least 1")
  expect_error(mpfn_message_passing(list(1), list(), list(), 0), "at least 1")
})

test_that("readouts: sum, mean and the gated sum of sigmoid(i) * j", {
  i_fn <- function(h, h0) h - h0
  j_fn <- function(h) 2 * h
  H0 <- .mp_H / 2
  gated <- colSums(plogis(.mp_H - H0) * (2 * .mp_H))
  expect_equal(morie_mpfn_readout(.mp_H), colSums(.mp_H))
  expect_equal(morie_mpfn_readout(.mp_H, "mean"), colMeans(.mp_H), tolerance = 1e-12)
  expect_equal(morie_mpfn_readout(.mp_H, "gated", H0, i_fn, j_fn), unname(gated), tolerance = 1e-12)
  Hl <- list(.mp_H[1, ], .mp_H[2, ], .mp_H[3, ])
  H0l <- list(H0[1, ], H0[2, ], H0[3, ])
  expect_equal(mpfn_readout(Hl), colSums(.mp_H))
  expect_equal(mpfn_readout(Hl, "mean"), colMeans(.mp_H), tolerance = 1e-12)
  expect_equal(mpfn_readout(Hl, "gated", H0l, i_fn, j_fn), unname(gated), tolerance = 1e-12)
  expect_error(morie_mpfn_readout(.mp_H, "max"), "readout must be one of")
  expect_error(morie_mpfn_readout(.mp_H, "gated"), "needs H0")
  expect_error(mpfn_readout(Hl, "max"), "got max")
  expect_error(mpfn_readout(Hl, "gated"), "needs H0")
})

test_that("is_permutation_invariant relabels nodes, edges and features consistently", {
  base <- colSums(.mp_pow(diag(3) + .mp_E, 2) %*% .mp_H)
  r <- morie_mpfn_is_permutation_invariant(.mp_H, .mp_adj0, .mp_ef0, c(2L, 0L, 1L), T = 2)
  expect_true(r$invariant)
  expect_equal(r$max_deviation, 0, tolerance = 1e-12)
  expect_equal(r$readout, unname(base), tolerance = 1e-12)
  rm <- morie_mpfn_is_permutation_invariant(.mp_H, .mp_adj0, .mp_ef0, c(1L, 2L, 0L), T = 3, how = "mean")
  expect_true(rm$invariant)
  expect_equal(rm$readout, unname(colMeans(.mp_pow(diag(3) + .mp_E, 3) %*% .mp_H)), tolerance = 1e-12)
  Hl <- list(.mp_H[1, ], .mp_H[2, ], .mp_H[3, ])
  rl <- mpfn_is_permutation_invariant(Hl, list("1" = c(2L, 3L), "2" = 1L, "3" = 1L),
                                      list("1,2" = 2, "3,1" = 0.5), c(3L, 1L, 2L), T = 2)
  expect_true(rl$invariant)
  expect_equal(rl$readout, unname(base), tolerance = 1e-12)
  expect_error(morie_mpfn_is_permutation_invariant(.mp_H, .mp_adj0, .mp_ef0, c(2L, 0L, 1L), how = "gated"),
               "needs H0")
})

test_that("mpfn_cheatsheet names the framework", {
  s <- mpfn_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "EIGHT")
  expect_match(s, "READOUT MUST BE TOO")
})
