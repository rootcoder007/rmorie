# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/xdeep_native.R (xDeepFM, Lian et al. 2018). The CIN
# layer X^k_h = sum_ij W^{k,h}_ij (X^{k-1}_i o X^0_j) is recomputed
# with sums of Hadamard products; the score is the logistic of
# bias + linear + CIN + DNN terms.

.xd_X0 <- matrix(c(1, 0.5, -1, 2, 0, 1), 3, 2)
.xd_W1 <- list(matrix(c(1, 0, 0.5, -1, 2, 0, 0, 1, 0.3), 3), matrix(0.2, 3, 3))
.xd_W2 <- list(matrix(c(1, -1, 0.5, 0, 2, 1), 2))

.xd_layer <- function(P, W) {
  lapply(W, function(Wh) {
    acc <- 0
    for (i in seq_len(nrow(P))) for (j in 1:3) acc <- acc + Wh[i, j] * P[i, ] * .xd_X0[j, ]
    acc
  })
}

test_that("hadamard is the element-wise product", {
  expect_equal(xdeep_hadamard(1:3, c(2, 0, -1)), c(2, 0, -3))
  expect_error(xdeep_hadamard(1:3, 1:2), "differ in length")
})

test_that("cin_layer and cin compress Hadamard interactions layer by layer", {
  l1 <- xdeep_cin_layer(.xd_X0, .xd_X0, .xd_W1)
  e1 <- .xd_layer(.xd_X0, .xd_W1)
  expect_equal(l1, e1, tolerance = 1e-12)
  l2 <- xdeep_cin_layer(do.call(rbind, l1), .xd_X0, .xd_W2)
  e2 <- .xd_layer(do.call(rbind, e1), .xd_W2)
  expect_equal(l2, e2, tolerance = 1e-12)
  for (fn in list(xdeep_cin, xdeep_xdeepfm, morie_xdeep)) {
    r <- fn(.xd_X0, list(.xd_W1, .xd_W2))
    expect_equal(r$pooled, c(vapply(e1, sum, 0), vapply(e2, sum, 0)), tolerance = 1e-12)
    expect_equal(r$degrees, 2:3)
  }
  expect_error(xdeep_cin_layer(matrix(1, 2, 3), .xd_X0, .xd_W1), "embedding size")
})

test_that("interaction_degree is layer + 2", {
  expect_identical(xdeep_interaction_degree(0)$degree, 2L)
  expect_identical(xdeep_interaction_degree(3)$degree, 5L)
  expect_error(xdeep_interaction_degree(-1), "cannot be negative")
})

test_that("xdeepfm_score adds linear, CIN and DNN terms under a logistic", {
  pooled <- xdeep_cin(.xd_X0, list(.xd_W1))$pooled
  wc <- c(0.3, -0.2)
  s <- xdeep_xdeepfm_score(c(1, 0, 2), c(0.5, 1, -0.25), .xd_X0, list(.xd_W1), wc,
                           dnn_output = 0.7, w_dnn = 2, bias = -0.1)
  z <- -0.1 + (0.5 - 0.5) + sum(wc * pooled) + 1.4
  expect_equal(s$logit, z, tolerance = 1e-12)
  expect_equal(s$probability, plogis(z), tolerance = 1e-12)
  expect_equal(s$cin, sum(wc * pooled), tolerance = 1e-12)
  expect_equal(xdeep_xdeepfm_score(0, 0, .xd_X0, list(.xd_W1), c(0, 0), bias = -800)$probability, 0)
  expect_error(xdeep_xdeepfm_score(1:2, 1, .xd_X0, list(.xd_W1), wc), "mis-sized")
  expect_error(xdeep_xdeepfm_score(1, 1, .xd_X0, list(.xd_W1), 1), "1 CIN weights for 2 pooled")
})

test_that("xdeep_cheatsheet explains vector-wise interactions", {
  expect_match(xdeep_cheatsheet(), "layer k = degree k+1", fixed = TRUE)
})
