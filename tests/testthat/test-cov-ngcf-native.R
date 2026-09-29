# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/ngcf_native.R (NGCF, Wang et al. 2019). The message
# p_ui (W1 e_i + W2 (e_i * e_u)) with p = 1/sqrt(|N_u||N_i|), the
# self-loop plus neighbour sum of eq. (4) through LeakyReLU, and the
# per-layer concatenation are all recomputed with matrix algebra.

.ng_E <- rbind(c(1, 0.5), c(-0.5, 1), c(0.25, -1), c(2, 0.5))
.ng_adj <- list(c(3L, 4L), 3L, c(1L, 2L), 1L)
.ng_W1 <- matrix(c(1, 0.5, -0.5, 2), 2)
.ng_W2 <- matrix(c(0.25, -1, 1, 0.5), 2)
.ng_leaky <- function(x, s = 0.2) ifelse(x >= 0, x, s * x)

.ng_layer <- function(E, W1, W2, aff = TRUE, slope = 0.2) {
  deg <- lengths(.ng_adj)
  t(vapply(seq_len(4), function(v) {
    msg <- function(w, p) p * (W1 %*% E[w, ] + if (aff) W2 %*% (E[w, ] * E[v, ]) else 0)
    acc <- msg(v, 1 / deg[v])
    for (w in .ng_adj[[v]]) acc <- acc + msg(w, 1 / sqrt(deg[v] * deg[w]))
    .ng_leaky(as.numeric(acc), slope)
  }, numeric(2)))
}

test_that("laplacian_coefficient is 1 / sqrt(|N_u| |N_i|)", {
  expect_equal(ngcf_laplacian_coefficient(4, 9), 1 / 6)
  expect_equal(ngcf_laplacian_coefficient(1, 1), 1)
  expect_error(ngcf_laplacian_coefficient(0, 3), "at least one neighbour")
})

test_that("message adds the elementwise affinity term", {
  ei <- c(0.4, -1)
  eu <- c(2, 0.5)
  p <- 0.3
  expect_equal(ngcf_message(ei, eu, .ng_W1, .ng_W2, p),
               as.numeric(p * (.ng_W1 %*% ei + .ng_W2 %*% (ei * eu))), tolerance = 1e-12)
  expect_equal(ngcf_message(ei, eu, .ng_W1, .ng_W2, p, affinity = FALSE),
               as.numeric(p * (.ng_W1 %*% ei)), tolerance = 1e-12)
})

test_that("propagate sums the self message and the neighbour messages through LeakyReLU", {
  expect_equal(ngcf_propagate(.ng_E, .ng_adj, .ng_W1, .ng_W2), .ng_layer(.ng_E, .ng_W1, .ng_W2),
               tolerance = 1e-12)
  expect_equal(ngcf_propagate(.ng_E, .ng_adj, .ng_W1, .ng_W2, affinity = FALSE, slope = 0.01),
               .ng_layer(.ng_E, .ng_W1, .ng_W2, aff = FALSE, slope = 0.01), tolerance = 1e-12)
  expect_error(ngcf_propagate(.ng_E, list(1L, integer(0), 1L, 1L), .ng_W1, .ng_W2), "node 2 has no neighbours")
})

test_that("stack_layers concatenates every layer; score is the inner product", {
  Ws <- list(list(.ng_W1, .ng_W2), list(diag(2), matrix(0, 2, 2)))
  E1 <- .ng_layer(.ng_E, .ng_W1, .ng_W2)
  E2 <- .ng_layer(E1, diag(2), matrix(0, 2, 2))
  for (fn in list(ngcf_stack_layers, neuralgraphcf, morie_ngcf)) {
    r <- fn(.ng_E, .ng_adj, Ws)
    expect_equal(r$layers[[2]], E1, tolerance = 1e-12)
    expect_equal(r$layers[[3]], E2, tolerance = 1e-12)
    expect_equal(r$final, cbind(.ng_E, E1, E2), tolerance = 1e-12)
    expect_equal(r$n_layers, 2L)
    expect_equal(ngcf_score(r$final, 1, 3), sum(r$final[1, ] * r$final[3, ]), tolerance = 1e-12)
  }
  z <- ngcf_stack_layers(.ng_E, .ng_adj, list())
  expect_equal(z$final, .ng_E)
  expect_equal(z$n_layers, 0L)
})

test_that("ngcf_cheatsheet gives the message and the coefficient", {
  s <- ngcf_cheatsheet()
  expect_match(s, "m_{u<-i} = p_ui (W1 e_i + W2 (e_i * e_u))", fixed = TRUE)
  expect_match(s, "1/sqrt(|N_u||N_i|)", fixed = TRUE)
})
