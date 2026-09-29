# Coverage for the Evoformer MSA-pair head (Jumper et al. 2021, Alg. 7
# and 10): the outer product mean over sequences, the pair bias (mean or
# projection), row attention softmax(q k' * scale + b) v with an optional
# gate, and the OPM-projected pair update, all recomputed with matrices.

.msa <- function() {
  set.seed(3)
  lapply(1:3, function(k) lapply(1:4, function(i) stats::rnorm(2)))
}
.arr <- function(msa, k) do.call(rbind, msa[[k]])

test_that("softmax is shift-invariant and normalised", {
  x <- c(2, -1, 0.5, 700)
  expect_equal(morie_alfomg_softmax(x), exp(x - 700) / sum(exp(x - 700)), tolerance = 1e-12)
  expect_equal(morie_alfomg_softmax(c(1, 1)), c(0.5, 0.5))
})

test_that("outer product mean averages x_i x_j' over sequences", {
  msa <- .msa()
  o <- morie_alfomg_opm(msa)
  M <- Reduce(`+`, lapply(1:3, function(k) outer(msa[[k]][[2]], msa[[k]][[4]]))) / 3
  expect_equal(o[[2]][[4]], as.vector(t(M)), tolerance = 1e-12)
  expect_length(o, 4L)
  expect_error(morie_alfomg_opm(list()), "no sequences")
  expect_error(morie_alfomg_opm(list(list())), "no positions")
  expect_error(morie_alfomg_opm(list(list(1, 2), list(1))), "same length")
  expect_error(morie_alfomg_opm(list(list(1, c(1, 2)))), "same width")
})

test_that("pair bias and row attention with a gate", {
  msa <- .msa()
  pair <- lapply(1:4, function(i) lapply(1:4, function(j) c(i - j, 0.1 * i * j, 1)))
  b <- morie_alfomg_bias(pair)
  expect_equal(b[3, 1], mean(c(2, 0.3, 1)), tolerance = 1e-12)
  w <- c(0.5, -1, 0.2)
  bw <- morie_alfomg_bias(pair, w)
  expect_equal(bw[2, 4], sum(pair[[2]][[4]] * w), tolerance = 1e-12)
  g <- c(0.5, 2)
  ra <- morie_alfomg_row_attention(msa, bw, gate = g)
  for (k in 1:3) {
    X <- .arr(msa, k)
    S <- X %*% t(X) / sqrt(2) + bw
    A <- exp(S - apply(S, 1, max))
    A <- A / rowSums(A)
    expect_equal(do.call(rbind, ra$attn[[k]]), A, tolerance = 1e-12)
    expect_equal(do.call(rbind, ra$out[[k]]), sweep(A %*% X, 2, g, "*"), tolerance = 1e-12)
  }
  expect_error(morie_alfomg_row_attention(msa, diag(3)), "one scalar per ordered pair")
  expect_error(morie_alfomg_row_attention(msa, bw, gate = 1), "one multiplier per channel")
})

test_that("the head updates the pair track by the projected OPM", {
  msa <- .msa()
  pair <- lapply(1:4, function(i) lapply(1:4, function(j) c(i, j)))
  wo <- list(c(1, 0, 0, 1), c(0.5, -0.5, 0.25, 0))
  h <- morie_alfomg(msa, pair, w_opm = wo, scale = 0.3)
  opm <- morie_alfomg_opm(msa)
  expect_equal(h$pair_out[[1]][[3]], c(1, 3) + c(sum(wo[[1]] * opm[[1]][[3]]), sum(wo[[2]] * opm[[1]][[3]])), tolerance = 1e-12)
  expect_true(h$pair_updated)
  ra <- morie_alfomg_row_attention(msa, morie_alfomg_bias(pair), scale = 0.3)
  expect_equal(h$attn, ra$attn, tolerance = 1e-12)
  dg <- unlist(lapply(ra$attn, function(A) vapply(1:4, function(i) A[[i]][i], 1)))
  expect_equal(h$self_attention, mean(dg), tolerance = 1e-12)
  expect_identical(h$scale, 0.3)
  n <- morie_alfomg(msa, pair)
  expect_false(n$pair_updated)
  expect_identical(n$pair_out, pair)
  expect_equal(n$scale, 1 / sqrt(2))
  expect_error(morie_alfomg(msa, pair[1:3]), "square over")
  expect_error(morie_alfomg(msa, pair, w_opm = list(1:3)), "whole outer product")
  expect_error(morie_alfomg(msa, pair, w_opm = list(1:4)), "pair width")
  expect_match(morie_alfomg_cheatsheet(), "OpenFold")
})
