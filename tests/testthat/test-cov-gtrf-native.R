# Coverage tests for R/gtrf_native.R (Dwivedi and Bresson 2021, graph
# transformer): Laplacian eigenvector encodings, sign flips, neighbourhood
# attention and the residual/normalised layer, for both adjacency formats.

# 0 - 1 - 2 - 3 and 1 - 3 (dict-of-dict for the bare spelling, list of
# neighbour vectors for the morie_gtrf_* spelling)
gt_dd <- list(`0` = list(`1` = 1), `1` = list(`0` = 1, `2` = 1, `3` = 1),
  `2` = list(`1` = 1, `3` = 1), `3` = list(`2` = 1, `1` = 1))
gt_lv <- list(`0` = 1L, `1` = c(0L, 2L, 3L), `2` = c(1L, 3L), `3` = c(1L, 2L))
gt_A <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 1), c(0, 1, 0, 1), c(0, 1, 1, 0))

test_that("graph Laplacians and positional encodings", {
  d <- rowSums(gt_A)
  Ln <- diag(4) - gt_A / sqrt(outer(d, d))
  expect_equal(morie_gtrf_laplacian(unname(gt_lv), 4), Ln, tolerance = 1e-12)
  expect_equal(morie_gtrf_laplacian(unname(gt_lv), 4, normalized = FALSE), diag(d) - gt_A)
  for (pe in list(laplacian_positional_encoding(gt_dd, 4, dim = 2), morie_gtrf_lap_pe(unname(gt_lv), 4, dim = 2))) {
    ev <- sort(eigen(Ln, symmetric = TRUE)$values)
    expect_equal(sort(pe$eigenvalues), ev[2:3], tolerance = 1e-12)
    expect_equal(Ln %*% pe$encoding, sweep(pe$encoding, 2, pe$eigenvalues, "*"), tolerance = 1e-12)
    expect_equal(crossprod(pe$encoding), diag(2), tolerance = 1e-12)
  }
  expect_error(morie_gtrf_lap_pe(unname(gt_lv), 4, dim = 4), "only 3")
  expect_error(laplacian_positional_encoding(gt_dd, 4, dim = 4), "only 3")
  # an isolated vertex keeps a unit diagonal in the normalised Laplacian
  L5 <- morie_gtrf_laplacian(c(unname(gt_lv), list(integer(0))), 5)
  expect_equal(L5[5, ], c(0, 0, 0, 0, 1))
})

test_that("random sign flips multiply each column by the documented +-1 draw", {
  pe <- matrix(1:8, 4, 2) / 10
  s <- ifelse(.ghc_unif(.ghc_rng(9), 2) < 0.5, 1, -1)
  expect_equal(random_sign_flip(pe, .ghc_rng(9)), sweep(pe, 2, s, "*"))
  expect_equal(morie_gtrf_sign_flip(pe, .ghc_rng(9)), sweep(pe, 2, s, "*"))
  expect_equal(abs(morie_gtrf_sign_flip(pe, .ghc_rng(2))), pe)
})

gt_H <- rbind(c(0.2, -0.1, 0.5), c(0.7, 0.3, -0.2), c(-0.4, 0.6, 0.1), c(0.1, 0.1, 0.9))
mk <- function(r, c, s) matrix(sin(s * (1:(r * c))) / 2, r, c)
WQ <- mk(2, 3, 1.1)
WK <- mk(2, 3, 1.7)
WV <- mk(3, 3, 0.9)

gt_ref_att <- function(bias = NULL) {
  Q <- gt_H %*% t(WQ)
  K <- gt_H %*% t(WK)
  V <- gt_H %*% t(WV)
  out <- matrix(0, 4, 3)
  for (i in 1:4) {
    nb <- which(gt_A[i, ] > 0)
    sc <- as.numeric(K[nb, , drop = FALSE] %*% Q[i, ]) / sqrt(2)
    if (!is.null(bias)) sc <- sc + bias[i, nb]
    w <- exp(sc - max(sc)) / sum(exp(sc - max(sc)))
    out[i, ] <- colSums(V[nb, , drop = FALSE] * w)
  }
  out
}

test_that("sparse attention is softmax over each neighbourhood, with edge bias", {
  expect_equal(morie_gtrf_sparse_attention(gt_H, gt_dd, WQ, WK, WV)$output, gt_ref_att(), tolerance = 1e-12)
  expect_equal(morie_gtrf_attention(gt_H, gt_lv, WQ, WK, WV)$output, gt_ref_att(), tolerance = 1e-12)
  B <- matrix(0, 4, 4)
  B[2, 3] <- B[3, 2] <- 0.7
  expect_equal(morie_gtrf_sparse_attention(gt_H, gt_dd, WQ, WK, WV, edge_bias = list(`(1, 2)` = 0.7))$output,
    gt_ref_att(B), tolerance = 1e-12)
  expect_equal(morie_gtrf_attention(gt_H, gt_lv, WQ, WK, WV, edge_bias = list(`2, 1` = 0.7))$output,
    gt_ref_att(B), tolerance = 1e-12)
  bad <- gt_lv
  bad$`0` <- integer(0)
  expect_error(morie_gtrf_attention(gt_H, bad, WQ, WK, WV), "no neighbours")
})

test_that("transformer layer: residual, norm, ReLU feed-forward, residual, norm", {
  W1 <- mk(4, 3, 1.3)
  W2 <- mk(3, 4, 0.6)
  bn <- function(X) {
    mu <- colMeans(X)
    sd <- sqrt(colMeans(sweep(X, 2, mu)^2) + 1e-5)
    sweep(sweep(X, 2, mu), 2, sd, "/")
  }
  ln <- function(X) t(apply(X, 1, function(r) (r - mean(r)) / sqrt(mean((r - mean(r))^2) + 1e-5)))
  for (nm in c("batch", "layer", "none")) {
    nf <- switch(nm, batch = bn, layer = ln, none = identity)
    res <- nf(gt_H + gt_ref_att())
    ref <- nf(res + pmax(res %*% t(W1), 0) %*% t(W2))
    for (f in list(graph_transformer_layer, graph_transformer, graphtransformer)) {
      expect_equal(f(gt_H, gt_dd, WQ, WK, WV, W1, W2, norm = nm), ref, tolerance = 1e-12, info = nm)
    }
    expect_equal(morie_gtrf_layer(gt_H, gt_lv, WQ, WK, WV, W1, W2, norm = nm), ref, tolerance = 1e-12, info = nm)
  }
  expect_error(graph_transformer_layer(gt_H, gt_dd, WQ, WK, WV, W1, W2, norm = "group"), "norm must be")
  expect_error(morie_gtrf_layer(gt_H, gt_lv, WQ, WK, WV, W1, W2, norm = "group"), "norm must be")
})
