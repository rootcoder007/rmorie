# Coverage for Crossformer (Zhang & Yan 2023): scaled dot-product
# attention by matrix algebra, the dimension-segment-wise embedding
# E x_seg + pos, the cross-time and router cross-dimension stages with
# residuals, segment merging by averaging and the complexity counts.

.att <- function(Q, K, V) {
  S <- Q %*% t(K) / sqrt(ncol(Q))
  A <- exp(S - apply(S, 1, max))
  A <- A / rowSums(A)
  list(out = A %*% V, weights = A)
}
.Z <- function() {
  set.seed(2)
  lapply(1:4, function(i) lapply(1:3, function(d) stats::rnorm(2)))
}

test_that("attention is softmax(Q K' / sqrt(d)) V", {
  set.seed(1)
  Q <- matrix(stats::rnorm(6), 3)
  K <- matrix(stats::rnorm(8), 4)
  V <- matrix(stats::rnorm(12), 4)
  a <- morie_crsfmr_attention(Q, K, V)
  r <- .att(Q, K, V)
  expect_equal(a$out, r$out, tolerance = 1e-12)
  expect_equal(a$weights, r$weights, tolerance = 1e-12)
  expect_identical(morie_crsfmr, morie_crsfmr_attention)
  expect_error(morie_crsfmr_attention(Q, K, V[1:3, ]), "same length \\(4, 3\\)")
  expect_error(morie_crsfmr_attention(Q, cbind(K, 1), V), "share a dimension")
})

test_that("DSW embedding maps each dimension's segment through E", {
  X <- matrix(1:12, 6)
  e <- morie_crsfmr_dsw_embed(X, 3)
  expect_equal(e$H[[2]][[1]], 4:6)
  expect_equal(e$H[[1]][[2]], 7:9)
  expect_identical(e$shape, c(2L, 2L, 3L))
  E <- matrix(c(1, 0, -1, 0.5, 0.5, 0.5), 2, byrow = TRUE)
  pos <- list(list(c(10, 0), c(0, 10)), list(c(1, 1), c(2, 2)))
  ee <- morie_crsfmr_dsw_embed(X, 3, E = E, pos = pos)
  expect_equal(ee$H[[2]][[2]], as.numeric(E %*% 10:12) + c(2, 2), tolerance = 1e-12)
  expect_equal(ee$H[[1]][[1]], as.numeric(E %*% 1:3) + c(10, 0), tolerance = 1e-12)
  expect_identical(ee$d_model, 2L)
  expect_error(morie_crsfmr_dsw_embed(X, 4), "not divisible")
  expect_error(morie_crsfmr_dsw_embed(X, 0), "at least 1")
  expect_error(morie_crsfmr_dsw_embed(X, 3, E = diag(2)), "3 columns")
  expect_error(morie_crsfmr_dsw_embed(matrix(0, 0, 2), 1), "empty")
})

test_that("cross-time stage is per-dimension self-attention plus residual", {
  Z <- .Z()
  out <- morie_crsfmr_cross_time_stage(Z)
  for (d in 1:3) {
    S <- t(sapply(Z, `[[`, d))
    ref <- S + .att(S, S, S)$out
    for (i in 1:4) expect_equal(out[[i]][[d]], ref[i, ], tolerance = 1e-12)
  }
  expect_error(morie_crsfmr_cross_time_stage(list()), "empty")
})

test_that("cross-dimension stage routes through c router vectors", {
  Z <- .Z()
  out <- morie_crsfmr_cross_dimension_stage(Z, n_router = 2)
  for (i in 1:4) {
    M <- do.call(rbind, Z[[i]])
    g <- .att(M[1:2, ], M, M)$out
    ref <- M + .att(M, g, g)$out
    for (d in 1:3) expect_equal(out[[i]][[d]], ref[d, ], tolerance = 1e-12)
  }
  R <- list(c(1, 0), c(0, 1))
  o2 <- morie_crsfmr_cross_dimension_stage(Z, router = R, n_router = 2)
  M <- do.call(rbind, Z[[3]])
  g <- .att(rbind(R[[1]], R[[2]]), M, M)$out
  expect_equal(o2[[3]][[2]], M[2, ] + .att(M, g, g)$out[2, ], tolerance = 1e-12)
  expect_error(morie_crsfmr_cross_dimension_stage(Z, n_router = 0), "at least 1")
  expect_error(morie_crsfmr_cross_dimension_stage(list()), "empty")
})

test_that("two-stage attention composes the stages; merge averages; complexity counts", {
  Z <- .Z()
  t2 <- morie_crsfmr_two_stage_attention(Z)
  expect_equal(t2$output, morie_crsfmr_cross_dimension_stage(morie_crsfmr_cross_time_stage(Z)), tolerance = 1e-12)
  expect_identical(t2$n_router, 3L)
  expect_identical(t2$complexity, morie_crsfmr_complexity(4, 3, 3))
  m <- morie_crsfmr_segment_merge(Z, 2)
  expect_length(m, 2L)
  expect_equal(m[[2]][[3]], (Z[[3]][[3]] + Z[[4]][[3]]) / 2, tolerance = 1e-12)
  expect_error(morie_crsfmr_segment_merge(Z, 3), "do not divide")
  expect_error(morie_crsfmr_segment_merge(Z, 1), "at least 2")
  cx <- morie_crsfmr_complexity(10, 7, 2)
  expect_identical(c(cx$cross_time, cx$cross_dimension_router, cx$cross_dimension_full, cx$flattened_2d),
                   c(700L, 140L, 490L, 4900L))
  expect_equal(cx$router_saving, 3.5)
})
