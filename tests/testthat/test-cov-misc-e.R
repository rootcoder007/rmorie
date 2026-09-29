# Coverage for cnsRos.R, colbrt.R, colE.R, comple.R, condie.R, contRC.R,
# convgs.R, convnx.R and cwcm.R: Rosenbaum bounds from the signed-rank
# moments, retrieval and recommendation scores from cosine similarities,
# ComplEx as Re(<h, r, conj(t)>), and the ConvNeXt block replayed on its
# SplitMix64 normal stream.

cosm <- function(A, B) (A / sqrt(rowSums(A^2))) %*% t(B / sqrt(rowSums(B^2)))

test_that("CnsRos bounds the signed-rank statistic", {
  d <- c(1.2, -0.4, 0.8, 0, 2.1, -0.4, 1.5)
  r <- CnsRos(d, Gamma = 2)
  dd <- d[d != 0]
  rk <- rank(abs(dd))
  W <- sum(rk[dd > 0])
  zu <- (W - 2 / 3 * sum(rk)) / sqrt(2 / 9 * sum(rk^2))
  zl <- (W - 1 / 3 * sum(rk)) / sqrt(2 / 9 * sum(rk^2))
  expect_equal(r$W, W)
  expect_equal(c(r$p_upper, r$p_lower), pnorm(c(zu, zl), lower.tail = FALSE), tolerance = 1e-12)
  g1 <- CnsRos(d)
  expect_equal(g1$p_upper, g1$p_lower)
  expect_error(CnsRos(c(0, 0)), "no non-zero")
  expect_error(CnsRos(d, Gamma = 0.5), "at least 1")
})

test_that("Colbrt sums the per-token maximum cosine", {
  Q <- rbind(c(1, 0, 1), c(0, 2, 1))
  D1 <- rbind(c(1, 1, 0), c(0, 1, 1), c(2, 0, 0))
  D2 <- rbind(c(0, 0, 1))
  r <- Colbrt(Q, list(D1, D2))
  ms <- rbind(apply(cosm(Q, D1), 1, max), apply(cosm(Q, D2), 1, max))
  expect_equal(r$max_sim, ms, tolerance = 1e-12)
  expect_equal(r$scores, rowSums(ms), tolerance = 1e-12)
  expect_equal(r$ranking, order(-rowSums(ms)) - 1L)
  expect_equal(Colbrt(Q, D1)$scores, sum(ms[1, ]), tolerance = 1e-12)
  expect_error(Colbrt(matrix(0, 0, 3), D1), "no tokens")
  expect_error(Colbrt(Q, list()), "at least one document")
  expect_error(Colbrt(Q, list(matrix(0, 0, 3))), "has no tokens")
  expect_error(Colbrt(Q, list(D1[, 1:2])), "dimensions disagree")
})

test_that("ColE and ContRC recommend by popularity and content", {
  R <- rbind(c(5, 0, 3, 0, 1), c(4, 2, 0, 0, 0), c(0, 3, 4, 1, 0), c(0, 0, 0, 0, 0))
  Fe <- rbind(c(1, 0), c(0.5, 0.5), c(0, 1), c(1, 1), c(0.2, 0.8))
  U <- rbind(c(1, 0), c(0.8, 0.2), c(0, 1), c(1, 0.1))
  p <- ColE(0, "popular", R)
  expect_equal(p$scores, colSums(R != 0))
  expect_equal(p$recommended, c(1L, 3L))
  expect_equal(p$is_cold, 0L)
  c0 <- ColE(0, "Content", R, item_features = Fe, topn = 1)
  prof <- colSums(R[1, ] * Fe) / sum(R[1, ])
  sc <- as.numeric(cosm(Fe, t(prof)))
  expect_equal(c0$scores, sc, tolerance = 1e-12)
  expect_equal(c0$recommended, setdiff(order(-sc), c(1, 3, 5))[1] - 1L)
  cold <- ColE(3, "content", R, item_features = Fe)
  expect_equal(cold$scores, as.numeric(cosm(Fe, t(colMeans(Fe)))), tolerance = 1e-12)
  expect_equal(cold$is_cold, 1L)
  md <- ColE(3, "metadata", R, user_features = U)
  sim <- as.numeric(cosm(U, U[4, , drop = FALSE]))[1:3]
  expect_equal(md$scores, as.numeric(sim %*% R[1:3, ]) / sum(sim), tolerance = 1e-12)
  expect_error(ColE(0), "R is required")
  expect_error(ColE(0, R = matrix(0, 0, 2)), "no rows")
  expect_error(ColE(9, R = R), "out of range")
  expect_error(ColE(0, "x", R), "popular, content or metadata")
  expect_error(ColE(0, "content", R), "needs item_features")
  expect_error(ColE(0, "content", R, item_features = Fe[1:2, ]), "one row per item")
  expect_error(ColE(0, "metadata", R), "needs user_features")
  expect_error(ColE(0, "metadata", R, user_features = U[1:2, ]), "one row per user")

  ct <- ContRC(Fe, NULL, ratings = R[1, ], topn = 2)
  expect_equal(ct$scores, sc, tolerance = 1e-12)
  expect_equal(ct$recommended, setdiff(order(-sc), c(1, 3, 5)) - 1L)
  pf <- ContRC(Fe, c(1, 2))
  expect_equal(pf$scores, as.numeric(cosm(Fe, t(c(1, 2)))), tolerance = 1e-12)
  expect_equal(pf$ranking, order(-pf$scores) - 1L)
  expect_error(ContRC(matrix(0, 0, 2), 1), "no rows")
  expect_error(ContRC(Fe, NULL, ratings = 1:3), "one entry per item")
  expect_error(ContRC(Fe, NULL, ratings = rep(0, 5)), "rated nothing")
  expect_error(ContRC(Fe, 1:3), "length-f profile")
  expect_error(ContRC(Fe, c(0, 0)), "zero norm")
})

test_that("Comple is the real part of the complex trilinear product", {
  tr <- rbind(c(0, 0, 1), c(1, 1, 0), c(2, 0, 2))
  re_e <- rbind(c(1, 0.5), c(-1, 2), c(0.3, 0.3))
  im_e <- rbind(c(0, 1), c(0.5, -0.5), c(1, 0))
  re_r <- rbind(c(2, 1), c(0.5, -1))
  im_r <- rbind(c(-1, 0), c(1, 1))
  r <- Comple(tr, 2, re_e, im_e, re_r, im_r)
  E <- matrix(complex(real = re_e, imaginary = im_e), 3)
  Rr <- matrix(complex(real = re_r, imaginary = im_r), 2)
  sc <- vapply(1:3, function(i) Re(sum(E[tr[i, 1] + 1, ] * Rr[tr[i, 2] + 1, ] * Conj(E[tr[i, 3] + 1, ]))), 0)
  expect_equal(r$scores, sc, tolerance = 1e-12)
  s <- 5
  z <- numeric(20)
  for (i in 1:20) {
    s <- (48271 * s) %% 2147483647
    z[i] <- qnorm(s / 2147483647)
  }
  mk <- function(v, rows) matrix(v, rows, 2, byrow = TRUE)
  rd <- Comple(tr, 2, seed = 5)
  ref <- Comple(tr, 2, mk(z[1:6], 3), mk(z[7:12], 3), mk(z[13:16], 2), mk(z[17:20], 2))
  expect_equal(rd$scores, ref$scores, tolerance = 1e-12)
})

test_that("condie is the Preacher-Rucker-Hayes conditional indirect effect", {
  w <- c(-1, 0, 2)
  r <- condie(0.5, 0.2, 0.4, w, sa1 = 0.1, sa3 = 0.05, sa1a3 = 0.001, sb = 0.08)
  sl <- 0.5 + 0.2 * w
  vs <- 0.01 + 2 * 0.001 * w + 0.0025 * w^2
  se <- sqrt(sl^2 * 0.0064 + 0.16 * vs)
  expect_equal(r$effect, 0.4 * sl, tolerance = 1e-12)
  expect_equal(r$se, se, tolerance = 1e-12)
  expect_equal(r$p_value, 2 * pnorm(-abs(0.4 * sl / se)), tolerance = 1e-12)
  expect_equal(r$estimate, mean(0.4 * sl), tolerance = 1e-12)
  one <- condie(0.5, 0, 0.4, 3)
  expect_equal(c(one$estimate, one$simple_slope), c(0.2, 0.5), tolerance = 1e-12)
  expect_true(is.na(one$se) && is.na(one$p_value))
  expect_same_function(morie_conditional_indirect_effect, condie)
})

test_that("Convgs, Cwcenter and Convnx", {
  lam <- c(0.8, 0.8, 0.7)
  v <- Convgs(lam)
  th <- 1 - lam^2
  expect_equal(v$ave, sum(lam^2) / (sum(lam^2) + sum(th)), tolerance = 1e-12)
  expect_equal(v$cr, sum(lam)^2 / (sum(lam)^2 + sum(th)), tolerance = 1e-12)
  expect_equal(v$adequate, 1L)
  expect_equal(Convgs(lam, c(1, 1, 1))$adequate, 0L)
  expect_error(Convgs(numeric(0)), "no loadings")
  expect_error(Convgs(lam, 1:2), "same length")
  expect_error(Convgs(lam, c(-1, 0, 0)), "non-negative")
  expect_error(Convgs(0, 0), "total variance is zero")

  y <- c(3, 5, 2, 8, 6, 1)
  g <- c("b", "a", "b", "a", "c", "c")
  cw <- Cwcenter(y, g)
  mm <- ave(y, g)
  expect_equal(cw$centered, y - mm, tolerance = 1e-12)
  expect_equal(cw$cluster_ids, c("b", "a", "c"))
  expect_equal(cw$icc_between, sum((mm - mean(y))^2) / sum((y - mean(y))^2), tolerance = 1e-12)
  expect_true(is.nan(Cwcenter(c(2, 2), c(1, 2))$icc_between))

  X <- rbind(c(1, 2, 0), c(0.5, -1, 3), c(2, 2, 1), c(0, 1, -1))
  z <- .ghc_norm(.ghc_rng(3), 9L + 4L, 0, 1)
  dw <- matrix(z[1:9], 3, 3, byrow = TRUE) / 3
  w1 <- 0.02 * z[10:11]
  w2 <- 0.02 * z[12:13]
  Xp <- X[c(1, 1:4, 4), c(1, 1:3, 3)]
  cv <- matrix(0, 4, 3)
  for (i in 1:4) for (j in 1:3) cv[i, j] <- sum(Xp[i:(i + 2), j:(j + 2)] * dw)
  h <- (cv - mean(cv)) / sqrt(mean((cv - mean(cv))^2) + 1e-6)
  acc <- (h * w1[1]) * pnorm(h * w1[1]) * w2[1] + (h * w1[2]) * pnorm(h * w1[2]) * w2[2]
  r <- Convnx(X, kernel = 3, expand = 2, layer_scale = 0.5, seed = 3)
  expect_equal(r$out, X + 0.5 * acc, tolerance = 1e-12)
  # the norm is taken of out - x, so the subtraction is recomputed as is
  expect_equal(r$residual_norm, sqrt(sum(((X + 0.5 * acc) - X)^2)), tolerance = 1e-12)
  expect_equal(Convnx(X)$out, X)
  expect_error(Convnx(matrix(0, 0, 2)), "no rows")
  expect_error(Convnx(X, filters = 0), "at least 1")
  expect_error(Convnx(X, kernel = 4), "odd")
  expect_error(Convnx(X, expand = 0), "at least 1")
})
