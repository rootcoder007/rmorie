# Coverage for vitatt .. vivkt exports. Every expectation is recomputed in
# the test body; weight matrices are replayed from the package's
# deterministic draw stream.

vit_soft <- function(S) {
  E <- exp(S - apply(S, 1, max))
  E / rowSums(E)
}

test_that("Vitatt and Vitscn compute scaled attention", {
  q <- rbind(c(0.2, 0.5, -0.1), c(0.4, -0.3, 0.8))
  k <- rbind(c(0.1, 0.2, 0.3), c(-0.2, 0.5, 0.1), c(0.3, -0.1, 0.2))
  v <- rbind(c(1, 0), c(0, 1), c(0.5, 0.5))
  r <- Vitatt(q, k, v)
  A <- vit_soft(q %*% t(k) / sqrt(3))
  expect_equal(r$attn, A, tolerance = 1e-12)
  expect_equal(r$output, A %*% v, tolerance = 1e-12)
  expect_equal(r$estimate, mean(A %*% v), tolerance = 1e-12)
  mk <- rbind(c(1, 0, 1), c(1, 1, 1))
  m <- Vitatt(q, k, v, mask = mk)
  S <- q %*% t(k) / sqrt(3)
  S[mk == 0] <- -Inf
  expect_equal(m$attn, vit_soft(S), tolerance = 1e-12)
  expect_error(Vitatt(q, k, v, mask = rbind(c(0, 0, 0), c(1, 1, 1))), "excludes every key")
  expect_error(Vitatt(q, k[, 1:2], v), "same width")
  B <- matrix(c(0.1, -0.2, 0, 0.3, 0.2, -0.1), 2)
  s <- Vitscn(q, k, v, tau = 0.2, B = B)
  cs <- (q %*% t(k)) / outer(sqrt(rowSums(q^2)), sqrt(rowSums(k^2))) / 0.2 + B
  expect_equal(s$similarities, cs, tolerance = 1e-12)
  expect_equal(s$output, vit_soft(cs) %*% v, tolerance = 1e-12)
  expect_error(Vitscn(q, k, v, tau = 0.01), "tau must exceed 0.01")
  expect_error(Vitscn(q * 0, k, v), "zero row")
})

test_that("Vitptm, Vitcls and Vitmlp replay the embedding stream", {
  img <- matrix(seq(0.1, 1.6, by = 0.1), 4, 4)
  pe <- Vitptm(img, 2, 3, w_scale = 0.5)
  P <- rbind(c(img[1:2, 1:2][1, ], img[1:2, 1:2][2, ]), c(img[1:2, 3:4][1, ], img[1:2, 3:4][2, ]),
             c(img[3:4, 1:2][1, ], img[3:4, 1:2][2, ]), c(img[3:4, 3:4][1, ], img[3:4, 3:4][2, ]))
  expect_equal(pe$patches, P)
  E <- .vitdraw(4, 3, 0, 0.5)
  expect_equal(pe$embeddings, P %*% E, tolerance = 1e-12)
  expect_equal(pe$skip_used, 12L)
  expect_error(Vitptm(img, 3, 3), "must divide both H and W")
  two <- Vitptm(list(img, 2 * img), 4, 2)
  expect_equal(two$patch_dim, 32)
  ct <- Vitcls(pe$embeddings, 4, w_scale = 0.5, skip = 12)
  cls <- .vitdraw(1, 3, 12, 0.5)
  pos <- .vitdraw(5, 3, 15, 0.5)
  expect_equal(ct$z0, rbind(cls, pe$embeddings) + pos, tolerance = 1e-12)
  expect_equal(ct$skip_used, 12L + 3L + 15L)
  expect_error(Vitcls(pe$embeddings, 5), "does not match")
  ml <- Vitmlp(ct$z0, 6, w_scale = 0.3, skip = 7)
  W1 <- .vitdraw(3, 6, 7, 0.3)
  W2 <- .vitdraw(6, 3, 7 + 18, 0.3)
  H <- ct$z0 %*% W1
  expect_equal(ml$output, (H * stats::pnorm(H)) %*% W2, tolerance = 1e-12)
  expect_equal(Vitmlp(ct$z0)$hidden_dim, 12L)
})

test_that("Vitfwd stacks pre-norm transformer blocks", {
  img <- matrix(seq(0.1, 1.6, by = 0.1), 4, 4)
  r <- Vitfwd(img, 2, embed_dim = 4, num_heads = 2, num_layers = 1, w_scale = 0.3, mlp_ratio = 2)
  pe <- Vitptm(img, 2, 4, 0.3, 0)
  ct <- Vitcls(pe$embeddings, 4, 0.3, pe$skip_used)
  skip <- ct$skip_used
  ln <- function(A) t(apply(A, 1, function(v) (v - mean(v)) / sqrt(mean((v - mean(v))^2) + 1e-6)))
  z <- ct$z0
  zn <- ln(z)
  heads <- NULL
  for (h in 1:2) {
    Uq <- .vitdraw(4, 2, skip, 0.3)
    Uk <- .vitdraw(4, 2, skip + 8, 0.3)
    Uv <- .vitdraw(4, 2, skip + 16, 0.3)
    skip <- skip + 24
    heads <- cbind(heads, vit_soft((zn %*% Uq) %*% t(zn %*% Uk) / sqrt(2)) %*% (zn %*% Uv))
  }
  z <- z + heads %*% .vitdraw(4, 4, skip, 0.3)
  skip <- skip + 16
  z2 <- ln(z)
  W1 <- .vitdraw(4, 8, skip, 0.3)
  W2 <- .vitdraw(8, 4, skip + 32, 0.3)
  H <- z2 %*% W1
  z <- z + (H * stats::pnorm(H)) %*% W2
  expect_equal(r$zL, z, tolerance = 1e-10)
  y <- (z[1, ] - mean(z[1, ])) / sqrt(mean((z[1, ] - mean(z[1, ]))^2) + 1e-6)
  expect_equal(r$y, y, tolerance = 1e-10)
  expect_equal(r$skip_used, skip + 64)
  expect_error(Vitfwd(img, 2, 5, 2, 1), "must divide embed_dim")
  expect_equal(Vitfwd(img, 2, 4, 1, 0)$zL, Vitfwd(img, 2, 4, 1, 0)$z0)
})

test_that("Viterb decodes the most likely path", {
  A <- rbind(c(0.7, 0.3), c(0.4, 0.6))
  B <- rbind(c(0.5, 0.4, 0.1), c(0.1, 0.3, 0.6))
  obs <- c(0, 1, 2, 2, 0)
  r <- Viterb(obs, A, B, init = c(0.6, 0.4))
  paths <- as.matrix(expand.grid(rep(list(1:2), 5)))
  lp <- apply(paths, 1, function(s) {
    log(c(0.6, 0.4)[s[1]]) + log(B[s[1], obs[1] + 1]) +
      sum(log(A[cbind(s[-5], s[-1])])) + sum(log(B[cbind(s[-1], obs[-1] + 1)]))
  })
  expect_equal(r$path, unname(paths[which.max(lp), ]) - 1L)
  expect_equal(r$estimate, max(lp), tolerance = 1e-12)
  expect_equal(Viterb(1, A, B)$path, which.max(B[, 2]) - 1L)
})

test_that("Vitfsv fits the few-shot head by ridge least squares", {
  X <- rbind(c(1, 0.2, -0.1), c(0.9, 0.1, 0.2), c(-0.2, 1.1, 0.3), c(0.1, 0.8, -0.2), c(0.2, -0.1, 1.0), c(-0.1, 0.3, 0.9))
  y <- c(1, 1, 2, 2, 3, 3)
  r <- Vitfsv(X, y, mode = "fewshot", ridge = 1e-3)
  W <- vapply(1:3, function(c) as.numeric(solve(crossprod(X) + diag(1e-3, 3), crossprod(X, ifelse(y == c, 1, -1)))), numeric(3))
  expect_equal(r$head, W, tolerance = 1e-10)
  pred <- apply(X %*% W, 1, which.max)
  expect_equal(r$pred, pred)
  expect_equal(r$accuracy, mean(pred == y))
  z <- Vitfsv(X, y)
  expect_equal(z$pred, rep(1L, 6))
  expect_equal(sum(z$confusion[, 1]), 6L)
  expect_error(Vitfsv(X, y, mode = "full"), "not implemented")
  expect_error(Vitfsv(X, y + 3, n_classes = 3), "labels must lie")
})

test_that("Vitlnorm, and Vivkt's two- and four-way decompositions", {
  X <- rbind(c(1, 2, 4), c(-1, 0, 3))
  r <- Vitlnorm(X, gamma = c(1, 2, 0.5), beta = c(0, 1, -1))
  Z <- t(apply(X, 1, function(v) (v - mean(v)) / sqrt(mean((v - mean(v))^2))))
  expect_equal(r$y, sweep(sweep(Z, 2, c(1, 2, 0.5), "*"), 2, c(0, 1, -1), "+"), tolerance = 1e-12)
  expect_error(Vitlnorm(c(1, 1, 1)), "zero variance")
  a <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  M <- c(1.2, 0.4, 1.5, 1.1, 0.2, 0.6, 1.3, 0.5, 1.8, 0.7)
  Y <- c(2.1, 1.0, 2.9, 2.2, 0.3, 1.4, 1.9, 0.8, 3.0, 1.2)
  v <- Vivkt(a, M, Y)
  th <- stats::coef(stats::lm(Y ~ a + M + a:M))
  be <- stats::coef(stats::lm(M ~ a))
  expect_equal(v$pde, unname(th["a"] + th["a:M"] * be[1]), tolerance = 1e-10)
  expect_equal(v$tie, unname(th["M"] * be["a"] + th["a:M"] * be["a"]), tolerance = 1e-10)
  expect_equal(v$check, 0, tolerance = 1e-12)
})
