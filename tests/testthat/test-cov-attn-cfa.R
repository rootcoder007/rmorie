# Coverage for adseqs.R, ali.R, alibi.R, alsR.R, arckn.R, asorxx.R,
# atalib.R, atfla.R, atq8.R, augmn.R, barte_native.R, bartkw.R, bcelO.R,
# ccelO.R, cnffvw.R, cfaftr.R and cfafm2.R: attention variants against
# plain softmax attention, EM and ALS updates replayed in matrix form,
# the AMH copula density against a finite-difference mixed partial, and
# factor-analysis fits checked through the ML stationarity equations.

sm_rows <- function(Z) {
  Z <- Z - apply(Z, 1, max)
  exp(Z) / rowSums(exp(Z))
}

Q <- rbind(c(1, 0.5), c(-0.3, 0.8), c(0.2, 0.2))
K <- rbind(c(0.4, -1), c(1, 1), c(0, 0.5), c(-0.7, 0.1))
V <- rbind(c(1, 2), c(0, -1), c(3, 0.5), c(-2, 1))
bias <- function(nq, nk, m, causal = FALSE) {
  B <- -m * abs(outer(seq_len(nq), seq_len(nk), "-"))
  if (causal) B[outer(seq_len(nq), seq_len(nk), "<")] <- -Inf
  B
}

test_that("Alibi and Atalib add the linear distance penalty", {
  S <- Q %*% t(K)
  a <- Alibi(S, slopes = 0.5)
  expect_equal(a$biased, S + bias(3, 4, 0.5), tolerance = 1e-12)
  expect_equal(Alibi(S)$bias, bias(3, 4, 2^-8), tolerance = 1e-12)
  expect_equal(Alibi(S, 0.5, causal = TRUE)$bias, bias(3, 4, 0.5, TRUE))
  expect_error(Alibi(matrix(0, 0, 2)), "empty")
  r <- Atalib(Q = Q, K = K, V = V, slopes = c(0.5, 0.25), causal = TRUE)
  W1 <- sm_rows(Q %*% t(K) / sqrt(2) + bias(3, 4, 0.5, TRUE))
  W2 <- sm_rows(Q %*% t(K) / sqrt(2) + bias(3, 4, 0.25, TRUE))
  expect_equal(r$weights, W1, tolerance = 1e-12)
  expect_equal(r$output, list(W1 %*% V, W2 %*% V), tolerance = 1e-12)
  one <- Atalib(Q = Q, K = K, V = V)
  expect_equal(one$output, sm_rows(Q %*% t(K) / sqrt(2) + bias(3, 4, 2^-8)) %*% V,
               tolerance = 1e-12)
  expect_error(Atalib(Q = Q, K = K), "all required")
  expect_error(Atalib(Q = Q[0, ], K = K, V = V), "non-empty")
  expect_error(Atalib(Q = Q, K = K[, 1], V = V), "key dimension")
  expect_error(Atalib(Q = Q, K = K, V = V[1:3, ]), "one row per key")
  expect_error(Atalib(Q = Q, K = K, V = V, slopes = numeric(0)), "slopes is empty")
})

test_that("Atfla tiles exact softmax attention", {
  sco <- Q %*% t(K) / sqrt(2)
  r <- Atfla(Q = Q, K = K, V = V, block_size = 3)
  expect_equal(r$output, sm_rows(sco) %*% V, tolerance = 1e-12)
  expect_equal(r$m, apply(sco, 1, max), tolerance = 1e-12)
  expect_equal(r$l, rowSums(exp(sco - apply(sco, 1, max))), tolerance = 1e-12)
  expect_equal(r$n_blocks, 2L)
  cs <- sco
  cs[outer(1:3, 1:4, "<")] <- -Inf
  c1 <- Atfla(Q = Q, K = K, V = V, block_size = 1, causal = TRUE)
  expect_equal(c1$output, sm_rows(cs) %*% V, tolerance = 1e-12)
  expect_equal(c1$n_blocks, 4L)
  expect_error(Atfla(Q = Q), "all required")
  expect_error(Atfla(Q = Q, K = K[0, ], V = V), "non-empty")
  expect_error(Atfla(Q = Q, K = K[, 1], V = V), "key dimension")
  expect_error(Atfla(Q = Q, K = K, V = V[1:2, ]), "one row per key")
  expect_error(Atfla(Q = Q, K = K, V = V, block_size = 0), "at least one")
})

test_that("Atq8 quantises rows to int8 before the float softmax", {
  rnd <- function(x) sign(x) * floor(abs(x) + 0.5)
  sc <- function(M) apply(abs(M), 1, max) / 127
  sq <- sc(Q)
  sk <- sc(K)
  sv <- sc(V)
  Qi <- rnd(Q / sq)
  Ki <- rnd(K / sk)
  Vi <- rnd(V / sv)
  S <- (Qi %*% t(Ki)) * outer(sq, sk) / sqrt(2)
  W <- sm_rows(S)
  O <- W %*% (Vi * sv)
  r <- Atq8(Q = Q, K = K, V = V)
  expect_equal(r$scores, S, tolerance = 1e-12)
  expect_equal(r$output, O, tolerance = 1e-12)
  expect_equal(r$max_abs_error_vs_float, max(abs(O - sm_rows(Q %*% t(K) / sqrt(2)) %*% V)),
               tolerance = 1e-12)
  Z <- rbind(c(0, 0), c(1, 1), c(2, -2))
  expect_equal(Atq8(Q = Z, K = K, V = V)$s_q, c(1, 1 / 127, 2 / 127), tolerance = 1e-15)
  own <- Atq8(Q = Q, K = K, V = V, scales = list(rep(0.01, 3), rep(0.01, 4), rep(0.02, 4)))
  Qc <- pmin(pmax(rnd(Q / 0.01), -127), 127)
  Kc <- pmin(pmax(rnd(K / 0.01), -127), 127)
  expect_equal(own$scores, Qc %*% t(Kc) * 1e-4 / sqrt(2), tolerance = 1e-12)
  expect_error(Atq8(Q = Q), "all required")
  expect_error(Atq8(Q = Q[0, ], K = K, V = V), "non-empty")
  expect_error(Atq8(Q = Q, K = K[, 1], V = V), "key dimension")
  expect_error(Atq8(Q = Q, K = K, V = V[1:2, ]), "one row per key")
  expect_error(Atq8(Q = Q, K = K, V = V, scales = list(1, 1)), "three vectors")
  expect_error(Atq8(Q = Q, K = K, V = V, scales = list(1, 1, 1)), "wrong length")
  expect_error(Atq8(Q = Q, K = K, V = V, scales = list(rep(-1, 3), rep(1, 4), rep(1, 4))),
               "positive")
})

test_that("ali is the Ali-Mikhail-Haq copula", {
  u <- c(0.3, 0.7, 0.5)
  v <- c(0.6, 0.2, 0.9, 0.1)
  r <- ali(u, v, theta = 0.5)
  uu <- u
  vv <- v[1:3]
  C <- function(a, b) a * b / (1 - 0.5 * (1 - a) * (1 - b))
  h <- 1e-4
  num <- (C(uu + h, vv + h) - C(uu + h, vv - h) - C(uu - h, vv + h) + C(uu - h, vv - h)) / (4 * h^2)
  expect_equal(r$cdf, C(uu, vv), tolerance = 1e-12)
  # central second differences with h = 1e-4 are accurate to about 1e-7
  expect_equal(r$density, num, tolerance = 1e-6)
  expect_equal(r$loglik, sum(log(r$density)), tolerance = 1e-12)
  expect_equal(r$n, 3L)
  # Kendall's tau = 1 - 4 * int int dC/du dC/dv
  dCu <- function(a, b) (C(a + h, b) - C(a - h, b)) / (2 * h)
  dCv <- function(a, b) (C(a, b + h) - C(a, b - h)) / (2 * h)
  g <- (seq_len(400) - 0.5) / 400
  G <- outer(g, g, function(a, b) dCu(a, b) * dCv(a, b))
  expect_equal(r$tau, 1 - 4 * mean(G), tolerance = 1e-5)
  expect_equal(ali(0.5, 0.4)$tau, 0)
  expect_equal(ali(0.5, 0.4)$density, 1)
  expect_equal(ali(0.5, 0.4, theta = 1)$tau, 1 / 3)
  expect_same_function(morie_ali_mikhail_haq_copula, ali)
})

test_that("Admixq replays the ADMIXTURE EM updates", {
  G <- rbind(c(0, 1, 2, 1, 0), c(2, 2, 1, 0, 1), c(1, 0, 0, 2, 2), c(1, 1, 2, 2, 0))
  r <- Admixq(G, K = 2, steps = 3)
  Qm <- outer(0:3, 0:1, function(i, k) 1 + ((i + k) %% 2))
  Qm <- Qm / rowSums(Qm)
  P <- outer(0:1, 0:4, function(k, j) (2 + ((k * 5 + j) %% 7)) / 10)
  ll <- function(Qm, P) sum(G * log(Qm %*% P) + (2 - G) * log(Qm %*% (1 - P)))
  ll0 <- ll(Qm, P)
  for (s in 1:3) {
    RA <- G / (Qm %*% P)
    RB <- (2 - G) / (Qm %*% (1 - P))
    Qn <- Qm * (RA %*% t(P) + RB %*% t(1 - P)) / 10
    num <- P * (t(Qm) %*% RA)
    P <- num / (num + (1 - P) * (t(Qm) %*% RB))
    Qm <- Qn
  }
  expect_equal(r$Q, Qm, tolerance = 1e-12)
  expect_equal(r$P, P, tolerance = 1e-12)
  expect_equal(c(r$loglik0, r$loglik), c(ll0, ll(Qm, P)), tolerance = 1e-12)
  expect_gte(r$loglik, r$loglik0)
  own <- Admixq(G, K = 2, steps = 0, Q0 = matrix(0.5, 4, 2), P0 = matrix(0.4, 2, 5))
  expect_equal(own$loglik, ll(matrix(0.5, 4, 2), matrix(0.4, 2, 5)), tolerance = 1e-12)
  expect_error(Admixq(matrix(0, 0, 2)), "non-empty")
  expect_error(Admixq(G, K = 0), "at least 1")
  expect_error(Admixq(G + 1), "lie in \\[0, 2\\]")
})

test_that("Alsmf alternates weighted ridge solves", {
  R <- rbind(c(0, 2, 1), c(3, 0, 0), c(1, 1, 0), c(0, 0, 4))
  r <- Alsmf(R, f = 2, lam = 0.5, alpha = 2, steps = 2)
  Cf <- 1 + 2 * R
  P <- (R > 0) + 0
  X <- outer(0:3, 0:1, function(u, k) ((u + k) %% 5 + 1) / 10)
  Y <- outer(0:2, 0:1, function(i, k) ((i + 2 * k) %% 7 + 1) / 10)
  for (s in 1:2) {
    for (u in 1:4) {
      X[u, ] <- solve(t(Y) %*% diag(Cf[u, ]) %*% Y + diag(0.5, 2), t(Y) %*% (Cf[u, ] * P[u, ]))
    }
    for (i in 1:3) {
      Y[i, ] <- solve(t(X) %*% diag(Cf[, i]) %*% X + diag(0.5, 2), t(X) %*% (Cf[, i] * P[, i]))
    }
  }
  expect_equal(r$X, X, tolerance = 1e-12)
  expect_equal(r$Y, Y, tolerance = 1e-12)
  expect_equal(r$loss, sum(Cf * (P - X %*% t(Y))^2) + 0.5 * (sum(X^2) + sum(Y^2)),
               tolerance = 1e-12)
  s0 <- Alsmf(R, steps = 0, X0 = matrix(0.1, 4, 2), Y0 = matrix(0.2, 3, 2))
  expect_equal(s0$fitted, matrix(0.04, 4, 3), tolerance = 1e-15)
  expect_error(Alsmf(R, f = 0), "at least 1")
  expect_error(Alsmf(-R), "non-negative")
})

test_that("Arckern is the Cho-Saul arc-cosine kernel", {
  X <- rbind(c(1, 0), c(1, 1), c(-1, 0.5))
  Z <- rbind(c(0.5, 2), c(-1, -1))
  J <- function(t) sin(t) + (pi - t) * cos(t)
  k1 <- function(a, b) {
    na <- sqrt(sum(a^2))
    nb <- sqrt(sum(b^2))
    na * nb * J(acos(min(1, max(-1, sum(a * b) / (na * nb))))) / pi
  }
  K1 <- outer(1:3, 1:2, Vectorize(function(i, j) k1(X[i, ], Z[j, ])))
  expect_equal(Arckern(X, Z)$K, K1, tolerance = 1e-12)
  # depth 2: the same map applied to the depth-1 kernel, with k1(x, x) = |x|^2
  K2 <- outer(1:3, 1:2, Vectorize(function(i, j) {
    kx <- sum(X[i, ]^2)
    kz <- sum(Z[j, ]^2)
    sqrt(kx * kz) * J(acos(min(1, K1[i, j] / sqrt(kx * kz)))) / pi
  }))
  a2 <- Arckern(X, Z, depth = 2)
  expect_equal(a2$K, K2, tolerance = 1e-12)
  expect_equal(c(a2$n, a2$m, a2$depth), c(3, 2, 2))
  expect_equal(diag(Arckern(X)$K), rowSums(X^2), tolerance = 1e-12)
})

test_that("Asorxx is Newman's nominal assortativity", {
  A <- rbind(c(0, 1, 1, 0, 0), c(1, 0, 1, 0, 1), c(1, 1, 0, 1, 0),
             c(0, 0, 1, 0, 1), c(0, 1, 0, 1, 0))
  att <- c("x", "x", "y", "y", "z")
  M <- outer(att, sort(unique(att)), "==") + 0
  E <- t(M) %*% A %*% M / sum(A)
  ab <- sum(rowSums(E) * colSums(E))
  rr <- (sum(diag(E)) - ab) / (1 - ab)
  r <- Asorxx(A, att)
  expect_equal(r$e, unname(E), tolerance = 1e-12)
  expect_equal(r$r, rr, tolerance = 1e-12)
  expect_equal(r$r_min, -ab / (1 - ab), tolerance = 1e-12)
  expect_equal(r$r_normalised, if (rr < 0) rr / abs(ab / (1 - ab)) else NA_real_)
  bip <- Asorxx(rbind(c(0, 1), c(1, 0)), c("a", "b"))
  expect_equal(c(bip$r, bip$r_normalised), c(-1, -1))
  expect_error(Asorxx(matrix(0, 0, 0), character(0)), "empty")
  expect_error(Asorxx(A[, 1:4], att), "not square")
  expect_error(Asorxx(A, att[-1]), "one entry per vertex")
  expect_error(Asorxx(-A, att), "negative edge weight")
  expect_error(Asorxx(diag(5), att), "no edges")
  expect_error(Asorxx(A, rep("q", 5)), "within one type")
})

test_that("Augmn reaches the Albert-Chib fixed point", {
  y <- c(1, 0, 1, 1, 0, 0, 1, 0)
  X <- cbind(1, c(0.5, -0.3, 1.2, 0.1, -0.8, 0.4, -0.2, 0.3))
  Z <- cbind(rep(c(1, 0), 4), rep(c(0, 1), 4))
  r <- Augmn(y, X, Z, sigma_g2 = 0.5, max_iter = 5000)
  W <- cbind(X, Z)
  eta <- as.numeric(X %*% r$beta_samples + Z %*% r$b)
  tm <- ifelse(y == 1, eta + dnorm(eta) / pnorm(eta), eta - dnorm(eta) / pnorm(-eta))
  # converged to |lat_k - lat_{k-1}| < 1e-13, so lat matches the final eta closely
  expect_equal(r$z_samples, tm, tolerance = 1e-9)
  sol <- solve(crossprod(W) + diag(c(0, 0, 2, 2)), crossprod(W, r$z_samples))
  expect_equal(c(r$beta_samples, r$b), as.numeric(sol), tolerance = 1e-9)
  expect_equal(r$estimate, mean(r$z_samples), tolerance = 1e-12)
  one <- Augmn(y, X, max_iter = 1)
  eta0 <- rep(0, 8)
  lat0 <- ifelse(y == 1, dnorm(0) / 0.5, -dnorm(0) / 0.5)
  W0 <- cbind(X, diag(8))
  expect_equal(c(one$beta_samples, one$b),
               as.numeric(solve(crossprod(W0) + diag(c(0, 0, rep(1, 8))), crossprod(W0, lat0))),
               tolerance = 1e-9)
  expect_equal(one$z_samples, lat0 + eta0, tolerance = 1e-12)
  expect_error(Augmn(numeric(0), X), "empty")
  expect_error(Augmn(y + 1, X), "0 or 1")
  expect_error(Augmn(y, X[-1, ]), "X has a different number")
  expect_error(Augmn(y, X, Z[-1, ]), "Z has a different number")
  expect_error(Augmn(y, X, Z, sigma_g2 = 0), "positive")
})

test_that("Barte, Bartkw, Bcelo, Ccelo and Cnffvw", {
  src <- c("the", "cat", "sat", "on", "the", "mat")
  expect_equal(Barte(src, src, mask_ratio = 0.4, seed = 3),
               morie_geron_bart(src, src, mask_ratio = 0.4, seed = 3))
  b <- Bartkw(4)
  expect_equal(b$w, 1 - (0:4) / 5, tolerance = 1e-12)
  expect_equal(b$estimate, 3, tolerance = 1e-12)
  expect_equal(Bartkw(c(0, 2, 5, 9), M = 4)$w, c(1, 0.6, 0, 0), tolerance = 1e-12)
  Y <- rbind(c(1, 0), c(0, 1), c(1, 1))
  P <- rbind(c(0.9, 0.2), c(0.3, 0.6), c(0.5, 0.8))
  bc <- Bcelo(Y, P)
  expect_equal(bc$loss, -sum(dbinom(Y, 1, P, log = TRUE)), tolerance = 1e-12)
  expect_equal(bc$mean_loss, bc$loss / 6, tolerance = 1e-12)
  expect_error(Bcelo(Y, P[1:2, ]), "same shape")
  expect_error(Bcelo(Y, P * 0), "strictly in")
  Yc <- rbind(c(1, 0, 0), c(0, 0, 1))
  Pc <- rbind(c(0.7, 0.2, 0.1), c(0.1, 0.3, 0.6))
  cc <- Ccelo(Yc, Pc)
  expect_equal(cc$loss, -log(0.7) - log(0.6), tolerance = 1e-12)
  expect_equal(cc$mean_loss, cc$loss / 2, tolerance = 1e-12)
  expect_error(Ccelo(Yc, Pc[, 1:2]), "same shape")
  expect_error(Ccelo(Yc, Pc - 0.5), "strictly positive")
  d <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0)
  x <- c(0.3, -0.2, 0.5, 0.1, -0.4, 0.2, 0.6, -0.1, 0.0, 0.4)
  y <- 1 + 0.8 * d + 0.5 * x + c(0.1, -0.2, 0.15, -0.05, 0.2, -0.1, 0.05, 0.1, -0.15, 0.02)
  f <- summary(lm(y ~ d + x))$coefficients
  cv <- Cnffvw(y, d, x, R2_Y = 0.1, R2_D = 0.2)
  expect_equal(cv$tau, f["d", 1], tolerance = 1e-9)
  expect_equal(cv$estimate, f["d", 1] - sign(f["d", 1]) * f["d", 2] * sqrt(7) * sqrt(0.1 * 0.2 / 0.8),
               tolerance = 1e-9)
})

test_that("Cfaftr and Cfafm2 satisfy the ML stationarity equations", {
  stat <- function(r, S, mask) {
    L <- as.matrix(r$loadings)
    Sig <- L %*% t(L) + diag(r$uniquenesses)
    Si <- solve(Sig)
    G <- Si %*% (Sig - S) %*% Si
    c(max(abs((G %*% L)[mask != 0])), max(abs(diag(G))))
  }
  l <- c(0.9, 0.7, 0.6, 0.8)
  S <- l %*% t(l) + diag(c(0.4, 0.5, 0.6, 0.3))
  S[1, 2] <- S[2, 1] <- S[1, 2] + 0.05
  S[3, 4] <- S[4, 3] <- S[3, 4] - 0.04
  r <- Cfaftr(S)
  expect_lt(max(stat(r, S, rep(1, 4))), 1e-8)
  Sig <- r$loadings %*% t(r$loadings) + diag(r$uniquenesses)
  expect_equal(r$fml, log(det(Sig)) - log(det(S)) + sum(diag(S %*% solve(Sig))) - 4,
               tolerance = 1e-10)
  expect_equal(r$estimate, sum(r$loadings^2) / sum(diag(S)), tolerance = 1e-12)
  fa <- factanal(covmat = S, factors = 1)
  # factanal stops its L-BFGS-B search at about 1e-5
  expect_equal(abs(r$loadings) / sqrt(diag(S)), abs(as.numeric(fa$loadings)), tolerance = 1e-4)
  S3 <- S[1:3, 1:3]
  S3[1, 2] <- S3[2, 1] <- 0.63
  e3 <- Cfaftr(S3)
  expect_equal(abs(e3$loadings[1]), sqrt(S3[1, 2] * S3[1, 3] / S3[2, 3]), tolerance = 1e-8)
  expect_equal(e3$spearman, e3$loadings[1], tolerance = 1e-8)
  expect_lt(e3$max_resid, 1e-8)
  fs <- Cfaftr(S, factor_structure = c(1, 1, 1, 0))
  expect_equal(c(fs$loadings[4], fs$uniquenesses[4]), c(0, S[4, 4]), tolerance = 1e-10)
  expect_lt(max(stat(fs, S, c(1, 1, 1, 0))), 1e-8)
  Xd <- rbind(c(1, 2, 0.5, 1), c(0.2, 1.1, 0.4, 0.3), c(1.4, 2.2, 1.9, 1.2),
              c(0.5, 0.3, 0.2, 0.8), c(2, 2.5, 1.1, 1.9), c(0.9, 1.7, 0.6, 0.4))
  expect_equal(Cfaftr(Xd)$loadings, Cfaftr(cov(Xd))$loadings, tolerance = 1e-10)
  expect_error(Cfaftr(S[1:2, 1:2]), "at least three items")
  expect_error(Cfaftr(S, factor_structure = c(1, 1)), "one entry per item")
  expect_error(Cfaftr(S, factor_structure = rep(0, 4)), "frees no loading")
  expect_error(Cfaftr(matrix(0, 0, 3)), "no rows")
  expect_error(Cfaftr(matrix(1:3, 1)), "at least two observations")

  L2 <- rbind(c(0.8, 0), c(0.7, 0), c(0.5, 0.4), c(0, 0.6), c(0, 0.9), c(0, 0.7))
  pat <- (L2 != 0) + 0
  S6 <- L2 %*% t(L2) + diag(c(0.3, 0.5, 0.4, 0.6, 0.2, 0.5))
  S6[1, 6] <- S6[6, 1] <- 0.06
  S6[2, 4] <- S6[4, 2] <- -0.05
  m2 <- Cfafm2(S6, pat)
  expect_true(all(m2$loadings[pat == 0] == 0))
  expect_lt(max(stat(m2, S6, pat)), 1e-8)
  expect_equal(m2$communality, rowSums(m2$loadings^2), tolerance = 1e-12)
  expect_error(Cfafm2(S6, matrix(0, 0, 2)), "factor_pattern is empty")
  expect_error(Cfafm2(S6, pat[1:5, ]), "one row per item")
  expect_error(Cfafm2(S6, matrix(numeric(0), 6, 0)), "at least one column")
  expect_error(Cfafm2(S6, pat * 0), "frees no loading")
})
