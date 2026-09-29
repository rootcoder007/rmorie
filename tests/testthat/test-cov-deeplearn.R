# Coverage for the deep-learning scoring functions (adGAN.R, advcmp.R,
# advinf.R, appnp.R, attsp.R, autoI.R, betvae.R, bert4r.R, deepF.R,
# dinmlt.R, dinopr.R, dinoss.R, dinov2.R, dnnmt.R, effnnt.R, esnnts.R,
# deepSVDD_native.R): every output recomputed with vectorised base R,
# random initialisations replayed from their documented streams.

sm_rows <- function(Z) {
  Z <- Z - apply(Z, 1, max)
  exp(Z) / rowSums(exp(Z))
}

test_that("Ganomscore, Advcmp and Advielbo follow their formulas", {
  z <- rbind(c(1, 2), c(0, 0), c(3, -1))
  zh <- rbind(c(1.5, 2), c(0.2, -0.1), c(1, 1))
  g <- Ganomscore(z, zh, threshold = 0.4)
  s <- rowSums(abs(z - zh))
  sc <- (s - min(s)) / (max(s) - min(s))
  expect_equal(g$score, s, tolerance = 1e-12)
  expect_equal(g$scaled, sc, tolerance = 1e-12)
  expect_equal(g$flagged, as.integer(sc > 0.4))
  expect_equal(Ganomscore(z, z)$scaled, rep(0, 3))
  expect_error(Ganomscore(z, zh[1:2, ]), "same shape")

  a <- Advcmp(0.1, delta = 1e-5, k = 50, delta_prime = 1e-6)
  lead <- sqrt(2 * 50 * log(1e6)) * 0.1
  quad <- 50 * 0.1 * (exp(0.1) - 1)
  expect_equal(a$epsilon_total, lead + quad, tolerance = 1e-12)
  expect_equal(a$delta_total, 50 * 1e-5 + 1e-6, tolerance = 1e-12)
  expect_equal(a$tighter, if (lead + quad < 5) "advanced" else "basic")
  expect_equal(a$epsilon_effective, min(lead + quad, 5), tolerance = 1e-12)
  expect_equal(Advcmp(1, k = 1)$tighter, "basic")
  expect_error(Advcmp(0), "epsilon must be positive")
  expect_error(Advcmp(1, delta = 1), "delta must lie")
  expect_error(Advcmp(1, k = 0), "at least one")
  expect_error(Advcmp(1, delta_prime = 0), "delta_prime")

  lj <- function(th) -0.5 * sum((th - c(1, -1))^2)
  eta <- rbind(c(0.3, -0.2), c(-1, 0.5), c(0.1, 0.1))
  mu <- c(0.5, 0)
  om <- c(-0.2, 0.3)
  v <- Advielbo(mu, om, eta, lj)
  ljs <- apply(eta, 1, function(e) lj(mu + exp(om) * e))
  expect_equal(v$logjoints, ljs, tolerance = 1e-12)
  expect_equal(v$elbo, mean(ljs) + sum(om) + (1 + log(2 * pi)), tolerance = 1e-12)
  expect_error(Advielbo(mu, om[1], eta, lj), "same length")
  expect_error(Advielbo(mu, om, eta[, 1], lj), "K columns")
})

test_that("Appnp iterates towards the personalised PageRank solve", {
  A <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 1), c(0, 1, 0, 0), c(0, 1, 0, 0))
  H <- rbind(c(1, 0), c(0.2, 0.5), c(-1, 2), c(0, 0))
  At <- A + diag(4)
  D <- diag(1 / sqrt(rowSums(At)))
  Ah <- D %*% At %*% D
  Zx <- solve(diag(4) - 0.9 * Ah, 0.1 * H)
  ex <- Appnp(A, H, alpha = 0.1, exact = TRUE, softmax = FALSE)
  expect_equal(ex$Z, Zx, tolerance = 1e-12)
  expect_equal(ex$K, 0L)
  it <- Appnp(A, H, alpha = 0.1, K = 3, softmax = FALSE)
  Z <- H
  for (k in 1:3) Z <- 0.9 * Ah %*% Z + 0.1 * H
  expect_equal(it$Z, Z, tolerance = 1e-12)
  # 0.9^600 is far below double precision, so the iterates reach the solve
  expect_equal(Appnp(A, H, alpha = 0.1, K = 600, softmax = FALSE)$Z, Zx, tolerance = 1e-10)
  expect_equal(Appnp(A, H, exact = TRUE)$Z, sm_rows(Zx), tolerance = 1e-12)
  expect_error(Appnp(A[, 1:3], H), "square")
  expect_error(Appnp(A, H[1:3, ]), "one row per node")
  expect_error(Appnp(A, H, alpha = 0), "alpha")
  expect_error(Appnp(-diag(4) * 2, H), "positive degree")
})

test_that("Sparseattn masks the softmax attention", {
  Q <- rbind(c(1, 0), c(0.5, -1))
  K <- rbind(c(1, 1), c(0, 2), c(-1, 0))
  V <- rbind(c(1, 2, 3), c(0, 1, 0), c(2, 2, 2))
  S <- rbind(c(1, 0, 1), c(1, 1, 1))
  r <- Sparseattn(Q, K, V, S)
  sco <- Q %*% t(K) / sqrt(2)
  W <- exp(sco) * S
  W <- W / rowSums(W)
  expect_equal(r$weight, W, tolerance = 1e-12)
  expect_equal(r$out, W %*% V, tolerance = 1e-12)
  expect_equal(r$density, 5 / 6)
  expect_equal(Sparseattn(Q, K, V)$weight, sm_rows(sco), tolerance = 1e-12)
  expect_error(Sparseattn(Q, K[, 1], V), "last dimension")
  expect_error(Sparseattn(Q, K, V[1:2, ]), "one row per key")
  expect_error(Sparseattn(Q, K, V, S[1, , drop = FALSE]), "nq by nk")
  expect_error(Sparseattn(Q, K, V, rbind(c(0, 0, 0), c(1, 1, 1))), "row 0 of S")
})

test_that("Autoint is multi-head self-attention with a ReLU residual", {
  x <- c(1, 0.5, -1)
  v <- rbind(c(0.2, 0.1), c(-0.3, 0.4), c(0.5, 0.5))
  Wq <- list(rbind(c(1, 0), c(0.5, 1)), diag(2))
  Wk <- rbind(c(0.3, -0.2), c(0.1, 0.9))
  Wres <- rbind(c(1, 0.2), c(0, -1))
  r <- Autoint(x, y = c(1, 0, 1), K = 2, Wq = Wq, Wk = Wk, Wres = Wres, v = v)
  E <- v * x
  acc <- 0
  for (h in 1:2) {
    Qm <- E %*% t(Wq[[h]])
    Km <- E %*% t(Wk)
    A <- sm_rows(Qm %*% t(Km))
    if (h == 1) A1 <- A
    acc <- acc + A %*% E
  }
  res <- pmax(acc + E %*% t(Wres), 0)
  expect_equal(r$e_res, res, tolerance = 1e-12)
  expect_equal(do.call(rbind, r$attention), A1, tolerance = 1e-12)
  expect_equal(r$estimate, sum(res), tolerance = 1e-12)
  p <- 1 / (1 + exp(-sum(res)))
  expect_equal(r$loss, -mean(c(1, 0, 1) * log(p) + c(0, 1, 0) * log(1 - p)), tolerance = 1e-12)
  plain <- Autoint(E)
  A0 <- sm_rows(E %*% t(E))
  expect_equal(plain$e_res, pmax(A0 %*% E + E, 0), tolerance = 1e-12)
  expect_true(is.nan(plain$loss))
})

test_that("Betavae, Dinoloss and Dinocenter", {
  x <- c(0.1, 0.4, -0.2)
  xh <- c(0, 0.5, -0.1)
  mu <- c(0.3, -0.5)
  lv <- c(-0.2, 0.1)
  b <- Betavae(x, xh, mu, lv, beta = 4, noisevar = 0.5)
  rec <- sum(dnorm(x, xh, sqrt(0.5), log = TRUE))
  kl <- sum(0.5 * (mu^2 + exp(lv) - 1 - lv))
  expect_equal(b$recon, rec, tolerance = 1e-12)
  expect_equal(b$kl, kl, tolerance = 1e-12)
  expect_equal(b$objective, rec - 4 * kl, tolerance = 1e-12)
  cp <- Betavae(x, xh, mu, lv, capacity = 1, gamma = 10)
  expect_equal(cp$penalty, 10 * abs(kl - 1), tolerance = 1e-12)
  expect_equal(Betavae(x, xh, mu, lv, beta = 2, capacity = 0)$penalty, 2 * kl, tolerance = 1e-12)
  expect_error(Betavae(x, xh[-1], mu, lv), "x and xhat")
  expect_error(Betavae(x, xh, mu, lv[-1]), "mu and logvar")
  expect_error(Betavae(x, xh, mu, lv, noisevar = 0), "strictly positive")

  s <- rbind(c(1, 2, 0.5), c(-1, 0, 1))
  tt <- rbind(c(0.5, 0.1, 0), c(0, 0.3, 0.2))
  cen <- c(0.1, 0, -0.1)
  d <- Dinoloss(s, tt, tau_s = 0.2, tau_t = 0.05, center = cen)
  Ps <- sm_rows(s / 0.2)
  Pt <- sm_rows(sweep(tt, 2, cen) / 0.05)
  expect_equal(d$per_view, -rowSums(Pt * log(Ps)), tolerance = 1e-12)
  expect_equal(d$loss, mean(d$per_view), tolerance = 1e-12)
  expect_equal(Dinoloss(s, tt)$p_t, sm_rows(tt / 0.04), tolerance = 1e-12)
  expect_error(Dinoloss(s, tt[1, , drop = FALSE]), "same shape")

  c1 <- Dinocenter(tt, center = cen, m = 0.8, tau_t = 0.1)
  expect_equal(c1$p_t, sm_rows(sweep(tt, 2, cen) / 0.1), tolerance = 1e-12)
  expect_equal(c1$center, 0.8 * cen + 0.2 * colMeans(tt), tolerance = 1e-12)
  expect_equal(Dinocenter(tt)$center, 0.1 * colMeans(tt), tolerance = 1e-12)
  expect_error(Dinocenter(tt, center = 1:2), "length K")
})

test_that("Bertrec scores the cloze positions", {
  seqs <- rbind(c(0, 3, 1, 2, 4, 1, 0, 2, 3, 4), c(1, 1, 2, 0, 3, 4, 2, 1, 0, 2))
  u <- Bertrec(seqs, K = 2, rho = 0.2)
  # uniform scores: every masked item has p = 1/5 and mid rank 3
  expect_equal(u$loss, log(5), tolerance = 1e-12)
  expect_equal(c(u$hr, u$ndcg, u$n_masked, u$n_items), c(0, 0, 4, 5))
  sc <- list(list(c(0, 0, 0, 0, 3), c(1, 2, 3, 4, 0)),
             list(c(0, 0, 0, 5, 0), c(0, 0, 1, 0, 0)))
  r <- Bertrec(seqs, K = 2, scores = sc, rho = 0.2)
  tg <- c(seqs[1, 5], seqs[1, 10], seqs[2, 5], seqs[2, 10]) + 1
  sl <- unlist(sc, recursive = FALSE)
  p <- vapply(1:4, function(j) exp(sl[[j]][tg[j]]) / sum(exp(sl[[j]])), 0)
  rk <- vapply(1:4, function(j) 1 + sum(sl[[j]] > sl[[j]][tg[j]]), 0)
  expect_equal(r$loss, -mean(log(p)), tolerance = 1e-12)
  expect_equal(r$hr, mean(rk <= 2))
  expect_equal(r$ndcg, sum(ifelse(rk <= 2, 1 / log2(rk + 1), 0)) / 4, tolerance = 1e-12)
  none <- Bertrec(seqs[, 1:3], rho = 0.2)
  expect_true(is.nan(none$loss) && none$n_masked == 0L)
  expect_equal(Bertrec(seqs, rho = 0)$n_masked, 2L)
})

test_that("DeepF replays its Gaussian initialisation", {
  X <- rbind(c(1, 0, 0.5), c(0.2, 1, -1), c(0, 0.3, 0.3))
  y <- c(1, 0, 1)
  r <- DeepF(X, y, K = 2, mlp_h = 3, w0 = 0.1, seed = 7, deep_scale = 2)
  p <- 3
  K <- 2
  h <- 3
  z <- .ghc_norm(.ghc_rng(7), p + p * K + p * K * h + 2 * h, 0, 0.1)
  w <- z[1:3]
  V <- matrix(z[4:9], p, K, byrow = TRUE)
  W1 <- matrix(z[10:27], p * K, h, byrow = TRUE)
  b1 <- z[28:30]
  W2 <- z[31:33]
  pair <- apply(X, 1, function(x) {
    G <- outer(x, x) * (V %*% t(V))
    sum(G[upper.tri(G)])
  })
  fm <- 0.1 + as.numeric(X %*% w) + pair
  emb <- t(apply(X, 1, function(x) as.numeric(t(V * x))))
  deep <- as.numeric(pmax(sweep(emb %*% W1, 2, b1, "+"), 0) %*% W2)
  ph <- 1 / (1 + exp(-(fm + 2 * deep)))
  expect_equal(r$fm_part, fm, tolerance = 1e-12)
  expect_equal(r$deep_part, 2 * deep, tolerance = 1e-12)
  expect_equal(r$p_hat, ph, tolerance = 1e-12)
  expect_equal(r$logloss, -mean(y * log(ph) + (1 - y) * log(1 - ph)), tolerance = 1e-12)
  expect_equal(r$estimate, mean(ph), tolerance = 1e-12)
  expect_true(is.nan(DeepF(X)$logloss))
  expect_error(DeepF(matrix(0, 0, 2)), "no rows")
  expect_error(DeepF(X, K = 0), "K must")
  expect_error(DeepF(X, mlp_h = 0), "mlp_h")
  expect_error(DeepF(X, y[-1]), "one label per row")
  expect_error(DeepF(X, c(0, 2, 1)), "binary")
})

test_that("Dinmlt and Dinov2 are multi-crop cross-entropies", {
  M <- rbind(c(1, 0, 0.5), c(0.2, 0.4, 0), c(0, 1, 1), c(-1, 0.5, 0), c(0.3, 0.3, 0.3))
  r <- Dinmlt(M, global_size = 2, local_size = 3, tau_s = 0.2, tau_t = 0.05)
  cc <- colMeans(M[1:2, ])
  Tt <- sm_rows(sweep(M[1:2, ], 2, cc) / 0.05)
  St <- sm_rows(M / 0.2)
  ce <- -Tt %*% t(log(St))
  expect_equal(r$teacher, Tt, tolerance = 1e-12)
  expect_equal(r$loss, (sum(ce) - sum(diag(ce[, 1:2]))) / 8, tolerance = 1e-12)
  expect_equal(r$n_pairs, 8L)
  expect_equal(r$student_entropy, -sum(St * log(St)) / 5, tolerance = 1e-12)
  cen <- c(0, 0.1, 0)
  rc <- Dinmlt(M, 2, 3, center = cen)
  expect_equal(rc$teacher, sm_rows(sweep(M[1:2, ], 2, cen) / 0.04), tolerance = 1e-12)
  expect_error(Dinmlt(matrix(0, 0, 3)), "no crops")
  expect_error(Dinmlt(M, global_size = 0, local_size = 5), "at least 1")
  expect_error(Dinmlt(M, 2, 2), "global_size \\+ local_size")
  expect_error(Dinmlt(M, 2, 3, tau_s = 0), "strictly positive")
  expect_error(Dinmlt(M, 2, 3, center = 1:2), "one entry per output")

  S <- rbind(c(1, 0, 0.5), c(0.2, 0.4, 0), c(0, 1, 1), c(-1, 0.5, 0), c(0.3, 0.1, 0.2))
  TT <- S[5:1, ]
  x <- rbind(c(1, 0), c(0.8, 0.6), c(0, 1), c(-1, 0.2))
  d <- Dinov2(x, S, TT, tau = 0.2, tau_t = 0.05, w_ibot = 0.5, w_koleo = 0.3)
  ce_row <- function(i) -sum(sm_rows(TT[i, , drop = FALSE] / 0.05) * log(sm_rows(S[i, , drop = FALSE] / 0.2)))
  dino <- ce_row(1)
  ibot <- mean(c(ce_row(3), ce_row(5)))
  En <- x / sqrt(rowSums(x^2))
  Dm <- as.matrix(dist(En))
  diag(Dm) <- Inf
  koleo <- -mean(log(apply(Dm, 1, min) + 1e-12))
  expect_equal(c(d$dino, d$ibot, d$koleo), c(dino, ibot, koleo), tolerance = 1e-12)
  expect_equal(d$loss, dino + 0.5 * ibot + 0.3 * koleo, tolerance = 1e-12)
  expect_equal(d$n_masked, 2L)
  m1 <- Dinov2(x, S, TT, tau = 0.2, tau_t = 0.05, mask = c(1, 0, 0, 0))
  expect_equal(m1$ibot, ce_row(2), tolerance = 1e-12)
  expect_equal(Dinov2(x, S, TT, mask = c(0, 0, 0, 0))$ibot, 0)
  expect_error(Dinov2(x, matrix(0, 0, 3), TT), "required")
  expect_error(Dinov2(x, S, TT[, 1:2]), "same shape")
  expect_error(Dinov2(x, S, TT, tau = 0), "strictly positive")
  expect_error(Dinov2(x, S, TT, mask = 1), "one flag per patch")
  expect_error(Dinov2(x[1, , drop = FALSE], S, TT), "at least two")
})

test_that("Dnnmt trains by full-batch backpropagation", {
  X <- rbind(c(0.5, -0.2), c(0.1, 0.8), c(-0.6, 0.3), c(0.9, 0.4))
  Y <- rbind(c(1, 0.2), c(0.3, 0.9), c(-0.5, 0.1), c(0.8, 1.5))
  lcg <- function(s) {
    # a = 16838 * 2^16 + 20077, split on the multiplier (not the state)
    (((16838 * s) %% 32768) * 65536 + (20077 * s) %% 2147483648 + 12345) %% 2147483648
  }
  s <- 5
  u <- numeric(3 * 3 + 4 * 2)
  for (i in seq_along(u)) {
    s <- lcg(s)
    u[i] <- 0.2 * s / 2147483648 - 0.1
  }
  W <- list(matrix(u[1:9], 3, 3, byrow = TRUE), matrix(u[10:17], 4, 2, byrow = TRUE))
  med <- apply(Y, 2, median)
  q <- apply(Y, 2, quantile, probs = c(0.25, 0.75), type = 7)
  dt <- pmax(abs(med - q[1, ]), abs(q[2, ] - med))
  wt <- c(dt[1], dt[1] / dt[2])
  train <- function(W, wt, act, dact, epochs, eta) {
    for (ep in seq_len(epochs)) {
      A1 <- cbind(1, X)
      Z1 <- A1 %*% W[[1]]
      A2 <- cbind(1, act(Z1))
      Yh <- A2 %*% W[[2]]
      loss <- sum(sweep((Yh - Y)^2, 2, wt, "*")) / (2 * 4 * 2)
      D2 <- sweep(Y - Yh, 2, wt, "*")
      D1 <- dact(Z1) * (D2 %*% t(W[[2]][-1, ]))
      W <- list(W[[1]] + eta * t(A1) %*% D1, W[[2]] + eta * t(A2) %*% D2)
    }
    list(W = W, loss = loss, Yh = Yh)
  }
  r <- Dnnmt(X, Y, layers = 3, eta = 0.05, epochs = 4, seed = 5)
  ref <- train(W, wt, function(z) pmax(z, 0), function(z) (z > 0) + 0, 4, 0.05)
  expect_equal(unname(r$head_weights), unname(wt), tolerance = 1e-12)
  expect_equal(r$weights, ref$W, tolerance = 1e-12)
  expect_equal(r$loss, ref$loss, tolerance = 1e-12)
  expect_equal(r$Y_hat, ref$Yh, tolerance = 1e-12)
  th <- Dnnmt(X, Y, 3, heads = c(1, 2), activation = "tanh", eta = 0.1, epochs = 3, init = W)
  ref2 <- train(W, c(1, 2), tanh, function(z) 1 - tanh(z)^2, 3, 0.1)
  expect_equal(th$weights, ref2$W, tolerance = 1e-12)
  sg <- Dnnmt(X, Y, 3, heads = c(1, 1), activation = "sigmoid", epochs = 2, init = W)
  sig <- function(z) 1 / (1 + exp(-z))
  ref3 <- train(W, c(1, 1), sig, function(z) sig(z) * (1 - sig(z)), 2, 0.1)
  expect_equal(sg$weights, ref3$W, tolerance = 1e-12)
  stop_early <- Dnnmt(X, Y, 3, heads = c(1, 1), epochs = 50, tol = 10, init = W)
  expect_equal(stop_early$epochs_run, 1L)
  expect_error(Dnnmt(matrix(0, 0, 2), Y, 3), "X is empty")
  expect_error(Dnnmt(matrix(0, 4, 0), Y, 3), "no columns")
  expect_error(Dnnmt(X, Y[1:3, ], 3), "disagree")
  expect_error(Dnnmt(X, matrix(0, 4, 0), 3), "no traits")
  expect_error(Dnnmt(X, Y, 0), "at least one unit")
  expect_error(Dnnmt(X, Y, 3, eta = 0), "eta must be positive")
  expect_error(Dnnmt(X, Y, 3, epochs = 0), "epochs")
  expect_error(Dnnmt(X, Y, 3, heads = 1), "one loss weight")
  expect_error(Dnnmt(X, Y, 3, heads = c(1, -1)), "non-negative")
  expect_error(Dnnmt(X, Y, 3, init = W[1]), "one weight matrix per layer")
  expect_error(Dnnmt(X, Y, 3, init = list(W[[1]][-1, ], W[[2]])), "layer 1 has the wrong shape")
  expect_error(Dnnmt(X, cbind(Y[, 1], 1), 3), "zero interquartile")
  expect_error(Dnnmt(X, Y, 3, heads = c(1, 1), activation = "gelu", epochs = 1), "unknown activation")
  expect_error(Dnnmt(X, Y, 3, heads = c(1, 1), out_activation = "gelu", epochs = 1), "unknown activation")
})

test_that("Mbconv expands, filters, squeezes and projects", {
  x <- c(0.5, -1, 2)
  r <- Mbconv(x, expand_ratio = 2, se_ratio = 0.5, phi = 2)
  e <- rep(x, 2) / 2
  m <- 6
  dw <- vapply(1:m, function(i) mean(e[max(1, i - 1):min(m, i + 1)]), 0)
  dw <- dw / (1 + exp(-dw))
  se <- 1 / (1 + exp(-mean(dw) * 0.5))
  ex <- dw * se
  y <- as.numeric(tapply(ex, rep(1:3, 2), mean)) + x
  expect_equal(r$y, y, tolerance = 1e-12)
  expect_equal(r$se, rep(se, m), tolerance = 1e-12)
  expect_equal(c(r$depth, r$width, r$resolution), c(1.2^2, 1.1^2, 1.15^2), tolerance = 1e-12)
  expect_equal(r$constraint, 1.2 * 1.1^2 * 1.15^2, tolerance = 1e-12)
  f <- Mbconv(x, expand_ratio = 2, se_ratio = 0.5, filters = 4)
  expect_equal(f$y, c(as.numeric(tapply(ex, c(1:4, 1:2), mean))), tolerance = 1e-12)
  expect_true(is.nan(f$depth))
  tiny <- Mbconv(1, expand_ratio = 0.1, filters = 2)
  expect_equal(tiny$y[2], 0)
})

test_that("Esnnts fits a ridge readout on its van der Corput reservoir", {
  vdc <- function(i, b) {
    f <- 1
    r <- 0
    k <- i + 1
    while (k > 0) {
      f <- f / b
      r <- r + f * (k %% b)
      k <- k %/% b
    }
    r
  }
  pr <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  drw <- function(k) 2 * vdc(k %/% 12 + 1, pr[k %% 12 + 1]) - 1
  y <- sin(seq(0.3, 4, length.out = 14))
  size <- 3
  r <- Esnnts(y, reservoir_size = size, spectral_radius = 0.8, leak = 0.7, ridge = 0.01,
              washout = 2)
  W <- matrix(vapply(0:8, drw, 0), 3, 3, byrow = TRUE)
  W <- W * 0.8 / max(rowSums(abs(W)))
  win <- vapply(9:11, drw, 0)
  x <- numeric(3)
  Xs <- NULL
  for (t in 2:14) {
    x <- 0.3 * x + 0.7 * tanh(win * y[t - 1] + as.numeric(W %*% x))
    if (t - 2 >= 2) Xs <- rbind(Xs, c(1, x))
  }
  tg <- y[4:14]
  v <- solve(crossprod(Xs) + diag(0.01, 4), crossprod(Xs, tg))
  mse <- mean((tg - Xs %*% v)^2)
  expect_equal(r$win, win, tolerance = 1e-12)
  expect_equal(r$coef, as.numeric(v), tolerance = 1e-10)
  expect_equal(r$mse, mse, tolerance = 1e-10)
  expect_equal(r$nrmse, sqrt(mse / mean((tg - mean(tg))^2)), tolerance = 1e-10)
  expect_equal(Esnnts(y, reservoir_size = 3)$washout, 3L)
  expect_error(Esnnts(1:2), "at least 3")
  expect_error(Esnnts(y, reservoir_size = 0), "positive")
  expect_error(Esnnts(y, spectral_radius = -1), "non-negative")
  expect_error(Esnnts(y, leak = 0), "leak")
  expect_error(Esnnts(y, ridge = -1), "ridge")
  expect_error(Esnnts(y, washout = 13), "washout")
})

test_that("svdd finds the minimum enclosing ball", {
  X <- rbind(c(0, 0), c(2, 0), c(1, 0.5), c(1, -0.3))
  r <- svdd(X, C = 1)
  # obtuse hull: the ball is the one on the diameter (0,0)-(2,0)
  expect_equal(r$center, c(1, 0), tolerance = 1e-8)
  expect_equal(r$radius2, 1, tolerance = 1e-8)
  expect_equal(r$alpha, c(0.5, 0.5, 0, 0), tolerance = 1e-8)
  expect_equal(sort(r$support), 1:2)
  expect_lt(r$kkt_violation, 1e-6)
  soft <- svdd(X, C = 0.3)
  expect_equal(sum(soft$alpha), 1, tolerance = 1e-12)
  expect_true(all(soft$alpha >= 0 & soft$alpha <= 0.3 + 1e-12))
  expect_equal(soft$center, as.numeric(soft$alpha %*% X), tolerance = 1e-12)
  expect_lt(soft$kkt_violation, 1e-6)
  rb <- svdd(X, C = 1, kernel = "RBF", gamma = 0.5)
  K <- exp(-0.5 * as.matrix(dist(X))^2)
  d2 <- 1 - 2 * as.numeric(K %*% rb$alpha) + sum(outer(rb$alpha, rb$alpha) * K)
  expect_true(all(d2 <= rb$radius2 + 1e-6))
  expect_equal(max(d2), rb$radius2, tolerance = 1e-6)
  expect_null(rb$center)
  expect_error(svdd(X[1, , drop = FALSE]), "at least two")
  expect_error(svdd(X, C = 0.1), "C >= 1/n")
  expect_error(svdd(X, kernel = "poly"), "linear' or 'rbf")
  expect_match(deepSVDD_cheatsheet(), "svdd")
})
