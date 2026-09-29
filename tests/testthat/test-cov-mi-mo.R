# Coverage for the fraction of missing information, delta-shift
# sensitivity, mixed-model likelihoods and variance reduction, cluster
# z-scores, Henderson's equations, the penalised multinomial likelihood,
# modularity wrappers, mixture-of-experts layers, the moments accountant
# and the Moran variants; recomputed with base R, mice and nlme-free
# closed forms.

test_that("fraction_missing_information and MissinM", {
  q <- c(1.1, 1.4, 0.9, 1.3, 1.2)
  u <- c(0.04, 0.05, 0.045, 0.05, 0.042)
  b <- var(q)
  ub <- mean(u)
  r <- 1.2 * b / ub
  lam <- 1.2 * b / (ub + 1.2 * b)
  df <- 4 / lam^2
  expect_equal(fraction_missing_information(q, u), (r + 2 / (df + 3)) / (r + 1), tolerance = 1e-12)
  skip_if_not_installed("mice")
  expect_equal(fraction_missing_information(q, u), mice::pool.scalar(q, u)$fmi, tolerance = 1e-9)
  m <- MissinM(c(3, NA, 5, 4, NA), c(1, 0, 1, 1, 0), c(-1, 0, 2), reference = 3)
  expect_equal(m$means, 4 + 0.4 * c(-1, 0, 2), tolerance = 1e-12)
  expect_equal(m$tipping_delta, (3 - 4) / 0.4, tolerance = 1e-12)
  expect_error(MissinM(1:3, c(0, 0, 0), 1), "no observed")
})

test_that("Mlfit, Mlloglik, Mlpv, Mlwz and Hendmme", {
  set.seed(1)
  n <- 12
  X <- cbind(1, rnorm(n))
  Z <- outer(rep(1:4, each = 3), 1:4, "==") * 1
  V <- 0.5 * Z %*% t(Z) + diag(n)
  y <- as.numeric(X %*% c(1, 2) + t(chol(V)) %*% rnorm(n))
  r <- Mlfit(y, X, V)
  Vi <- solve(V)
  bh <- solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
  rr <- y - X %*% bh
  ll <- -0.5 * (as.numeric(determinant(V)$modulus) + sum(rr * (Vi %*% rr)) + n * log(2 * pi))
  expect_equal(r$loglik, ll, tolerance = 1e-9)
  expect_equal(r$bic, -2 * ll + 3 * log(n), tolerance = 1e-9)
  expect_error(Mlfit(y, X, -V), "not positive definite")
  m <- Mlloglik(X, y)
  f <- lm(y ~ X - 1)
  expect_equal(m$loglik, as.numeric(logLik(f)), tolerance = 1e-9)
  expect_equal(Mlloglik(X, y, beta = c(1, 2), sigma2 = 2)$loglik, sum(dnorm(y, X %*% c(1, 2), sqrt(2), log = TRUE)),
               tolerance = 1e-12)
  cl <- rep(1:4, each = 3)
  pv <- Mlpv(y, X[, 2], cl)
  yw <- y - ave(y, cl)
  xw <- X[, 2] - ave(X[, 2], cl)
  s0 <- sum(yw^2) / (n - 4)
  s1 <- sum(residuals(lm(yw ~ 0 + xw))^2) / (n - 5)
  expect_equal(pv$estimate, (s0 - s1) / s0, tolerance = 1e-9)
  z <- Mlwz(y, cl)
  expect_equal(z$z, (y - ave(y, cl)) / ave(y, cl, FUN = sd), tolerance = 1e-12)
  expect_equal(Mlwz(y, cl, ddof = 0)$cluster_sds[1], sqrt(mean((y[1:3] - mean(y[1:3]))^2)), tolerance = 1e-12)
  h <- Hendmme(X, Z, y, diag(4) / 0.5)
  LHS <- rbind(cbind(crossprod(X), crossprod(X, Z)), cbind(crossprod(Z, X), crossprod(Z) + diag(4) / 0.5))
  sol <- solve(LHS, c(crossprod(X, y), crossprod(Z, y)))
  expect_equal(c(h$beta, h$u), sol, tolerance = 1e-9)
  expect_equal(h$beta, as.numeric(bh), tolerance = 1e-9)
})

test_that("Mnpenlik is the penalised multinomial log-likelihood", {
  X <- cbind(c(0.2, -1, 0.5, 1.4, -0.3), c(1, 0, 1, 1, 0))
  y <- c(1, 3, 2, 1, 3)
  b0 <- c(0.1, -0.2)
  B <- matrix(c(0.5, -0.3, 0.2, 0.8), 2)
  r <- Mnpenlik(X, y, b0, B, lam = 0.4)
  eta <- cbind(sweep(X %*% t(B), 2, b0, "+"), 0)
  P <- exp(eta) / rowSums(exp(eta))
  ll <- sum(log(P[cbind(1:5, y)]))
  expect_equal(r$loglik, ll, tolerance = 1e-12)
  expect_equal(r$penalized_loglik, ll - 0.4 * sum(B^2), tolerance = 1e-12)
  expect_equal(Mnpenlik(X, y, b0, B, 0.4, penalty = "lasso")$penalty, 0.4 * sum(abs(B)), tolerance = 1e-12)
})

test_that("Modlar and Modulq are Newman-Girvan modularity", {
  A <- matrix(0, 6, 6)
  A[1:3, 1:3] <- 1
  A[4:6, 4:6] <- 1
  diag(A) <- 0
  A[3, 4] <- A[4, 3] <- 1
  lab <- c(0, 0, 0, 1, 1, 1)
  k <- rowSums(A)
  q <- sum((A - outer(k, k) / sum(A)) * outer(lab, lab, "==")) / sum(A)
  expect_equal(Modlar(A, lab)$Q, q, tolerance = 1e-12)
  expect_equal(Modulq(A, lab)$Q, q, tolerance = 1e-12)
  expect_error(Modulq(A, lab[-1]), "one entry per node")
})

test_that("Moelayer, Moeswitch and Moetop route tokens to experts", {
  x <- c(0.5, -1, 2)
  Wg <- matrix(c(0.2, 0.1, -0.3, 0.5, 0.4, 0.2, -0.1, 0.3, 0.1, 0.6, -0.2, 0), 3)
  E <- rbind(c(1, 0), c(0, 1), c(1, 1), c(-1, 2))
  r <- Moelayer(NULL, x = x, W_g = Wg, experts = E, top_k = 2)
  h <- as.numeric(x %*% Wg)
  ch <- sort(order(-h)[1:2])
  g <- exp(h[ch] - max(h[ch]))
  g <- g / sum(g)
  expect_equal(r$chosen, ch - 1L)
  expect_equal(r$out, as.numeric(g %*% E[ch, ]), tolerance = 1e-12)
  nz <- Moelayer(NULL, x = x, W_g = Wg, experts = E, top_k = 1, W_noise = Wg, noise = c(1, -1, 0.5, 0))
  hn <- h + c(1, -1, 0.5, 0) * log1p(exp(as.numeric(x %*% Wg)))
  expect_equal(nz$h, hn, tolerance = 1e-12)
  toks <- rbind(x, c(1, 1, 0), c(-1, 0.5, 0.2))
  sw <- Moeswitch(NULL, x = toks, W_g = Wg, capacity = 1.5)
  Pr <- t(apply(toks %*% Wg, 1, function(v) exp(v - max(v)) / sum(exp(v - max(v)))))
  cap <- as.integer(1.5 * 3 / 4)
  expect_equal(sw$expert_capacity, cap)
  expect_equal(sw$P, colMeans(Pr), tolerance = 1e-12)
  expect_equal(sw$aux_loss, 0.01 * 4 * sum(sw$f * colMeans(Pr)), tolerance = 1e-12)
  Es <- lapply(1:4, function(i) matrix(i * c(0.1, 0.2, 0.3, -0.1, 0, 0.2), 3))
  mt <- Moetop(toks, Wg, Es, k = 2)
  out <- t(vapply(1:3, function(t) {
    o <- order(-Pr[t, ])[1:2]
    gn <- Pr[t, o] / sum(Pr[t, o])
    as.numeric(gn[1] * toks[t, ] %*% Es[[o[1]]] + gn[2] * toks[t, ] %*% Es[[o[2]]])
  }, numeric(2)))
  expect_equal(mt$output, out, tolerance = 1e-12)
  fcount <- tabulate(apply(Pr, 1, which.max), 4) / 3
  expect_equal(mt$aux_loss, 0.01 * 4 * sum(fcount * colMeans(Pr)), tolerance = 1e-12)
  expect_error(Moetop(toks, Wg, Es[1:3]), "one expert per router column")
})

test_that("Dpacct minimises the RDP-to-DP bound over orders", {
  r <- Dpacct(1.1, 0.01, 1000, delta = 1e-5)
  lam <- 2:64
  a <- 1000 * 1e-4 * lam * (lam + 1) / (0.99 * 1.21)
  eps <- (a + log(1e5)) / lam
  expect_equal(r$epsilon, min(eps), tolerance = 1e-12)
  expect_equal(r$order, lam[which.min(eps)])
  expect_error(Dpacct(1, 1, 10), "sample_rate")
})

test_that("bivariate_morans_i, empirical_bayes_moran and Lisamoran", {
  W <- matrix(0, 5, 5)
  for (i in 1:4) W[i, i + 1] <- W[i + 1, i] <- 1
  x <- c(3, 5, 4, 9, 8)
  y <- c(1, 2, 2, 5, 6)
  r <- bivariate_morans_i(x, y, W)
  Wr <- W / rowSums(W)
  xs <- (x - mean(x)) / sd(x)
  ys <- (y - mean(y)) / sd(y)
  expect_equal(r$statistic, sum(xs * (Wr %*% ys)) / sum(xs^2), tolerance = 1e-12)
  u <- bivariate_morans_i(x, y, W, scale = FALSE, row_standardize = FALSE)
  expect_equal(u$statistic, sum(x * (W %*% y)) / sum(x^2), tolerance = 1e-12)
  O <- c(5, 12, 3, 20, 9)
  N <- c(1000, 1500, 800, 2000, 1200)
  e <- empirical_bayes_moran(O, N, W)
  p <- O / N
  b <- sum(O) / sum(N)
  s2 <- sum(N * (p - b)^2) / sum(N)
  a <- max(0, s2 - b / mean(N))
  z <- (p - b) / sqrt(a + b / N)
  zc <- z - mean(z)
  expect_equal(e$statistic, 5 / sum(W) * sum(zc * (W %*% zc)) / sum(zc^2), tolerance = 1e-12)
  expect_equal(e$eb_rates, b + a * (p - b) / (a + b / N), tolerance = 1e-12)
  expect_error(empirical_bayes_moran(O, -N, W), "strictly positive")
  expect_equal(Lisamoran(x, Wr)$local, Localmoran(x, Wr)$local)
})
