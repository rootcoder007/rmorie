# Coverage for the Kalman filter and RTS smoother, k-anonymity, Cohen's
# kappa, Katz centralities, replicated kernel BLUP, DP composition, the
# kernel change-point statistic, KEGG enrichment, K-fold CV error, R-GCN
# and the Pareto-k diagnostic; recomputed with dlm, loo and base R.

test_that("Kalmf and KalmS agree with dlm's filter and smoother", {
  set.seed(1)
  y <- cumsum(rnorm(15)) + rnorm(15, sd = 0.5)
  F <- matrix(c(1, 0, 1, 1), 2)
  H <- matrix(c(1, 0), 1)
  Q <- diag(c(0.1, 0.01))
  R <- matrix(0.25)
  f <- Kalmf(y, F, H, Q, R, x0 = c(0, 0), P0 = diag(2))
  s <- KalmS(y, F, H, Q, R, x0 = c(0, 0), P0 = diag(2))
  xs <- matrix(0, 15, 2)
  x <- c(0, 0)
  P <- diag(2)
  ll <- 0
  for (t in 1:15) {
    xp <- F %*% x
    Pp <- F %*% P %*% t(F) + Q
    S <- H %*% Pp %*% t(H) + R
    v <- y[t] - H %*% xp
    K <- Pp %*% t(H) %*% solve(S)
    x <- xp + K %*% v
    P <- Pp - K %*% H %*% Pp
    xs[t, ] <- x
    ll <- ll - 0.5 * (log(2 * pi) + log(det(S)) + v^2 / S)
  }
  expect_equal(t(vapply(f$state, identity, numeric(2))), xs, tolerance = 1e-9)
  expect_equal(f$loglik, as.numeric(ll), tolerance = 1e-9)
  expect_equal(s$loglik, f$loglik, tolerance = 1e-12)
  skip_if_not_installed("dlm")
  mod <- dlm::dlm(FF = H, V = R, GG = F, W = Q, m0 = c(0, 0), C0 = diag(2))
  df <- dlm::dlmFilter(y, mod)
  expect_equal(t(vapply(f$state, identity, numeric(2))), unname(df$m[-1, ]), tolerance = 1e-9)
  ds <- dlm::dlmSmooth(df)
  # the RTS gain uses Pp + 1e-12 I, so the smoothed means agree to about 1e-11
  expect_equal(t(vapply(s$smoothed, identity, numeric(2))), unname(ds$s[-1, ]), tolerance = 1e-9)
  expect_error(Kalmf(y, F, matrix(1, 1, 3), Q, R), "m x d")
  expect_error(KalmS(numeric(0), F, H, Q, R), "empty")
})

test_that("Kanon, Kappacoef and Kcompo", {
  qi <- cbind(c(1, 1, 2, 2, 2, 3), c(0, 0, 1, 1, 1, 0))
  r <- Kanon(1:6, qi, k = 2)
  expect_equal(c(r$min_class_size, r$max_class_size, r$n_classes), c(1, 3, 3))
  expect_equal(r$satisfies, 0)
  expect_equal(r$n_violating, 1)
  expect_equal(Kanon(1:5, qi[1:5, ], 2)$satisfies, 1)
  expect_error(Kanon(1:6, qi, 0), "at least 1")

  kp <- Kappacoef(40, 10, 5, 45)
  p0 <- 85 / 100
  pe <- (45 / 100) * (50 / 100) + (55 / 100) * (50 / 100)
  expect_equal(kp$kappa, (p0 - pe) / (1 - pe), tolerance = 1e-12)
  expect_error(Kappacoef(0, 0, 0, 0), "at least one")

  kc <- Kcompo(1:10, c(0.5, 0.2, 0.3), deltas = c(1e-5, 0, 2e-5))
  expect_equal(c(kc$epsilon_total, kc$delta_total), c(1, 3e-5), tolerance = 1e-12)
  expect_equal(kc$laplace_scale, 1 / c(0.5, 0.2, 0.3), tolerance = 1e-12)
  expect_equal(kc$pure_dp, 0)
  expect_equal(Kcompo(NULL, 1)$pure_dp, 1)
  expect_error(Kcompo(1, c(1, -1)), "positive")
})

test_that("Katzc and Katzcn solve (I - alpha A) x = rhs", {
  A <- matrix(c(0, 1, 1, 0,
                1, 0, 1, 1,
                0, 1, 0, 1,
                1, 0, 0, 0), 4, byrow = TRUE)
  w <- c(1, 2, 0.5, 1)
  k <- Katzc(w, A, alpha = 0.2, beta = 0.1)
  expect_equal(k$centrality, as.numeric(solve(diag(4) - 0.2 * A, 0.2 * A %*% w)) + 0.1, tolerance = 1e-12)
  # the Neumann series sum_k alpha^k A^k (alpha A w) gives the same vector
  s <- numeric(4)
  term <- 0.2 * A %*% w
  for (i in 1:200) {
    s <- s + term
    term <- 0.2 * A %*% term
  }
  expect_equal(k$centrality, as.numeric(s) + 0.1, tolerance = 1e-12)
  kn <- Katzcn(A, alpha = 0.2, beta = 2)
  expect_equal(kn$centrality, as.numeric(solve(diag(4) - 0.2 * A, rep(2, 4))), tolerance = 1e-12)
  expect_error(Katzc(w, A, alpha = 0), "positive")
  expect_error(Katzcn(A[1:3, ]), "square")
})

test_that("Kernblup forms sigma2 Z K Z'", {
  Z <- rbind(c(1, 0), c(1, 0), c(0, 1))
  K <- matrix(c(1, 0.4, 0.4, 2), 2)
  r <- Kernblup(Z, K, 0.5)
  expect_equal(r$K_star, 0.5 * Z %*% K %*% t(Z), tolerance = 1e-12)
  expect_equal(c(r$n, r$J), c(3L, 2L))
})

test_that("morie_kcusum is the kernel Fisher discriminant scan", {
  set.seed(2)
  x <- c(rnorm(8, 0, 0.5), rnorm(8, 2, 0.5))
  r <- morie_kcusum(x, kernel = "gaussian", gamma = 0.1, threshold = 1)
  D2 <- as.matrix(dist(x))^2
  bw <- median(sqrt(D2[upper.tri(D2)]))
  Kg <- exp(-D2 / (2 * bw^2))
  e <- eigen(Kg, symmetric = TRUE)
  keep <- e$values > 1e-12 * max(abs(e$values))
  C <- t(e$vectors[, keep] %*% diag(sqrt(e$values[keep])))
  n <- 16
  Ts <- vapply(2:14, function(k) {
    C1 <- C[, 1:k, drop = FALSE]
    C2 <- C[, (k + 1):n, drop = FALSE]
    d <- rowMeans(C2) - rowMeans(C1)
    S1 <- tcrossprod(C1 - rowMeans(C1)) / k
    S2 <- tcrossprod(C2 - rowMeans(C2)) / (n - k)
    Sw <- (k * S1 + (n - k) * S2) / n
    M <- Sw + 0.1 * diag(nrow(C))
    kf <- k * (n - k) / n * sum(d * solve(M, d))
    d1 <- sum(diag(solve(M, Sw)))
    d2 <- sum(diag(solve(M, solve(M, Sw %*% Sw))))
    (kf - d1) / sqrt(2 * d2)
  }, 0)
  # eigenvectors are sign-ambiguous but the statistic is invariant to sign flips
  expect_equal(r$T, Ts, tolerance = 1e-8)
  expect_equal(r$estimate, 1 + which.max(Ts))
  expect_equal(r$bandwidth, bw, tolerance = 1e-12)
  expect_equal(r$detected, max(Ts) > 1)
  lin <- morie_kcusum(x, kernel = "linear", kmin = 3, kmax = 12)
  expect_length(lin$T, 10)
  expect_error(morie_kcusum(1:3), "n >= 4")
  expect_error(morie_kcusum(x, kernel = "poly"), "kernel must be")
})

test_that("Keggp computes hypergeometric tails with BH q-values", {
  set.seed(3)
  g <- rbinom(40, 1, 0.3)
  P <- cbind(rbinom(40, 1, 0.4), c(rep(1, 12), rep(0, 28)), rbinom(40, 1, 0.2))
  P[, 2] <- pmax(P[, 2], g * rbinom(40, 1, 0.8))
  r <- Keggp(g, P, alpha = 0.1)
  pv <- vapply(1:3, function(j) phyper(sum(P[g == 1, j]) - 1, sum(P[, j]), 40 - sum(P[, j]), sum(g),
                                       lower.tail = FALSE), 0)
  expect_equal(r$pvalue, pv, tolerance = 1e-12)
  expect_equal(r$qvalue, p.adjust(pv, "BH"), tolerance = 1e-12)
  expect_equal(r$top_pathway, which.min(pv) - 1L)
  expect_equal(r$n_significant, sum(p.adjust(pv, "BH") <= 0.1))
  expect_error(Keggp(g * 2, P), "0/1 indicator")
  expect_error(Keggp(rep(0, 40), P), "no genes selected")
})

test_that("Kfcve averages the fold MSEs", {
  y <- c(1, 2, 3, 4, 5, 6, 7)
  yh <- list(c(1.1, 2.2, 2.9), c(4.5, 5), c(6, 7.5))
  r <- Kfcve(y, yh)
  mse <- c(mean((y[1:3] - yh[[1]])^2), mean((y[4:5] - yh[[2]])^2), mean((y[6:7] - yh[[3]])^2))
  expect_equal(r$mse_fold, mse, tolerance = 1e-12)
  expect_equal(r$cv_error, mean(mse), tolerance = 1e-12)
  f <- Kfcve(y, list(c(2, 4), c(1, 5)), folds = list(c(0, 2), c(1, 3)))
  expect_equal(f$mse_fold, c(mean((c(1, 3) - c(2, 4))^2), mean((c(2, 4) - c(1, 5))^2)), tolerance = 1e-12)
  expect_error(Kfcve(y, list(1)), "prediction count")
  expect_error(Kfcve(y, list(1), folds = list(9)), "out of range")
})

test_that("Kgnn aggregates per relation with mean normalisation", {
  X <- matrix(c(1, 0, 2, -1, 0.5, 1), 3)
  A1 <- matrix(c(0, 1, 1, 1, 0, 0, 0, 1, 0), 3, byrow = TRUE)
  A2 <- matrix(c(0, 0, 1, 0, 0, 0, 1, 1, 0), 3, byrow = TRUE)
  W1 <- matrix(c(1, 0.5, -0.5, 1), 2)
  W2 <- matrix(c(0.3, 0, 0, 0.3), 2)
  r <- Kgnn(list(A1, A2), X, list(W1, W2))
  mean_agg <- function(A) {
    d <- rowSums(A != 0)
    (A != 0) %*% X / ifelse(d > 0, d, 1)
  }
  Z <- mean_agg(A1) %*% W1 + mean_agg(A2) %*% W2 + X
  expect_equal(r$H, pmax(Z, 0), tolerance = 1e-12)
  expect_error(Kgnn(list(A1), X, list(W1, W2)), "different lengths")
  expect_error(Kgnn(list(A1[1:2, ]), X, list(W1)), "wrong node count")
})

test_that("Khatd is the Zhang-Stephens tail fit of the PSIS weights", {
  set.seed(4)
  L <- matrix(rnorm(100 * 3, -1, c(0.2, 0.8, 1.5)), 100, byrow = TRUE)
  r <- Khatd(L)
  zs <- function(x) {
    N <- length(x)
    M <- 30 + floor(sqrt(N))
    th <- 1 / x[N] + (1 - sqrt(M / (1:M - 0.5))) / (3 * x[floor(N / 4 + 0.5)])
    lt <- vapply(th, function(a) {
      k <- mean(log1p(-a * x))
      N * (log(-a / k) - k - 1)
    }, 0)
    w <- exp(lt - max(lt))
    k <- mean(log1p(-sum(th * w) / sum(w) * x))
    k * N / (N + 10) + 0.5 * 10 / (N + 10)
  }
  kref <- apply(-L, 2, function(lr) {
    lw <- sort(lr - max(lr))
    zs(sort(exp(lw[81:100]) - exp(lw[80])))
  })
  expect_equal(r$k, kref, tolerance = 1e-12)
  expect_equal(r$n_bad, sum(kref > 0.7))
  expect_true(is.nan(Khatd(L[1:20, ])$estimate))
})
