# Coverage for rkhsb .. robust_wilcox exports. Every expectation is
# recomputed in the test body.

cov_k6 <- function() {
  x <- c(0.1, 0.5, 0.9, 1.4, 2.0)
  exp(-outer(x, x, "-")^2)
}

test_that("Rkhsbayes and Rkhsnorm follow the kernel BLUP and RKHS norm", {
  K <- cov_k6()
  y <- c(1.2, 0.8, 1.9, 2.4, 1.1)
  r <- Rkhsbayes(y, K, sigma2_u = 2, sigma2_e = 0.5)
  # u = s2u K (s2u K + s2e I)^-1 (y - mu)
  u <- 2 * K %*% solve(2 * K + 0.5 * diag(5), y - mean(y))
  expect_equal(r$u, as.numeric(u), tolerance = 1e-8)
  expect_equal(r$mu, mean(y))
  expect_equal(Rkhsbayes(y, K, mu = 1)$mu, 1)
  b <- c(0.5, -1, 0.3, 0.2, 0.7)
  n <- Rkhsnorm(b, K)
  expect_equal(n$norm2, sum(b * (K %*% b)), tolerance = 1e-12)
  expect_equal(n$norm, sqrt(sum(b * (K %*% b))), tolerance = 1e-12)
  expect_error(Rkhsnorm(c(1, -1), matrix(c(0, 1, 1, 0), 2)), "not positive semi-definite")
})

test_that("Rkhsmt converges to the fixed point of its conditional-mean updates", {
  K <- cov_k6() + diag(0.1, 5)
  Y <- cbind(c(1.2, 0.8, 1.9, 2.4, 1.1), c(0.3, 0.5, 0.2, 0.9, 0.4))
  r <- Rkhsmt(Y, K, n_iter = 3000)
  J <- 5
  nT <- 2
  Ki <- solve(K)
  b1 <- r$b1
  expect_equal(r$mu, colMeans(Y - b1), tolerance = 1e-9)
  expect_equal(r$Sigma_T, (t(b1) %*% Ki %*% b1 + diag(2)) / (4 + J - nT - 1), tolerance = 1e-9)
  E <- Y - matrix(r$mu, J, nT, byrow = TRUE) - b1
  expect_equal(r$R, (crossprod(E) + diag(2)) / (4 + J - nT - 1), tolerance = 1e-9)
  M <- kronecker(solve(r$Sigma_T), Ki) + kronecker(solve(r$R), diag(J))
  rhs <- (Y - matrix(r$mu, J, nT, byrow = TRUE)) %*% solve(r$R)
  expect_equal(as.numeric(b1), as.numeric(solve(M, as.numeric(rhs))), tolerance = 1e-8)
  expect_equal(r$gebv, b1, tolerance = 1e-12)
  expect_error(Rkhsmt(Y[1, , drop = FALSE], K[1, 1, drop = FALSE]), "two lines")
  expect_error(Rkhsmt(Y, K[1:4, 1:4]), "square matrix of order J")
  expect_error(Rkhsmt(Y, K, v_T = 2), "degrees of freedom")
  expect_error(Rkhsmt(Y, K, n_iter = 0), "at least 1")
})

test_that("morie_rkmeans returns a fixed point of trimmed concentration steps", {
  X <- rbind(c(0, 0), c(0.3, 0.1), c(0.1, 0.4), c(0.2, 0.2), c(5, 5), c(5.2, 4.9), c(4.8, 5.3),
             c(5.1, 5.1), c(20, -20))
  r <- morie_rkmeans(X, k = 2, alpha = 0.12, seed = 3)
  expect_equal(r$n_kept, ceiling(9 * 0.88))
  expect_equal(r$outliers, 8L)
  C <- do.call(rbind, r$centers)
  d <- apply(X, 1, function(x) min(sqrt(colSums((t(C) - x)^2))))
  lab <- apply(X, 1, function(x) which.min(sqrt(colSums((t(C) - x)^2))) - 1L)
  kept <- sort(order(d^2)[1:r$n_kept]) - 1L
  expect_equal(r$kept, kept)
  expect_equal(r$labels[kept + 1], lab[kept + 1])
  for (j in 0:1) {
    expect_equal(r$centers[[j + 1]], colMeans(X[kept[lab[kept + 1] == j] + 1, , drop = FALSE]), tolerance = 1e-12)
  }
  expect_equal(r$criterion, mean(d[kept + 1]^2), tolerance = 1e-12)
  ra <- morie_rkmeans(X, k = 2, alpha = 0.12, penalty = "absolute", seed = 3)
  for (j in 0:1) {
    pts <- X[ra$kept[ra$labels[ra$kept + 1] == j] + 1, , drop = FALSE]
    m <- ra$centers[[j + 1]]
    D <- pts - matrix(m, nrow(pts), 2, byrow = TRUE)
    nd <- sqrt(rowSums(D^2))
    at <- nd < 1e-9
    g <- colSums(D[!at, , drop = FALSE] / nd[!at])
    # spatial median optimality: the unit vectors to the other points sum
    # to zero, or to length <= 1 when the median sits on a data point
    if (any(at)) expect_lte(sqrt(sum(g^2)), 1) else expect_lt(max(abs(g)), 1e-6)
  }
  expect_error(morie_rkmeans(X, k = 10), "exceeds n")
  expect_error(morie_rkmeans(X, alpha = 1), "alpha must lie")
  expect_error(morie_rkmeans(X, penalty = "cube"), "penalty must be one of")
  expect_error(morie_rkmeans(X, k = 2, alpha = 0.9), "fewer than k")
})

test_that("morie_rmrl_cheatsheet describes QRM", {
  s <- morie_rmrl_cheatsheet()
  expect_type(s, "character")
  expect_match(s, "Icarte")
  expect_match(s, "QRM")
})

test_that("morie_rmsdtr recovers a proper rotation exactly", {
  P <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 2, 0), c(0, 0, 1.5), c(1, 1, 1))
  th <- 0.7
  R <- rbind(c(cos(th), -sin(th), 0), c(sin(th), cos(th), 0), c(0, 0, 1))
  Q <- P %*% t(R) + matrix(c(1, -2, 0.5), 5, 3, byrow = TRUE)
  r <- morie_rmsdtr(P, Q)
  expect_equal(r$estimate, 0, tolerance = 1e-10)
  expect_equal(r$rotation, R, tolerance = 1e-10)
  expect_equal(r$det, 1, tolerance = 1e-12)
  # noisy case: the SVD form of Kabsch's solution
  Qn <- Q + rbind(c(0.1, 0, -0.05), c(0, 0.08, 0), c(-0.02, 0, 0.1), c(0.05, -0.1, 0), c(0, 0.03, -0.04))
  Pc <- sweep(P, 2, colMeans(P))
  Qc <- sweep(Qn, 2, colMeans(Qn))
  s <- svd(t(Qc) %*% Pc)
  U <- s$u %*% diag(c(1, 1, det(s$u %*% t(s$v)))) %*% t(s$v)
  rn <- morie_rmsdtr(P, Qn)
  expect_equal(rn$rotation, U, tolerance = 1e-10)
  expect_equal(rn$estimate, sqrt(mean(rowSums((Pc %*% t(U) - Qc)^2))), tolerance = 1e-10)
  expect_error(morie_rmsdtr(P[1:2, ], Q[1:2, ]), ">= 3 paired")
  expect_error(morie_rmsdtr(P, Q, weights = rep(-1, 5)), "non-negative")
})

test_that("Rmsetst, Rmsnorm and Robcov evaluate their formulas", {
  y <- c(1, 2.5, 3, 4.2)
  yh <- c(1.2, 2, 3.3, 4)
  r <- Rmsetst(y, yh)
  expect_equal(r$rmse, sqrt(mean((y - yh)^2)), tolerance = 1e-12)
  expect_error(Rmsetst(y, yh[-1]), "same length")
  expect_error(Rmsetst(numeric(0), numeric(0)), "non-empty")
  a <- c(0.5, -1.2, 2.0, 0.3)
  g <- c(1, 2, 0.5, 1)
  b <- c(0, 0.1, 0, -0.1)
  n <- Rmsnorm(NULL, x = a, g = g, b = b, eps = 1e-3)
  rms <- sqrt(mean(a^2) + 1e-3)
  expect_equal(n$out, a / rms * g + b, tolerance = 1e-12)
  pn <- Rmsnorm(a, p = 0.5)
  expect_equal(pn$rms, sqrt(mean(a[1:2]^2)), tolerance = 1e-12)
  expect_equal(pn$k_partial, 2L)
  skip_if_not_installed("sandwich")
  X <- cbind(1, c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6), c(1, 0, 1, 1, 0, 0, 1, 0))
  yy <- c(1.3, 2.2, 0.1, 1.9, 3.5, -0.2, 0.9, 1.6)
  f <- stats::lm(yy ~ X - 1)
  for (k in c("HC0", "HC1", "HC2", "HC3")) {
    expect_equal(unname(Robcov(X, yy, kind = k)$V), unname(sandwich::vcovHC(f, type = k)), tolerance = 1e-10)
  }
  expect_equal(Robcov(X, yy)$coef, unname(stats::coef(f)), tolerance = 1e-10)
})

test_that("RND bonus is the predictor's error against a frozen random net", {
  X <- rbind(c(0.2, -0.5), c(1.0, 0.3), c(-0.4, 0.8), c(0.6, 0.1), c(0.9, -0.7))
  r <- morie_rndnet(X, n_hidden = 4, n_out = 3, lr = 0.1, normalize_obs = FALSE, normalize_reward = FALSE,
                    update = FALSE, seed = 2)
  tg <- r$target
  f <- tanh(sweep(X %*% tg$W1, 2, tg$b1, "+")) %*% tg$W2
  # the predictor starts at W = 0, so without updates its output is 0
  expect_equal(r$raw_error, rowSums(f^2), tolerance = 1e-12)
  expect_equal(r$returns, as.numeric(stats::filter(rowSums(f^2), 0.99, method = "recursive")), tolerance = 1e-12)
  ru <- morie_rndnet(X, n_hidden = 4, n_out = 3, lr = 0.1, normalize_obs = FALSE, normalize_reward = TRUE,
                     seed = 2)
  pf <- ru$predictor$feat
  W <- matrix(0, 4, 3)
  raw <- numeric(5)
  for (t in 1:5) {
    h <- tanh(as.numeric(X[t, ] %*% pf$W1) + pf$b1)
    err <- as.numeric(h %*% W) - f[t, ]
    raw[t] <- sum(err^2)
    W <- W - 0.1 * 2 * outer(h, err)
  }
  ret <- as.numeric(stats::filter(raw, 0.99, method = "recursive"))
  sdv <- vapply(1:5, function(t) if (t < 2) 1 else stats::sd(ret[1:t]) + 1e-8, 0)
  expect_equal(ru$raw_error, raw, tolerance = 1e-12)
  expect_equal(ru$intrinsic_reward, raw / sdv, tolerance = 1e-12)
  expect_equal(ru$predictor$W, W, tolerance = 1e-12)
  cr <- morie_rndnet_combine_returns(c(1, 0, 2, 1), c(0.5, 0.2, 0.1, 0.3), gamma_ext = 0.9, gamma_int = 0.8,
                                     done = c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(cr$return_ext, c(1, 0, 2 + 0.9, 1), tolerance = 1e-12)
  expect_equal(cr$return_int, rev(as.numeric(stats::filter(rev(c(0.5, 0.2, 0.1, 0.3)), 0.8, method = "recursive"))),
               tolerance = 1e-12)
  expect_error(morie_rndnet_combine_returns(1:2, 1), "same length")
  expect_error(morie_rndnet(X, init_steps = 5), "init_steps")
  expect_error(morie_rndnet(X, clip = 0), "clip must be > 0")
})

test_that("robust PCA pieces: univariate MCD, classification and the clean-data limit", {
  v <- c(1.2, 1.5, 1.1, 1.4, 1.3, 9.0, 1.6, -5.0, 1.25)
  h <- 5
  s <- sort(v)
  ss <- vapply(1:(9 - h + 1), function(i) sum((s[i:(i + h - 1)] - mean(s[i:(i + h - 1)]))^2), 0)
  i0 <- which.min(ss)
  u <- univariate_mcd(v, h = h, consistent = FALSE)
  expect_equal(unname(u["loc"]), mean(s[i0:(i0 + h - 1)]), tolerance = 1e-12)
  expect_equal(unname(u["scale"]), stats::sd(s[i0:(i0 + h - 1)]), tolerance = 1e-12)
  uc <- univariate_mcd(v, h = h)
  a <- h / 9
  expect_equal(unname(uc["scale"]), unname(u["scale"]) * sqrt(a / stats::pchisq(stats::qchisq(a, 1), 3)),
               tolerance = 1e-12)
  expect_error(univariate_mcd(1), "two values")
  expect_error(univariate_mcd(v, h = 1), "h must lie")
  cl <- classify_outliers(c(1, 3, 1, 3), c(1, 1, 4, 4), 2, 2)
  expect_equal(cl, c("regular", "good leverage", "orthogonal outlier", "bad leverage"))
  X <- cbind(c(1.2, 2.3, 3.1, 4.8, 5.0, 6.7, 7.2, 8.1, 2.9, 4.4),
             c(2.0, 1.5, 3.9, 3.1, 5.5, 4.2, 6.8, 6.1, 2.2, 3.7),
             c(0.3, -0.2, 0.9, 0.1, 1.4, 0.2, 1.1, 0.7, 0.4, 0.5))
  # alpha = 1 keeps every point, so without reweighting ROBPCA is classical PCA
  r <- morie_robpca(X, k = 2, alpha = 1, reweight = FALSE)
  pc <- stats::prcomp(X)
  expect_equal(r$eigenvalues, pc$sdev[1:2]^2, tolerance = 1e-8)
  expect_equal(r$center, colMeans(X), tolerance = 1e-10)
  expect_equal(unname(abs(r$loadings %*% pc$rotation[, 1:2])), diag(2), tolerance = 1e-8)
  expect_equal(r$loadings %*% t(r$loadings), diag(2), tolerance = 1e-10)
  fit <- matrix(r$center, 10, 3, byrow = TRUE) + r$scores %*% r$loadings
  expect_equal(r$orthogonal_distance, sqrt(rowSums((X - fit)^2)), tolerance = 1e-10)
  expect_equal(r$score_distance, sqrt(rowSums(sweep(r$scores^2, 2, r$eigenvalues, "/"))), tolerance = 1e-10)
  expect_equal(r$sd_cutoff, sqrt(stats::qchisq(0.975, 2)), tolerance = 1e-12)
  expect_error(morie_robpca(X, alpha = 0.4), "alpha must lie")
  expect_error(morie_robpca(X[1:2, ]), "at least three")
})

test_that("MAD and winsorizing helpers", {
  x <- c(2.1, NA, 3.5, 1.2, 9.8, 2.9, 3.3, 0.4, 4.1, 2.2, 3.0)
  v <- x[!is.na(x)]
  expect_equal(morie_mad(x), stats::mad(v, constant = 1), tolerance = 1e-12)
  expect_equal(morie_mad_rescaled(x), stats::mad(v), tolerance = 1e-12)
  expect_equal(morie_mad_rescaled(x, constant = 2), 2 * stats::mad(v, constant = 1), tolerance = 1e-12)
  w <- morie_winsorize(x, tr = 0.2)
  g <- floor(0.2 * 10)
  s <- sort(v)
  expect_equal(w, pmin(pmax(v, s[g + 1]), s[10 - g]))
  expect_equal(morie_winsorized_mean(x, 0.2), mean(pmin(pmax(v, s[g + 1]), s[10 - g])), tolerance = 1e-12)
  skip_if_not_installed("WRS2")
  expect_equal(morie_winsorized_mean(v, 0.2), WRS2::winmean(v, 0.2), tolerance = 1e-12)
  expect_equal(morie_winsorize(v, 0), v)
})
