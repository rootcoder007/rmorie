# Coverage for acigls_native.R, bmtme.R and cmaopt.R: the IPW-GLS
# sandwich against sandwich::vcovCL, the BMTME conditional-mean sweeps
# against covariance-form (Kalman gain) posterior means, and CMA-ES
# generations replayed from Hansen's update equations.

test_that("morie_acigls is weighted least squares with a CR1 sandwich", {
  y <- c(2.1, 2.5, 1.9, 3.4, 3.8, 3.1, 3.6, 1.2, 1.5, 2.2)
  A <- cbind(1, c(0.5, 1, 0, 1.5, 2, 0.8, 1.1, -0.5, 0, 0.7))
  w <- c(1, 2, 1.5, 1, 0.5, 2, 1, 1.2, 0.8, 1)
  cl <- c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4)
  f <- lm(y ~ A - 1, weights = w)
  r <- morie_acigls(y, A, w, cl)
  expect_equal(r$coefficients, unname(coef(f)), tolerance = 1e-10)
  expect_equal(r$naive_std_errors, unname(coef(summary(f))[, 2]), tolerance = 1e-10)
  expect_error(morie_acigls(1, 1, 1, 1), "at least two")
  expect_error(morie_acigls(y, A[-1, ], w, cl), "rows but y")
  expect_error(morie_acigls(y, A[, 0], w, cl), "no columns")
  expect_error(morie_acigls(y, A, w[-1], cl), "weights but")
  expect_error(morie_acigls(y, A, -w, cl), "positive")
  expect_error(morie_acigls(y, A, w, cl[-1]), "cluster labels")
  expect_error(morie_acigls(y, A, w, rep(1, 10)), "two clusters")
  expect_error(morie_acigls(y[1:2], A[1:2, ], w[1:2], 1:2), "cannot support")
  expect_error(morie_acigls(y, cbind(A, A[, 2]), w, cl), "singular")
  skip_if_not_installed("sandwich")
  V <- sandwich::vcovCL(f, cluster = cl, type = "HC1", cadjust = TRUE)
  expect_equal(r$vcov, unname(V), tolerance = 1e-10)
  raw <- morie_acigls(y, A, w, cl, small_sample = FALSE)
  expect_equal(raw$vcov, unname(sandwich::vcovCL(f, cluster = cl, type = "HC0", cadjust = FALSE)),
               tolerance = 1e-10)
  expect_equal(r$inflation, r$std_errors / r$naive_std_errors, tolerance = 1e-12)
})

test_that("Bmtme sweeps are the conditional posterior means", {
  G <- rbind(c(1, 0.3, 0.1), c(0.3, 1, 0.2), c(0.1, 0.2, 1))
  Y <- rbind(c(1.2, 0.5), c(0.8, 0.9), c(1.5, 0.2), c(1.1, 0.7), c(0.6, 1.2), c(1.4, 0.4))
  X <- cbind(c(0.2, -0.1, 0.4, 0.3, -0.2, 0.1))
  J <- 3
  I <- 2
  nT <- 2
  N <- 6
  Z1 <- kronecker(matrix(1, I, 1), diag(J))
  ref <- function(sweeps, X = NULL) {
    mu <- c(0, 0)
    b1 <- matrix(0, J, nT)
    b2 <- matrix(0, N, nT)
    beta <- matrix(0, 1, nT)
    St <- diag(nT)
    Se <- diag(I)
    R <- diag(nT)
    fitX <- function() if (is.null(X)) 0 else X %*% beta
    for (s in seq_len(sweeps)) {
      if (!is.null(X)) {
        beta <- solve(crossprod(X), crossprod(X, Y - matrix(mu, N, nT, byrow = TRUE) - Z1 %*% b1 - b2))
      }
      mu <- colMeans(Y - fitX() - Z1 %*% b1 - b2)
      Rr <- Y - matrix(mu, N, nT, byrow = TRUE) - fitX() - b2
      Hm <- kronecker(diag(nT), Z1)
      P <- kronecker(St, G)
      b1 <- matrix(P %*% t(Hm) %*% solve(Hm %*% P %*% t(Hm) + kronecker(R, diag(N)), c(Rr)), J, nT)
      Rr <- Y - matrix(mu, N, nT, byrow = TRUE) - fitX() - Z1 %*% b1
      P2 <- kronecker(St, kronecker(Se, G))
      b2 <- matrix(P2 %*% solve(P2 + kronecker(R, diag(N)), c(Rr)), N, nT)
      SEG <- solve(kronecker(Se, G))
      St <- (t(b1) %*% solve(G, b1) + t(b2) %*% SEG %*% b2 + diag(nT)) / (nT + 2 + J + N - nT - 1)
      B2s <- matrix(0, J * nT, I)
      for (j in 1:J) for (t in 1:nT) for (e in 1:I) B2s[(j - 1) * nT + t, e] <- b2[(e - 1) * J + j, t]
      Se <- (t(B2s) %*% kronecker(solve(G), solve(St)) %*% B2s + diag(I)) / (I + 2 + J * nT - I - 1)
      E <- Y - matrix(mu, N, nT, byrow = TRUE) - fitX() - Z1 %*% b1 - b2
      R <- (crossprod(E) + diag(nT)) / (nT + 2 + N - nT - 1)
    }
    list(b1 = b1, b2 = b2, St = St, Se = Se, R = R, mu = mu, beta = beta)
  }
  e <- ref(2)
  r <- Bmtme(Y, G, n_env = 2, n_iter = 2)
  # the kernel adds a 1e-12 ridge to every solve
  expect_equal(r$b1, e$b1, tolerance = 1e-9)
  expect_equal(r$b2, e$b2, tolerance = 1e-9)
  expect_equal(r$Sigma_T, e$St, tolerance = 1e-9)
  expect_equal(r$Sigma_E, e$Se, tolerance = 1e-9)
  expect_equal(r$R, e$R, tolerance = 1e-9)
  expect_equal(r$gebv, Z1 %*% e$b1 + e$b2, tolerance = 1e-9)
  ex <- ref(1, X)
  rx <- Bmtme(Y, G, 2, n_iter = 1, X = X)
  expect_equal(rx$beta, ex$beta, tolerance = 1e-9)
  expect_equal(rx$mu, ex$mu, tolerance = 1e-9)
  expect_equal(rx$b2, ex$b2, tolerance = 1e-9)
  expect_equal(dim(r$beta), c(0L, 2L))
  expect_error(Bmtme(Y[1, , drop = FALSE], G, 1), "two observations")
  expect_error(Bmtme(Y[, 0], G, 2), "one trait")
  expect_error(Bmtme(Y, G[, 1:2], 2), "square")
  expect_error(Bmtme(Y, G + upper.tri(G), 2), "symmetric")
  expect_error(Bmtme(Y, G, 0), "at least 1")
  expect_error(Bmtme(Y, G, 3), "n_env \\* nrow")
  expect_error(Bmtme(Y, G, 2, X = X[-1, , drop = FALSE]), "different number of rows")
  expect_error(Bmtme(Y, G, 2, v_T = 3), "exceed n_T")
  expect_error(Bmtme(Y, G, 2, v_E = 3), "exceed n_env")
  expect_error(Bmtme(Y, G, 2, n_iter = 0), "at least 1")
})

test_that("cmaopt replays CMA-ES generations", {
  f <- function(v) (1 - v[1])^2 + 5 * (v[2] - v[1]^2)^2
  lam <- 6
  iters <- 3
  Z <- matrix(qnorm(((1:(lam * iters * 2)) * 0.6180339887) %% 1), ncol = 2)
  r <- cmaopt(f, c(-1, 1), sigma = 0.4, Z = Z, lam = lam, iters = iters)
  N <- 2
  mu <- 3
  w <- log(3.5) - log(1:3)
  w <- w / sum(w)
  me <- 1 / sum(w^2)
  cc <- (4 + me / N) / (N + 4 + 2 * me / N)
  cs <- (me + 2) / (N + me + 5)
  c1 <- 2 / ((N + 1.3)^2 + me)
  cmu <- min(1 - c1, 2 * (me - 2 + 1 / me) / ((N + 2)^2 + me))
  damps <- 1 + 2 * max(0, sqrt((me - 1) / (N + 1)) - 1) + cs
  chiN <- sqrt(N) * (1 - 1 / (4 * N) + 1 / (21 * N^2))
  m <- c(-1, 1)
  sg <- 0.4
  C <- diag(2)
  pc <- ps <- c(0, 0)
  best <- Inf
  for (g in 1:iters) {
    ev <- eigen(C, symmetric = TRUE)
    B <- ev$vectors
    Dh <- sqrt(pmax(ev$values, 0))
    Zg <- Z[(g - 1) * lam + 1:lam, ]
    Yg <- Zg %*% B %*% diag(Dh) %*% t(B)
    Xg <- sweep(sg * Yg, 2, m, "+")
    fit <- apply(Xg, 1, f)
    best <- min(best, fit)
    sel <- order(fit)[1:mu]
    mold <- m
    m <- colSums(w * Xg[sel, ])
    dl <- (m - mold) / sg
    ps <- (1 - cs) * ps + sqrt(cs * (2 - cs) * me) * as.numeric(B %*% diag(1 / Dh) %*% t(B) %*% dl)
    hs <- sqrt(sum(ps^2)) / sqrt(1 - (1 - cs)^(2 * g)) / chiN < 1.4 + 2 / 3
    pc <- (1 - cc) * pc + hs * sqrt(cc * (2 - cc) * me) * dl
    Ys <- Yg[sel, ]
    C <- (1 - c1 - cmu) * C + c1 * (outer(pc, pc) + (1 - hs) * cc * (2 - cc) * C) +
      cmu * t(Ys) %*% diag(w) %*% Ys
    sg <- sg * exp(cs / damps * (sqrt(sum(ps^2)) / chiN - 1))
  }
  expect_equal(r$xmean, m, tolerance = 1e-12)
  expect_equal(r$C, C, tolerance = 1e-12)
  expect_equal(r$sigma, sg, tolerance = 1e-12)
  expect_equal(r$fbest, best, tolerance = 1e-12)
  expect_equal(r$evals, 18L)
  z0 <- morie_cma_es(function(v) sum(v^2), c(0, 0), 0.5, matrix(0, 12, 2), lam = 4, iters = 3)
  expect_equal(c(z0$fbest, z0$evals), c(0, 12))
  expect_same_function(morie_cma_es, cmaopt)
})
