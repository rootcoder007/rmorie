# Coverage for the logistic log-likelihood, SIR Metropolis, Linformer
# attention, LINE embeddings, IRT linking (moment, Haebara, Stocking-Lord),
# link prediction, the bivariate linear model, linear Thompson sampling,
# linear-blip SNMMs, local Moran, Ljung-Box, a LLaMA block, G^2, the LMM
# form, prediction intervals and least median of squares.

vdc <- function(i, b) {
  k <- i + 1
  f <- 1
  s <- 0
  while (k > 0) {
    f <- f / b
    s <- s + f * (k %% b)
    k <- k %/% b
  }
  s
}

test_that("Lgobj is the logistic log-likelihood and score", {
  X <- cbind(1, c(-1, 0.5, 2, 0.3, -0.7))
  y <- c(0, 1, 1, 0, 0)
  b <- c(-0.2, 0.8)
  r <- Lgobj(y, X, b)
  p <- plogis(X %*% b)[, 1]
  expect_equal(r$loglik, sum(dbinom(y, 1, p, log = TRUE)), tolerance = 1e-12)
  expect_equal(r$gradient, as.numeric(crossprod(X, y - p)), tolerance = 1e-12)
  expect_error(Lgobj(y + 1, X, b), "0 or 1")
  expect_error(Lgobj(y, X, 1), "columns of X")
})

test_that("morie_likemc replays its random-walk Metropolis chain", {
  y <- c(3, 5, 8, 11, 9, 6)
  mod <- list(S0 = 99, I0 = 1, N = 100)
  r <- morie_likemc(mod, y, list(), n_iter = 6, seed = 3, step = 0.2)
  inc <- function(b, g) {
    S <- 99
    I <- 1
    out <- numeric(6)
    for (k in 1:6) {
      lam <- max(b * S * I / 100, 1e-12)
      out[k] <- lam
      S <- max(S - lam, 0)
      I <- max(I + lam - g * I, 0)
    }
    out
  }
  lp <- function(b, g) sum(dpois(y, inc(b, g), log = TRUE)) + dlnorm(b, log(0.5), 1, log = TRUE) +
    dlnorm(g, log(0.2), 1, log = TRUE)
  e <- .ghc_rng(3)
  u <- function() .ghc_unif(e, 1)
  rn <- function() {
    u1 <- u()
    u2 <- u()
    sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  }
  b <- 0.5
  g <- 0.2
  cur <- lp(b, g)
  ch <- matrix(0, 6, 2)
  for (k in 1:6) {
    pb <- b * exp(0.2 * rn())
    pg <- g * exp(0.2 * rn())
    pl <- lp(pb, pg)
    if (log(u()) < pl - cur) {
      b <- pb
      g <- pg
      cur <- pl
    }
    ch[k, ] <- c(b, g)
  }
  got <- t(vapply(r$chain, function(v) v[1:2], numeric(2)))
  expect_equal(got, ch, tolerance = 1e-12)
  expect_equal(r$logpost_final, cur, tolerance = 1e-9)
  expect_equal(r$R0_mean, mean(ch[, 1] / ch[, 2]), tolerance = 1e-12)
  expect_error(morie_likemc(mod, 1, list(), 5), "two observed counts")
  expect_error(morie_likemc(mod, y, list(), 5, burn = 5), "burn-in")
})

test_that("Linatt projects keys and values before softmax attention", {
  set.seed(1)
  Q <- matrix(rnorm(8), 4)
  K <- matrix(rnorm(8), 4)
  V <- matrix(rnorm(12), 4)
  E <- matrix(rnorm(8), 2)
  F <- matrix(rnorm(8), 2)
  r <- Linatt(Q, K, V, E, F)
  S <- Q %*% t(E %*% K) / sqrt(2)
  P <- exp(S - apply(S, 1, max))
  P <- P / rowSums(P)
  expect_equal(r$weights, P, tolerance = 1e-12)
  expect_equal(r$output, P %*% (F %*% V), tolerance = 1e-12)
  expect_error(Linatt(Q, K[, 1, drop = FALSE], V, E, F), "K width")
  expect_error(Linatt(Q, K, V, E[1, , drop = FALSE], F), "share k")
})

test_that("Lineembed evaluates the LINE objectives and takes gradient steps", {
  W <- matrix(c(0, 1, 1, 0, 1, 0, 2, 0, 1, 2, 0, 1, 0, 0, 1, 0), 4)
  U0 <- outer(1:4, 1:2, function(i, j) vapply((i - 1) * 2 + (j - 1), vdc, 0, b = 2) - 0.5)
  Uc <- outer(1:4, 1:2, function(i, j) vapply((i - 1) * 2 + (j - 1), vdc, 0, b = 3) - 0.5)
  o1 <- function(U) -sum(W * log(plogis(U %*% t(U))))
  r <- Lineembed(W, dim = 2, order = 1)
  expect_equal(r$O_start, o1(U0), tolerance = 1e-12)
  s <- Lineembed(W, dim = 2, order = 1, steps = 2, lr = 0.1)
  U <- U0
  for (st in 1:2) {
    G <- W * (plogis(U %*% t(U)) - 1)
    U <- U - 0.1 * (G %*% U + t(G) %*% U)
  }
  expect_equal(s$U, U, tolerance = 1e-12)
  expect_equal(s$O, o1(U), tolerance = 1e-12)
  r2 <- Lineembed(W, dim = 2, order = 2)
  Lg <- U0 %*% t(Uc)
  lse <- apply(Lg, 1, function(v) log(sum(exp(v))))
  expect_equal(r2$O, -sum(W * (Lg - lse)), tolerance = 1e-12)
})

test_that("the IRT linking functions", {
  af <- c(1.2, 0.8, 1.5, 1.0)
  bf <- c(-0.5, 0.3, 1.1, -1.2)
  A0 <- 1.3
  B0 <- 0.4
  at <- af / A0
  bt <- A0 * bf + B0
  mm <- morie_linkmm(af, bf, at, bt)
  expect_equal(c(mm$A, mm$B), c(mean(af) / mean(at), mean(bt) - mean(af) / mean(at) * mean(bf)), tolerance = 1e-12)
  expect_equal(mm$A, A0, tolerance = 1e-12)
  ms <- morie_linkmm(af, bf, at, bt, method = "mean_sigma")
  expect_equal(c(ms$A, ms$B), c(A0, B0), tolerance = 1e-12)
  expect_error(morie_linkmm(af, bf, at, bt, method = "haebara"), "method must be")
  fr <- cbind(af, bf, c(0.1, 0.2, 0.15, 0))
  to <- cbind(at, bt, fr[, 3])
  # an exact transform makes the criteria vanish at (A0, B0); Nelder-Mead lands within ~1e-6
  h <- morie_linkhae(fr, to)
  expect_equal(c(h$A, h$B), c(A0, B0), tolerance = 1e-5)
  expect_lt(h$criterion, 1e-12)
  hs <- morie_linkhae(fr, to, symmetric = TRUE)
  expect_equal(c(hs$A, hs$B), c(A0, B0), tolerance = 1e-5)
  sl <- morie_linkqp(fr, to)
  expect_equal(c(sl$A, sl$B), c(A0, B0), tolerance = 1e-5)
  pp <- function(th, a, b, c) c + (1 - c) * plogis(a * (th - b))
  g <- seq(-4, 4, length.out = 41)
  crit <- mean(vapply(g, function(t) (sum(pp(t, to[, 1], to[, 2], to[, 3])) -
                                       sum(pp(t, fr[, 1] / sl$A, sl$A * fr[, 2] + sl$B, fr[, 3])))^2, 0))
  expect_equal(sl$criterion, crit, tolerance = 1e-12)
  expect_error(morie_linkqp(fr[1, , drop = FALSE], to[1, , drop = FALSE]), "2 common items")
})

test_that("Linkpr and link_prediction score common neighbours", {
  A <- matrix(0, 6, 6)
  e <- rbind(c(1, 2), c(1, 3), c(2, 3), c(2, 4), c(3, 4), c(4, 5), c(3, 6), c(1, 6))
  A[e] <- 1
  A <- A + t(A)
  r <- Linkpr(A, 2, 6)
  cn <- intersect(which(A[2, ] > 0), which(A[6, ] > 0))
  deg <- rowSums(A)
  expect_equal(r$common_neighbours, cn)
  expect_equal(c(r$cn, r$aa, r$ra), c(length(cn), sum(1 / log(deg[cn])), sum(1 / deg[cn])), tolerance = 1e-12)
  expect_equal(link_prediction(A, 2, 6, "ra")$estimate, sum(1 / deg[cn]), tolerance = 1e-12)
  expect_error(Linkpr(A, 0, 2), "valid node")
  expect_error(Linkpr(A, 1, 2, "jaccard"), "method must be")
})

test_that("LinModel and Lints", {
  m <- LinModel(0.8, 2, 1.5)
  sy <- sqrt(0.64 * 4 + 2.25)
  expect_equal(c(m$sigma_y, m$r), c(sy, 1.6 / sy), tolerance = 1e-12)
  expect_error(LinModel(1, -1, 1), ">= 0")
  X <- rbind(c(1, 0), c(0, 1), c(0.5, 0.5))
  P <- rbind(c(1, 0), c(0, 1), c(1, 0), c(0.5, 0.5))
  rw <- c(1, 0.2, 0.8, 0.5)
  r <- Lints(X, P, rw, R = 0.5, delta = 0.1, z = c(0.3, -1))
  B <- diag(2) + crossprod(P)
  mu <- solve(B, crossprod(P, rw))[, 1]
  v <- 0.5 * sqrt(18 * log(4 / 0.1))
  mt <- mu + v * as.numeric(t(chol(solve(B))) %*% c(0.3, -1))
  expect_equal(r$mu_tilde, mt, tolerance = 1e-12)
  expect_equal(r$arm, which.max(X %*% mt) - 1L)
  lc <- (48271 * 1) %% 2147483647
  z1 <- qnorm(lc / 2147483647)
  z2 <- qnorm(((48271 * lc) %% 2147483647) / 2147483647)
  expect_equal(Lints(X, P, rw)$mu_tilde, mu + v * as.numeric(t(chol(solve(B))) %*% c(z1, z2)), tolerance = 1e-12)
})

test_that("morie_linwlr g-estimates and IPW-fits the linear blip", {
  set.seed(3)
  n <- 100
  W <- rnorm(n)
  A <- rbinom(n, 1, plogis(0.5 * W))
  y <- 1 + W + A * (2 + 0.5 * W) + rnorm(n)
  r <- morie_linwlr(y, A, W)
  pi <- fitted(glm(A ~ W, family = binomial))
  B <- cbind(1, W)
  M <- crossprod(B, (A - pi) * A * B)
  psi <- solve(M + diag(1e-10, 2), crossprod(B, (A - pi) * y))[, 1]
  expect_equal(r$psi, unname(psi), tolerance = 1e-7)
  res <- y - A * as.numeric(B %*% psi)
  bread <- M / n
  meat <- crossprod((A - pi) * res * B) / n
  Vs <- solve(bread) %*% meat %*% t(solve(bread)) / n
  expect_equal(r$psi_se, unname(sqrt(diag(Vs))), tolerance = 1e-6)
  w <- morie_linwlr(y, A, W, method = "wls")
  ww <- ifelse(A == 1, 1 / pi, 1 / (1 - pi))
  f <- lm(y ~ A + I(A * W) + W, weights = ww)
  expect_equal(w$psi, unname(coef(f)[2:3]), tolerance = 1e-7)
  expect_equal(morie_linwlr_blip(A, W, r$psi), A * (r$psi[1] + r$psi[2] * W), tolerance = 1e-12)
  expect_equal(morie_linwlr_blip(A, NULL, 3), 3 * A)
  expect_match(morie_linwlr_cheatsheet(), "blip")
  expect_error(morie_linwlr(y, A, W, method = "dr"), "method must be")
  expect_error(morie_linwlr(y, A, W, propensity = rep(1, n)), "positivity")
})

test_that("Localmoran, Ljungbox and Llgsq", {
  W <- matrix(0, 5, 5)
  for (i in 1:4) W[i, i + 1] <- W[i + 1, i] <- 0.5
  W[1, 2] <- W[5, 4] <- 1
  x <- c(3, 5, 4, 9, 8)
  lm_ <- Localmoran(x, W)
  z <- x - mean(x)
  m2 <- sum(z^2) / 5
  expect_equal(lm_$local, z * (W %*% z)[, 1] / m2, tolerance = 1e-12)
  expect_equal(Localmoran(x, W, mlvar = FALSE)$m2, sum(z^2) / 4, tolerance = 1e-12)
  skip_if_not_installed("spdep")
  lw <- spdep::mat2listw(W, style = "W")
  expect_equal(lm_$local, unname(spdep::localmoran(x, lw)[, "Ii"]), tolerance = 1e-12)
  set.seed(4)
  y <- arima.sim(list(ar = 0.4), 60)
  lb <- Ljungbox(y, lags = 5, fitdf = 1)
  bt <- Box.test(y, lag = 5, type = "Ljung-Box", fitdf = 1)
  expect_equal(lb$statistic, unname(bt$statistic), tolerance = 1e-12)
  expect_equal(lb$p_value, bt$p.value, tolerance = 1e-12)
  expect_error(Ljungbox(1:3, lags = 5), "lags")
  o <- matrix(c(12, 5, 7, 9, 14, 3), 2)
  g <- Llgsq(o)
  e <- outer(rowSums(o), colSums(o)) / sum(o)
  expect_equal(g$g2, 2 * sum(o * log(o / e)), tolerance = 1e-12)
  expect_equal(g$chisq, unname(chisq.test(o)$statistic), tolerance = 1e-12)
  expect_equal(g$pvalue, pchisq(g$g2, 2, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(Llgsq(o, expected = o), "df is required")
})

test_that("Llamablock runs RMSNorm, RoPE attention and a SwiGLU FFN", {
  set.seed(5)
  X <- matrix(rnorm(12), 3)
  mk <- function(r, c) matrix(rnorm(r * c, sd = 0.5), r, c)
  Wq <- mk(4, 4)
  Wk <- mk(4, 4)
  Wv <- mk(4, 4)
  Wo <- mk(4, 4)
  W1 <- mk(4, 6)
  W3 <- mk(4, 6)
  W2 <- mk(6, 4)
  r <- Llamablock(X, Wq = Wq, Wk = Wk, Wv = Wv, Wo = Wo, W1 = W1, W3 = W3, W2 = W2)
  rms <- function(v) v / sqrt(mean(v^2))
  rope <- function(v, pos) {
    out <- v
    for (j in c(1, 3)) {
      th <- pos * 10000^(-(j - 1) / 4)
      out[j] <- v[j] * cos(th) - v[j + 1] * sin(th)
      out[j + 1] <- v[j] * sin(th) + v[j + 1] * cos(th)
    }
    out
  }
  Xn <- t(apply(X, 1, rms))
  q <- t(vapply(1:3, function(t) rope(as.numeric(Xn[t, ] %*% Wq), t - 1), numeric(4)))
  k <- t(vapply(1:3, function(t) rope(as.numeric(Xn[t, ] %*% Wk), t - 1), numeric(4)))
  v <- Xn %*% Wv
  h <- X
  for (t in 1:3) {
    s <- as.numeric(k[1:t, , drop = FALSE] %*% q[t, ]) / 2
    a <- exp(s - max(s)) / sum(exp(s - max(s)))
    h[t, ] <- X[t, ] + as.numeric(colSums(a * v[1:t, , drop = FALSE]) %*% Wo)
  }
  hn <- t(apply(h, 1, rms))
  g <- hn %*% W1
  out <- h + (g * plogis(g) * (hn %*% W3)) %*% W2
  expect_equal(r$h, h, tolerance = 1e-12)
  expect_equal(r$out, out, tolerance = 1e-12)
  r2 <- Llamablock(X, model = list(Wq = Wq, Wk = Wk, Wv = Wv, Wo = Wo, W1 = W1, W3 = W3, W2 = W2))
  expect_equal(r2$out, out, tolerance = 1e-12)
})

test_that("Lmmform and Lmpi", {
  X <- cbind(1, c(0.5, 1, 1.5, 2))
  Z <- rbind(c(1, 0), c(1, 0), c(0, 1), c(0, 1))
  S <- matrix(c(2, 0.5, 0.5, 1), 2)
  r <- Lmmform(X, c(1, 2), Z, c(0.3, -0.2), S)
  expect_equal(r$mean_conditional, as.numeric(X %*% c(1, 2) + Z %*% c(0.3, -0.2)), tolerance = 1e-12)
  expect_equal(r$V, Z %*% S %*% t(Z) + diag(4), tolerance = 1e-12)
  expect_error(Lmmform(X, 1, Z, c(1, 1), S), "match the columns")
  set.seed(6)
  x <- runif(15)
  y <- 1 + 2 * x + rnorm(15, sd = 0.3)
  f <- lm(y ~ x)
  pr <- Lmpi(cbind(1, x), y, c(1, 0.4), level = 0.9)
  ref <- predict(f, data.frame(x = 0.4), interval = "prediction", level = 0.9)
  expect_equal(c(pr$fit, pr$lower, pr$upper), unname(ref[1, ]), tolerance = 1e-9)
  cf <- Lmpi(cbind(1, x), y, c(1, 0.4), level = 0.9, mean = TRUE)
  refc <- predict(f, data.frame(x = 0.4), interval = "confidence", level = 0.9)
  expect_equal(c(cf$lower, cf$upper), unname(refc[1, 2:3]), tolerance = 1e-9)
  expect_error(Lmpi(cbind(1, x), y, 1), "same length as a row")
})

test_that("Lmsreg minimises the median squared residual over elemental fits", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(2.1, 3.9, 6.2, 8.1, 9.8, 30, -5)
  r <- Lmsreg(y, x)
  cand <- y / x
  med <- function(v) {
    s <- sort(v)
    n <- length(s)
    if (n %% 2) s[(n + 1) / 2] else mean(s[n / 2 + 0:1])
  }
  obj <- vapply(cand, function(b) med((y - b * x)^2), 0)
  expect_equal(r$estimate, min(obj), tolerance = 1e-12)
  expect_equal(r$coef, cand[which.min(obj)], tolerance = 1e-12)
  expect_equal(r$scale, 1.483 * sqrt(min(obj)), tolerance = 1e-12)
  ri <- Lmsreg(y, cbind(1, x))
  expect_equal(ri$intercept_col, 0)
  expect_equal(ri$estimate, med(ri$residual^2), tolerance = 1e-12)
  expect_error(Lmsreg(y[1:1], cbind(1, x)[1, , drop = FALSE]), "at least p")
})
