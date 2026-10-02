# Coverage for tmlbas .. tmlinf exports. Every expectation is recomputed in
# the test body; the propensity and outcome fits come from glm/lm, and the
# logistic fluctuations are solved here by Newton on the score equations.

tml_data <- function() {
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7)
  D <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0)
  y <- c(2.1, 1.0, 2.9, 1.4, 0.3, 2.2, 1.9, 0.8, 3.0, 1.6, 0.9, 1.5)
  list(x = x, D = D, y = y)
}

tml_fluct <- function(Y, off, H) {
  H <- as.matrix(H)
  e <- rep(0, ncol(H))
  for (i in 1:200) {
    mu <- stats::plogis(off + as.numeric(H %*% e))
    st <- solve(crossprod(H * (mu * (1 - mu)), H), crossprod(H, Y - mu))
    e <- e + as.numeric(st)
    if (max(abs(st)) < 1e-14) break
  }
  e
}

tml_linear <- function(y, D, W, gbound = 0.025) {
  # the toy design separates a few points; the g-model is truncated at gbound for exactly that
  g <- stats::fitted(suppressWarnings(stats::glm(D ~ 0 + W, family = stats::binomial(), control = list(epsilon = 1e-14, maxit = 100))))
  g <- pmin(pmax(g, gbound), 1 - gbound)
  qb <- stats::coef(stats::lm(y ~ 0 + D + W))
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  Q <- ifelse(D == 1, Q1, Q0)
  H <- D / g - (1 - D) / (1 - g)
  eps <- sum(H * (y - Q)) / sum(H^2)
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  psi <- mean(Q1s - Q0s)
  ic <- H * (y - Q - eps * H) + Q1s - Q0s - psi
  list(psi = unname(psi), se = unname(stats::sd(ic) / sqrt(length(y))), eps = unname(eps), ic = unname(ic))
}

test_that("Tmlbas, Tmlfed and Tmldis", {
  d <- tml_data()
  bl <- c(1.2, 0.8, 1.5, 1.0, 0.4, 1.3, 1.1, 0.6, 1.7, 0.9, 0.5, 1.0)
  r <- Tmlbas(d$y, d$D, d$x, bl)
  ref <- tml_linear(d$y, d$D, cbind(1, d$x, bl))
  # the package IRLS runs 25 ridge-1e-8 steps against glm's converged fit
  expect_equal(r$estimate, ref$psi, tolerance = 1e-7)
  expect_equal(r$se, ref$se, tolerance = 1e-7)
  expect_equal(r$eps, ref$eps, tolerance = 1e-7)
  site <- rep(1:2, each = 6)
  f <- Tmlfed(d$y, d$D, d$x, site)
  s <- lapply(1:2, function(k) {
    i <- site == k
    tml_linear(d$y[i], d$D[i], cbind(1, d$x[i]))
  })
  w <- vapply(s, function(z) 6 / stats::var(z$ic), 0)
  ps <- vapply(s, function(z) z$psi, 0)
  expect_equal(f$site_psi, ps, tolerance = 1e-7)
  expect_equal(f$estimate, sum(w * ps) / sum(w), tolerance = 1e-7)
  expect_equal(f$se, sqrt(1 / sum(w)), tolerance = 1e-7)
  S <- d$D
  t <- Tmldis(d$y, S, d$x)
  b1 <- stats::coef(stats::lm(d$y[S == 1] ~ d$x[S == 1]))
  std <- mean(b1[1] + b1[2] * d$x[S == 0])
  expect_equal(t$estimate, mean(d$y[S == 1]) - std, tolerance = 1e-10)
  expect_equal(t$crude, mean(d$y[S == 1]) - mean(d$y[S == 0]), tolerance = 1e-12)
  res <- stats::residuals(stats::lm(d$y[S == 1] ~ d$x[S == 1]))
  expect_equal(t$se, stats::sd(res) / sqrt(6), tolerance = 1e-10)
  tt <- Tmldis(d$y, S, d$x, X_target = c(0, 1))
  expect_equal(tt$estimate, mean(d$y[S == 1]) - (b1[[1]] + b1[[2]] / 2), tolerance = 1e-10)
})

tml_bin <- function() {
  d <- tml_data()
  Y <- c(1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1)
  Q1 <- stats::plogis(0.2 + 0.8 * d$x)
  Q0 <- stats::plogis(-0.4 + 0.6 * d$x)
  g1 <- stats::plogis(0.1 + 0.5 * d$x)
  list(Y = Y, A = d$D, Q1 = Q1, Q0 = Q0, QA = ifelse(d$D == 1, Q1, Q0), g1 = g1, x = d$x)
}

tml_target <- function(Y, A, QA, Q1, Q0, g1) {
  H1 <- A / g1
  H0 <- (1 - A) / (1 - g1)
  e <- tml_fluct(Y, stats::qlogis(QA), cbind(H0, H1))
  list(Q1s = stats::plogis(stats::qlogis(Q1) + e[2] / g1), Q0s = stats::plogis(stats::qlogis(Q0) + e[1] / (1 - g1)),
       QAs = stats::plogis(stats::qlogis(QA) + e[1] * H0 + e[2] * H1), e = e)
}

test_that("Tmleboot and Tmleinf give influence-curve and bootstrap intervals", {
  b <- tml_bin()
  r <- Tmleboot(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1, B = 5, seed = 2)
  f <- tml_target(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1)
  psi <- mean(f$Q1s) - mean(f$Q0s)
  ic <- b$A / b$g1 * (b$Y - f$QAs) + f$Q1s - mean(f$Q1s) -
    ((1 - b$A) / (1 - b$g1) * (b$Y - f$QAs) + f$Q0s - mean(f$Q0s))
  expect_equal(r$estimate, psi, tolerance = 1e-10)
  expect_equal(r$ic_se, stats::sd(ic) / sqrt(12), tolerance = 1e-10)
  s <- 2
  reps <- numeric(0)
  for (k in 1:5) {
    idx <- integer(12)
    for (i in 1:12) {
      s <- (48271 * s) %% 2147483647
      idx[i] <- min(floor(s / 2147483647 * 12), 11) + 1
    }
    if (length(unique(b$A[idx])) < 2) next
    fb <- tml_target(b$Y[idx], b$A[idx], b$QA[idx], b$Q1[idx], b$Q0[idx], b$g1[idx])
    reps <- c(reps, mean(fb$Q1s) - mean(fb$Q0s))
  }
  expect_equal(r$boot_mean, mean(reps), tolerance = 1e-10)
  expect_equal(r$boot_se, stats::sd(reps), tolerance = 1e-10)
  expect_equal(r$B, length(reps))
  expect_error(Tmleboot(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1, B = 1), "B must be at least 2")
  expect_error(Tmleboot(b$Y, b$A[-1], b$QA, b$Q1, b$Q0, b$g1), "one entry per observation")
  i <- Tmleinf(0.3, ic, level = 0.9, null_value = 0.1)
  se <- stats::sd(ic) / sqrt(12)
  expect_equal(i$se, se, tolerance = 1e-12)
  expect_equal(i$ci_lower, 0.3 - stats::qnorm(0.95) * se, tolerance = 1e-12)
  expect_equal(i$p_value, 2 * stats::pnorm(-abs(0.2 / se)), tolerance = 1e-12)
  expect_equal(i$score_solved, 1)
  expect_error(Tmleinf(0.3, 1), "at least two")
  expect_error(Tmleinf(0.3, ic, level = 1), "strictly between")
})

test_that("Tmlecat fluctuates each treatment level", {
  b <- tml_bin()
  A <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 3, 1, 2)
  Q <- cbind(b$Q0, b$Q1, stats::plogis(0.5 * b$x))
  G <- cbind(rep(0.3, 12), rep(0.3, 12), rep(0.4, 12))
  r <- Tmlecat(b$Y, A, Q, G, ref = 1)
  QA <- Q[cbind(1:12, A)]
  psi <- ics <- NULL
  for (a in 1:3) {
    H <- (A == a) / G[, a]
    e <- stats::uniroot(function(e) sum(H * (b$Y - stats::plogis(stats::qlogis(QA) + e * H))),
                        c(-20, 20), tol = 1e-14)$root
    Qas <- stats::plogis(stats::qlogis(Q[, a]) + e / G[, a])
    psi <- c(psi, mean(Qas))
    ics <- cbind(ics, H * (b$Y - stats::plogis(stats::qlogis(QA) + e * H)) + Qas - mean(Qas))
  }
  expect_equal(r$psi, psi, tolerance = 1e-10)
  expect_equal(r$contrast, psi - psi[1], tolerance = 1e-10)
  expect_equal(r$contrast_se, c(0, sqrt(apply(ics[, 2:3] - ics[, 1], 2, stats::var) / 12)), tolerance = 1e-9)
  expect_error(Tmlecat(b$Y, A, Q, G[, 1:2]), "one column per level")
  expect_error(Tmlecat(b$Y, A + 1, Q, G), "one-based level labels")
  expect_error(Tmlecat(b$Y * 2, A, Q, G), "Y must lie")
  expect_error(Tmlecat(b$Y, A, Q, G, ref = 4), "ref must be a level")
})

test_that("Tmlecde targets the controlled direct effect at M = m", {
  b <- tml_bin()
  M <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  hm <- rep(0.6, 12)
  r <- Tmlecde(b$Y, b$A, M, b$QA, b$Q1, b$Q0, b$g1, hm, m = 1)
  H1 <- (M == 1) * b$A / (b$g1 * 0.6)
  H0 <- (M == 1) * (1 - b$A) / ((1 - b$g1) * 0.6)
  e <- tml_fluct(b$Y, stats::qlogis(b$QA), cbind(H0, H1))
  expect_equal(r$epsilon, e, tolerance = 1e-9)
  Q1s <- stats::plogis(stats::qlogis(b$Q1) + e[2] / (b$g1 * 0.6))
  Q0s <- stats::plogis(stats::qlogis(b$Q0) + e[1] / ((1 - b$g1) * 0.6))
  expect_equal(r$estimate, mean(Q1s) - mean(Q0s), tolerance = 1e-10)
  expect_equal(r$max_weight, max(H1, H0), tolerance = 1e-12)
  expect_error(Tmlecde(b$Y, b$A + 1, M, b$QA, b$Q1, b$Q0, b$g1, hm), "binary 0/1")
})

test_that("Comptml targets each clr coordinate", {
  b <- tml_bin()
  Yc <- cbind(1 + b$x^2, 2 + b$Y, 1.5 + 0.2 * seq_len(12))
  L <- log(Yc)
  Z <- L - rowSums(L) / 3
  Q1 <- sweep(matrix(0.1, 12, 3), 2, colMeans(Z), "+")
  Q0 <- sweep(matrix(-0.1, 12, 3), 2, colMeans(Z), "+")
  r <- Comptml(Yc, b$A, Q1, Q0, b$g1)
  eff <- vapply(1:3, function(j) {
    lo <- min(Z[, j], Q1[, j], Q0[, j])
    rg <- max(Z[, j], Q1[, j], Q0[, j]) - lo
    q1 <- pmin(pmax((Q1[, j] - lo) / rg, 1e-6), 1 - 1e-6)
    q0 <- pmin(pmax((Q0[, j] - lo) / rg, 1e-6), 1 - 1e-6)
    f <- tml_target((Z[, j] - lo) / rg, b$A, ifelse(b$A == 1, q1, q0), q1, q0, b$g1)
    (mean(f$Q1s) - mean(f$Q0s)) * rg
  }, 0)
  expect_equal(r$effect, eff - mean(eff), tolerance = 1e-9)
  expect_equal(r$sum_effect, 0, tolerance = 1e-12)
  expect_equal(r$perturbation, exp(r$effect) / sum(exp(r$effect)), tolerance = 1e-12)
  expect_error(Comptml(-Yc, b$A, Q1, Q0, b$g1), "strictly positive")
  expect_error(Comptml(Yc[, 1, drop = FALSE], b$A, Q1, Q0, b$g1), "at least two parts")
})

test_that("Tmleext reweights external controls by the participation odds", {
  d <- tml_data()
  ext <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
  D <- c(1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0)
  r <- Tmleext(d$y, D, d$x, external = ext)
  S <- 1 - ext
  ps <- stats::fitted(stats::glm(S ~ d$x, family = stats::binomial(), control = list(epsilon = 1e-14)))
  w <- ifelse(S == 1, 1, ps / (1 - ps))
  expect_equal(r$ess, sum(w[S == 0])^2 / sum(w[S == 0]^2), tolerance = 1e-8)
  g <- stats::fitted(stats::glm(D ~ d$x, family = stats::binomial(), control = list(epsilon = 1e-14)))
  lo <- min(d$y)
  rg <- max(d$y) - lo
  ys <- (d$y - lo) / rg
  qb <- stats::coef(stats::lm(ys ~ D + d$x))
  q1 <- pmin(pmax(qb[1] + qb[2] + qb[3] * d$x, 1e-8), 1 - 1e-8)
  q0 <- pmin(pmax(qb[1] + qb[3] * d$x, 1e-8), 1 - 1e-8)
  H <- D / g - (1 - D) / (1 - g)
  e <- tml_fluct(ys, stats::qlogis(ifelse(D == 1, q1, q0)), H)
  q1s <- stats::plogis(stats::qlogis(q1) + e / g)
  q0s <- stats::plogis(stats::qlogis(q0) - e / (1 - g))
  # both package fits carry 1e-10 ridges and a 60-step IRLS
  expect_equal(r$estimate, sum(w * (q1s - q0s)) * rg / sum(w), tolerance = 1e-7)
  expect_equal(r$n_external, 4L)
  expect_true(is.finite(r$psi_internal))
  nx <- Tmleext(d$y, d$D, d$x)
  expect_equal(nx$ess, 0)
})

test_that("Tmlhrz targets the marginal hazard ratio at the last event time", {
  tm <- c(2, 5, 3, 4, 1, 5, 4, 3, 2, 5, 4, 1)
  ev <- c(1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1)
  d <- tml_data()
  r <- Tmlhrz(tm, ev, d$D, d$x)
  W <- cbind(1, d$x)
  g <- stats::fitted(stats::glm(d$D ~ d$x, family = stats::binomial(), control = list(epsilon = 1e-14)))
  g <- pmin(pmax(g, 0.025), 0.975)
  grid <- sort(unique(tm[ev == 1]))
  K <- length(grid)
  pp <- do.call(rbind, lapply(1:12, function(i) {
    k <- which(grid <= tm[i])
    cbind(i = i, k = k, y = as.numeric(ev[i] == 1 & grid[k] == tm[i]))
  }))
  hf <- stats::glm(pp[, "y"] ~ grid[pp[, "k"]] + d$D[pp[, "i"]] + d$x[pp[, "i"]], family = stats::binomial(),
                   control = list(epsilon = 1e-14, maxit = 100))
  hb <- stats::coef(hf)
  haz <- function(a) outer(seq_len(12), seq_len(K), function(i, k) pmin(pmax(stats::plogis(hb[1] + hb[2] * grid[k] + hb[3] * a + hb[4] * d$x[i]), 1e-6), 1 - 1e-6))
  h0 <- haz(0)
  h1 <- haz(1)
  surv <- function(h) t(apply(1 - h, 1, cumprod))
  s0 <- surv(h0)
  s1 <- surv(h1)
  lag <- function(s) cbind(1, s[, -K, drop = FALSE])
  H0 <- -(d$D == 0) / (1 - g) * s0[, K] / lag(s0)
  H1 <- -(d$D == 1) / g * s1[, K] / lag(s1)
  ix <- pp[, c("i", "k")]
  Hobs <- ifelse(d$D[ix[, 1]] == 1, H1[ix], H0[ix])
  hobs <- ifelse(d$D[ix[, 1]] == 1, h1[ix], h0[ix])
  e <- tml_fluct(pp[, "y"], stats::qlogis(hobs), Hobs)
  ps0 <- mean(surv(stats::plogis(stats::qlogis(h0) + e * H0))[, K])
  ps1 <- mean(surv(stats::plogis(stats::qlogis(h1) + e * H1))[, K])
  # the package's hazard IRLS takes 25 ridge-1e-8 steps
  expect_equal(r$eps, e, tolerance = 1e-6)
  expect_equal(c(r$s0, r$s1), c(ps0, ps1), tolerance = 1e-7)
  expect_equal(r$estimate, log(ps1) / log(ps0), tolerance = 1e-7)
  expect_equal(r$t0, 5)
  expect_error(Tmlhrz(tm, 0 * ev, d$D, d$x), "no observed events")
  expect_error(Tmlhrz(tm, ev[-1], d$D, d$x), "share one length")
})
