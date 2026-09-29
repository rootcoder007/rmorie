# Coverage for tmlitr .. tmlmrd exports. Every expectation is recomputed in
# the test body from glm/lm fits. The package's logistic IRLS takes 25
# ridge-1e-8 steps, so fitted propensities agree with glm to ~1e-9 and the
# targeted estimates are compared at 1e-7.

tml3_data <- function() {
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7, 0.4, -0.3)
  D <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
  y <- c(2.1, 1.0, 2.9, 1.4, 0.3, 2.2, 1.9, 0.8, 3.0, 1.6, 0.9, 1.5, 2.4, 1.1)
  list(x = x, D = D, y = y)
}

tml3_glm <- function(f) {
  stats::fitted(stats::glm(f, family = stats::binomial(), control = list(epsilon = 1e-14, maxit = 100)))
}

test_that("Tmlitr targets the value of the estimated rule", {
  d <- tml3_data()
  v <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1)
  r <- Tmlitr(d$y, d$D, d$x, v)
  g <- pmin(pmax(tml3_glm(d$D ~ d$x), 0.025), 0.975)
  qb <- stats::coef(stats::lm(d$y ~ 0 + d$D + I(rep(1, 14)) + d$x + I(d$D * d$x)))
  qh <- function(a) qb[1] * a + qb[2] + qb[3] * d$x + qb[4] * a * d$x
  blip <- qh(1) - qh(0)
  rule <- as.numeric(stats::fitted(stats::lm(blip ~ v)) > 0)
  gd <- ifelse(rule == 1, g, 1 - g)
  H <- (d$D == rule) / gd
  Qobs <- qh(d$D)
  eps <- sum(H * (d$y - Qobs)) / sum(H^2)
  Qds <- ifelse(rule == 1, qh(1), qh(0)) + eps / gd
  expect_equal(r$estimate, mean(Qds), tolerance = 1e-7)
  expect_equal(r$n_treated, sum(rule))
  ic <- H * (d$y - Qobs - eps * H) + Qds - mean(Qds)
  expect_equal(r$se, stats::sd(ic) / sqrt(14), tolerance = 1e-7)
  expect_error(Tmlitr(d$y, d$D[-1], d$x, v), "share one length")
})

test_that("Tmlmct factorises the joint propensity of a treatment vector", {
  d <- tml3_data()
  A <- cbind(d$D, c(0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1))
  r <- Tmlmct(d$y, A, d$x)
  p1 <- tml3_glm(A[, 1] ~ d$x)
  b2 <- stats::coef(stats::glm(A[, 2] ~ d$x + A[, 1], family = stats::binomial(), control = list(epsilon = 1e-14)))
  p2 <- function(a) stats::plogis(b2[1] + b2[2] * d$x + b2[3] * a)
  g1 <- pmin(pmax(p1 * p2(1), 0.025), 0.975)
  g0 <- pmin(pmax((1 - p1) * (1 - p2(0)), 0.025), 0.975)
  qb <- stats::coef(stats::lm(d$y ~ 0 + A + I(rep(1, 14)) + d$x))
  Q1 <- qb[1] + qb[2] + qb[3] + qb[4] * d$x
  Q0 <- qb[3] + qb[4] * d$x
  Qobs <- as.numeric(cbind(A, 1, d$x) %*% qb)
  H <- (A[, 1] == 1 & A[, 2] == 1) / g1 - (A[, 1] == 0 & A[, 2] == 0) / g0
  eps <- sum(H * (d$y - Qobs)) / sum(H^2)
  expect_equal(r$estimate, mean(Q1 + eps / g1 - Q0 + eps / g0), tolerance = 1e-7)
  expect_equal(r$q, 2L)
  expect_error(Tmlmct(d$y, A[-1, ], d$x), "share n rows")
})

test_that("TmlMd splits natural direct and indirect effects", {
  d <- tml3_data()
  M <- c(1.2, 0.4, 1.5, 0.6, 0.2, 1.1, 1.3, 0.5, 1.8, 0.7, 0.9, 0.3, 1.4, 0.6)
  r <- TmlMd(d$y, d$D, M, d$x)
  m0 <- stats::coef(stats::lm(M[d$D == 0] ~ d$x[d$D == 0]))
  m1 <- stats::coef(stats::lm(M[d$D == 1] ~ d$x[d$D == 1]))
  M0 <- m0[1] + m0[2] * d$x
  M1 <- m1[1] + m1[2] * d$x
  qb <- stats::coef(stats::lm(d$y ~ d$D + M + d$x))
  qf <- function(a, m) qb[1] + qb[2] * a + qb[3] * m + qb[4] * d$x
  g <- pmin(pmax(tml3_glm(d$D ~ d$x), 0.025), 0.975)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * (d$y - qf(d$D, M))) / sum(H^2)
  nde <- mean(qf(1, M0) + eps / g - qf(0, M0) + eps / (1 - g))
  nie <- mean(qf(1, M1) - qf(1, M0))
  expect_equal(r$estimate, nde, tolerance = 1e-7)
  expect_equal(r$nie, nie, tolerance = 1e-10)
  expect_equal(r$total, nde + nie, tolerance = 1e-7)
})

test_that("Tmlmda weights by the observation probability", {
  d <- tml3_data()
  mis <- c(0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)
  r <- Tmlmda(d$y, d$D, d$x, mis)
  del <- 1 - mis
  g <- pmin(pmax(tml3_glm(d$D ~ d$x), 0.025), 0.975)
  pb <- stats::coef(stats::glm(del ~ d$D + d$x, family = stats::binomial(), control = list(epsilon = 1e-14)))
  pih <- function(a) pmin(pmax(stats::plogis(pb[1] + pb[2] * a + pb[3] * d$x), 0.025), 1)
  qb <- stats::coef(stats::lm(d$y ~ d$D + d$x, subset = del == 1))
  Q <- function(a) qb[1] + qb[2] * a + qb[3] * d$x
  H <- del / ifelse(d$D == 1, pih(1), pih(0)) * (d$D / g - (1 - d$D) / (1 - g))
  res <- ifelse(del == 1, d$y - Q(d$D), 0)
  eps <- sum(H * res) / sum(H^2)
  psi <- mean(Q(1) + eps / (g * pih(1)) - Q(0) + eps / ((1 - g) * pih(0)))
  # the observation model is nearly separated, so its IRLS stops short of glm
  expect_equal(r$estimate, psi, tolerance = 1e-6)
  expect_equal(r$n_obs, 11)
  expect_error(Tmlmda(d$y, d$D, d$x, rep(1, 14)), "fewer than two observed")
})

test_that("Tmlmlt contrasts each arm with the first", {
  d <- tml3_data()
  arm <- c(2, 0, 1, 1, 0, 2, 1, 2, 0, 1, 2, 0, 0, 1)
  r <- Tmlmlt(d$y, arm, d$x, c(0, 1, 2))
  raw <- vapply(0:2, function(a) pmin(pmax(tml3_glm(as.numeric(arm == a) ~ d$x), 0.01), 0.99), numeric(14))
  g <- pmin(pmax(raw / rowSums(raw), 0.01), 0.99)
  ind <- vapply(0:2, function(a) as.numeric(arm == a), numeric(14))
  qb <- stats::coef(stats::lm(d$y ~ d$x + ind[, 2] + ind[, 3]))
  Qobs <- as.numeric(cbind(1, d$x, ind[, 2:3]) %*% qb)
  psi <- vapply(1:3, function(j) {
    Qj <- qb[1] + qb[2] * d$x + (j == 2) * qb[3] + (j == 3) * qb[4]
    H <- ind[, j] / g[, j]
    e <- sum(H * (d$y - Qobs)) / sum(H^2)
    mean(Qj + e / g[, j])
  }, 0)
  expect_equal(r$psi, psi, tolerance = 1e-7)
  expect_equal(r$estimate, psi[3] - psi[1], tolerance = 1e-7)
  expect_error(Tmlmlt(d$y, arm, d$x, c(0, 1)), "not in arm_set")
  expect_error(Tmlmlt(d$y, arm, d$x, 0), "at least two arms")
})

test_that("Tmlmnl cross-fits its learners over five interleaved folds", {
  d <- tml3_data()
  mq <- function(Xtr, ytr, Xte) rep(mean(ytr), nrow(Xte)) + 0.5 * Xte[, 1]
  mg <- function(Xtr, dtr, Xte) rep(mean(dtr), nrow(Xte))
  r <- Tmlmnl(d$y, d$D, d$x, ml_q = mq, ml_g = mg)
  fold <- (0:13) %% 5
  g <- Qo <- Q1 <- Q0 <- numeric(14)
  for (k in 0:4) {
    te <- fold == k
    g[te] <- min(max(mean(d$D[!te]), 0.025), 0.975)
    Qo[te] <- mean(d$y[!te]) + 0.5 * d$D[te]
    Q1[te] <- mean(d$y[!te]) + 0.5
    Q0[te] <- mean(d$y[!te])
  }
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * (d$y - Qo)) / sum(H^2)
  expect_equal(r$estimate, mean(Q1 + eps / g - Q0 + eps / (1 - g)), tolerance = 1e-12)
  expect_equal(r$eps, eps, tolerance = 1e-12)
  dflt <- Tmlmnl(d$y, d$D, d$x)
  expect_equal(dflt$n_folds, 5L)
  expect_true(is.finite(dflt$estimate))
  expect_error(Tmlmnl(d$y, d$D, d$x[-1]), "one row per subject")
})

test_that("Tmlmpi targets the probabilistic index from targeted CDFs", {
  d <- tml3_data()
  r <- Tmlmpi(d$y, d$D, d$x)
  g <- pmin(pmax(tml3_glm(d$D ~ d$x), 0.025), 0.975)
  grid <- sort(unique(d$y))
  Fm <- sapply(grid, function(t) {
    z <- as.numeric(d$y <= t)
    qb <- stats::coef(stats::lm(z ~ d$D + d$x))
    vapply(0:1, function(a) {
      Q <- qb[1] + qb[2] * a + qb[3] * d$x
      ga <- if (a == 1) g else 1 - g
      H <- (d$D == a) / ga
      e <- sum(H * (z - Q)) / sum(H^2)
      mean(pmin(pmax(Q + e / ga, 0), 1))
    }, 0)
  })
  F0 <- pmin(cummax(Fm[1, ]), 1)
  F1 <- pmin(cummax(Fm[2, ]), 1)
  psi <- sum(0.5 * (c(0, F0[-length(F0)]) + F0) * diff(c(0, F1)))
  expect_equal(r$estimate, psi, tolerance = 1e-7)
  expect_equal(r$n_grid, length(grid))
})

test_that("Tmlerd is the logistic-fluctuation risk difference", {
  d <- tml3_data()
  Y <- as.numeric(d$y > 1.5)
  Q1 <- stats::plogis(0.2 + 0.8 * d$x)
  Q0 <- stats::plogis(-0.4 + 0.6 * d$x)
  g1 <- stats::plogis(0.1 + 0.5 * d$x)
  QA <- ifelse(d$D == 1, Q1, Q0)
  r <- Tmlerd(Y, d$D, QA, Q1, Q0, g1, level = 0.9)
  H1 <- d$D / g1
  H0 <- (1 - d$D) / (1 - g1)
  e <- c(0, 0)
  for (i in 1:100) {
    mu <- stats::plogis(stats::qlogis(QA) + e[1] * H0 + e[2] * H1)
    Hm <- cbind(H0, H1)
    st <- solve(crossprod(Hm * (mu * (1 - mu)), Hm), crossprod(Hm, Y - mu))
    e <- e + as.numeric(st)
    if (max(abs(st)) < 1e-14) break
  }
  expect_equal(r$epsilon, e, tolerance = 1e-9)
  mu1 <- mean(stats::plogis(stats::qlogis(Q1) + e[2] / g1))
  mu0 <- mean(stats::plogis(stats::qlogis(Q0) + e[1] / (1 - g1)))
  expect_equal(r$estimate, mu1 - mu0, tolerance = 1e-10)
  expect_equal(r$psi_init, mean(Q1) - mean(Q0), tolerance = 1e-12)
  expect_equal(r$ic_mean, 0, tolerance = 1e-10)
  expect_error(Tmlerd(Y + 1, d$D, QA, Q1, Q0, g1), "Y must lie")
  expect_error(Tmlerd(Y, d$D + 1, QA, Q1, Q0, g1), "binary 0/1")
})

test_that("Tmlmpc targets a cause-specific cumulative hazard contrast", {
  tm <- c(2, 5, 3, 4, 1, 5, 4, 3, 2, 5, 4, 1, 3, 2)
  st <- c(1, 0, 2, 1, 1, 0, 2, 1, 0, 1, 1, 2, 1, 0)
  d <- tml3_data()
  r <- Tmlmpc(tm, st, d$D, d$x)
  g <- pmin(pmax(tml3_glm(d$D ~ d$x), 0.025), 0.975)
  grid <- sort(unique(tm[st > 0]))
  K <- length(grid)
  pp <- do.call(rbind, lapply(1:14, function(i) cbind(i = i, k = which(grid <= tm[i]))))
  ii <- pp[, 1]
  kk <- pp[, 2]
  fitb <- function(yb) stats::coef(stats::glm(yb ~ grid[kk] + d$D[ii] + d$x[ii], family = stats::binomial(),
                                              control = list(epsilon = 1e-14, maxit = 100)))
  last <- grid[kk] == tm[ii]
  b1 <- fitb(as.numeric(st[ii] == 1 & last))
  b2 <- fitb(as.numeric(st[ii] == 2 & last))
  bc <- fitb(as.numeric(st[ii] == 0 & last))
  hz <- function(b, a) outer(1:14, 1:K, function(i, k) pmin(pmax(stats::plogis(b[1] + b[2] * grid[k] + b[3] * a + b[4] * d$x[i]), 1e-8), 1 - 1e-8))
  risk <- function(a) {
    s <- (1 - pmin(pmax(hz(b1, a) + hz(b2, a), 1e-8), 1 - 1e-8)) * (1 - hz(bc, a))
    cbind(1, t(apply(s, 1, cumprod))[, -K, drop = FALSE])
  }
  hobs <- hz(b1, 1) * d$D + hz(b1, 0) * (1 - d$D)
  yb <- as.numeric(st[ii] == 1 & last)
  lam <- vapply(0:1, function(a) {
    ga <- if (a == 1) g else 1 - g
    Hf <- (d$D == a) / ga / pmax(risk(a), 1e-8)
    hv <- Hf[cbind(ii, kk)]
    ho <- hobs[cbind(ii, kk)]
    e <- stats::uniroot(function(e) sum(hv * (yb - stats::plogis(stats::qlogis(ho) + e * hv))), c(-10, 10), tol = 1e-14)$root
    mean(rowSums(stats::plogis(stats::qlogis(hz(b1, a)) + e * Hf)))
  }, 0)
  expect_equal(c(r$lam0, r$lam1), lam, tolerance = 1e-6)
  expect_equal(r$estimate, lam[2] - lam[1], tolerance = 1e-6)
  expect_equal(r$t0, 5)
  expect_error(Tmlmpc(tm, 0 * st, d$D, d$x), "no observed transitions")
})
