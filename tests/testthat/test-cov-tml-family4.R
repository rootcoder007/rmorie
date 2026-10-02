# Coverage for tmlmro .. tmlqlc exports. Every expectation is recomputed in
# the test body from glm/lm fits and root-found fluctuations. The package's
# IRLS and least squares carry 1e-8 to 1e-10 ridges, hence 1e-7 tolerances
# on targeted estimates.

tml4_data <- function() {
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7, 0.4, -0.3)
  D <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
  y <- c(2.1, 1.0, 2.9, 1.4, 0.3, 2.2, 1.9, 0.8, 3.0, 1.6, 0.9, 1.5, 2.4, 1.1)
  list(x = x, D = D, y = y)
}

tml4_glm <- function(f) {
  # the toy design separates a few points; the g-model is truncated at gbound for exactly that
  stats::fitted(suppressWarnings(stats::glm(f, family = stats::binomial(), control = list(epsilon = 1e-14, maxit = 100))))
}

tml4_linear <- function(y, D, W) {
  g <- pmin(pmax(tml4_glm(D ~ 0 + W), 0.025), 0.975)
  qb <- stats::coef(stats::lm(y ~ 0 + D + W))
  Q1 <- as.numeric(cbind(1, W) %*% qb)
  Q0 <- as.numeric(cbind(0, W) %*% qb)
  H <- D / g - (1 - D) / (1 - g)
  eps <- sum(H * (y - ifelse(D == 1, Q1, Q0))) / sum(H^2)
  psi <- mean(Q1 + eps / g - Q0 + eps / (1 - g))
  ic <- H * (y - ifelse(D == 1, Q1, Q0) - eps * H) + Q1 + eps / g - Q0 + eps / (1 - g) - psi
  list(psi = psi, se = stats::sd(ic) / sqrt(length(y)), eps = eps)
}

tml4_s03 <- function(y, D, x) {
  g <- if (is.null(x)) rep(mean(D), length(y)) else tml4_glm(D ~ x)
  lo <- min(y)
  rg <- max(y) - lo
  ys <- (y - lo) / rg
  qb <- if (is.null(x)) stats::coef(stats::lm(ys ~ D)) else stats::coef(stats::lm(ys ~ D + x))
  xb <- if (is.null(x)) 0 else qb[3] * x
  q1 <- pmin(pmax(qb[1] + qb[2] + xb, 1e-8), 1 - 1e-8)
  q0 <- pmin(pmax(qb[1] + xb, 1e-8), 1 - 1e-8)
  H <- D / g - (1 - D) / (1 - g)
  qa <- ifelse(D == 1, q1, q0)
  e <- stats::uniroot(function(e) sum(H * (ys - stats::plogis(stats::qlogis(qa) + e * H))), c(-30, 30), tol = 1e-14)$root
  q1s <- stats::plogis(stats::qlogis(q1) + e / g)
  q0s <- stats::plogis(stats::qlogis(q0) - e / (1 - g))
  ps <- mean(q1s - q0s)
  inf <- rg * (H * (ys - ifelse(D == 1, q1s, q0s)) + q1s - q0s - ps)
  list(psi = rg * ps, se = sqrt(sum(inf^2)) / length(y), g = unname(g), q1 = unname(q1s), q0 = unname(q0s),
       eps = e, lo = lo, rg = rg)
}

test_that("Tmleor and Tmlerr give targeted odds and risk ratios", {
  d <- tml4_data()
  Y <- as.numeric(d$y > 1.5)
  Q1 <- stats::plogis(0.2 + 0.8 * d$x)
  Q0 <- stats::plogis(-0.4 + 0.6 * d$x)
  g1 <- stats::plogis(0.1 + 0.5 * d$x)
  QA <- ifelse(d$D == 1, Q1, Q0)
  r <- Tmleor(Y, d$D, QA, Q1, Q0, g1)
  H1 <- d$D / g1
  H0 <- (1 - d$D) / (1 - g1)
  Hm <- cbind(H0, H1)
  e <- c(0, 0)
  for (i in 1:100) {
    mu <- stats::plogis(stats::qlogis(QA) + as.numeric(Hm %*% e))
    st <- solve(crossprod(Hm * (mu * (1 - mu)), Hm), crossprod(Hm, Y - mu))
    e <- e + as.numeric(st)
    if (max(abs(st)) < 1e-14) break
  }
  m1 <- mean(stats::plogis(stats::qlogis(Q1) + e[2] / g1))
  m0 <- mean(stats::plogis(stats::qlogis(Q0) + e[1] / (1 - g1)))
  expect_equal(r$estimate, (m1 / (1 - m1)) / (m0 / (1 - m0)), tolerance = 1e-10)
  expect_equal(r$log_or, log(r$estimate), tolerance = 1e-12)
  expect_error(Tmleor(Y, d$D, QA, Q1, Q0, g1[-1]), "one entry per observation")
  rr <- Tmlerr(d$y, d$D, d$x)
  f <- tml4_s03(d$y, d$D, d$x)
  e1 <- mean(f$lo + f$rg * f$q1)
  e0 <- mean(f$lo + f$rg * f$q0)
  expect_equal(rr$ey1, e1, tolerance = 1e-7)
  expect_equal(rr$rr, e1 / e0, tolerance = 1e-7)
  ic <- (d$D / f$g * (d$y - f$lo - f$rg * f$q1) + f$lo + f$rg * f$q1 - e1) / e1 -
    ((1 - d$D) / (1 - f$g) * (d$y - f$lo - f$rg * f$q0) + f$lo + f$rg * f$q0 - e0) / e0
  expect_equal(rr$se_log, sqrt(sum(ic^2)) / 14, tolerance = 1e-7)
})

test_that("Tmlnde2, Tmlnte and Tmlper are linear-fluctuation TMLEs", {
  d <- tml4_data()
  M <- c(1.2, 0.4, 1.5, 0.6, 0.2, 1.1, 1.3, 0.5, 1.8, 0.7, 0.9, 0.3, 1.4, 0.6)
  r <- Tmlnde2(d$y, d$D, M, d$x)
  mb <- stats::coef(stats::lm(M[d$D == 0] ~ d$x[d$D == 0]))
  Mh <- mb[1] + mb[2] * d$x
  qb <- stats::coef(stats::lm(d$y ~ d$D + M + d$x))
  g <- pmin(pmax(tml4_glm(d$D ~ d$x), 0.025), 0.975)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * (d$y - stats::fitted(stats::lm(d$y ~ d$D + M + d$x)))) / sum(H^2)
  psi <- mean(qb[2] + eps / g + eps / (1 - g))
  expect_equal(r$estimate, unname(psi), tolerance = 1e-7)
  expect_equal(r$m_shift, mean(Mh - M), tolerance = 1e-10)
  te <- Tmlnte(d$y, d$D, M, d$x)
  a <- tml4_linear(d$y, d$D, cbind(1, d$x))
  b <- tml4_linear(d$y, d$D, cbind(1, d$x, M))
  expect_equal(te$estimate, a$psi, tolerance = 1e-7)
  expect_equal(te$se, a$se, tolerance = 1e-7)
  expect_equal(te$nde_naive, b$psi, tolerance = 1e-7)
  tt <- seq(0, 13) / 2
  pr <- Tmlper(d$y, d$D, cbind(tt, d$x), period = 4, n_fourier = 1)
  W <- cbind(1, tt, d$x, cos(2 * pi * tt / 4), sin(2 * pi * tt / 4))
  f <- tml4_linear(d$y, d$D, W)
  expect_equal(pr$estimate, f$psi, tolerance = 1e-7)
  expect_equal(pr$n_basis, 2L)
})

test_that("Tmlnsm targets the counterfactual median difference", {
  d <- tml4_data()
  r <- Tmlnsm(d$y, d$D, d$x, bw = 0.5)
  g <- pmin(pmax(tml4_glm(d$D ~ d$x), 0.025), 0.975)
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
  med <- function(Fa) {
    Fa <- pmin(cummax(Fa), 1)
    j <- which(Fa >= 0.5)[1]
    w <- (0.5 - Fa[j - 1]) / (Fa[j] - Fa[j - 1])
    m <- grid[j - 1] + w * (grid[j] - grid[j - 1])
    c(m, sum(diff(c(0, Fa)) * stats::dnorm((m - grid) / 0.5) / 0.5))
  }
  m0 <- med(Fm[1, ])
  m1 <- med(Fm[2, ])
  expect_equal(c(r$m0, r$m1), c(m0[1], m1[1]), tolerance = 1e-7)
  expect_equal(c(r$f0, r$f1), c(m0[2], m1[2]), tolerance = 1e-7)
  expect_equal(r$estimate, m1[1] - m0[1], tolerance = 1e-7)
  expect_error(Tmlnsm(d$y, d$D, d$x, bw = 0), "bw must be positive")
})

test_that("Tmleoutcomeonlyregr is g-computation beside the targeted value", {
  d <- tml4_data()
  r <- Tmleoutcomeonlyregr(d$y, d$D, d$x)
  f <- stats::lm(d$y ~ d$D + d$x)
  expect_equal(r$psi_gcomp, unname(stats::coef(f)[2]), tolerance = 1e-9)
  expect_equal(r$se, 0, tolerance = 1e-12)
  expect_equal(r$rmse_resid, sqrt(mean(stats::residuals(f)^2)), tolerance = 1e-9)
  expect_equal(r$psi_tmle, tml4_s03(d$y, d$D, d$x)$psi, tolerance = 1e-7)
})

test_that("Tmlphd reduces to a mean-only outcome model under a large penalty", {
  d <- tml4_data()
  r <- Tmlphd(d$y, d$D, d$x, lam = 50)
  g <- rep(mean(d$D), 14)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * (d$y - mean(d$y))) / sum(H^2)
  expect_equal(r$nz_q, 0L)
  expect_equal(r$nz_g, 0L)
  expect_equal(r$estimate, mean(eps / g + eps / (1 - g)), tolerance = 1e-8)
  z <- Tmlphd(d$y, d$D, d$x, lam = 0)
  # coordinate descent without a penalty approaches OLS and the glm
  f <- tml4_linear(d$y, d$D, cbind(1, d$x))
  expect_equal(z$estimate, f$psi, tolerance = 1e-6)
  expect_error(Tmlphd(d$y, d$D, d$x, lam = -1), "non-negative")
})

test_that("Tmlepool pools site TMLEs by inverse variance", {
  d <- tml4_data()
  site <- rep(c("a", "b"), each = 7)
  r <- Tmlepool(d$y, d$D, d$x, site = site)
  fs <- lapply(c("a", "b"), function(s) {
    i <- site == s
    tml4_s03(d$y[i], d$D[i], d$x[i])
  })
  ps <- vapply(fs, function(f) f$psi, 0)
  se <- vapply(fs, function(f) f$se, 0)
  w <- 1 / se^2
  pool <- sum(w * ps) / sum(w)
  Q <- sum(w * (ps - pool)^2)
  expect_equal(r$site_psi, ps, tolerance = 1e-7)
  expect_equal(r$estimate, pool, tolerance = 1e-7)
  expect_equal(r$Q, Q, tolerance = 1e-6)
  expect_equal(r$I2, max(0, (Q - 1) / Q), tolerance = 1e-6)
  one <- Tmlepool(d$y, d$D)
  expect_equal(one$estimate, tml4_s03(d$y, d$D, NULL)$psi, tolerance = 1e-7)
})

test_that("Tmlqlc backs targeted Q-functions through the stages", {
  s <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.2, 0.7, -0.4, 1.0, 0.1, -0.6)
  a <- c(1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0)
  rw <- c(1.0, 0.4, 1.5, 0.6, 0.1, 1.2, 0.9, 0.3, 0.8, 0.5, 0.7, 0.2)
  tm <- rep(1:2, each = 6)
  r <- Tmlqlc(s, a, rw, tm)
  V <- rep(0, 6)
  for (t in 2:1) {
    i <- tm == t
    ps <- rw[i] + if (t < 2) V else 0
    st <- s[i]
    at <- a[i]
    qb <- stats::coef(stats::lm(ps ~ st * at))
    q <- function(v) qb[1] + qb[2] * st + qb[3] * v + qb[4] * st * v
    b1 <- pmin(pmax(tml4_glm(at ~ st), 0.025), 0.975)
    as_ <- as.numeric(q(1) >= q(0))
    ba <- ifelse(as_ == 1, b1, 1 - b1)
    H <- (at == as_) / ba
    eps <- sum(H * (ps - q(at))) / sum(H^2)
    V <- ifelse(as_ == 1, q(1), q(0)) + eps / ba
  }
  expect_equal(r$estimate, mean(V), tolerance = 1e-7)
  expect_equal(r$n_stages, 2L)
  expect_error(Tmlqlc(s, a, rw, c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2)), "same number of rows")
})
