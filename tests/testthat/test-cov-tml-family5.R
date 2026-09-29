# Coverage for tmlqsa .. tmltrn exports. Every expectation is recomputed in
# the test body from glm/lm fits; the package IRLS and least squares carry
# 1e-8 to 1e-10 ridges, hence 1e-7 tolerances on targeted estimates.

tml5_data <- function() {
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7, 0.4, -0.3)
  D <- c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
  y <- c(2.1, 1.0, 2.9, 1.4, 0.3, 2.2, 1.9, 0.8, 3.0, 1.6, 0.9, 1.5, 2.4, 1.1)
  list(x = x, D = D, y = y)
}

tml5_g <- function(D, x, lo = 0.025) {
  g <- stats::fitted(stats::glm(D ~ x, family = stats::binomial(), control = list(epsilon = 1e-14, maxit = 100)))
  unname(pmin(pmax(g, lo), 1 - lo))
}

tml5_bin <- function() {
  d <- tml5_data()
  Q1 <- stats::plogis(0.2 + 0.8 * d$x)
  Q0 <- stats::plogis(-0.4 + 0.6 * d$x)
  list(Y = as.numeric(d$y > 1.5), A = d$D, Q1 = Q1, Q0 = Q0, QA = ifelse(d$D == 1, Q1, Q0),
       g1 = stats::plogis(0.1 + 2.5 * d$x))
}

tml5_target <- function(b, gl) {
  g1 <- pmin(pmax(b$g1, gl), 1 - gl)
  Hm <- cbind((1 - b$A) / (1 - g1), b$A / g1)
  e <- c(0, 0)
  for (i in 1:100) {
    mu <- stats::plogis(stats::qlogis(b$QA) + as.numeric(Hm %*% e))
    st <- solve(crossprod(Hm * (mu * (1 - mu)), Hm), crossprod(Hm, b$Y - mu))
    e <- e + as.numeric(st)
    if (max(abs(st)) < 1e-14) break
  }
  list(e = e, g1 = g1, Hm = Hm, QAs = stats::plogis(stats::qlogis(b$QA) + as.numeric(Hm %*% e)),
       Q1s = stats::plogis(stats::qlogis(b$Q1) + e[2] / g1), Q0s = stats::plogis(stats::qlogis(b$Q0) + e[1] / (1 - g1)))
}

test_that("Tmleqs and Tmlestab track the efficient score and truncation", {
  b <- tml5_bin()
  r <- Tmleqs(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1)
  f <- tml5_target(b, 0.025)
  hh <- b$A / f$g1 - (1 - b$A) / (1 - f$g1)
  s0 <- mean(hh * (b$Y - b$QA) + b$Q1 - b$Q0 - (mean(b$Q1) - mean(b$Q0)))
  psi1 <- mean(f$Q1s) - mean(f$Q0s)
  expect_equal(r$score_init, s0, tolerance = 1e-12)
  expect_equal(r$psi_final, psi1, tolerance = 1e-10)
  expect_equal(r$score_final, 0, tolerance = 1e-10)
  expect_equal(r$shift, psi1 - (mean(b$Q1) - mean(b$Q0)), tolerance = 1e-10)
  s <- Tmlestab(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1, gbounds = c(0.05, 0.2))
  est <- vapply(c(0.05, 0.2), function(gl) {
    t <- tml5_target(b, gl)
    mean(t$Q1s) - mean(t$Q0s)
  }, 0)
  expect_equal(s$estimate, est, tolerance = 1e-10)
  expect_equal(s$n_truncated, c(sum(b$g1 < 0.05 | b$g1 > 0.95), sum(b$g1 < 0.2 | b$g1 > 0.8)))
  expect_equal(s$max_weight[2], max(1 / pmin(pmax(b$g1, 0.2), 0.8)[b$A == 1], 1 / (1 - pmin(pmax(b$g1, 0.2), 0.8))[b$A == 0]),
               tolerance = 1e-12)
  expect_error(Tmlestab(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1, gbounds = 0.6), "strictly between 0 and 0.5")
  expect_error(Tmleqs(b$Y, b$A, b$QA, b$Q1, b$Q0, b$g1[-1]), "one entry per observation")
})

tml5_s03 <- function(y, D, x, trim) {
  g <- stats::fitted(stats::glm(D ~ x, family = stats::binomial(), control = list(epsilon = 1e-14, maxit = 100)))
  if (trim > 0) g <- pmin(pmax(g, trim), 1 - trim)
  lo <- min(y)
  rg <- max(y) - lo
  ys <- (y - lo) / rg
  qb <- stats::coef(stats::lm(ys ~ D + x))
  q1 <- pmin(pmax(qb[1] + qb[2] + qb[3] * x, 1e-8), 1 - 1e-8)
  q0 <- pmin(pmax(qb[1] + qb[3] * x, 1e-8), 1 - 1e-8)
  H <- D / g - (1 - D) / (1 - g)
  qa <- ifelse(D == 1, q1, q0)
  e <- stats::uniroot(function(e) sum(H * (ys - stats::plogis(stats::qlogis(qa) + e * H))), c(-30, 30), tol = 1e-14)$root
  rg * mean(stats::plogis(stats::qlogis(q1) + e / g) - stats::plogis(stats::qlogis(q0) - e / (1 - g)))
}

test_that("Tmlerob bounds the propensity before targeting", {
  d <- tml5_data()
  x2 <- d$x * 3
  r <- Tmlerob(d$y, d$D, x2, trim = 0.1)
  g0 <- stats::fitted(stats::glm(d$D ~ x2, family = stats::binomial(), control = list(epsilon = 1e-14)))
  expect_equal(r$n_trimmed, sum(g0 < 0.1 | g0 > 0.9))
  expect_equal(r$min_g, min(g0), tolerance = 1e-8)
  expect_equal(r$estimate, tml5_s03(d$y, d$D, x2, 0.1), tolerance = 1e-7)
  expect_equal(r$psi_untrimmed, tml5_s03(d$y, d$D, x2, 0), tolerance = 1e-7)
})

test_that("Tmlrct, Tmlsbg and Tmltrn target sub-population effects", {
  d <- tml5_data()
  n1 <- 6
  S <- c(rep(1, n1), rep(0, 8))
  r <- Tmlrct(d$y[1:6], d$y[7:14], d$D, d$x)
  g0 <- mean(d$D[1:6])
  qb <- stats::coef(stats::lm(d$y ~ d$D + d$x + S))
  pt <- 6 / 14
  Qobs <- stats::fitted(stats::lm(d$y ~ d$D + d$x + S))
  H <- S / pt * (d$D / g0 - (1 - d$D) / (1 - g0))
  eps <- sum(H * (d$y - Qobs)) / sum(H^2)
  expect_equal(r$estimate, unname(qb[2]) + eps / pt * (1 / g0 + 1 / (1 - g0)), tolerance = 1e-9)
  expect_equal(r$g_rct, g0)
  expect_error(Tmlrct(d$y[1], d$y[-1], d$D, d$x), "at least two trial rows")
  sg <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1)
  s <- Tmlsbg(d$y, d$D, d$x, sg)
  g <- tml5_g(d$D, d$x)
  f <- stats::lm(d$y ~ d$D + d$x)
  ps <- mean(sg)
  H <- sg / ps * (d$D / g - (1 - d$D) / (1 - g))
  eps <- sum(H * stats::residuals(f)) / sum(H^2)
  est <- sum(sg * (stats::coef(f)[2] + eps / ps * (1 / g + 1 / (1 - g)))) / sum(sg)
  expect_equal(s$estimate, unname(est), tolerance = 1e-7)
  expect_error(Tmlsbg(d$y, d$D, d$x, rep(0, 14)), "subgroup is empty")
  Sv <- c(1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1)
  t <- Tmltrn(d$y, d$D, d$x, Sv)
  p <- tml5_g(Sv, d$x)
  src <- Sv == 1
  gs <- stats::coef(stats::glm(d$D[src] ~ d$x[src], family = stats::binomial(), control = list(epsilon = 1e-14)))
  g2 <- pmin(pmax(stats::plogis(gs[1] + gs[2] * d$x), 0.025), 0.975)
  qs <- stats::coef(stats::lm(d$y[src] ~ d$D[src] + d$x[src]))
  Q1 <- qs[1] + qs[2] + qs[3] * d$x
  Q0 <- qs[1] + qs[3] * d$x
  ptg <- mean(Sv == 0)
  od <- (1 - p) / p
  H <- Sv / ptg * od * (d$D / g2 - (1 - d$D) / (1 - g2))
  eps <- sum((H * (d$y - ifelse(d$D == 1, Q1, Q0)))[src]) / sum(H^2)
  est <- mean((Q1 + eps * od / (ptg * g2) - Q0 + eps * od / (ptg * (1 - g2)))[!src])
  expect_equal(t$estimate, unname(est), tolerance = 1e-7)
  expect_equal(t$n_target, 4L)
})

test_that("Tmlrec and Tmlric", {
  d <- tml5_data()
  ev <- c(2, 0, 3, 1, 0, 2, 1, 0, 4, 1, 1, 0, 2, 1)
  fu <- c(1, 2, 1.5, 1, 2, 1, 1.2, 1.8, 1, 2, 1.1, 1.4, 1.3, 1.6)
  r <- Tmlrec(fu, ev, d$D, d$x)
  rate <- ev / fu
  g <- tml5_g(d$D, d$x)
  f <- stats::lm(rate ~ d$D + d$x)
  qb <- stats::coef(f)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * stats::residuals(f)) / sum(H^2)
  m1 <- mean(qb[1] + qb[2] + qb[3] * d$x + eps / g)
  m0 <- mean(qb[1] + qb[3] * d$x - eps / (1 - g))
  expect_equal(c(r$mu1, r$mu0), c(m1, m0), tolerance = 1e-7)
  expect_equal(r$estimate, m1 / m0, tolerance = 1e-7)
  expect_error(Tmlrec(fu - 2, ev, d$D, d$x), "must be positive")
  y <- as.numeric(d$y > 1.5)
  k <- Tmlric(y, d$D, d$x, prevalence = 0.1)
  n1 <- sum(y)
  w <- ifelse(y == 1, 0.1 * 14 / n1, 0.9 * 14 / (14 - n1))
  g <- tml5_g(d$D, d$x, 0.01)
  qf <- stats::glm(y ~ d$D + d$x, family = stats::binomial(), control = list(epsilon = 1e-14))
  qq <- function(a) pmin(pmax(stats::plogis(stats::coef(qf)[1] + stats::coef(qf)[2] * a + stats::coef(qf)[3] * d$x), 1e-6), 1 - 1e-6)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(w * H * (y - qq(d$D))) / sum(w * H^2)
  psi <- sum(w * (qq(1) + eps / g - qq(0) + eps / (1 - g))) / sum(w)
  expect_equal(k$estimate, unname(psi), tolerance = 1e-7)
})

test_that("Tmlres adds the estimated second-order U-statistic", {
  d <- tml5_data()
  r <- Tmlres(d$y, d$D, d$x)
  g <- tml5_g(d$D, d$x)
  f <- stats::lm(d$y ~ d$D + d$x)
  qb <- stats::coef(f)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * stats::residuals(f)) / sum(H^2)
  psi1 <- mean(qb[2] + eps / g + eps / (1 - g))
  phi <- cbind(1, d$x, d$x^2)
  Kk <- phi %*% solve(crossprod(phi) / 14 + diag(1e-8, 3)) %*% t(phi)
  a <- (d$D - g) / (g * (1 - g))
  bb <- H * (stats::residuals(f) - eps * H)
  M <- outer(a, bb) * Kk
  if22 <- -(sum(M) - sum(diag(M))) / (14 * 13)
  expect_equal(r$psi1, unname(psi1), tolerance = 1e-7)
  expect_equal(r$if22, if22, tolerance = 1e-7)
  expect_equal(r$estimate, unname(psi1) + if22, tolerance = 1e-7)
  expect_error(Tmlres(d$y[1:2], d$D[1:2], d$x[1:2]), ">= 3")
})

test_that("Tmlsbs screens covariates on one half and targets on the other", {
  i <- 1:20
  x <- sin(i * 1.7)
  D <- as.numeric(cos(i * 0.7) > 0)
  y <- 1 + D + x + 0.3 * cos(i)
  X <- cbind(x, as.numeric(sin(i * 0.9) > 0), x^2)
  r <- Tmlsbs(y, D, X)
  sel <- seq(2, 20, by = 2)
  est <- seq(1, 19, by = 2)
  res <- stats::residuals(stats::lm(y[sel] ~ D[sel]))
  sc <- abs(vapply(1:3, function(j) stats::cor(X[sel, j], res), 0))
  keep <- sort(order(-sc)[1:2])
  expect_equal(r$selected, keep)
  W <- X[est, keep, drop = FALSE]
  g <- tml5_g(D[est], W)
  f <- stats::lm(y[est] ~ D[est] + W)
  H <- D[est] / g - (1 - D[est]) / (1 - g)
  eps <- sum(H * stats::residuals(f)) / sum(H^2)
  expect_equal(r$estimate, unname(stats::coef(f)[2]) + mean(eps / g + eps / (1 - g)), tolerance = 1e-7)
  expect_error(Tmlsbs(y[1:7], D[1:7], X[1:7, ]), ">= 8")
})

test_that("Superlrn runs Frank-Wolfe on the candidate simplex", {
  Y <- c(1.2, 0.8, 2.1, 1.5, 0.3, 1.9, 1.1, 0.7)
  Z <- cbind(Y + c(0.3, -0.2, 0.1, 0.4, -0.3, 0.2, -0.1, 0.0), rep(mean(Y), 8), Y * 0.8 + 0.2)
  r <- Superlrn(Z, Y, iters = 50)
  risk <- colMeans((Y - Z)^2)
  a <- as.numeric(seq_len(3) == which.min(risk))
  for (t in 1:50) {
    gr <- -2 / 8 * as.numeric(t(Z) %*% (Y - Z %*% a))
    v <- which.min(gr)
    a <- (1 - 2 / (t + 1)) * a
    a[v] <- a[v] + 2 / (t + 1)
  }
  a <- a / sum(a)
  expect_equal(r$weights, a, tolerance = 1e-12)
  expect_equal(r$risk, risk, tolerance = 1e-12)
  expect_equal(r$sl_risk, mean((Y - Z %*% a)^2), tolerance = 1e-12)
  expect_equal(r$discrete_index, which.min(risk))
  expect_error(Superlrn(Z, Y[-1]), "one entry per observation")
})

test_that("Tmlspl targets a direct effect at the mean neighbour exposure", {
  d <- tml5_data()
  A <- outer(1:14, 1:14, function(a, b) as.numeric((a + b) %% 5 == 0 & a != b))
  r <- Tmlspl(d$y, d$D, d$x, A)
  E <- as.numeric(A %*% d$D) / rowSums(A)
  g <- stats::fitted(stats::glm(d$D ~ d$x + E, family = stats::binomial(), control = list(epsilon = 1e-14)))
  g <- pmin(pmax(g, 0.025), 0.975)
  f <- stats::lm(d$y ~ d$D + d$x + E)
  qb <- stats::coef(f)
  H <- d$D / g - (1 - d$D) / (1 - g)
  eps <- sum(H * stats::residuals(f)) / sum(H^2)
  psi <- mean(qb[2] + eps / g + eps / (1 - g))
  expect_equal(r$ebar, mean(E), tolerance = 1e-12)
  expect_equal(r$estimate, unname(psi), tolerance = 1e-7)
  ic <- H * (stats::residuals(f) - eps * H) + qb[2] + eps / g + eps / (1 - g) - psi
  ce <- ic - mean(ic)
  M <- (A != 0) | diag(14) == 1
  expect_equal(r$se, sqrt(sum(outer(ce, ce) * M)) / 14, tolerance = 1e-7)
  cst <- Tmlspl(d$y, d$D, d$x, A, exposure_summary = function(D, A) as.numeric(A %*% D))
  expect_equal(cst$ebar, mean(A %*% d$D), tolerance = 1e-12)
  expect_error(Tmlspl(d$y, d$D, d$x, A[-1, ]), "n by n")
})
