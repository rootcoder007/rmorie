# Coverage for product-of-coefficients CIs, mediated interaction, MSM
# mediation, the Mercer check, S/T/X/R metalearners, METEOR, MIRT
# loading conversion, model-based RL, Metropolis-Hastings, MI degrees of
# freedom and Rubin's rules, DIM mutual information, KSG MI, MICE
# initialisation and compensatory MIRT; recomputed in the test body.

vdcb <- function(i, b) {
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

test_that("MedCI uses base-2/base-3 quasi-random normal products", {
  r <- MedCI(0.4, 0.3, 0.1, 0.12, n_sim = 500, level = 0.9)
  za <- qnorm(vapply(0:499, vdcb, 0, b = 2))
  zb <- qnorm(vapply(0:499, vdcb, 0, b = 3))
  pr <- (0.4 + 0.1 * za) * (0.3 + 0.12 * zb)
  expect_equal(c(r$ci_lo, r$ci_hi), unname(quantile(pr, c(0.05, 0.95))), tolerance = 1e-12)
  expect_equal(r$sobel_se, sqrt(0.16 * 0.0144 + 0.09 * 0.01), tolerance = 1e-12)
  expect_equal(r$se_mc, sd(pr), tolerance = 1e-12)
  expect_error(MedCI(1, 1, 0, 1), "strictly positive")
})

test_that("Medint and Medmsm", {
  set.seed(1)
  n <- 80
  X <- rbinom(n, 1, 0.5)
  M <- 0.5 + X + rnorm(n)
  Y <- 1 + X + M + 0.5 * X * M + rnorm(n)
  th <- unname(coef(lm(Y ~ X + M + I(X * M))))
  be <- unname(coef(lm(M ~ X)))
  r <- Medint(Y, X, M, a = 2, astar = 0)
  expect_equal(r$estimate, th[4] * be[2] * 4, tolerance = 1e-9)
  H <- rnorm(n)
  A <- rbinom(n, 1, plogis(0.4 * H))
  Mm <- 0.3 + A + rnorm(n)
  y2 <- A + Mm + 0.3 * A * Mm + H + rnorm(n)
  ms <- Medmsm(y2, A, Mm, H)
  g <- pmin(pmax(fitted(glm(A ~ H, family = binomial)), 0.025), 0.975)
  w <- ifelse(A == 1, 1 / g, 1 / (1 - g))
  t2 <- coef(lm(y2 ~ A + Mm + I(A * Mm), weights = w))
  b2 <- coef(lm(Mm ~ A, weights = w))
  # the internal IRLS runs 25 steps with a 1e-8 ridge
  expect_equal(ms$estimate, unname(t2[2] + t2[4] * b2[1]), tolerance = 1e-6)
  expect_equal(ms$nie, unname(b2[2] * (t2[3] + t2[4])), tolerance = 1e-6)
})

test_that("Mercerchk and Meta1l", {
  K <- matrix(c(2, 1, 0.5, 1, 2, 1, 0.5, 1, 2), 3)
  r <- Mercerchk(K)
  expect_true(r$is_kernel)
  expect_equal(r$min_eigenvalue, min(eigen(K)$values), tolerance = 1e-12)
  expect_false(Mercerchk(matrix(c(1, 2, 2, 1), 2))$is_kernel)
  set.seed(2)
  n <- 60
  X <- matrix(rnorm(2 * n), n)
  w <- rbinom(n, 1, 0.5)
  y <- 1 + X[, 1] + w * (1 + X[, 2]) + rnorm(n)
  m <- Meta1l(y, w, X)
  f1 <- lm(y ~ X, subset = w == 1)
  f0 <- lm(y ~ X, subset = w == 0)
  D <- cbind(1, X)
  expect_equal(m$cate_t, as.numeric(D %*% (coef(f1) - coef(f0))), tolerance = 1e-9)
  expect_equal(m$estimate$s, unname(coef(lm(y ~ X + w))["w"]), tolerance = 1e-9)
  d1 <- y[w == 1] - as.numeric(D[w == 1, ] %*% coef(f0))
  d0 <- as.numeric(D[w == 0, ] %*% coef(f1)) - y[w == 0]
  t1 <- coef(lm(d1 ~ X[w == 1, ]))
  t0 <- coef(lm(d0 ~ X[w == 0, ]))
  expect_equal(m$cate_x, mean(w) * as.numeric(D %*% t0) + (1 - mean(w)) * as.numeric(D %*% t1), tolerance = 1e-9)
  ry <- residuals(lm(y ~ X))
  rw <- residuals(lm(w ~ X))
  expect_equal(m$coef_r, unname(coef(lm(ry ~ 0 + I(D * rw)))), tolerance = 1e-9)
  expect_error(Meta1l(y, w + 1, X), "binary")
})

test_that("Meteor scores exact unigram matches with the fragmentation penalty", {
  r <- Meteor("the cat sat on the mat", "on the mat sat the cat")
  m <- 6
  P <- 1
  R <- 1
  fm <- 10 * P * R / (R + 9 * P)
  cand <- c("the", "cat", "sat", "on", "the", "mat")
  ref <- c("on", "the", "mat", "sat", "the", "cat")
  used <- rep(FALSE, 6)
  rj <- integer(0)
  for (w in cand) {
    j <- which(!used & ref == w)[1]
    used[j] <- TRUE
    rj <- c(rj, j)
  }
  ch <- 1 + sum(diff(rj) != 1)
  expect_equal(r$matches, 6L)
  expect_equal(r$chunks, ch)
  expect_equal(r$score, fm * (1 - 0.5 * (ch / m)^3), tolerance = 1e-12)
  z <- Meteor("a b", "c d")
  expect_equal(z$score, 0)
  u <- Meteor("A B C", c("a", "b", "x", "c"))
  expect_equal(c(u$precision, u$recall, u$chunks), c(1, 3 / 4, 2), tolerance = 1e-12)
  expect_error(Meteor("", "a"), "nonempty")
})

test_that("morie_mfird converts between MIRT slopes and loadings", {
  a <- rbind(c(1.2, 0.3), c(0.5, 0.9))
  P <- matrix(c(1, 0.3, 0.3, 1), 2)
  r <- morie_mfird(a, d = c(-0.5, 0.4), P = P)
  q <- rowSums((a %*% P) * a)
  expect_equal(r$loadings, a / sqrt(1 + q), tolerance = 1e-12)
  expect_equal(r$thresholds, -c(-0.5, 0.4) / sqrt(1 + q), tolerance = 1e-12)
  back <- morie_mfird(r$loadings, P = P, inverse = TRUE)
  expect_equal(back$discriminations, a, tolerance = 1e-12)
  expect_error(morie_mfird(matrix(1, 1, 2), inverse = TRUE), "communality")
})

test_that("Modelrl plans on the maximum-likelihood model", {
  env <- rbind(c(0, 0, 1, 1), c(0, 0, 0, 0), c(0, 1, 0, 2), c(1, 0, 2, 2), c(1, 1, 0, 0),
               c(2, 0, 0, 0), c(2, 0, 1, 2))
  r <- Modelrl(env, gamma = 0.8)
  Rm <- matrix(NA, 3, 2)
  Pl <- array(0, c(3, 2, 3))
  for (s in 0:2) for (a in 0:1) {
    rw <- env[env[, 1] == s & env[, 2] == a, , drop = FALSE]
    if (nrow(rw)) {
      Rm[s + 1, a + 1] <- mean(rw[, 3])
      Pl[s + 1, a + 1, ] <- tabulate(rw[, 4] + 1, 3) / nrow(rw)
    }
  }
  V <- numeric(3)
  for (it in 1:2000) {
    Q <- Rm + 0.8 * apply(Pl, c(1, 2), function(p) sum(p * V))
    V <- apply(Q, 1, max, na.rm = TRUE)
  }
  expect_equal(r$v, V, tolerance = 1e-9)
  expect_equal(r$policy, apply(Q, 1, which.max) - 1L)
  expect_error(Modelrl(env, planner = "pi"), "only planner")
})

test_that("mhmcmc and morie_metropolis_hastings replay the supplied uniforms and increments", {
  tg <- function(x) exp(-0.5 * x^2)
  u <- c(0.2, 0.9, 0.5, 0.1)
  z <- c(0.8, -1.5, 0.3)
  r <- mhmcmc(tg, x0 = 1, n_iter = 6, u = u, z = z, scale = 0.7)
  x <- 1
  ch <- numeric(6)
  for (i in 1:6) {
    p <- x + 0.7 * z[(i - 1) %% 3 + 1]
    if (u[(i - 1) %% 4 + 1] < min(tg(p) / tg(x), 1)) x <- p
    ch[i] <- x
  }
  expect_equal(r$chain, ch, tolerance = 1e-12)
  expect_equal(r$estimate, mean(ch), tolerance = 1e-12)
  q <- function(a, b) dnorm(a, b + 0.1, 1)
  rq <- morie_metropolis_hastings(tg, 0, 4, u, z, q = q, burn = 1)
  x <- 0
  ch2 <- numeric(4)
  for (i in 1:4) {
    p <- x + z[(i - 1) %% 3 + 1]
    rat <- tg(p) / tg(x) * q(x, p) / q(p, x)
    if (u[i] < min(rat, 1)) x <- p
    ch2[i] <- x
  }
  expect_equal(rq$chain, ch2, tolerance = 1e-12)
  expect_equal(rq$estimate, mean(ch2[2:4]), tolerance = 1e-12)
})

test_that("morie_midegf, Mifmi and morie_miefcl implement Rubin's rules", {
  q <- c(1.1, 1.4, 0.9, 1.3, 1.2)
  u <- c(0.04, 0.05, 0.045, 0.05, 0.042)
  r <- morie_miefcl(q, u, nu_com = 50)
  b <- var(q)
  tt <- mean(u) + 1.2 * b
  lam <- 1.2 * b / tt
  dfo <- 4 / lam^2
  nuo <- 51 / 53 * 50 * (1 - lam)
  expect_equal(c(r$estimate, r$t, r$b), c(mean(q), tt, b), tolerance = 1e-12)
  expect_equal(r$df, dfo * nuo / (dfo + nuo), tolerance = 1e-12)
  skip_if_not_installed("mice")
  ps <- mice::pool.scalar(q, u, n = 52, k = 2)
  expect_equal(r$df, ps$df, tolerance = 1e-9)
  expect_equal(r$fmi, ps$fmi, tolerance = 1e-9)
  expect_equal(r$riv, ps$r, tolerance = 1e-12)
  d <- morie_midegf(b, tt, 5, nu_com = 50)
  expect_equal(d$df, r$df, tolerance = 1e-12)
  expect_equal(morie_midegf(b, tt, 5)$df, dfo, tolerance = 1e-12)
  expect_error(morie_midegf(b, tt, 1), "at least 2")
  f <- Mifmi(b, mean(u), 5)
  expect_equal(f$estimate, lam, tolerance = 1e-12)
  rr <- 1.2 * b / mean(u)
  nu <- 4 * (1 + 1 / rr)^2
  expect_equal(f$gamma, (rr + 2 / (nu + 3)) / (rr + 1), tolerance = 1e-12)
  expect_error(morie_miefcl(1, 1), "at least two")
})

test_that("morie_mienco and morie_miest1", {
  cr <- function(s, p) sum(s * p)
  s <- c(1, -0.5)
  pos <- list(c(1, 0), c(0.5, 0.5))
  neg <- list(c(-1, 1), c(0, 2), c(0.2, 0.1))
  r <- morie_mienco(s, pos, neg, cr)
  P <- vapply(pos, cr, 0, s = s)
  N <- vapply(neg, cr, 0, s = s)
  expect_equal(r$estimate, mean(-log1p(exp(-P))) - mean(log1p(exp(N))), tolerance = 1e-12)
  d <- morie_mienco(s, pos, neg, cr, "dv")
  expect_equal(d$estimate, mean(P) - log(mean(exp(N))), tolerance = 1e-12)
  expect_error(morie_mienco(s, pos, neg, cr, "nwj"), "estimator must be")
  set.seed(3)
  x <- rnorm(40)
  y <- x + rnorm(40, sd = 0.5)
  k <- 3
  dx <- abs(outer(x, x, "-"))
  dy <- abs(outer(y, y, "-"))
  dz <- pmax(dx, dy)
  diag(dz) <- Inf
  eps <- apply(dz, 1, function(v) sort(v)[k])
  diag(dx) <- diag(dy) <- Inf
  nx <- rowSums(dx < eps)
  ny <- rowSums(dy < eps)
  mi1 <- digamma(k) - mean(digamma(nx + 1) + digamma(ny + 1)) + digamma(40)
  expect_equal(morie_miest1(x, y, k = 3)$mi, mi1, tolerance = 1e-12)
  idx <- t(apply(dz, 1, function(v) order(v)[1:k]))
  ex <- vapply(1:40, function(i) max(dx[i, idx[i, ]]), 0)
  ey <- vapply(1:40, function(i) max(dy[i, idx[i, ]]), 0)
  mi2 <- digamma(k) - 1 / k - mean(digamma(pmax(rowSums(dx <= ex), 1)) + digamma(pmax(rowSums(dy <= ey), 1))) + digamma(40)
  expect_equal(morie_miest1(x, y, k = 3, algorithm = 2)$mi, mi2, tolerance = 1e-12)
  expect_error(morie_miest1(x, y, k = 40), "1 <= k < n")
})

test_that("morie_miord2 hot-deck starts replay the stream and keep observed cells", {
  D <- cbind(c(1, NA, 3, 4, 5, 6), c(2.1, 3.9, NA, 8.2, NA, 12.1))
  r <- morie_miord2(D, m = 2, maxit = 0, seed = 4)
  e <- .ghc_rng(4)
  for (ch in 1:2) {
    cur <- D
    for (j in 1:2) {
      obs <- D[!is.na(D[, j]), j]
      for (i in which(is.na(D[, j]))) cur[i, j] <- obs[min(floor(.ghc_unif(e, 1) * length(obs)), length(obs) - 1) + 1]
    }
    expect_equal(r$imputations[[ch]], cur)
  }
  f <- morie_miord2(D, m = 2, maxit = 2, seed = 4)
  for (imp in f$imputations) expect_equal(imp[!is.na(D)], D[!is.na(D)])
  expect_false(anyNA(f$imputations[[1]]))
  expect_equal(f$column_means[[2]], colMeans(f$imputations[[2]]))
  expect_error(morie_miord2(cbind(NA, 1:3)), "no observed values")
})

test_that("Mirt2 and Mirt3 are compensatory MIRT likelihoods", {
  th <- rbind(c(0.2, -0.5, 1), c(1, 0.3, -0.2), c(-0.7, 0.8, 0.1))
  y <- c(1, 0, 1)
  a <- c(1.1, 0.6, 0.4)
  r <- Mirt3(y, th, a, d = -0.2, D = 1.702)
  p <- plogis(1.702 * (-0.2 + th %*% a))[, 1]
  expect_equal(r$loglik, sum(dbinom(y, 1, p, log = TRUE)), tolerance = 1e-12)
  g <- Mirt2(y, th[, 1:2], a[1:2], d = 0.1, c = 0.2)
  p2 <- 0.2 + 0.8 * plogis(0.1 + th[, 1:2] %*% a[1:2])[, 1]
  expect_equal(g$p, p2, tolerance = 1e-12)
  expect_error(Mirt3(y, th, a[1:2], 0), "exactly")
  expect_error(Mirt2(y, th, a, 0, c = 1), "c must")
})
