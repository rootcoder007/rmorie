# Coverage for item information, weighting-class nonresponse adjustment,
# DR-DiD treatment rules and interactions, iterative Q-learning, the
# survey jackknife, Jaro-Winkler and join counts; recomputed with glm(),
# lm() and spdep in the test body.

drdid <- function(dy, D, X) {
  n <- length(dy)
  Z <- if (is.null(X)) matrix(1, n, 1) else cbind(1, X)
  pi <- if (ncol(Z) == 1) rep(mean(D), n) else fitted(glm(D ~ Z - 1, family = binomial))
  b0 <- qr.solve(Z[D == 0, , drop = FALSE], dy[D == 0])
  mu0 <- as.numeric(Z %*% b0)
  w1 <- D / sum(D)
  q <- pi * (1 - D) / (1 - pi)
  w0 <- q / sum(q)
  tau <- sum((w1 - w0) * (dy - mu0))
  inf <- n * (w1 - w0) * (dy - mu0) - tau
  list(tau = tau, se = sqrt(sum(inf^2)) / n)
}

test_that("Itinft sums a^2 P (1 - P) over items", {
  th <- c(-1, 0, 1.5)
  a <- c(1.2, 0.8)
  b <- c(0, 1)
  r <- Itinft(th, a, b)
  I <- outer(th, 1:2, function(t, j) a[j]^2 * plogis(a[j] * (t - b[j])) * (1 - plogis(a[j] * (t - b[j]))))
  expect_equal(r$information, I, tolerance = 1e-12)
  expect_equal(r$se, 1 / sqrt(rowSums(I)), tolerance = 1e-12)
  expect_equal(r$estimate, max(rowSums(I)), tolerance = 1e-12)
  expect_error(Itinft(th, c(1, -1), b), "positive")
  expect_error(Itinft(th, 1, b), "different lengths")
})

test_that("Itnnrs inflates respondents' weights within classes", {
  y <- c(3, 5, NA, 2, 8, NA, 4, 6)
  R <- as.numeric(!is.na(y))
  y[is.na(y)] <- 0
  X <- c(1, 1, 1, 2, 2, 2, 2, 1)
  d <- c(1, 2, 1, 1, 1, 3, 2, 1)
  r <- Itnnrs(y, R, X, d)
  f <- ave(d, X, FUN = sum) / ave(d * R, X, FUN = sum)
  aw <- d * f
  est <- sum((aw * y)[R == 1]) / sum(aw[R == 1])
  expect_equal(r$estimate, est, tolerance = 1e-12)
  expect_equal(r$se, sqrt(sum((aw * (y - est))[R == 1]^2)) / sum(aw[R == 1]), tolerance = 1e-12)
  expect_equal(r$adjusted_total, sum(d), tolerance = 1e-12)
  expect_equal(r$n_classes, 2L)
  expect_equal(Itnnrs(y, R, NULL)$estimate, mean(y[R == 1]), tolerance = 1e-12)
  expect_error(Itnnrs(y, c(0, 0, 0, 1, 1, 1, 1, 0), X), "no respondents")
})

test_that("Itrct1 contrasts DR-DiD effects across levels and Itr2dd picks the best threshold rule", {
  set.seed(1)
  n <- 80
  X <- cbind(rnorm(n), rnorm(n))
  D <- rbinom(n, 1, plogis(0.3 * X[, 1]))
  V <- rep(0:1, 40)
  dy <- 0.5 * X[, 1] + D * (1 + V) + rnorm(n, sd = 0.5)
  r <- Itrct1(dy, D, V, X)
  a0 <- drdid(dy[V == 0], D[V == 0], X[V == 0, ])
  a1 <- drdid(dy[V == 1], D[V == 1], X[V == 1, ])
  expect_equal(r$att, c(a0$tau, a1$tau), tolerance = 1e-8)
  expect_equal(r$estimate, a1$tau - a0$tau, tolerance = 1e-8)
  expect_equal(r$se, sqrt(a0$se^2 + a1$se^2), tolerance = 1e-8)
  expect_equal(r$att_overall, drdid(dy, D, X)$tau, tolerance = 1e-8)
  expect_error(Itrct1(dy, D, c(rep(2, 79), 3), X), "only one treatment arm")

  it <- Itr2dd(dy, D, X, min_frac = 0.25)
  best <- -Inf
  for (j in 1:2) for (cc in unique(quantile(X[, j], seq(0.1, 0.9, 0.1), names = FALSE))) {
    idx <- which(X[, j] > cc)
    if (length(idx) < 20 || length(idx) > 60 || sum(D[idx]) %in% c(0, length(idx))) next
    tt <- drdid(dy[idx], D[idx], X[idx, ])$tau
    if (tt > best) {
      best <- tt
      bj <- j
      bc <- cc
    }
  }
  expect_equal(it$estimate, best, tolerance = 1e-8)
  expect_equal(c(it$feature, it$threshold), c(bj - 1, bc), tolerance = 1e-12)
  expect_equal(it$gain, best - drdid(dy, D, X)$tau, tolerance = 1e-8)
  expect_error(Itr2dd(dy, D, X, min_frac = 1), "min_frac")
})

test_that("Itrlrn runs backward least-squares Q-learning", {
  set.seed(2)
  m <- 30
  S <- matrix(rnorm(2 * m), 2 * m)
  A <- rbinom(2 * m, 1, 0.5)
  tm <- rep(1:2, each = m)
  Rw <- S[, 1] * A + rnorm(2 * m, sd = 0.2)
  r <- Itrlrn(S, A, Rw, tm, gamma = 0.9)
  g2 <- (m + 1):(2 * m)
  f2 <- lm(Rw[g2] ~ S[g2, 1] + A[g2] + I(A[g2] * S[g2, 1]))
  b2 <- coef(f2)
  V2 <- pmax(b2[1] + b2[2] * S[g2, 1], b2[1] + b2[2] * S[g2, 1] + b2[3] + b2[4] * S[g2, 1])
  g1 <- 1:m
  f1 <- lm(Rw[g1] + 0.9 * V2 ~ S[g1, 1] + A[g1] + I(A[g1] * S[g1, 1]))
  b1 <- coef(f1)
  V1 <- pmax(b1[1] + b1[2] * S[g1, 1], b1[1] + b1[2] * S[g1, 1] + b1[3] + b1[4] * S[g1, 1])
  expect_equal(r$stage_value, c(mean(V1), mean(V2)), tolerance = 1e-8)
  expect_equal(r$coef, unname(c(b1, b2)), tolerance = 1e-8)
  expect_equal(r$share_treated[2], mean(b2[3] + b2[4] * S[g2, 1] > 0), tolerance = 1e-12)
  expect_error(Itrlrn(S, A, Rw, c(tm[-1], 3)), "different numbers of records")
  expect_error(Itrlrn(S, A + 1, Rw, tm), "0 or 1")
})

test_that("Jackvar is the delete-one (or supplied-replicate) jackknife of a weighted mean", {
  y <- c(2, 5, 3, 8, 6)
  w <- c(1, 2, 1, 1, 3)
  r <- Jackvar(y, w)
  th <- sum(w * y) / sum(w)
  tr <- vapply(1:5, function(i) sum((w * y)[-i]) / sum(w[-i]), 0)
  expect_equal(r$variance, 4 / 5 * sum((tr - th)^2), tolerance = 1e-12)
  Rm <- rbind(w * c(2, 0, 1, 1, 1), w * c(1, 1, 2, 0, 1), w * c(0, 2, 1, 1, 1))
  rr <- Jackvar(y, w, Rm)
  t3 <- apply(Rm, 1, function(ww) sum(ww * y) / sum(ww))
  expect_equal(rr$variance, 2 / 3 * sum((t3 - th)^2), tolerance = 1e-12)
  expect_equal(Jackvar(y)$variance, var(y) / 5, tolerance = 1e-12)
  expect_error(Jackvar(1), "two observations")
})

test_that("Jarow follows the Jaro-Winkler definition", {
  jw <- function(s, t, p = 0.1) {
    a <- strsplit(s, "")[[1]]
    b <- strsplit(t, "")[[1]]
    win <- max(max(length(a), length(b)) %/% 2 - 1, 0)
    fb <- rep(FALSE, length(b))
    ma <- integer(0)
    for (i in seq_along(a)) for (j in max(1, i - win):min(length(b), i + win)) {
      if (!fb[j] && a[i] == b[j]) {
        fb[j] <- TRUE
        ma <- c(ma, i)
        break
      }
    }
    m <- length(ma)
    tt <- sum(a[ma] != b[fb]) / 2
    ja <- (m / length(a) + m / length(b) + (m - tt) / m) / 3
    l <- 0
    for (i in 1:min(4, length(a), length(b))) if (a[i] == b[i]) l <- l + 1 else break
    c(ja, ja + l * p * (1 - ja))
  }
  for (pr in list(c("MARTHA", "MARHTA"), c("DIXON", "DICKSONX"), c("JELLYFISH", "SMELLYFISH"))) {
    r <- Jarow(pr[1], pr[2])
    ref <- jw(pr[1], pr[2])
    expect_equal(c(r$jaro, r$estimate), ref, tolerance = 1e-12)
  }
  expect_equal(Jarow("MARTHA", "MARHTA")$jaro, (1 + 1 + 5 / 6) / 3, tolerance = 1e-12)
  expect_equal(Jarow("", "abc")$estimate, 0)
  expect_equal(Jarow("abc", "xyz")$estimate, 0)
})

test_that("Joincnt matches spdep::joincount.test", {
  W <- matrix(0, 8, 8)
  for (i in 1:7) W[i, i + 1] <- W[i + 1, i] <- 1
  W[1, 8] <- W[8, 1] <- 1
  W[2, 6] <- W[6, 2] <- 1
  x <- c(1, 1, 0, 1, 0, 0, 1, 0)
  r <- Joincnt(x, W)
  expect_equal(r$BB, sum(W * outer(x, x)) / 2)
  expect_equal(r$WW, sum(W * outer(1 - x, 1 - x)) / 2)
  expect_equal(r$BW, sum(W * outer(x, x, "!=")) / 2)
  skip_if_not_installed("spdep")
  lw <- spdep::mat2listw(W, style = "B")
  jc <- spdep::joincount.test(factor(x, levels = c(0, 1)), lw)
  expect_equal(c(r$WW, r$E_WW, r$V_WW), unname(jc[[1]]$estimate), tolerance = 1e-9)
  expect_equal(c(r$BB, r$E_BB, r$V_BB), unname(jc[[2]]$estimate), tolerance = 1e-9)
  expect_equal(r$z_BB, unname(jc[[2]]$statistic), tolerance = 1e-9)
  expect_error(Joincnt(x, W[1:7, 1:7]), "N x N")
  expect_error(Joincnt(x + 1, W), "0/1")
})
