# Coverage for r0 .. rdfunc exports. Every expectation is recomputed in
# the test body.

test_that("R0 and R0bayse invert the final-size equation", {
  expect_equal(R0(beta = 0.6, gamma = 0.2)$R0, 3, tolerance = 1e-12)
  ar <- 0.6
  # 1 - ar = exp(-R0 ar) has the closed form R0 = -log(1 - ar) / ar
  r <- R0(attack_rate = ar)
  expect_equal(r$R0, -log(1 - ar) / ar, tolerance = 1e-10)
  expect_equal(r$route, 2)
  expect_equal(R0bayse(attack_rate = 0.3)$estimate, -log(0.7) / 0.3, tolerance = 1e-10)
  expect_error(R0(beta = 1, gamma = 0), "gamma must be positive")
  expect_error(R0(attack_rate = 1), "in \\(0, 1\\)")
  expect_error(R0(), "Provide")
})

test_that("Radj is the adjusted coefficient of determination", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(1.2, 1.9, 3.4, 3.8, 5.3, 5.9, 7.4, 7.6)
  f <- summary(stats::lm(y ~ x))
  r <- Radj(8, 1, r2 = f$r.squared)
  expect_equal(r$radj, f$adj.r.squared, tolerance = 1e-12)
  rss <- sum(f$residuals^2)
  ssy <- sum((y - mean(y))^2)
  expect_equal(Radj(8, 1, rss = rss, ssy = ssy)$radj, f$adj.r.squared, tolerance = 1e-12)
  expect_error(Radj(2, 1, r2 = 0.5), "at least 1")
  expect_error(Radj(8, 1), "supply either")
  expect_error(Radj(8, 1, rss = -1, ssy = 2), "non-negative")
  expect_error(Radj(8, -1, r2 = 0.5), "non-negative")
})

test_that("Raklng rakes weights to both margins multiplicatively", {
  y <- c(3, 5, 4, 10, 12, 7, 8, 6)
  w0 <- c(1, 2, 1, 3, 1, 2, 2, 1)
  sex <- c("m", "f", "m", "f", "f", "m", "f", "m")
  age <- c("y", "y", "o", "o", "y", "o", "y", "o")
  r <- Raklng(y, w0, list(list(sex, c(m = 60, f = 40)), list(age, c(y = 45, o = 55))))
  w <- r$weights
  expect_equal(as.numeric(tapply(w, sex, sum)[c("m", "f")]), c(60, 40), tolerance = 1e-10)
  expect_equal(as.numeric(tapply(w, age, sum)[c("y", "o")]), c(45, 55), tolerance = 1e-10)
  # raking multiplies by a sex factor and an age factor: log(w / w0) is additive
  fit <- stats::lm(log(w / w0) ~ factor(sex) + factor(age))
  expect_lt(max(abs(stats::residuals(fit))), 1e-10)
  expect_equal(r$estimate, sum(w * y) / sum(w), tolerance = 1e-12)
  expect_error(Raklng(y, w0, list(list(sex, c(m = 60, f = 40)), list(age, c(y = 45, o = 50)))),
               "inconsistent totals")
  expect_error(Raklng(y, w0, list(list(sex, c(m = 100)))), "no target for a level")
  expect_error(Raklng(y, -w0, list(list(sex, c(m = 60, f = 40)))), "positive")
  expect_error(Raklng(y, w0, list()), "at least one margin")
})

test_that("Ramsw is the Ramsay-weighted IRLS location", {
  y <- c(2.1, 2.5, 1.9, 2.2, 2.4, 9.0, 2.0, 2.3)
  r <- Ramsw(y, 0.3)
  s <- stats::mad(y)
  w <- exp(-0.3 * abs((y - r$estimate) / s))
  expect_equal(r$scale, s, tolerance = 1e-12)
  expect_equal(r$estimate, sum(w * y) / sum(w), tolerance = 1e-12)
  expect_equal(Ramsw(y, 0)$estimate, mean(y), tolerance = 1e-12)
  expect_error(Ramsw(numeric(0), 1), "empty")
  expect_error(Ramsw(y, -1), "non-negative")
})

test_that("Randres is Warner's randomized-response estimator", {
  y <- c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0)
  r <- Randres(y, truth = c(1, 0, 0, 1, 0, 1, 0, 0, 1, 0), p = 0.75)
  lam <- 0.6
  expect_equal(r$estimate, (lam - 0.25) / 0.5, tolerance = 1e-12)
  expect_equal(r$se, sqrt(lam * (1 - lam) / (10 * 0.25)), tolerance = 1e-12)
  expect_equal(r$truth_rate, 0.4)
  expect_true(is.nan(Randres(y, p = 0.5)$estimate))
})

test_that("RandW sums the geometric series of the product graph", {
  A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3)
  B <- matrix(c(0, 1, 1, 0), 2)
  r <- RandW(A, B, lam = 0.1)
  W <- kronecker(A, B)
  S <- diag(6)
  P <- diag(6)
  for (k in 1:200) {
    P <- 0.1 * P %*% W
    S <- S + P
  }
  expect_equal(r$estimate, sum(S), tolerance = 1e-12)
  expect_equal(r$trace, sum(diag(S)), tolerance = 1e-12)
  expect_error(RandW(A, B, lam = 0), "lam must be positive")
  expect_error(RandW(A[1:2, ], B), "G1 must be square")
  expect_error(RandW(matrix(0, 0, 0), B), "non-empty")
})

test_that("Randwk propagates the random-walk law", {
  G <- matrix(c(0, 2, 1, 1, 0, 0, 1, 3, 0), 3, byrow = TRUE)
  P <- G / rowSums(G)
  r <- Randwk(G, start = 2, steps = 3)
  p <- c(0, 1, 0) %*% P %*% P %*% P
  expect_equal(r$p, as.numeric(p), tolerance = 1e-12)
  expect_equal(r$argmax, which.max(p) - 1L)
  expect_equal(Randwk(G, start = 1, steps = 0)$p, c(1, 0, 0))
  expect_error(Randwk(G, start = 4), "outside")
  expect_error(Randwk(G, steps = -1), "non-negative")
  expect_error(Randwk(-G), "non-negative")
  expect_error(Randwk(matrix(c(0, 0, 1, 0), 2)), "positive degree")
})

test_that("morie_ranova gives the ANOVA variance components", {
  y <- c(5.1, 4.8, 5.5, 6.2, 6.0, 6.5, 6.1, 4.2, 4.6, 7.0, 6.8)
  g <- c("a", "a", "a", "b", "b", "b", "b", "c", "c", "d", "d")
  r <- morie_ranova(y, g)
  tab <- stats::anova(stats::lm(y ~ factor(g)))
  ni <- as.numeric(table(g))
  n0 <- (11 - sum(ni^2) / 11) / 3
  expect_equal(c(r$msa, r$mse), tab[["Mean Sq"]], tolerance = 1e-12)
  expect_equal(r$sigma2_a, max(0, (tab[["Mean Sq"]][1] - tab[["Mean Sq"]][2]) / n0), tolerance = 1e-12)
  expect_equal(r$icc, r$sigma2_a / (r$sigma2_a + r$sigma2_e), tolerance = 1e-12)
  expect_false(r$balanced)
  expect_error(morie_ranova(y, rep("a", 11)), "two classes")
  expect_error(morie_ranova(1:3, c("a", "b", "c")), "replication")
  expect_error(morie_ranova(y, g[-1]), "equal length")
})

test_that("morie_raoscot applies the first-order Rao-Scott correction", {
  ph <- c(0.3, 0.45, 0.25)
  p0 <- c(1 / 3, 1 / 3, 1 / 3)
  x2 <- 200 * sum((ph - p0)^2 / p0)
  r <- morie_raoscot(ph, p0, 200)
  expect_equal(r$statistic, x2, tolerance = 1e-12)
  expect_equal(r$p_value, stats::pchisq(x2, 2, lower.tail = FALSE), tolerance = 1e-12)
  d <- c(1.5, 2, 1.2)
  rd <- morie_raoscot(ph, p0, 200, deffs = d)
  lam <- sum((1 - p0) * d) / 2
  expect_equal(rd$corrected, x2 / lam, tolerance = 1e-12)
  V <- matrix(c(0.002, -0.001, -0.001, -0.001, 0.003, -0.002, -0.001, -0.002, 0.003), 3)
  rv <- morie_raoscot(ph, p0, 200, V = V)
  expect_equal(rv$lambda_bar, (sum(diag(V) / p0) - sum(V)) / 2, tolerance = 1e-12)
  expect_error(morie_raoscot(ph, c(0.5, 0.5, 0), 200), "p0 must be positive")
  expect_error(morie_raoscot(c(0.5, 0.6, 0.1), p0, 200), "sum to 1")
  expect_error(morie_raoscot(ph, p0, 1), "at least 2")
  expect_error(morie_raoscot(ph, p0, 200, deffs = c(1, 2)), "k positive cell deffs")
})

test_that("RAPPOR privacy parameters, encoder and decoder", {
  st <- morie_rappor_star(0.5, 0.5, 0.75)
  expect_equal(st$q_star, 0.25 * 1.25 + 0.5 * 0.75, tolerance = 1e-12)
  expect_equal(st$p_star, 0.25 * 1.25 + 0.5 * 0.5, tolerance = 1e-12)
  ep <- morie_rappor_epsilon(2, 0.5, 0.5, 0.75)
  expect_equal(ep$eps_infinity, 2 * 2 * log(0.75 / 0.25), tolerance = 1e-12)
  expect_equal(ep$eps_1, 2 * log(st$q_star * (1 - st$p_star) / (st$p_star * (1 - st$q_star))),
               tolerance = 1e-12)
  expect_error(morie_rappor_epsilon(0, 0.5), "at least 1")
  expect_error(morie_rappor_epsilon(1, 0), "f must lie in \\(0, 2\\)")
  expect_error(morie_rappor_star(2, 0.5, 0.5), "f must lie")
  vals <- c("b", "a", "c", "a", "b")
  r <- morie_rappor(vals, f = 0.4, p = 0.3, q = 0.8, variant = "basic", seed = 6)
  g <- .ghc_rng(6)
  alph <- c("a", "b", "c")
  cnt <- numeric(3)
  for (v in vals) {
    B <- as.integer(alph == v)
    Bp <- vapply(B, function(b) {
      u <- .ghc_unif(g, 1L)
      if (u < 0.2) 1L else if (u < 0.4) 0L else b
    }, 0L)
    S <- vapply(Bp, function(b) as.integer(.ghc_unif(g, 1L) < (if (b == 1L) 0.8 else 0.3)), 0L)
    cnt <- cnt + S
  }
  expect_equal(as.numeric(r$counts), cnt)
  expect_equal(r$alphabet, alph)
  dec <- morie_rappor_decode(r$counts, r$cohort_sizes, f = 0.4, p = 0.3, q = 0.8)
  shift <- 0.3 + 0.2 * 0.8 - 0.2 * 0.3
  expect_equal(as.numeric(dec$estimate), (cnt - shift * 5) / (0.6 * 0.5), tolerance = 1e-12)
  # the Bloom filter sets at most h bits from the documented rolling hash
  bloom <- function(s, k, h, cohort) {
    sort(unique(vapply(0:(h - 1), function(j) {
      acc <- (cohort * 7919 + j * 104729 + 1) %% 2147483647
      for (cc in utf8ToInt(s)) acc <- (acc * 131 + cc) %% 2147483647
      acc %% k
    }, 0)))
  }
  r1 <- morie_rappor("xyz", k = 16, h = 2, f = 0, p = 0, q = 1, variant = "full")
  want <- integer(16)
  want[bloom("xyz", 16, 2, 0) + 1] <- 1L
  expect_equal(r1$reports[[1]], want)
  expect_error(morie_rappor(vals, variant = "other"), "full, one-time or basic")
  expect_error(morie_rappor_decode(r$counts, r$cohort_sizes, f = 1), "carry no signal")
})

test_that("Rbfkern is exp(-gamma |x - z|^2)", {
  X <- rbind(c(0.1, 0.5), c(1.2, -0.3), c(0.7, 0.8))
  Z <- rbind(c(0, 0), c(1, 1))
  r <- Rbfkern(X, gamma = 0.7, Z = Z)
  D2 <- outer(1:3, 1:2, Vectorize(function(i, j) sum((X[i, ] - Z[j, ])^2)))
  expect_equal(r$K, exp(-0.7 * D2), tolerance = 1e-12)
  expect_equal(Rbfkern(X)$gamma, 0.5)
  expect_equal(diag(Rbfkern(X)$K), rep(1, 3), tolerance = 1e-12)
})

test_that("RbfGrid evaluates a thin-plate interpolant on a grid", {
  X <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.5), c(0.2, 0.8))
  y <- c(1, 2, 0.5, 3, 1.8, 1.1)
  xs <- c(0, 0.5, 1)
  ys <- c(0, 1)
  g <- RbfGrid(X, y, xs, ys)
  tp <- function(r) ifelse(r > 0, r^2 * log(r), 0)
  A <- tp(as.matrix(stats::dist(X)))
  Pm <- cbind(1, X)
  sol <- solve(rbind(cbind(A, Pm), cbind(t(Pm), matrix(0, 3, 3))), c(y, 0, 0, 0))
  f <- function(p) sum(sol[1:6] * tp(sqrt(colSums((t(X) - p)^2)))) + sum(sol[7:9] * c(1, p))
  want <- t(vapply(ys, function(yy) vapply(xs, function(xx) f(c(xx, yy)), 0), numeric(3)))
  expect_equal(g$surface, want, tolerance = 1e-10)
  # an interpolant reproduces the data at data sites on the grid
  expect_equal(g$surface[1, c(1, 3)], y[1:2], tolerance = 1e-10)
  expect_equal(g$surface[2, c(1, 3)], y[3:4], tolerance = 1e-10)
})

test_that("sharp and fuzzy RDD and the IK bandwidth", {
  skip_if_not_installed("sandwich")
  x <- seq(-1, 1, length.out = 41)
  e <- sin(7 * seq_along(x)) * 0.2
  y <- 1 + 0.8 * x + 0.5 * x^2 + ifelse(x >= 0, 1.5, 0) + e
  r <- Causrdd(x, y, h = 0.6)
  w <- pmax(1 - abs(x / 0.6), 0)
  side <- function(k) {
    f <- stats::lm(y ~ x, weights = w, subset = k & w > 0)
    list(a = unname(stats::coef(f)[1]), v = sandwich::vcovHC(f, type = "HC0")[1, 1])
  }
  L <- side(x < 0)
  R <- side(x >= 0)
  expect_equal(r$estimate, R$a - L$a, tolerance = 1e-10)
  expect_equal(r$se, sqrt(L$v + R$v), tolerance = 1e-9)
  ru <- Causrdd(x, y, h = 0.6, kernel = "uniform")
  fl <- stats::lm(y ~ x, subset = x < 0 & abs(x) <= 0.6)
  fr <- stats::lm(y ~ x, subset = x >= 0 & abs(x) <= 0.6)
  expect_equal(ru$estimate, unname(stats::coef(fr)[1] - stats::coef(fl)[1]), tolerance = 1e-10)
  expect_error(Causrdd(x, y, h = 0.6, kernel = "epan"), "triangular or uniform")
  expect_error(Causrdd(x, y, h = -1), "positive")
  tr <- as.numeric(x >= 0.1 | (x > -0.3 & x < -0.1))
  fz <- Causrddf(x, y, tr, h = 0.6, h_treat = 0.5)
  jy <- Causrdd(x, y, h = 0.6)
  jw <- Causrdd(x, tr, h = 0.5)
  expect_equal(fz$estimate, jy$estimate / jw$estimate, tolerance = 1e-12)
  expect_equal(fz$se, sqrt((jy$se^2 + fz$estimate^2 * jw$se^2) / jw$estimate^2), tolerance = 1e-12)
  expect_error(Causrddf(x, y, rep(1, 41), h = 0.6, h_treat = 0.6), "no first-stage discontinuity")
  hk <- Causrddh(x, y)
  h1 <- 1.84 * stats::sd(x) * 41^-0.2
  expect_equal(hk$h1, h1, tolerance = 1e-12)
  il <- x >= -h1 & x < 0
  ir <- x >= 0 & x <= h1
  expect_equal(hk$f_hat, (sum(il) + sum(ir)) / (2 * 41 * h1), tolerance = 1e-12)
  expect_equal(hk$sigma2, ((sum(il) - 1) * stats::var(y[il]) + (sum(ir) - 1) * stats::var(y[ir])) /
                 (sum(il) + sum(ir)), tolerance = 1e-12)
  keep <- x >= stats::median(x[x < 0]) & x <= stats::median(x[x >= 0])
  xk <- x[keep]
  m3 <- 6 * unname(stats::coef(stats::lm(y[keep] ~ as.numeric(xk >= 0) + xk + I(xk^2) + I(xk^3)))[5])
  expect_equal(hk$m3, m3, tolerance = 1e-8)
  ho <- 3.4375 * (2 * hk$sigma2 / (hk$f_hat * ((hk$m2_right - hk$m2_left)^2 + hk$r_right + hk$r_left)))^0.2 *
    41^-0.2
  expect_equal(hk$estimate, ho, tolerance = 1e-12)
  expect_error(Causrddh(x[1:5], y[1:5]), "at least 10")
})

test_that("Ratedist reaches the Bernoulli rate-distortion function", {
  Hb <- function(p) -p * log(p) - (1 - p) * log(1 - p)
  r <- Ratedist(c(0.5, 0.5), D = 0.1)
  # R(D) = H(p) - H(D) nats under Hamming distortion, D < min(p, 1 - p)
  expect_equal(r$rate, log(2) - Hb(0.1), tolerance = 1e-8)
  expect_equal(r$distortion_achieved, 0.1, tolerance = 1e-8)
  expect_equal(r$bits, r$rate / log(2), tolerance = 1e-12)
  r2 <- Ratedist(c(3, 7), D = 0.2)
  expect_equal(r2$rate, Hb(0.3) - Hb(0.2), tolerance = 1e-8)
  expect_equal(r2$slope, -1 / r2$beta, tolerance = 1e-12)
})
