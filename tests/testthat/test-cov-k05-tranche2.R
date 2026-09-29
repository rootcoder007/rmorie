# Coverage tests for the untested exports of R/k05_tranche2.R: Sobel
# tests, IRT test characteristic and information curves, Lord's
# chi-square, Cochran's Q and the Tarone-Ware family.

test_that("Sobel, Aroian and Goodman indirect-effect tests", {
  a <- 0.6
  b <- 0.4
  sa <- 0.15
  sb <- 0.1
  for (v in c("sobel", "aroian", "goodman")) {
    vv <- b^2 * sa^2 + a^2 * sb^2 + switch(v, sobel = 0, aroian = sa^2 * sb^2, goodman = -sa^2 * sb^2)
    r <- morie_sobel_test(a, b, sa, sb, variant = v)
    expect_equal(r$se, sqrt(vv), tolerance = 1e-12)
    expect_equal(r$pvalue, 2 * pnorm(-abs(a * b / sqrt(vv))), tolerance = 1e-12)
  }
  expect_error(morie_sobel_test(a, b, sa, sb, "bootstrap"), "variant must be")
  expect_error(morie_sobel_test(0, 0, 1, 1, "goodman"), "non-positive variance")
})

test_that("3PL test characteristic and information curves", {
  a <- c(1.2, 0.8, 1.5)
  b <- c(-0.5, 0.3, 1)
  c <- c(0.2, 0, 0.1)
  th <- c(-1, 0, 1.5)
  P <- function(t) c + (1 - c) * plogis(1.7 * a * (t - b))
  tcc <- morie_tcc(th, a, b, c, D = 1.7)
  expect_equal(tcc$tcc, vapply(th, function(t) sum(P(t)), 0), tolerance = 1e-12)
  expect_equal(tcc$floor, 0.3)
  inf <- morie_test_information(th, a, b, c, D = 1.7)
  I3 <- function(t) {
    p <- P(t)
    sum((1.7 * a)^2 * (1 - p) / p * ((p - c) / (1 - c))^2)
  }
  expect_equal(inf$information, vapply(th, I3, 0), tolerance = 1e-12)
  expect_equal(inf$sem, 1 / sqrt(inf$information), tolerance = 1e-12)
  i2 <- morie_test_information(0.2, a, b)
  p2 <- plogis(a * (0.2 - b))
  expect_equal(i2$information, sum(a^2 * p2 * (1 - p2)), tolerance = 1e-12)
  expect_error(morie_tcc(0, a[1:2], b), "one entry per item")
  expect_error(morie_tcc(0, a, b, c = 1.5), "0 <= c_i < upper_i")
})

test_that("Lord chi-square and Cochran Q", {
  bR <- c(1.1, -0.3)
  bF <- c(0.8, 0.1)
  VR <- rbind(c(0.04, 0.01), c(0.01, 0.05))
  VF <- rbind(c(0.03, 0), c(0, 0.02))
  d <- bR - bF
  r <- morie_lord_chisq(bR, bF, VR, VF)
  expect_equal(r$statistic, as.numeric(t(d) %*% solve(VR + VF) %*% d), tolerance = 1e-12)
  expect_equal(r$pvalue, pchisq(r$statistic, 2, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(morie_lord_chisq(bR, bF, VR)$statistic, as.numeric(t(d) %*% solve(VR) %*% d), tolerance = 1e-12)
  expect_error(morie_lord_chisq(bR, bF[1], VR), "same length")
  y <- c(0.2, 0.5, -0.1, 0.4)
  v <- c(0.02, 0.05, 0.03, 0.04)
  w <- 1 / v
  expect_equal(morie_cochran_q(y, v)$statistic, sum(w * (y - sum(w * y) / sum(w))^2), tolerance = 1e-12)
})

test_that("Tarone-Ware family of weighted log-rank tests", {
  t <- c(3, 5, 7, 2, 8, 9, 4, 6, 10, 1, 5, 7)
  e <- c(1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1)
  g <- rep(c("a", "b"), 6)
  skip_if_not_installed("survival")
  lr <- morie_tarone_ware(t, e, g, weight = "logrank")
  expect_equal(lr$statistic, survival::survdiff(survival::Surv(t, e) ~ g)$chisq, tolerance = 1e-10)
  ut <- sort(unique(t[e == 1]))
  n <- sapply(ut, function(s) sum(t >= s))
  n1 <- sapply(ut, function(s) sum(t >= s & g == "a"))
  d <- sapply(ut, function(s) sum(t == s & e == 1))
  d1 <- sapply(ut, function(s) sum(t == s & e == 1 & g == "a"))
  v <- d * (n - d) * n1 * (n - n1) / (n^2 * (n - 1))
  k <- n > 1
  for (wt in c("tarone-ware", "gehan")) {
    w <- if (wt == "gehan") n else sqrt(n)
    s <- morie_tarone_ware(t, e, g, weight = wt)
    expect_equal(s$statistic, sum((w * (d1 - d * n1 / n))[k])^2 / sum((w^2 * v)[k]), tolerance = 1e-12, info = wt)
  }
  pw <- cumprod(c(1, 1 - d / (n + 1)))[seq_along(ut)]
  pt <- morie_tarone_ware(t, e, g, weight = "peto")
  expect_equal(pt$statistic, sum((pw * (d1 - d * n1 / n))[k])^2 / sum((pw^2 * v)[k]), tolerance = 1e-12)
  expect_error(morie_tarone_ware(t, e, rep(1:3, 4)), "exactly 2 groups")
  expect_error(morie_tarone_ware(t, e, g, weight = "fh"), "weight must be")
})
