# Coverage for Louvain (two variants), the LP duality certificate, lppd /
# WAIC, logistic residuals, the log-rank test, Wald intervals, the
# least-squares line and its reverse, least trimmed squares, leverage and
# Lempel-Ziv complexity; recomputed with base R, survival and exhaustive
# search.

two_cliques <- function() {
  A <- matrix(0, 8, 8)
  A[1:4, 1:4] <- 1
  A[5:8, 5:8] <- 1
  diag(A) <- 0
  A[4, 5] <- A[5, 4] <- 1
  A
}
modq <- function(A, lab, g = 1) {
  k <- rowSums(A)
  m2 <- sum(A)
  sum((A - g * outer(k, k) / m2) * outer(lab, lab, "==")) / m2
}

test_that("louv, morie_louv, LuvR and louvain find and score the two cliques", {
  A <- two_cliques()
  r <- louv(A)
  expect_equal(r$n_communities, 2L)
  expect_equal(r$communities, rep(0:1, each = 4))
  expect_equal(r$estimate, modq(A, r$communities), tolerance = 1e-12)
  expect_equal(morie_louv(A)$estimate, r$estimate)
  expect_equal(louv(matrix(0, 3, 3))$n_communities, 3L)
  l <- LuvR(A)
  expect_equal(l$communities, rep(0:1, each = 4))
  expect_equal(l$estimate, modq(A, l$communities), tolerance = 1e-12)
  g <- LuvR(A, resolution = 0.3)
  expect_equal(g$estimate, modq(A, g$communities, 0.3), tolerance = 1e-12)
  expect_equal(louvain(A)$communities, l$communities)
  expect_error(LuvR(A[1:3, ]), "square")
  expect_error(LuvR(matrix(0, 2, 2)), "positive total weight")
})

test_that("Lpdual certifies LP optimality", {
  A <- rbind(c(1, 2), c(3, 1))
  b <- c(4, 6)
  cc <- c(1, 1)
  x <- c(1.6, 1.2)
  y <- c(0.4, 0.2)
  r <- Lpdual(A, b, cc, x, y)
  expect_equal(r$primal_objective, 2.8, tolerance = 1e-12)
  expect_equal(r$dual_objective, sum(b * y), tolerance = 1e-12)
  expect_equal(r$surplus, as.numeric(t(A) %*% y) - cc, tolerance = 1e-12)
  expect_equal(r$optimal, 1)
  expect_equal(r$dual_A, t(A))
  w <- Lpdual(A, b, cc, c(1, 1), c(1, 1))
  expect_equal(w$optimal, 0)
  expect_equal(w$gap, sum(b) - 2, tolerance = 1e-12)
  expect_error(Lpdual(A, b[1], cc), "one entry per constraint")
})

test_that("Lppd, Lrresid and Lrwald", {
  set.seed(1)
  L <- matrix(rnorm(40 * 3, -1, 0.4), 40)
  r <- Lppd(L)
  lp <- sum(log(colMeans(exp(L))))
  pw <- sum(apply(L, 2, var))
  expect_equal(c(r$lppd, r$p_waic, r$waic), c(lp, pw, -2 * (lp - pw)), tolerance = 1e-12)
  expect_error(Lppd(L[1, , drop = FALSE]), "two posterior draws")

  x <- c(0.5, 1.2, -0.3, 2.0, 0.8, -1.1, 1.5, 0.1)
  n <- c(5, 6, 4, 8, 5, 3, 7, 6)
  yy <- c(2, 4, 1, 7, 3, 0, 6, 2)
  f <- glm(cbind(yy, n - yy) ~ x, family = binomial)
  rr <- Lrresid(yy, fitted(f), n, hat = hatvalues(f))
  expect_equal(rr$pearson, unname(residuals(f, "pearson")), tolerance = 1e-9)
  expect_equal(rr$deviance, unname(residuals(f, "deviance")), tolerance = 1e-9)
  expect_equal(rr$D, deviance(f), tolerance = 1e-9)
  expect_equal(rr$delta_d, unname(rr$deviance^2 + rr$pearson^2 * hatvalues(f) / (1 - hatvalues(f))), tolerance = 1e-9)
  expect_error(Lrresid(1, 1), "strictly inside")

  w <- Lrwald(c(0.5, -1.2), c(0.2, 0.8), level = 0.9)
  expect_equal(w$z, c(2.5, -1.5), tolerance = 1e-12)
  expect_equal(w$ci_low, c(0.5, -1.2) - qnorm(0.95) * c(0.2, 0.8), tolerance = 1e-12)
  expect_equal(w$pvalue, 2 * pnorm(-abs(c(2.5, -1.5))), tolerance = 1e-12)
  expect_error(Lrwald(1, 0), "positive")
})

test_that("Logrank matches survival::survdiff", {
  skip_if_not_installed("survival")
  tm <- c(3, 5, 5, 7, 8, 10, 12, 4, 6, 6, 9, 11, 13, 15)
  ev <- c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1)
  g <- rep(1:2, each = 7)
  r <- Logrank(tm, ev, g)
  sd <- survival::survdiff(survival::Surv(tm, ev) ~ g)
  expect_equal(r$statistic, sd$chisq, tolerance = 1e-9)
  expect_equal(r$observed, sd$obs[1])
  expect_equal(r$expected, sd$exp[1], tolerance = 1e-9)
  expect_error(Logrank(tm, ev, rep(1:3, length.out = 14)), "exactly two groups")
})

test_that("LsqFit, LsqResid and LsqRev", {
  x <- c(1, 2, 4, 5, 8)
  y <- c(2.1, 2.9, 5.2, 5.8, 9.1)
  f <- LsqFit(x, y)
  co <- coef(lm(y ~ x))
  expect_equal(c(f$A, f$B), unname(co[2:1]), tolerance = 1e-12)
  expect_equal(f$S, sum(residuals(lm(y ~ x))^2), tolerance = 1e-12)
  d <- LsqFit()
  expect_equal(c(d$A, d$B), unname(coef(lm(c(1, 1, 3, 4, 6) ~ c(2, 3, 3, 5, 7)))[2:1]), tolerance = 1e-12)
  expect_equal(LsqResid(x, y)$residual_sum, 0, tolerance = 1e-12)
  rv <- LsqRev(x, y)
  expect_equal(c(rv$C, rv$D), unname(coef(lm(x ~ y))[2:1]), tolerance = 1e-12)
  expect_error(LsqFit(c(1, 1), c(2, 3)), "slope undefined")
})

test_that("Ltsreg reaches the exhaustive least-trimmed-squares minimum", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(1.1, 2.2, 2.8, 4.1, 5.2, 20, 6.8, -9)
  X <- cbind(1, x)
  r <- Ltsreg(y, X)
  h <- r$h
  best <- Inf
  for (s in combn(8, h, simplify = FALSE)) {
    rss <- sum(residuals(lm(y[s] ~ x[s]))^2)
    if (rss < best) best <- rss
  }
  expect_equal(r$estimate, best, tolerance = 1e-9)
  expect_equal(r$estimate, sum(sort(r$residual^2)[1:h]), tolerance = 1e-9)
  expect_equal(r$breakdown_theorem1, (4 - 2 + 2) / 8)
  expect_error(Ltsreg(y, X, h = 1), "at least p")
  expect_error(Ltsreg(y, X, h = 9), "cannot exceed")
})

test_that("lvrgh and morie_lvrgh are the hat-matrix diagonal", {
  X <- cbind(c(1, 2, 3, 4, 10), c(2, 1, 0, 1, 3))
  r <- lvrgh(X)
  h <- hatvalues(lm(rnorm(5) ~ X))
  expect_equal(r$leverage, unname(h), tolerance = 1e-12)
  expect_equal(r$trace, 3, tolerance = 1e-12)
  expect_equal(r$high, unname(h > 6 / 5))
  expect_equal(morie_lvrgh(X, intercept = FALSE)$leverage, unname(diag(X %*% solve(crossprod(X), t(X)))), tolerance = 1e-12)
})

test_that("Lzcomp counts the LZ76 exhaustive-history phrases", {
  lz <- function(s) {
    n <- length(s)
    str <- paste(s, collapse = "")
    p <- 1
    c <- 0
    while (p <= n) {
      L <- 1
      while (p + L - 1 <= n && grepl(substr(str, p, p + L - 1), substr(str, 1, p + L - 2), fixed = TRUE)) L <- L + 1
      c <- c + 1
      p <- p + L
    }
    c
  }
  s1 <- as.integer(strsplit("0001101001000101", "")[[1]])
  r <- Lzcomp(s1)
  expect_equal(r$estimate, lz(s1))
  expect_equal(r$normalized, lz(s1) * log(16) / log(2) / 16, tolerance = 1e-12)
  set.seed(2)
  s2 <- sample(0:2, 40, TRUE)
  expect_equal(Lzcomp(s2)$estimate, lz(s2))
  expect_equal(Lzcomp(rep(1, 10))$estimate, lz(rep(1, 10)))
})
