# Coverage for survey-sampling and measurement helpers (adjsrs.R,
# desigeff.R, clstrs.R, cluster.R, covpop.R, fpcadj.R, cv1gn.R, ctde.R,
# ctomeg.R, contse.R): design effects from Kish's formulas and the
# one-way ANOVA ICC, regressions against lm, SimCSE replayed on the
# SplitMix64 dropout stream.

test_that("Neffsrs and Fpc: Kish effective size and finite population correction", {
  w <- c(1, 2, 2, 3, 5)
  r <- Neffsrs(w)
  expect_equal(r$neff, sum(w)^2 / sum(w^2), tolerance = 1e-12)
  expect_equal(r$cv2, mean((w - mean(w))^2) / mean(w)^2, tolerance = 1e-12)
  expect_error(Neffsrs(numeric(0)), "non-empty")
  expect_error(Neffsrs(c(1, 0)), "strictly positive")
  f <- Fpc(200, 50)
  expect_equal(c(f$fpc, f$se_factor, f$fraction), c(0.75, sqrt(0.75), 0.25), tolerance = 1e-12)
  expect_equal(Fpc(Inf, 10)$fpc, 1)
  expect_error(Fpc(10, 0), "at least 1")
  expect_error(Fpc(5, 10), "at least n")
})

test_that("Desigeff multiplies the weighting and clustering design effects", {
  y <- c(3, 4, 5, 8, 9, 7, 2, 3, 1, 6)
  w <- c(1, 2, 1, 1, 3, 1, 2, 1, 1, 2)
  cl <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
  r <- Desigeff(y, w, cl)
  dw <- 10 * sum(w^2) / sum(w)^2
  a <- anova(lm(y ~ factor(cl)))
  msb <- a$`Mean Sq`[1]
  msw <- a$`Mean Sq`[2]
  m0 <- (10 - sum(c(3, 3, 4)^2) / 10) / 2
  rho <- (msb - msw) / (msb + (m0 - 1) * msw)
  expect_equal(c(r$deff_w, r$rho), c(dw, rho), tolerance = 1e-12)
  expect_equal(r$estimate, dw * (1 + (m0 - 1) * rho), tolerance = 1e-12)
  expect_equal(Desigeff(y)$estimate, 1)
  expect_error(Desigeff(numeric(0)), "empty")
  expect_error(Desigeff(y, w[-1]), "same length")
  expect_error(Desigeff(y, -w), "non-negative")
  expect_error(Desigeff(y, w * 0), "sum to zero")
  expect_error(Desigeff(y, cluster = cl[-1]), "same length")
})

test_that("Clusdes picks the integer cluster size minimising the variance", {
  r <- Clusdes(rho = 0.05, S2 = 4, c1 = 50, c2 = 2, budget = 2000)
  kopt <- sqrt(50 * 0.95 / (2 * 0.05))
  V <- function(k) {
    m <- 2000 / (50 + 2 * k)
    4 / (m * k) * (1 + (k - 1) * 0.05)
  }
  k <- floor(kopt) + c(0, 1)
  best <- k[which.min(vapply(k, V, 0))]
  expect_equal(r$k_opt, kopt, tolerance = 1e-12)
  expect_equal(r$k, best)
  expect_equal(r$variance, V(best), tolerance = 1e-12)
  expect_equal(r$cost, 2000, tolerance = 1e-12)
  expect_equal(Clusdes(1, 4, 50, 2, 2000)$k_opt, 1)
  expect_error(Clusdes(0, 4, 1, 1, 10), "0 < rho")
  expect_error(Clusdes(0.1, 0, 1, 1, 10), "S2")
  expect_error(Clusdes(0.1, 1, 0, 1, 10), "costs")
  expect_error(Clusdes(0.1, 1, 5, 5, 10), "budget")
})

test_that("Clus1 estimates a mean from equal-size clusters", {
  Y <- rbind(c(3, 4, 5), c(8, 9, 7), c(2, 3, 1), c(6, 5, 7))
  r <- Clus1(Y, M = 20, level = 0.9)
  cm <- rowMeans(Y)
  v <- (16 / 20) * var(cm) / 4
  expect_equal(r$estimate, mean(cm), tolerance = 1e-12)
  expect_equal(r$se, sqrt(v), tolerance = 1e-12)
  expect_equal(r$ci_lower, mean(cm) - qnorm(0.95) * sqrt(v), tolerance = 1e-12)
  expect_equal(r$deff, v / (var(as.numeric(Y)) / 12), tolerance = 1e-12)
  expect_equal(r$within_var, mean(apply(Y, 1, var)), tolerance = 1e-12)
  expect_true(is.nan(Clus1(Y[, 1, drop = FALSE])$rho))
  expect_error(Clus1(Y[1, , drop = FALSE]), "two clusters")
})

test_that("Covpop rakes weights to stratum totals", {
  y <- c(1, 2, 3, 4, 5, 6)
  w <- c(1, 1, 2, 2, 1, 3)
  s <- c("a", "a", "b", "b", "b", "a")
  r <- Covpop(y, w, c(10, 20), strata = s)
  f <- c(10 / 5, 20 / 5)
  wa <- w * f[match(s, c("a", "b"))]
  expect_equal(r$factors, f, tolerance = 1e-12)
  expect_equal(r$estimate, sum(wa * y) / sum(wa), tolerance = 1e-12)
  expect_equal(Covpop(y, w, 99)$total, 99, tolerance = 1e-12)
  expect_error(Covpop(numeric(0), numeric(0), 1), "empty")
  expect_error(Covpop(y, w[-1], 1), "same length")
  expect_error(Covpop(y, w, 1, strata = s[-1]), "same length")
  expect_error(Covpop(y, w, c(1, 2, 3), strata = s), "one entry per stratum")
  expect_error(Covpop(y, w * 0, 1), "zero estimated size")
})

test_that("Cv1gn is K-fold GBLUP (ridge) cross-validation", {
  X <- cbind(c(0, 1, 2, 1, 0, 2, 1, 0), c(1, 1, 0, 2, 2, 0, 1, 1), c(2, 0, 1, 1, 0, 1, 2, 0))
  y <- c(1.2, 2.3, 3.1, 2.8, 1.1, 3.5, 2.2, 0.9)
  r <- Cv1gn(y, X, n_folds = 4, lam = 0.5)
  fold <- (0:7) %% 4
  yh <- numeric(8)
  for (f in 0:3) {
    tr <- fold != f
    mu <- mean(y[tr])
    b <- solve(crossprod(X[tr, ]) + diag(0.5 + 1e-12, 3), crossprod(X[tr, ], y[tr] - mu))
    yh[!tr] <- mu + X[!tr, ] %*% b
  }
  expect_equal(r$y_hat, yh, tolerance = 1e-10)
  expect_equal(r$pa, cor(y, yh), tolerance = 1e-10)
  expect_equal(r$mse, mean((y - yh)^2), tolerance = 1e-10)
  expect_error(Cv1gn(1, X[1, , drop = FALSE], 2), "two lines")
  expect_error(Cv1gn(y, X[-1, ], 4), "different number of rows")
  expect_error(Cv1gn(y, X, 1), "n_folds")
  expect_error(Cv1gn(y, X, 4, lam = -1), "non-negative")
})

test_that("Ctde is the controlled direct effect with exposure-mediator interaction", {
  a <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0)
  cc <- c(0.3, -0.2, 0.5, 0.1, -0.4, 0.2, 0.6, -0.1, 0.0, 0.4)
  m <- 0.5 + 0.8 * a + 0.3 * cc + c(0.1, -0.2, 0.15, -0.05, 0.2, -0.1, 0.05, 0.1, -0.15, 0.02)
  y <- 1 + 0.6 * a + 0.9 * m + 0.4 * a * m + 0.2 * cc +
    c(0.05, -0.1, 0.02, 0.08, -0.03, 0.01, -0.06, 0.04, 0.02, -0.02)
  r <- Ctde(a, m, y, m = 0.7, C = cc)
  th <- coef(lm(y ~ a + m + I(a * m) + cc))
  be <- coef(lm(m ~ a + cc))
  expect_equal(r$estimate, unname(th[2] + th[4] * 0.7), tolerance = 1e-8)
  expect_equal(r$tnie, unname((th[3] * be[2] + th[4] * be[2])), tolerance = 1e-8)
  expect_equal(r$pnde, unname(th[2] + th[4] * (be[1] + be[3] * mean(cc))), tolerance = 1e-8)
  expect_error(Ctde(numeric(0), numeric(0), numeric(0), 0), "empty")
  expect_error(Ctde(a, m[-1], y, 0), "same length")
  expect_error(Ctde(a, m, y, 0, C = cc[-1]), "one row per observation")
  expect_error(Ctde(a[1:4], m[1:4], y[1:4], 0, C = cc[1:4]), "too few")
})

test_that("Ctomeg computes McDonald's omega and Cronbach's alpha", {
  X <- cbind(c(3, 4, 2, 5, 4, 3, 5, 2), c(2, 4, 2, 5, 3, 3, 4, 1), c(3, 5, 1, 4, 4, 2, 5, 2))
  lam <- c(0.9, 1.0, 1.1)
  r <- Ctomeg(X, lam)
  S <- cov(X)
  expect_equal(r$omega, sum(lam)^2 / sum(S), tolerance = 1e-12)
  expect_equal(r$alpha, 1.5 * (1 - sum(diag(S)) / sum(S)), tolerance = 1e-12)
  expect_equal(r$uniquenesses, diag(S) - lam^2, tolerance = 1e-12)
  expect_equal(Ctomeg(S, lam)$omega, r$omega, tolerance = 1e-12)
  expect_error(Ctomeg(X, 1), "two items")
  expect_error(Ctomeg(X[1, , drop = FALSE], lam), "two observations")
  expect_error(Ctomeg(X[, 1:2], lam), "different item counts")
  expect_error(Ctomeg(matrix(0, 3, 3), lam), "not positive")
})

test_that("Contse is the SimCSE loss on dropout-perturbed embeddings", {
  H <- rbind(c(0.5, 1.2, -0.3, 0.8), c(-0.7, 0.4, 1.1, 0.2), c(0.9, -0.5, 0.3, 1.4))
  r <- Contse(H, tau = 0.1, dropout = 0.2, seed = 3)
  e <- .ghc_rng(3)
  A <- B <- matrix(0, 3, 4)
  for (i in 1:3) {
    ma <- mb <- numeric(4)
    for (k in 1:4) {
      ma[k] <- if (.ghc_unif(e, 1L) < 0.2) 0 else 1 / 0.8
      mb[k] <- if (.ghc_unif(e, 1L) < 0.2) 0 else 1 / 0.8
    }
    A[i, ] <- H[i, ] * ma / sqrt(sum((H[i, ] * ma)^2))
    B[i, ] <- H[i, ] * mb / sqrt(sum((H[i, ] * mb)^2))
  }
  S <- A %*% t(B) / 0.1
  per <- log(rowSums(exp(S))) - diag(S)
  expect_equal(r$per_item, per, tolerance = 1e-10)
  expect_equal(r$loss, mean(per), tolerance = 1e-10)
  expect_equal(r$alignment, sum((A - B)^2) / 3, tolerance = 1e-10)
  D <- as.matrix(dist(A))^2
  expect_equal(r$uniformity, log(mean(exp(-2 * D[upper.tri(D)]))), tolerance = 1e-10)
  expect_error(Contse(matrix(numeric(0), 0, 2)), "no rows")
  expect_error(Contse(H, tau = 0), "strictly positive")
  expect_error(Contse(H, dropout = 1), "\\[0, 1\\)")
})
