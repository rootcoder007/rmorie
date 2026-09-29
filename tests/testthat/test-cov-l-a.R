# Coverage for Laplacian eigenmaps, the Laplace mechanism, lasso helpers,
# layer normalisation, LD statistics and pruning, l-diversity, the Leiden
# wrappers, edit distance, left-truncated Kaplan-Meier and lagged-value
# IPTW; recomputed with eigen(), survival, adist(), glm() and lm().

test_that("LapEig and laplacian_eigenmaps solve L f = lambda D f", {
  W <- matrix(c(0, 1, 1, 0, 0,
                1, 0, 1, 0, 0,
                1, 1, 0, 0.5, 0,
                0, 0, 0.5, 0, 2,
                0, 0, 0, 2, 0), 5, byrow = TRUE)
  r <- LapEig(W, k = 2)
  d <- rowSums(W)
  Ls <- diag(5) - W / sqrt(outer(d, d))
  e <- eigen(Ls, symmetric = TRUE)
  o <- order(e$values)
  expect_equal(r$all_eigenvalues, e$values[o], tolerance = 1e-12)
  expect_equal(r$eigenvalues, e$values[o][2:3], tolerance = 1e-12)
  for (j in 1:2) {
    f <- e$vectors[, o[j + 1]] / sqrt(d)
    f <- f * sign(f[which.max(abs(f))])
    expect_equal(r$embedding[, j], f, tolerance = 1e-9)
    expect_equal(as.numeric((diag(d) - W) %*% r$embedding[, j]), r$eigenvalues[j] * d * r$embedding[, j],
                 tolerance = 1e-9)
  }
  expect_equal(laplacian_eigenmaps(W, 2)$eigenvalues, r$eigenvalues)
  expect_error(LapEig(W, k = 5), "1 <= k < n")
  W0 <- W
  W0[5, ] <- W0[, 5] <- 0
  expect_error(LapEig(W0), "positive degree")
})

test_that("morie_laplc draws one Laplace variate under the local seed", {
  r <- morie_laplc(10, sensitivity = 2, epsilon = 0.5, seed = 11)
  set.seed(11)
  u <- runif(1)
  noise <- if (u < 0.5) 4 * log(2 * u) else -4 * log(2 * (1 - u))
  expect_equal(r$noise, noise, tolerance = 1e-12)
  expect_equal(r$value, 10 + noise, tolerance = 1e-12)
  expect_equal(r$scale, 4)
})

test_that("Lassoobj and Lassrg", {
  X <- cbind(c(1, 2, 3, 4, 5, 6), c(0.5, -1, 0.2, 1.4, -0.3, 0.8))
  y <- c(1.2, 2.1, 2.9, 4.4, 4.8, 6.3)
  b <- c(0.1, 0.9, -0.2)
  o <- Lassoobj(X, y, b, 0.5)
  rss <- sum((y - cbind(1, X) %*% b)^2)
  expect_equal(o$prss, rss + 0.5 * (0.9 + 0.2), tolerance = 1e-12)
  expect_equal(o$soft, sign(b) * pmax(abs(b) - 0.5, 0), tolerance = 1e-12)
  expect_equal(Lassoobj(X, y, b[2:3], 0.5, add_intercept = FALSE)$penalty, 0.5 * 1.1, tolerance = 1e-12)
  expect_error(Lassoobj(X, y, b[1:2], 1), "one entry per column")
  Xc <- cbind(1, X)
  l <- Lassrg(y, Xc, lam = 0.8)
  g <- as.numeric(crossprod(Xc, y - Xc %*% l$beta))
  # KKT: intercept column unpenalised, others |g| <= lambda with equality on the active set
  expect_lt(abs(g[1]), 1e-9)
  for (j in 2:3) {
    if (l$beta[j] != 0) expect_equal(g[j], 0.8 * sign(l$beta[j]), tolerance = 1e-9) else expect_lte(abs(g[j]), 0.8 + 1e-9)
  }
  expect_equal(l$objective, 0.5 * sum((y - Xc %*% l$beta)^2) + 0.8 * sum(abs(l$beta[2:3])), tolerance = 1e-12)
  expect_true(l$converged)
})

test_that("Layrnm normalises each row", {
  X <- rbind(c(1, 2, 4), c(-1, 0, 5))
  r <- Layrnm(X, gamma = c(1, 2, 0.5), beta = 0.1)
  mu <- rowMeans(X)
  v <- rowMeans((X - mu)^2)
  Xh <- (X - mu) / sqrt(v + 1e-5)
  expect_equal(r$output, sweep(Xh, 2, c(1, 2, 0.5), "*") + 0.1, tolerance = 1e-12)
  expect_equal(Layrnm(c(3, 5), eps = 0)$normalized, c(-1, 1), tolerance = 1e-12)
  expect_error(Layrnm(1), "at least 2 features")
})

test_that("morie_ld and Ldcmpr compute D, D' and r^2", {
  a <- c(1, 1, 0, 1, 0, 0, 1, 1)
  b <- c(1, 0, 0, 1, 0, 1, 1, 1)
  D <- mean(a * b) - mean(a) * mean(b)
  r2 <- D^2 / (mean(a) * (1 - mean(a)) * mean(b) * (1 - mean(b)))
  expect_equal(morie_ld(a, b)$statistic, r2, tolerance = 1e-12)
  expect_equal(morie_ld(rep(1, 8), b)$statistic, 0)
  expect_error(morie_ld(a, b * 2), "only 0 and 1")
  p <- Ldcmpr(a, b, phased = TRUE)
  expect_equal(p$estimate, r2, tolerance = 1e-12)
  dmax <- if (D > 0) min(mean(a) * (1 - mean(b)), (1 - mean(a)) * mean(b)) else min(mean(a) * mean(b), (1 - mean(a)) * (1 - mean(b)))
  expect_equal(p$Dprime, D / dmax, tolerance = 1e-12)
  g1 <- c(0, 1, 2, 1, 0, 2, 1, 1, 0)
  g2 <- c(0, 1, 2, 2, 0, 1, 1, 0, 0)
  u <- Ldcmpr(g1, g2)
  dp <- Dprime(g1, g2)
  expect_equal(u$estimate, dp$r2)
  expect_equal(u$Dprime, dp$estimate)
  expect_equal(u$r2_genotypic, cor(g1, g2)^2, tolerance = 1e-12)
  expect_error(Ldcmpr(a, b[-1], phased = TRUE), "same length")
})

test_that("morie_ldiff and Ldiff measure l-diversity per equivalence class", {
  qi <- cbind(c(1, 1, 1, 2, 2, 2, 2), c(0, 0, 0, 1, 1, 1, 1))
  s <- c("flu", "flu", "hiv", "flu", "cold", "cold", "hiv")
  r <- morie_ldiff(1:7, qi, s, l = 2, c = 2)
  H <- function(v) {
    p <- table(v) / length(v)
    -sum(p * log(p))
  }
  ents <- c(H(s[1:3]), H(s[4:7]))
  expect_equal(r$distinct_l, 2)
  expect_equal(r$min_entropy, min(ents), tolerance = 1e-12)
  expect_equal(r$entropy_l, exp(min(ents)), tolerance = 1e-12)
  cm <- max(2 / 1, 2 / 2)
  expect_equal(unname(r$c_min), cm, tolerance = 1e-12)
  expect_equal(r$satisfies_recursive, as.numeric(cm < 2))
  expect_equal(Ldiff(1:7, qi, s, l = 2, c = 2), Dpld(1:7, qi, s, l = 2, c = 2))
  expect_error(morie_ldiff(1:7, qi, s, l = 0), "at least 1")
})

test_that("Ldprun drops the lower-MAF member of each correlated pair", {
  set.seed(1)
  g <- sample(0:2, 40, TRUE, prob = c(0.5, 0.35, 0.15))
  h <- sample(0:2, 40, TRUE)
  k <- ifelse(g == 2, 1, g)
  G <- cbind(g, h, g, k)
  r <- Ldprun(G, window = 4, step = 1, r2_threshold = 0.5)
  mf <- apply(G, 2, function(x) min(mean(x) / 2, 1 - mean(x) / 2))
  expect_equal(r$maf, unname(mf), tolerance = 1e-12)
  expect_true(3L %in% r$drop)
  expect_false(2L %in% r$drop)
  expect_equal(r$estimate, 4 - length(r$drop))
  expect_equal(cor(G[, r$keep])[upper.tri(diag(length(r$keep)))]^2 <= 0.5, rep(TRUE, choose(length(r$keep), 2)))
  expect_error(Ldprun(G, window = 1), "window >= 2")
})

test_that("Leid, LemR and leiden_grph delegate to Leidenclus", {
  A <- matrix(0, 6, 6)
  A[1:3, 1:3] <- 1
  A[4:6, 4:6] <- 1
  diag(A) <- 0
  A[3, 4] <- A[4, 3] <- 0.2
  ref <- Leidenclus(A, 1, "modularity", 20)
  expect_equal(Leid(NULL, A), ref)
  expect_equal(LemR(A), Leidenclus(A, resolution = 1, quality = "modularity", max_iter = 20, seed = 0))
  expect_equal(leiden_grph(A, resolution = 0.5)$estimate, LemR(A, resolution = 0.5)$estimate)
})

test_that("Editdist is the weighted Levenshtein distance", {
  expect_equal(Editdist("kitten", "sitting"), adist("kitten", "sitting")[1, 1])
  expect_equal(Editdist("flaw", "lawn", insert = 2, delete = 1, substitute = 3),
               adist("flaw", "lawn", costs = list(insertions = 2, deletions = 1, substitutions = 3))[1, 1])
  expect_equal(Editdist("", "abc", insert = 2), 6)
  expect_equal(Editdist(c(1, 2, 3), c(1, 3)), 1)
})

test_that("Lftrt is the left-truncated Kaplan-Meier estimator", {
  skip_if_not_installed("survival")
  entry <- c(0, 1, 2, 0, 3, 1, 0, 2)
  time <- c(4, 5, 6, 3, 8, 7, 2, 9)
  ev <- c(1, 0, 1, 1, 1, 0, 1, 1)
  r <- Lftrt(entry, time, ev, alpha = 0.1)
  f <- survival::survfit(survival::Surv(entry, time, ev) ~ 1)
  ok <- f$n.event > 0
  expect_equal(r$times, f$time[ok])
  expect_equal(r$survival, f$surv[ok], tolerance = 1e-12)
  se <- (f$surv * f$std.err)[ok]
  # survival reports NaN (0 * Inf) once S reaches 0; Lftrt reports 0 there
  fin <- is.finite(se)
  expect_equal(r$se[fin], se[fin], tolerance = 1e-9)
  expect_equal(r$se[!fin], rep(0, sum(!fin)))
  expect_equal(r$ci_lower, pmax(r$survival - qnorm(0.95) * r$se, 0), tolerance = 1e-12)
})

test_that("lagged_design and morie_lggvls build lagged-value IPTW", {
  set.seed(2)
  n <- 120
  L0 <- rnorm(n)
  A0 <- rbinom(n, 1, plogis(0.5 * L0))
  L1 <- 0.5 * L0 + A0 + rnorm(n)
  A1 <- rbinom(n, 1, plogis(0.3 * L1 + 0.5 * A0))
  y <- L1 + A0 + A1 + rnorm(n)
  ld <- lagged_design(list(L0, L1), NULL, k_time = 1, lag = 1)
  expect_equal(ld, list(L1, L0))
  r <- morie_lggvls(y, list(A0, A1), list(L0, L1), lag = 1)
  pd0 <- fitted(glm(A0 ~ L0, family = binomial))
  pd1 <- fitted(glm(A1 ~ L1 + L0 + A0, family = binomial))
  pn1 <- fitted(glm(A1 ~ A0, family = binomial))
  w0 <- ifelse(A0 == 1, mean(A0) / pd0, (1 - mean(A0)) / (1 - pd0))
  w1 <- ifelse(A1 == 1, pn1 / pd1, (1 - pn1) / (1 - pd1))
  w <- w0 * w1
  # the internal IRLS stops on a 1e-8 coefficient step, glm() on a 1e-8 deviance change
  expect_equal(r$weights, unname(w), tolerance = 1e-7)
  cum <- A0 + A1
  f <- lm(y ~ cum, weights = w)
  expect_equal(r$estimate, unname(coef(f)[2]), tolerance = 1e-7)
  expect_equal(r$se, unname(summary(f)$coefficients[2, 2]), tolerance = 1e-7)
  fe <- morie_lggvls(y, list(A0, A1), list(L0, L1), contrast = "final", trim = 0.9)
  wt <- pmin(pmax(w, quantile(w, 0.1)), quantile(w, 0.9))
  expect_equal(fe$estimate, unname(coef(lm(y ~ A1, weights = wt))[2]), tolerance = 1e-7)
  expect_error(morie_lggvls(y, list(A0), list(L0), contrast = "peak"), "contrast must be")
  expect_error(morie_lggvls(y, list(A0), list(L0), trim = 0.3), "trim must be")
})
