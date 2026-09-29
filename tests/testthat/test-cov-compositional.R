# Coverage for the Aitchison compositional-data files (ait*.R): expected
# values are recomputed from the log-ratio definitions with base R.

clr_ref <- function(x) log(x) - mean(log(x))
sbp_ref <- function(D) {
  vapply(seq_len(D - 1), function(i) {
    c(rep(1 / i, i), -1, rep(0, D - i - 1)) * sqrt(i / (i + 1))
  }, numeric(D))
}
comp_x <- c(0.1, 0.25, 0.4, 0.05, 0.2)
comp_y <- c(0.3, 0.1, 0.2, 0.25, 0.15)
comp_X <- rbind(
  c(0.20, 0.30, 0.50), c(0.25, 0.25, 0.50), c(0.10, 0.60, 0.30),
  c(0.30, 0.20, 0.50), c(0.15, 0.45, 0.40), c(0.35, 0.35, 0.30),
  c(0.05, 0.70, 0.25), c(0.40, 0.10, 0.50)
)

test_that("Alr and Alrinv are inverse log-ratio maps", {
  r <- Alr(comp_x)
  expect_equal(r$alr, log(comp_x[1:4] / comp_x[5]), tolerance = 1e-12)
  r2 <- Alr(comp_x, ref = 2)
  expect_equal(r2$alr, log(comp_x[-2] / comp_x[2]), tolerance = 1e-12)
  expect_equal(Alrinv(r2$alr, ref = 2)$composition, comp_x / sum(comp_x),
               tolerance = 1e-12)
  expect_equal(Alrinv(c(0, log(2)), total = 10)$composition, 10 * c(1, 2, 1) / 4,
               tolerance = 1e-12)
  expect_error(Alr(1), "at least two")
  expect_error(Alr(c(1, 0)), "strictly positive")
  expect_error(Alr(comp_x, ref = 9), "1-based")
  expect_error(Alrinv(numeric(0)), "non-empty")
  expect_error(Alrinv(1, ref = 5), "1-based")
})

test_that("Clr and Clrinv", {
  r <- Clr(comp_x)
  expect_equal(r$clr, clr_ref(comp_x), tolerance = 1e-12)
  expect_equal(r$geomean, exp(mean(log(comp_x))), tolerance = 1e-12)
  expect_equal(Clrinv(r$clr, total = 100)$composition, 100 * comp_x / sum(comp_x),
               tolerance = 1e-12)
  expect_error(Clr(numeric(0)), "non-empty")
  expect_error(Clr(c(1, -1)), "strictly positive")
  expect_error(Clrinv(numeric(0)), "non-empty")
})

test_that("Aitilr and Aitilri use the Egozcue SBP basis", {
  V <- sbp_ref(5)
  expect_equal(crossprod(V), diag(4), tolerance = 1e-12)
  r <- Aitilr(comp_x)
  expect_equal(r$y, as.numeric(crossprod(V, clr_ref(comp_x))), tolerance = 1e-12)
  expect_equal(r$norm, sqrt(sum(clr_ref(comp_x)^2)), tolerance = 1e-12)
  expect_equal(r$aitchison_norm, r$norm, tolerance = 1e-12)
  W <- qr.Q(qr(cbind(1, matrix(c(1, 2, 0, -1, 3, 1, 0, 2, 1, 1, -2, 0, 1, 1, 1), 5))))[, 2:4]
  r3 <- Aitilr(comp_x, V = W)
  expect_equal(r3$y, as.numeric(crossprod(W, clr_ref(comp_x))), tolerance = 1e-12)
  inv <- Aitilri(r$y, kappa = 2)
  expect_equal(inv$x, 2 * comp_x / sum(comp_x), tolerance = 1e-12)
  expect_equal(Aitilri(r3$y, V = W)$logx_unclosed, as.numeric(W %*% r3$y),
               tolerance = 1e-12)
  expect_error(Aitilr(1), "at least 2")
  expect_error(Aitilr(c(1, 0)), "strictly positive")
  expect_error(Aitilr(comp_x, V = diag(3)), "wrong number of rows")
  expect_error(Aitilri(numeric(0)), "empty")
  expect_error(Aitilri(1, kappa = 0), "positive")
  expect_error(Aitilri(c(1, 2), V = W), "columns")
})

test_that("closure, perturbation, powering, subcomposition, amalgamation", {
  expect_equal(Compclos(c(2, 3, 5), total = 100)$closed, c(20, 30, 50),
               tolerance = 1e-12)
  expect_error(Compclos(c(1, 0)), "strictly positive")
  p <- Perturb(comp_x, comp_y)
  expect_equal(p$composition, comp_x * comp_y / sum(comp_x * comp_y), tolerance = 1e-12)
  expect_error(Perturb(1:3, 1:2), "same number")
  expect_error(Perturb(c(1, 0), c(1, 1)), "strictly positive")
  pw <- Powering(1.7, comp_x, total = 3)
  expect_equal(pw$composition, 3 * comp_x^1.7 / sum(comp_x^1.7), tolerance = 1e-12)
  expect_equal(Clr(pw$composition)$clr, 1.7 * clr_ref(comp_x), tolerance = 1e-12)
  expect_error(Powering(2, c(1, -1)), "strictly positive")
  s <- Subcomp(comp_x, c(1, 3, 4))
  expect_equal(s$composition, comp_x[c(1, 3, 4)] / sum(comp_x[c(1, 3, 4)]),
               tolerance = 1e-12)
  expect_error(Subcomp(comp_x, 1), "at least two")
  expect_error(Subcomp(comp_x, c(1, 7)), "one-based")
  expect_error(Subcomp(c(1, 0, 1), 1:2), "strictly positive")
  a <- Amalgam(comp_x, c(2, 5))
  raw <- c(comp_x[c(1, 3, 4)], comp_x[2] + comp_x[5])
  expect_equal(a$composition, raw / sum(raw), tolerance = 1e-12)
  expect_equal(a$amalgamated, 0.45, tolerance = 1e-12)
  expect_error(Amalgam(comp_x, 2), "at least two")
  expect_error(Amalgam(comp_x, c(0, 2)), "one-based")
  expect_error(Amalgam(c(0, 1, 1), 1:2), "strictly positive")
})

test_that("Aitchison geometry: distance, norm, inner product, geometric mean", {
  d <- Compdist(comp_x, comp_y)
  expect_equal(d$distance, sqrt(sum((clr_ref(comp_x) - clr_ref(comp_y))^2)),
               tolerance = 1e-12)
  expect_error(Compdist(1:3, 1:2), "same number")
  expect_error(Compdist(1, 1), "at least two")
  expect_error(Compdist(c(1, 0), c(1, 1)), "strictly positive")
  expect_equal(Compnorm(comp_x)$norm, sqrt(sum(clr_ref(comp_x)^2)), tolerance = 1e-12)
  expect_error(Compnorm(1), "at least two")
  expect_error(Compnorm(c(1, 0)), "strictly positive")
  ip <- Compip(comp_x, comp_y)
  ref <- sum(clr_ref(comp_x) * clr_ref(comp_y))
  expect_equal(ip$inner, ref, tolerance = 1e-12)
  expect_equal(ip$inner_pairwise, ref, tolerance = 1e-12)
  expect_equal(ip$cos_angle, ref / sqrt(sum(clr_ref(comp_x)^2) * sum(clr_ref(comp_y)^2)),
               tolerance = 1e-12)
  expect_true(is.nan(Compip(c(1, 1), c(1, 2))$cos_angle))
  expect_error(Compip(1:3, 1:2), "same number")
  expect_error(Compip(c(1, 0), c(1, 1)), "strictly positive")
  g <- Compgeo(comp_x)
  expect_equal(g$geomean, prod(comp_x)^(1 / 5), tolerance = 1e-12)
  expect_error(Compgeo(c(1, 0)), "strictly positive")
})

test_that("centre, log-ratio mean, variation matrix and total variance", {
  gm <- apply(comp_X, 2, function(v) exp(mean(log(v))))
  expect_equal(Compcen(comp_X)$center, gm / sum(gm), tolerance = 1e-12)
  expect_error(Compcen(-comp_X), "strictly positive")
  lm <- Complrm(comp_X, total = 100)
  Z <- t(apply(comp_X, 1, clr_ref))
  expect_equal(lm$clr_mean, colMeans(Z), tolerance = 1e-12)
  expect_equal(lm$center, 100 * gm / sum(gm), tolerance = 1e-12)
  expect_error(Complrm(-comp_X), "strictly positive")
  L <- log(comp_X)
  tau <- outer(1:3, 1:3, Vectorize(function(i, j) var(L[, i] - L[, j])))
  cv <- Compvar(comp_X)
  expect_equal(cv$variation, tau, tolerance = 1e-12)
  expect_equal(cv$totvar, sum(tau) / (2 * 3), tolerance = 1e-12)
  expect_error(Compvar(-comp_X), "strictly positive")
  tv <- Comptotvar(comp_X)
  expect_equal(tv$totvar, sum(tau) / 6, tolerance = 1e-12)
  expect_equal(tv$totvar_trace, sum(apply(Z, 2, var)), tolerance = 1e-12)
  expect_error(Comptotvar(-comp_X), "strictly positive")
})

test_that("Hillq, Ginisimp and Compjsd diversity measures", {
  a <- c(10, 0, 5, 3, 2)
  p <- a / sum(a)
  pp <- p[p > 0]
  expect_equal(Hillq(a, q = 0)$hill, 4, tolerance = 1e-12)
  expect_equal(Hillq(a, q = 1)$hill, exp(-sum(pp * log(pp))), tolerance = 1e-12)
  expect_equal(Hillq(a, q = 2)$hill, 1 / sum(pp^2), tolerance = 1e-12)
  expect_equal(Hillq(a, q = 0.5)$hill, sum(pp^0.5)^2, tolerance = 1e-12)
  expect_error(Hillq(numeric(0)), "non-empty")
  expect_error(Hillq(c(1, -1)), "non-negative")
  expect_error(Hillq(c(0, 0)), "all be zero")
  g <- Ginisimp(a)
  expect_equal(g$D, 1 - sum(p^2), tolerance = 1e-12)
  expect_equal(g$inv_simpson, 1 / sum(p^2), tolerance = 1e-12)
  expect_true(is.nan(Ginisimp(c(0, 0))$D))
  q <- c(1, 4, 2, 2, 1)
  q <- q / sum(q)
  m <- (p + q) / 2
  kl <- function(u, v) sum(ifelse(u > 0, u * log2(u / v), 0))
  expect_equal(Compjsd(a, c(1, 4, 2, 2, 1))$estimate, (kl(p, m) + kl(q, m)) / 2,
               tolerance = 1e-12)
})

test_that("Aitzmu multiplicative replacement keeps ratios of non-zero parts", {
  X <- rbind(c(0.5, 0, 0.3, 0.2), c(0, 0.6, 0, 0.4))
  r <- Aitzmu(X, delta = 0.01)
  expect_equal(r$X_imp[1, ], c(0.5 * 0.99, 0.01, 0.3 * 0.99, 0.2 * 0.99), tolerance = 1e-12)
  expect_equal(r$X_imp[2, ], c(0.01, 0.6 * 0.98, 0.01, 0.4 * 0.98), tolerance = 1e-12)
  expect_equal(r$n_zero, 3L)
  v <- Aitzmu(c(0.5, 0, 0.5), delta = c(0.1, 0.02, 0.1))
  expect_equal(v$X_imp, c(0.49, 0.02, 0.49), tolerance = 1e-12)
  expect_error(Aitzmu(X, delta = c(1, 2)), "wrong length")
  expect_error(Aitzmu(X, delta = 0), "strictly positive")
  expect_error(Aitzmu(c(1, -1), 0.1), "negative")
  expect_error(Aitzmu(c(0, 0), 0.1), "sums to zero")
  expect_error(Aitzmu(c(0.01, 0), 0.1), "exceeds")
  expect_error(Aitzmu(1, 0.1), "at least 2")
})

test_that("Aitdir and Aitdrl match the Dirichlet density and score", {
  al <- c(2, 3.5, 1.2)
  x <- c(0.2, 0.5, 0.3)
  lf <- lgamma(sum(al)) - sum(lgamma(al)) + sum((al - 1) * log(x))
  r <- Aitdir(x, al)
  expect_equal(r$log_f, lf, tolerance = 1e-12)
  expect_equal(r$f, exp(lf), tolerance = 1e-12)
  expect_error(Aitdir(1, 1), "at least 2")
  expect_error(Aitdir(x, c(1, 1)), "different lengths")
  expect_error(Aitdir(x, c(1, 0, 1)), "strictly positive")
  expect_error(Aitdir(c(0, 0.5, 0.5), al), "inside")
  expect_error(Aitdir(c(0.2, 0.2, 0.2), al), "sum to one")
  l <- Aitdrl(al, comp_X)
  ll <- sum(apply(comp_X, 1, function(z)
    lgamma(sum(al)) - sum(lgamma(al)) + sum((al - 1) * log(z))))
  expect_equal(l$ll, ll, tolerance = 1e-12)
  sc <- nrow(comp_X) * (digamma(sum(al)) - digamma(al)) + colSums(log(comp_X))
  expect_equal(l$score, sc, tolerance = 1e-9)
  expect_equal(Aitdrl(al, x)$ll, lf, tolerance = 1e-12)
  expect_error(Aitdrl(1, x), "at least 2")
  expect_error(Aitdrl(c(1, 0, 1), x), "strictly positive")
  expect_error(Aitdrl(al, c(0.5, 0.5)), "does not match")
  expect_error(Aitdrl(al, c(0.5, 0.5, 0.5)), "sum to one")
  expect_error(Aitdrl(al, c(0, 0.5, 0.5)), "inside")
})

test_that("Aitdrr maximises the Dirichlet-regression likelihood", {
  X <- cbind(1, c(-1, -0.5, 0, 0.5, 1, 1.5, -1.5, 0.2))
  r <- Aitdrr(X, comp_X, max_iter = 4000L)
  A <- exp(X %*% r$beta)
  ll <- sum(vapply(seq_len(8), function(i) lgamma(sum(A[i, ])) - sum(lgamma(A[i, ])) +
    sum((A[i, ] - 1) * log(comp_X[i, ])), numeric(1)))
  expect_equal(r$ll, ll, tolerance = 1e-12)
  expect_equal(r$alpha, A, tolerance = 1e-12)
  expect_equal(r$phi, mean(rowSums(A)), tolerance = 1e-12)
  negll <- function(b) {
    B <- matrix(b, 2)
    Ab <- exp(X %*% B)
    -sum(lgamma(rowSums(Ab)) - rowSums(lgamma(Ab)) + rowSums((Ab - 1) * log(comp_X)))
  }
  opt <- stats::optim(as.numeric(r$beta), negll, method = "BFGS",
                      control = list(reltol = 1e-14, maxit = 1000))
  # the ascent stops on its score tolerance; BFGS from there gains < 1e-6
  expect_lt(-opt$value - r$ll, 1e-6)
  expect_error(Aitdrr(X, comp_X[1:3, ]), "different row counts")
  expect_error(Aitdrr(X, comp_X * 2), "sum to one")
  expect_error(Aitdrr(X, comp_X, ref = 5), "out of range")
  expect_error(Aitdrr(X[, 1, drop = FALSE], matrix(1, 8, 1)), "at least 2")
})

test_that("Aitcrg is OLS of ilr(Y) on X", {
  X <- cbind(1, c(-1, -0.5, 0, 0.5, 1, 1.5, -1.5, 0.2))
  r <- Aitcrg(X, comp_X)
  Yi <- t(apply(comp_X, 1, clr_ref)) %*% sbp_ref(3)
  B <- qr.solve(X, Yi)
  expect_equal(r$Y_ilr, Yi, tolerance = 1e-12)
  expect_equal(r$beta, B, tolerance = 1e-10)
  expect_equal(r$sse, sum((Yi - X %*% B)^2), tolerance = 1e-10)
  fc <- t(apply(X %*% B %*% t(sbp_ref(3)), 1, function(z) exp(z) / sum(exp(z))))
  expect_equal(r$fitted_comp, fc, tolerance = 1e-10)
  expect_error(Aitcrg(X, comp_X[1:3, ]), "different row counts")
  expect_error(Aitcrg(X, -comp_X), "positive")
  expect_error(Aitcrg(X, matrix(1, 8, 1)), "at least 2")
})

test_that("Aitcap classifies by summed Aitchison distance among k neighbours", {
  y <- c(1, 1, 2, 1, 2, 1, 2, 1)
  xn <- rbind(c(0.12, 0.55, 0.33), c(0.33, 0.2, 0.47))
  r <- Aitcap(comp_X, y, xn, k = 3)
  Zt <- t(apply(comp_X, 1, clr_ref))
  pred <- apply(xn, 1, function(z) {
    d <- sqrt(colSums((t(Zt) - clr_ref(z))^2))
    nb <- order(d)[1:3]
    tot <- tapply(d[nb], y[nb], sum)
    as.numeric(names(tot)[which.min(tot)])
  })
  expect_equal(r$yhat, pred)
  expect_equal(r$dist, sqrt(colSums((t(Zt) - clr_ref(xn[1, ]))^2)), tolerance = 1e-12)
  s <- Aitcap(comp_X, y, xn[1, ], k = 1)
  expect_length(s$yhat, 1L)
  expect_error(Aitcap(comp_X, y[-1], xn, 3), "different lengths")
  expect_error(Aitcap(comp_X, y, xn, 0), "k must lie")
  expect_error(Aitcap(comp_X, y, c(0.5, 0.5), 3), "wrong number of parts")
  expect_error(Aitcap(-comp_X, y, xn, 3), "positive")
})

test_that("Compkm is Lloyd k-means on clr coordinates", {
  X <- rbind(comp_X, comp_X[, 3:1])
  r <- Compkm(X, k = 2)
  Z <- t(apply(X, 1, clr_ref))
  km <- stats::kmeans(Z, centers = Z[1:2, ], algorithm = "Lloyd", iter.max = 50)
  expect_equal(r$cluster, as.integer(km$cluster))
  expect_equal(r$clr_centers, unname(km$centers), tolerance = 1e-12)
  expect_equal(r$tot_withinss, km$tot.withinss, tolerance = 1e-12)
  expect_error(Compkm(X, k = 1), "2 <= k")
  expect_error(Compkm(-X), "strictly positive")
})

test_that("Clrmedian is the Weiszfeld spatial median of the clr rows", {
  r <- Clrmedian(comp_X, steps = 5000)
  Z <- t(apply(comp_X, 1, clr_ref))
  dv <- sweep(Z, 2, r$clrmed)
  g <- colSums(dv / sqrt(rowSums(dv^2)))
  expect_lt(max(abs(g)), 1e-8)
  expect_equal(r$objective, sum(sqrt(rowSums(dv^2))), tolerance = 1e-12)
  expect_equal(r$clrmean, colMeans(Z), tolerance = 1e-12)
  expect_equal(r$median, exp(r$clrmed) / sum(exp(r$clrmed)), tolerance = 1e-12)
  expect_error(Clrmedian(matrix(1, 2, 1)), "two parts")
  expect_error(Clrmedian(-comp_X), "strictly positive")
})

test_that("Clrpca matches prcomp on the clr coordinates", {
  r <- Clrpca(comp_X, k = 2)
  Z <- t(apply(comp_X, 1, clr_ref))
  pc <- stats::prcomp(Z)
  expect_equal(r$values[1:2], pc$sdev[1:2]^2, tolerance = 1e-10)
  expect_equal(abs(r$loadings), abs(unname(pc$rotation[, 1:2])), tolerance = 1e-10)
  expect_equal(abs(r$scores), abs(unname(pc$x[, 1:2])), tolerance = 1e-10)
  expect_equal(r$prop_var, pc$sdev[1:2]^2 / sum(pc$sdev^2), tolerance = 1e-10)
  expect_error(Clrpca(-comp_X), "strictly positive")
})

test_that("Permanova pseudo-F from pairwise Aitchison distances", {
  g <- c("a", "a", "b", "a", "b", "c", "b", "c")
  r <- Permanova(comp_X, g)
  Z <- t(apply(comp_X, 1, clr_ref))
  D2 <- as.matrix(stats::dist(Z))^2
  sst <- sum(D2[upper.tri(D2)]) / 8
  ssw <- sum(vapply(c("a", "b", "c"), function(l) {
    s <- D2[g == l, g == l]
    sum(s[upper.tri(s)]) / sum(g == l)
  }, numeric(1)))
  expect_equal(r$SST, sst, tolerance = 1e-12)
  expect_equal(r$SSW, ssw, tolerance = 1e-12)
  expect_equal(r$F, ((sst - ssw) / 2) / (ssw / 5), tolerance = 1e-12)
  re <- Permanova(comp_X, g, aitchison = FALSE)
  D2e <- as.matrix(stats::dist(comp_X))^2
  expect_equal(re$SST, sum(D2e[upper.tri(D2e)]) / 8, tolerance = 1e-12)
  expect_error(Permanova(comp_X, g[-1]), "one label")
  expect_error(Permanova(comp_X, rep("a", 8)), "two groups")
  expect_error(Permanova(comp_X[1:2, ], c("a", "b")), "three units")
})

test_that("logistic-normal density, fit and simulation", {
  mu <- c(0.2, -0.3)
  S <- matrix(c(0.5, 0.1, 0.1, 0.3), 2)
  x <- c(0.3, 0.2, 0.5)
  y <- log(x[1:2] / x[3])
  q <- sum((y - mu) * solve(S, y - mu))
  ld <- -log(2 * pi) - 0.5 * log(det(S)) - sum(log(x)) - 0.5 * q
  r <- Lgtnpdf(x, mu, S)
  expect_equal(r$log_density, ld, tolerance = 1e-12)
  expect_error(Lgtnpdf(1, 0, 1), "at least two")
  expect_error(Lgtnpdf(c(1, 0), 0, 1), "strictly positive")
  expect_error(Lgtnpdf(x, 0, S), "D-1 entries")
  expect_error(Lgtnpdf(x, mu, diag(3)), "\\(D-1\\) x \\(D-1\\)")
  f <- Lgtnfit(comp_X)
  Y <- log(comp_X[, 1:2] / comp_X[, 3])
  expect_equal(f$mu, colMeans(Y), tolerance = 1e-12)
  expect_equal(f$Sigma, unname(cov(Y)), tolerance = 1e-12)
  llr <- sum(apply(comp_X, 1, function(z) {
    d <- log(z[1:2] / z[3]) - colMeans(Y)
    -log(2 * pi) - 0.5 * log(det(cov(Y))) - sum(log(z)) -
      0.5 * sum(d * solve(cov(Y), d))
  }))
  expect_equal(f$loglik, llr, tolerance = 1e-10)
  expect_equal(Lgtnfit(comp_X, ddof = 0)$Sigma, unname(cov(Y)) * 7 / 8, tolerance = 1e-12)
  expect_error(Lgtnfit(comp_X[1, , drop = FALSE]), "at least two")
  expect_error(Lgtnfit(comp_X[, 1, drop = FALSE]), "two parts")
  expect_error(Lgtnfit(comp_X[1:2, ], ddof = 2), "ddof")
  sm <- Lgtnsim(mu, S, n = 3, seed = 7, total = 10)
  s <- 7
  L <- t(chol(S))
  ref <- t(vapply(1:3, function(t) {
    z <- numeric(2)
    for (i in 1:2) {
      s <<- (48271 * s) %% 2147483647
      z[i] <- qnorm(s / 2147483647)
    }
    yy <- mu + as.numeric(L %*% z)
    10 * c(exp(yy), 1) / sum(c(exp(yy), 1))
  }, numeric(3)))
  expect_equal(sm$sample, ref, tolerance = 1e-12)
  expect_error(Lgtnsim(numeric(0), S, 3), "at least one")
  expect_error(Lgtnsim(mu, diag(3), 3), "\\(D-1\\) x \\(D-1\\)")
  expect_error(Lgtnsim(mu, S, 0), "at least 1")
})
