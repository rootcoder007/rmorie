# Coverage tests for R/gwasem_native.R (EMMAX, Kang et al. 2010): IBS
# kinship, Gower normalisation, REML/ML variance components against a
# direct V^-1 computation, the GLS F and score tests, genomic control and
# the per-marker REML path.

gw_G <- function() {
  cbind(c(2, 1, 0, 2, 1, 0, 2, 1, 0, 2), c(0, 1, 1, 2, 0, 0, 1, 2, 2, 1),
        1, c(1, 0, 2, 1, 1, 0, 0, 2, 1, 2))
}
gw_y <- function() 0.5 * gw_G()[, 1] + sin(1:10)

gw_ll <- function(delta, y, X, K, ml = FALSE) {
  n <- length(y)
  V <- K + delta * diag(n)
  Vi <- solve(V)
  M <- t(X) %*% Vi %*% X
  b <- solve(M, t(X) %*% Vi %*% y)
  r <- y - X %*% b
  rss <- c(t(r) %*% Vi %*% r)
  ldV <- c(determinant(V)$modulus)
  if (ml) return(-0.5 * (n * log(2 * pi * rss / n) + n + ldV))
  df <- n - ncol(X)
  -0.5 * (df * log(2 * pi * rss / df) + df + ldV + c(determinant(M)$modulus))
}

test_that("IBS kinship and Gower normalisation", {
  G <- gw_G()
  S <- morie_gwasem_kinship(G)
  expect_equal(S, 1 - as.matrix(dist(G, "manhattan")) / 8, ignore_attr = TRUE)
  expect_equal(morie_gwasem_kinship_ibs(G), S)
  n <- 10
  expect_equal(morie_gwasem_gower(S), S * (n - 1) / (sum(diag(S)) - sum(S) / n), tolerance = 1e-12)
  P <- diag(n) - 1 / n
  expect_equal(sum(diag(P %*% morie_gwasem_gower(S) %*% P)), n - 1, tolerance = 1e-12)
  expect_error(morie_gwasem_kinship(matrix(0, 0, 3)), "non-empty")
  expect_error(morie_gwasem_kinship_ibs(matrix(0, 2, 0)), "non-empty")
  expect_error(morie_gwasem_gower(matrix(1)), "at least two")
  expect_error(morie_gwasem_gower(matrix(1, 3, 3)), "zero centred trace")
})

test_that("REML and ML variance components maximise the profile likelihood", {
  y <- gw_y()
  S <- morie_gwasem_kinship(gw_G())
  for (ml in c(FALSE, TRUE)) {
    vc <- morie_gwasem_reml(y, S, ml = ml)
    K <- vc$kinship_normalized + vc$shift * diag(10)
    X <- matrix(1, 10, 1)
    expect_equal(vc$loglik, gw_ll(vc$delta, y, X, K, ml), tolerance = 1e-9)
    # the log-delta search is bounded at 10; an interior optimum is a local max
    if (log(vc$delta) < 9.99) expect_gte(vc$loglik, gw_ll(vc$delta * exp(0.01), y, X, K, ml))
    expect_gte(vc$loglik, gw_ll(vc$delta * exp(-0.01), y, X, K, ml))
    V <- K + vc$delta * diag(10)
    b <- solve(t(X) %*% solve(V, X), t(X) %*% solve(V, y))
    r <- y - X %*% b
    expect_equal(vc$sigma_a2, c(t(r) %*% solve(V, r)) / (10 - !ml), tolerance = 1e-9)
    expect_equal(vc$sigma_e2, vc$sigma_a2 * vc$delta, tolerance = 1e-12)
    expect_equal(vc$pseudo_heritability, 1 / (1 + vc$delta), tolerance = 1e-12)
    rss0 <- sum((y - mean(y))^2)
    df0 <- 10 - !ml
    ll0 <- -0.5 * (df0 * log(2 * pi * rss0 / df0) + df0) - if (ml) 0 else 0.5 * log(10)
    expect_equal(vc$loglik_null, ll0, tolerance = 1e-12)
    expect_equal(vc$lrt, max(0, 2 * (vc$loglik - ll0)), tolerance = 1e-12)
  }
  cv <- morie_gwasem_reml(y, S, covariates = cos(1:10))
  Kc <- cv$kinship_normalized + cv$shift * diag(10)
  expect_equal(cv$loglik, gw_ll(cv$delta, y, cbind(1, cos(1:10)), Kc), tolerance = 1e-9)
  expect_error(morie_gwasem_reml(y[-1], S), "n x n")
  expect_error(morie_gwasem_reml(y, S, covariates = 1:3), "one covariate row")
})

test_that("EMMAX F and score tests equal direct GLS with the null delta", {
  G <- gw_G()
  y <- gw_y()
  r <- morie_gwasem(y, G)
  vc <- r$variance_components
  V <- vc$kinship_normalized + (vc$shift + vc$delta) * diag(10)
  Vi <- solve(V)
  expect_equal(r$skipped, 3L)
  expect_true(is.na(r$beta[3]) && r$pvalue[3] == 1)
  sc <- morie_gwasem(y, G, test = "score")
  one <- matrix(1, 10, 1)
  for (j in c(1, 2, 4)) {
    X <- cbind(1, G[, j])
    Mi <- solve(t(X) %*% Vi %*% X)
    b <- Mi %*% t(X) %*% Vi %*% y
    e <- y - X %*% b
    s2 <- c(t(e) %*% Vi %*% e) / 8
    expect_equal(r$beta[j], b[2], tolerance = 1e-9)
    expect_equal(r$se[j], sqrt(s2 * Mi[2, 2]), tolerance = 1e-9)
    expect_equal(r$stat[j], b[2]^2 / (s2 * Mi[2, 2]), tolerance = 1e-9)
    expect_equal(r$pvalue[j], pf(r$stat[j], 1, 8, lower.tail = FALSE), tolerance = 1e-9)
    b0 <- sum(Vi %*% y) / sum(Vi)
    r0 <- y - b0
    s20 <- c(t(r0) %*% Vi %*% r0) / 9
    xr <- G[, j] - sum(Vi %*% G[, j]) / sum(Vi)
    chi <- c(t(xr) %*% Vi %*% r0)^2 / (c(t(xr) %*% Vi %*% xr) * s20)
    expect_equal(sc$stat[j], chi, tolerance = 1e-9)
    expect_equal(sc$pvalue[j], 2 * pnorm(-sqrt(chi)), tolerance = 1e-12)
  }
  expect_equal(r$lambda_gc, median(r$stat[-3]) / qchisq(0.5, 1), tolerance = 1e-12)
  expect_equal(morie_gwasem(y, G, min_maf = 0.49)$skipped, which(pmin(colSums(G) / 20, 1 - colSums(G) / 20) < 0.49 | apply(G, 2, var) == 0))
  kin <- morie_gwasem_kinship(G)
  expect_equal(morie_gwasem(y, G, kinship = kin)$stat, r$stat)
  expect_error(morie_gwasem(y[-1], G), "one genotype row")
  expect_error(morie_gwasem(y, G, trait = "count"), "quantitative")
  expect_error(morie_gwasem(y, G, trait = "binary"), "coded 0/1")
  expect_error(morie_gwasem(y, G, test = "lrt"), "'f' or 'score'")
})

test_that("per-marker REML re-estimates delta with the marker in the model", {
  G <- gw_G()
  y <- gw_y()
  S <- morie_gwasem_kinship(G)
  r <- morie_gwasem(y, G, per_marker_reml = TRUE, covariates = cos(1:10))
  for (j in c(1, 4)) {
    vcj <- morie_gwasem_reml(y, S, cbind(cos(1:10), G[, j]))
    expect_equal(vcj$loglik, gw_ll(vcj$delta, y, cbind(1, cos(1:10), G[, j]), vcj$kinship_normalized + vcj$shift * diag(10)), tolerance = 1e-9)
    V <- vcj$kinship_normalized + (vcj$shift + vcj$delta) * diag(10)
    X <- cbind(1, cos(1:10), G[, j])
    Mi <- solve(t(X) %*% solve(V, X))
    b <- Mi %*% t(X) %*% solve(V, y)
    e <- y - X %*% b
    s2 <- c(t(e) %*% solve(V, e)) / 7
    expect_equal(r$beta[j], b[3], tolerance = 1e-9)
    expect_equal(r$se[j], sqrt(s2 * Mi[3, 3]), tolerance = 1e-9)
  }
})

test_that("genomic control inflation factor", {
  s <- c(0.2, 1.5, 0.7, 3.1)
  expect_equal(morie_gwasem_gc(s), mean(c(0.7, 1.5)) / qchisq(0.5, 1), tolerance = 1e-12)
  expect_equal(morie_gwasem_gc(s[-4]), 0.7 / qchisq(0.5, 1), tolerance = 1e-12)
  # df > 1 uses the Wilson-Hilferty median, within 1% of the exact median
  expect_equal(morie_gwasem_gc(s, df = 3), 1.1 / (3 * (1 - 2 / 27)^3), tolerance = 1e-12)
  expect_equal(3 * (1 - 2 / 27)^3, qchisq(0.5, 3), tolerance = 1e-2)
  expect_error(morie_gwasem_gc(numeric(0)), "no statistics")
  for (f in c(0.3, 2, 9)) for (d2 in c(3, 8, 40)) {
    expect_equal(.gwasem_f_sf(f, 1, d2), pf(f, 1, d2, lower.tail = FALSE), tolerance = 1e-10)
  }
  expect_equal(.gwasem_f_sf(0, 1, 5), 1)
})
