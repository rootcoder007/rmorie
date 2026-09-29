# Coverage for SMILES descriptors, Morris screening, PWM scanning with
# exact p-values, mean reciprocal rank, MC-NNM matrix completion, MSE and
# SSE losses, stratified Manski bounds and Monte Carlo zonal statistics;
# recomputed from the definitions in the test body.

test_that("LipinskiDescriptors and SmilesHba count atoms, H-bond groups, rotors and TPSA", {
  m <- c(H = 1.008, C = 12.011, N = 14.007, O = 15.999)
  e <- LipinskiDescriptors("CCO")
  expect_equal(e$molecular_weight, 2 * m[["C"]] + m[["O"]] + 6 * m[["H"]], tolerance = 1e-12)
  expect_equal(c(e$hba, e$hbd, e$rotatable_bonds), c(1, 1, 0))
  expect_equal(e$tpsa, 20.23, tolerance = 1e-12)
  a <- LipinskiDescriptors("CC(=O)Oc1ccccc1C(=O)O")
  expect_equal(a$molecular_weight, 9 * m[["C"]] + 8 * m[["H"]] + 4 * m[["O"]], tolerance = 1e-12)
  expect_equal(c(a$hba, a$hbd), c(4, 1))
  # Ertl contributions: two carbonyl O (17.07), one ester O (9.23), one hydroxyl O (20.23)
  expect_equal(a$tpsa, 2 * 17.07 + 9.23 + 20.23, tolerance = 1e-12)
  expect_equal(SmilesHba("CC(=O)N"), 2)
  expect_equal(LipinskiDescriptors("CCCC")$rotatable_bonds, 1L)
})

test_that("morie_morrisM recovers exact elementary effects of a linear model", {
  cf <- c(2, -1, 0.5)
  bnds <- list(c(0, 1), c(-2, 2), c(10, 12))
  r <- morie_morrisM(function(x) sum(cf * x), k = 3, r = 5, p = 4, seed = 2, bounds = bnds)
  width <- vapply(bnds, diff, 0)
  expect_equal(r$mu, cf * width, tolerance = 1e-12)
  expect_equal(r$mu_star, abs(cf * width), tolerance = 1e-12)
  expect_equal(r$sigma, rep(0, 3), tolerance = 1e-12)
  expect_equal(r$n_runs, 5L * 4L)
  expect_equal(r$delta, 4 / 6, tolerance = 1e-12)
  q <- morie_morrisM(function(x) x[1] * x[2], k = 2, r = 3, seed = 1)
  expect_true(all(q$sigma >= 0))
  expect_error(morie_morrisM(function(x) 0, k = 2, bounds = list(c(0, 1))), "k pairs")
})

test_that("Motfom scans a PWM and computes exact p-values", {
  pwm <- rbind(c(8, 1, 1, 0), c(1, 1, 7, 1), c(0, 9, 0, 1))
  seq <- "TAGCAGCTGAC"
  r <- Motfom(seq, pwm, pseudocount = 0.5)
  pr <- (pwm + 0.5) / rowSums(pwm + 0.5)
  llr <- log2(pr / 0.25)
  il <- round(llr * 1000)
  code <- match(strsplit(seq, "")[[1]], c("A", "C", "G", "T"))
  sc <- vapply(1:9, function(i) sum(llr[cbind(1:3, code[i:(i + 2)])]), 0)
  expect_equal(r$scores, sc, tolerance = 1e-12)
  km <- as.matrix(expand.grid(1:4, 1:4, 1:4))
  ks <- apply(km, 1, function(a) sum(il[cbind(1:3, a)]))
  pv <- vapply(1:9, function(i) mean(ks >= sum(il[cbind(1:3, code[i:(i + 2)])])), 0)
  expect_equal(r$pvalues, pv, tolerance = 1e-12)
  expect_equal(r$best_position, which.max(sc) - 1L)
  expect_error(Motfom("AC", pwm, pseudocount = 0.5), "shorter than motif")
  expect_error(Motfom(seq, pwm), "pseudocount")
})

test_that("Mrr, Msetst and Ssello", {
  pr <- rbind(c(3, 1, 2), c(2, 3, 1), c(1, 2, 3))
  rel <- rbind(c(2, 9), c(7, 8), c(1, 5))
  r <- Mrr(pr, rel)
  expect_equal(r$rr, c(1 / 3, 0, 1), tolerance = 1e-12)
  expect_equal(r$estimate, 4 / 9, tolerance = 1e-12)
  m <- Msetst(c(1, 2, 3), c(1.5, 2, 2))
  expect_equal(c(m$mse, m$rmse), c(1.25 / 3, sqrt(1.25 / 3)), tolerance = 1e-12)
  s <- Ssello(matrix(1:4, 2), matrix(c(1, 3, 2, 5), 2))
  expect_equal(s$loss, 0.5 * (1 + 1 + 1), tolerance = 1e-12)
  expect_error(Ssello(matrix(1, 2, 2), matrix(1, 2, 3)), "same shape")
})

test_that("Mscmcl reaches the soft-impute fixed point", {
  set.seed(1)
  U <- matrix(rnorm(8 * 2), 8)
  V <- matrix(rnorm(6 * 2), 6)
  Y <- U %*% t(V) + matrix(rnorm(48, sd = 0.1), 8)
  D <- matrix(0, 8, 6)
  D[7:8, 5:6] <- 1
  Y[D == 1] <- Y[D == 1] + 2
  r <- Mscmcl(Y, D, lam = 0.5)
  expect_equal(r$converged, 1)
  Z <- ifelse(D == 0, Y, r$L)
  s <- svd(Z)
  L2 <- s$u %*% diag(pmax(s$d - 0.5, 0)) %*% t(s$v)
  expect_equal(r$L, L2, tolerance = 1e-8)
  expect_equal(r$att, mean((Y - r$L)[D == 1]), tolerance = 1e-12)
  expect_equal(r$nuclear, sum(svd(r$L)$d), tolerance = 1e-9)
  expect_error(Mscmcl(Y, matrix(1, 8, 6), 0.5), "every cell is treated")
  expect_error(Mscmcl(Y, D[, 1:5], 0.5), "same shape")
})

test_that("Mskbnd2 aggregates stratum-wise no-assumption bounds", {
  y <- c(3, 5, 2, 8, 4, 6, 7, 1)
  D <- c(1, 0, 1, 1, 0, 0, 1, 0)
  X <- c(1, 1, 1, 2, 2, 2, 2, 1)
  r <- Mskbnd2(y, D, X, 0, 10)
  b <- vapply(1:2, function(k) {
    i <- X == k
    p1 <- mean(D[i])
    m1 <- mean(y[i & D == 1])
    m0 <- mean(y[i & D == 0])
    c(m1 * p1 - (m0 * (1 - p1) + 10 * p1), m1 * p1 + 10 * (1 - p1) - m0 * (1 - p1))
  }, numeric(2))
  w <- c(4, 4) / 8
  expect_equal(c(r$lower, r$upper), as.numeric(b %*% w), tolerance = 1e-12)
  expect_equal(c(r$inter_lower, r$inter_upper), c(max(b[1, ]), min(b[2, ])), tolerance = 1e-12)
  expect_error(Mskbnd2(y, D, X, 5, 4), "y_min")
  expect_error(Mskbnd2(y + 20, D, X, 0, 10), "must lie in")
})

test_that("McZonal estimates areas from the Philox point set", {
  B <- rbind(c(-1, 1), c(-1, 1))
  r <- McZonal(function(x, y) x^2 + y^2 <= 1, B, value = function(x, y) x^2, n = 4000, seed = 3)
  U <- matrix(.morie_random_uniform(8000, seed = 3, stream = 0), 4000, 2, byrow = TRUE)
  P <- 2 * U - 1
  hit <- rowSums(P^2) <= 1
  expect_equal(r$hits, sum(hit))
  expect_equal(r$area, 4 * mean(hit), tolerance = 1e-12)
  expect_equal(r$se_area, 4 * sqrt(mean(hit) * (1 - mean(hit)) / 4000), tolerance = 1e-12)
  expect_equal(r$zone_mean, mean(P[hit, 1]^2), tolerance = 1e-12)
  expect_lt(abs(r$area - pi), 4 * r$se_area)
})
