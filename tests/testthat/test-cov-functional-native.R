# Coverage for fgam_native.R, funBoot/funCA/funcal/funkM/fwxF natives and
# fxidf.R: penalised fits are checked at their normal equations (B-spline
# design from splines::splineDesign), canonical correlations against
# stats::cancor, bootstrap replicates on the SplitMix64 stream, the FWI
# system against the Van Wagner-Pickett (1985) test day.

fn_t <- seq(0, 1, length.out = 7)
fn_X <- rbind(sin(2 * pi * fn_t), cos(2 * pi * fn_t), fn_t, fn_t^2, 1 - fn_t,
              0.5 * sin(pi * fn_t), fn_t^3, 0.3 + 0.2 * fn_t)
fn_w <- c(0.5, rep(1, 5), 0.5) / 6

test_that("morie_fgam solves the tensor-product penalised normal equations", {
  y <- as.numeric(fn_X %*% (fn_w * fn_t)) + c(0.1, -0.05, 0.02, 0.07, -0.1, 0.03, -0.02, 0.05)
  r <- morie_fgam_functional_gam(fn_X, y, n_x = 4, n_t = 5, lam_x = 0.5, lam_t = 2)
  lo <- min(fn_X)
  hi <- max(fn_X)
  kx <- c(rep(lo, 4), rep(hi, 4))
  kt <- c(rep(0, 4), 0.5, rep(1, 4))
  cl <- function(x, a, b) pmin(pmax(x, a + 1e-12), b - 1e-12)
  Bt <- splines::splineDesign(kt, cl(fn_t, 0, 1), ord = 4)
  Z <- t(vapply(1:8, function(i) {
    Bx <- splines::splineDesign(kx, cl(fn_X[i, ], lo, hi), ord = 4)
    colSums(fn_w * t(vapply(1:7, function(t) kronecker(Bx[t, ], Bt[t, ]), numeric(20))))
  }, numeric(20)))
  D2 <- function(n) diff(diag(n), differences = 2)
  P <- 0.5 * kronecker(crossprod(D2(4)), diag(5)) + 2 * kronecker(diag(4), crossprod(D2(5)))
  A <- crossprod(Z) + P
  A <- A + diag(1e-8 * sum(diag(A)) / 20, 20)
  th <- solve(A, crossprod(Z, y - mean(y)))
  expect_equal(r$coefficients, as.numeric(th), tolerance = 1e-8)
  expect_equal(r$fitted, mean(y) + as.numeric(Z %*% th), tolerance = 1e-8)
  expect_equal(r$edf, sum(diag(Z %*% solve(A, t(Z)))), tolerance = 1e-8)
  expect_same_function(morie_fgam, morie_fgam_functional_gam)
  expect_equal(morie_fgam_functional_gam(fn_X, y, basis = 4)$n_t, 4L)
  expect_error(morie_fgam_functional_gam(fn_X[0, ], numeric(0)), "no curves")
  expect_error(morie_fgam_functional_gam(fn_X, y[-1]), "responses")
  expect_error(morie_fgam_functional_gam(fn_X, y, n_x = 3), "at least 4")
})

test_that("morie_funBoot draws curve resamples on the SplitMix64 stream", {
  r <- morie_funBoot(fn_X, B = 6L, alpha = 0.2, seed = 3)
  e <- .ghc_rng(3)
  reps <- t(vapply(1:6, function(b) {
    idx <- vapply(1:8, function(i) min(floor(.ghc_unif(e, 1L) * 8), 7) + 1, 0)
    colMeans(fn_X[idx, ])
  }, numeric(7)))
  ctr <- colMeans(reps)
  d <- sqrt(rowSums(sweep(reps, 2, ctr)^2) / 7)
  expect_equal(r$center, ctr, tolerance = 1e-12)
  expect_equal(r$distances, d, tolerance = 1e-12)
  expect_equal(r$radius, sort(d)[ceiling(0.8 * 6)], tolerance = 1e-12)
  expect_equal(r$estimate, colMeans(fn_X), tolerance = 1e-12)
  s <- morie_funBoot(fn_X, B = 4L, metric = "sup", smooth = 0.1, seed = 1)
  e <- .ghc_rng(1)
  reps2 <- t(vapply(1:4, function(b) {
    rows <- lapply(1:8, function(i) {
      k <- min(floor(.ghc_unif(e, 1L) * 8), 7) + 1
      fn_X[k, ] + 0.1 * .ghc_norm(e, 1L)
    })
    colMeans(do.call(rbind, rows))
  }, numeric(7)))
  expect_equal(s$distances, apply(abs(sweep(reps2, 2, colMeans(reps2))), 1, max), tolerance = 1e-12)
  expect_same_function(functional_bootstrap_band, morie_funBoot)
  expect_error(morie_funBoot(fn_X[1:2, ]), "n >= 3")
  expect_error(morie_funBoot(fn_X, alpha = 1), "\\(0, 1\\)")
  expect_error(morie_funBoot(fn_X, metric = "l1"), "'l2' or 'sup'")
})

test_that("morie_funCA canonical correlations equal cancor on the FPC scores", {
  Y <- cbind(fn_X[, 7:1] * 0.7, rowMeans(fn_X)) + 0.1 * cos(outer(1:8, 1:8))
  r <- morie_funCA_functional_cca(fn_X, Y, p = 2, q = 2)
  fp <- function(M, w, k) {
    C <- crossprod(sweep(M, 2, colMeans(M))) / nrow(M)
    e <- eigen(outer(sqrt(w), sqrt(w)) * C, symmetric = TRUE)
    phi <- e$vectors[, 1:k] / sqrt(w)
    sweep(M, 2, colMeans(M)) %*% (phi * w)
  }
  wy <- c(0.5, rep(1, 6), 0.5) / 7
  cc <- cancor(fp(fn_X, fn_w, 2), fp(Y, wy, 2))$cor
  expect_equal(r$correlations, cc, tolerance = 1e-8)
  expect_equal(r$p, 2L)
  auto <- morie_funCA_functional_cca(fn_X, Y)
  expect_gte(auto$explained_x, 0.95)
  expect_same_function(morie_funCA, morie_funCA_functional_cca)
  expect_error(morie_funCA_functional_cca(fn_X, Y[-1, ]), "same number of curves")
  expect_error(morie_funCA_functional_cca(fn_X[1:2, ], Y[1:2, ]), "three paired")
  expect_error(morie_funCA_functional_cca(matrix(1, 4, 3), matrix(1:12, 4)), "no variation")
})

test_that("morie_funcal_cheatsheet and morie_funkM_imputed_svd_error", {
  expect_type(morie_funcal_cheatsheet(), "character")
  expect_match(morie_funcal_cheatsheet(), "eggNOG")
  rt <- data.frame(u = c(0, 0, 1, 2, 2, 3), i = c(0, 2, 1, 0, 3, 2), r = c(5, 3, 4, 2, 1, 4))
  s <- morie_funkM_imputed_svd_error(rt, 4, 4, rank = 1)
  M <- matrix(0, 4, 4)
  M[cbind(rt$u + 1, rt$i + 1)] <- rt$r
  sv <- svd(M)
  A <- sv$d[1] * outer(sv$u[, 1], sv$v[, 1])
  expect_equal(s$rmse_on_observed, sqrt(mean((rt$r - A[cbind(rt$u + 1, rt$i + 1)])^2)),
               tolerance = 1e-12)
  sm <- morie_funkM_imputed_svd_error(list(c(0, 0, 5), c(1, 1, 3)), 2, 2, rank = 2, fill = "mean")
  expect_equal(sm$rmse_on_observed, 0, tolerance = 1e-12)
  s2 <- morie_funkM_imputed_svd_error(rt, 4, 4, rank = 2)
  A2 <- sv$u[, 1:2] %*% diag(sv$d[1:2]) %*% t(sv$v[, 1:2])
  expect_equal(s2$rmse_on_observed, sqrt(mean((rt$r - A2[cbind(rt$u + 1, rt$i + 1)])^2)),
               tolerance = 1e-12)
  expect_error(morie_funkM_imputed_svd_error(rt, 4, 4, fill = "x"), "zero or mean")
  expect_error(morie_funkM_imputed_svd_error(list(c(1, 2)), 2, 2), "\\(u, i, r\\)")
  expect_error(morie_funkM_imputed_svd_error(data.frame(a = 1), 2, 2), "columns u, i, r")
  expect_error(morie_funkM_imputed_svd_error(1:3, 2, 2), "data.frame or list")
})

test_that("morie_fwxF follows the FTR-33 equations on a dry day", {
  r <- morie_fwxF(17, 42, 25, 0, 4)
  m0 <- 147.2 * (101 - 85) / (59.5 + 85)
  ed <- 0.942 * 42^0.679 + 11 * exp((42 - 100) / 10) + 0.18 * (21.1 - 17) * (1 - exp(-0.115 * 42))
  k0 <- 0.424 * (1 - (42 / 100)^1.7) + 0.0694 * sqrt(25) * (1 - (42 / 100)^8)
  m <- ed + (m0 - ed) * 10^(-k0 * 0.581 * exp(0.0365 * 17))
  ffmc <- 59.5 * (250 - m) / (147.2 + m)
  dmc <- 6 + 100 * 1.894 * (17 + 1.1) * (100 - 42) * 12.8 * 1e-6
  dc <- 15 + 0.5 * (0.36 * (17 + 2.8) + 0.9)
  fm <- 147.2 * (101 - ffmc) / (59.5 + ffmc)
  # the FTR-33 program folds 0.208 * 91.9 into the constant 19.115
  isi <- 19.115 * exp(0.05039 * 25) * exp(-0.1386 * fm) * (1 + fm^5.31 / 4.93e7)
  bui <- if (dmc <= 0.4 * dc) 0.8 * dmc * dc / (dmc + 0.4 * dc) else
    dmc - (1 - 0.8 * dc / (dmc + 0.4 * dc)) * (0.92 + (0.0114 * dmc)^1.7)
  bb <- 0.1 * isi * (0.626 * bui^0.809 + 2)
  fwi <- if (bb > 1) exp(2.72 * (0.434 * log(bb))^0.647) else bb
  expect_equal(c(r$ffmc, r$dmc, r$dc, r$isi, r$bui, r$fwi), c(ffmc, dmc, dc, isi, bui, fwi),
               tolerance = 1e-12)
  expect_equal(r$dsr, 0.0272 * fwi^1.77, tolerance = 1e-12)
  wet <- morie_fwxF(c(17, 12), c(42, 90), c(25, 5), c(0, 12), c(4, 4))
  expect_lt(wet$ffmc[2], wet$ffmc[1])
  expect_lt(wet$dc[2], wet$dc[1])
  expect_same_function(fire_weather_index, morie_fwxF)
  expect_error(morie_fwxF(1:2, 40, 5, 0, 4), "equal length")
  expect_error(morie_fwxF(10, 40, 5, 0, 13), "1..12")
  expect_error(morie_fwxF(1:2, c(40, 40), c(5, 5), c(0, 0), 1:3), "scalar or match")
  expect_error(morie_fwxF(10, 140, 5, 0, 4), "\\[0, 100\\]")
})

test_that("Fxidf fits the X by V interaction", {
  x <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0)
  v <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 0)
  y <- 1 + 2 * x + 0.5 * v + 1.5 * x * v + c(0.1, -0.2, 0.05, 0.15, -0.1, 0.2, -0.05, -0.1, 0.1, 0)
  r <- Fxidf(y, x, v)
  f <- lm(y ~ x * v)
  cf <- summary(f)$coefficients
  expect_equal(r$estimate, unname(coef(f)["x:v"]), tolerance = 1e-10)
  expect_equal(r$se, cf["x:v", 2], tolerance = 1e-10)
  expect_equal(r$effect_at_1, unname(coef(f)["x"] + coef(f)["x:v"]), tolerance = 1e-10)
})
