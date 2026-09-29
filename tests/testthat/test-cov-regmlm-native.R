# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/regmlm_native.R (REGENIE Step 1, Mbatchou et al.
# 2021). Blocks never straddle a chromosome; level-0 ridges are
# (X'X + lam I)^-1 X'y at lam = p (1 - h) / h; level 1 stacks them
# under cross-validation; LOCO drops each chromosome's blocks; the
# Step 2 test matches lm() with the offset.

.rg_G <- outer(1:12, 1:7, function(i, j) ((i * j * 7) %% 5) - 2 + 0.1 * j)
.rg_y <- c(1.2, -0.4, 0.8, 2.1, -1, 0.3, 0.9, -0.7, 1.5, 0.2, -0.3, 1.1)

.rg_ridge <- function(X, y, lam) solve(crossprod(X) + lam * diag(ncol(X)), crossprod(X, y))

test_that("make_blocks cuts at block_size and at every chromosome change", {
  b <- make_blocks(7, c(1, 1, 1, 2, 2, 3, 3), block_size = 2)
  expect_equal(vapply(b, function(x) x$start, 0), c(0, 2, 3, 5))
  expect_equal(vapply(b, function(x) x$stop, 0), c(2, 3, 5, 7))
  expect_equal(vapply(b, function(x) x$chromosome, 0L), c(1L, 1L, 2L, 3L))
  expect_equal(vapply(b, function(x) x$size, 0), c(2, 1, 2, 2))
  one <- make_blocks(5, block_size = 10)
  expect_length(one, 1L)
  expect_equal(one[[1]]$size, 5)
  expect_error(make_blocks(0), "no markers")
  expect_error(make_blocks(3, block_size = 0), "block_size")
  expect_error(make_blocks(3, c(1, 2)), "one chromosome label")
})

test_that("ridge_fit is (X'X + lam I)^-1 X'y", {
  f <- ridge_fit(.rg_G, .rg_y, 2.5)
  expect_equal(f$beta, as.numeric(.rg_ridge(.rg_G, .rg_y, 2.5)), tolerance = 1e-12)
  expect_equal(f$fitted, as.numeric(.rg_G %*% f$beta), tolerance = 1e-12)
  fl <- ridge_fit(lapply(1:12, function(i) .rg_G[i, ]), .rg_y, 2.5)
  expect_equal(fl$beta, f$beta, tolerance = 1e-12)
  expect_error(ridge_fit(.rg_G, .rg_y[-1], 1), "same length")
  expect_error(ridge_fit(.rg_G, .rg_y, -1), "cannot be negative")
})

test_that("level-0 fits J ridges per block; level 1 stacks with CV", {
  bl <- make_blocks(7, c(1, 1, 1, 2, 2, 2, 2), block_size = 3)
  l0 <- level0_predictors(.rg_G, .rg_y, bl, n_ridge = 2)
  expect_equal(l0$n_predictors, 3 * 2)
  lam <- 3 * (1 - 1 / 3) / (1 / 3)
  expect_equal(l0$predictors[[1]], as.numeric(.rg_G[, 1:3] %*% .rg_ridge(.rg_G[, 1:3], .rg_y, lam)),
               tolerance = 1e-12)
  expect_equal(l0$meta[[2]]$lam, 3 * (1 - 2 / 3) / (2 / 3))
  expect_equal(l0$reduction, 7 / 6)
  X <- do.call(cbind, l0$predictors)
  s <- level1_stack(l0$predictors, .rg_y, "kfold", k = 3)
  expect_equal(s$weights, as.numeric(.rg_ridge(X, .rg_y, 6)), tolerance = 1e-12)
  oof <- numeric(12)
  for (f in 0:2) {
    te <- which((0:11) %% 3 == f)
    oof[te] <- X[te, ] %*% .rg_ridge(X[-te, ], .rg_y[-te], 6)
  }
  expect_equal(s$out_of_fold, oof, tolerance = 1e-12)
  lo <- level1_stack(l0$predictors, .rg_y, "loo", lam = 1)
  expect_equal(lo$out_of_fold[5], sum(X[5, ] * .rg_ridge(X[-5, ], .rg_y[-5], 1)), tolerance = 1e-12)
  expect_error(level1_stack(l0$predictors, .rg_y, "boot"), "cv must be one of")
  expect_error(level1_stack(list(), .rg_y), "no level-0")
})

test_that("loco_predictions drop the blocks of the left-out chromosome", {
  preds <- list(1:3, c(2, 0, 1), c(5, 5, 5))
  meta <- list(list(chromosome = 1), list(chromosome = 2), list(chromosome = 2))
  w <- c(0.5, 2, -1)
  r <- loco_predictions(preds, meta, w)
  expect_equal(r$loco[["1"]], 2 * c(2, 0, 1) - c(5, 5, 5))
  expect_equal(r$loco[["2"]], 0.5 * (1:3))
  expect_equal(loco_predictions(preds, meta, w, chromosomes = 3)$loco[["3"]], 0.5 * (1:3) + 2 * c(2, 0, 1) - 5)
})

test_that("test_variant matches lm() with the offset and a normal p-value", {
  g <- c(0, 1, 2, 1, 0, 1, 2, 2, 0, 1, 0, 1)
  off <- seq(-0.5, 0.6, length.out = 12)
  cv <- cos(1:12)
  r <- test_variant(g, .rg_y, offset = off, covariates = list(cv))
  fit <- summary(lm(.rg_y ~ g + cv, offset = off))$coefficients
  expect_equal(r$beta, fit["g", 1], tolerance = 1e-12)
  expect_equal(r$se, fit["g", 2], tolerance = 1e-12)
  expect_equal(r$p_value, 2 * pnorm(-abs(fit["g", 1] / fit["g", 2])), tolerance = 1e-12)
  r0 <- test_variant(g, .rg_y)
  expect_equal(r0$beta, unname(coef(lm(.rg_y ~ g))[2]), tolerance = 1e-12)
  expect_error(test_variant(g[-1], .rg_y), "lengths differ")
  expect_error(test_variant(g, .rg_y, offset = 1:3), "every sample")
  expect_error(test_variant(rep(1, 12), .rg_y), "monomorphic")
})

test_that("morie_regmlm chains blocks, level 0, level 1 and LOCO", {
  chr <- c(1, 1, 1, 2, 2, 2, 2)
  for (fn in list(morie_regmlm, whole_genome_regression)) {
    r <- fn(.rg_G, .rg_y, chromosomes = chr, block_size = 2, n_ridge = 2, k = 3)
    bl <- make_blocks(7, chr, 2)
    l0 <- level0_predictors(.rg_G, .rg_y, bl, 2)
    l1 <- level1_stack(l0$predictors, .rg_y, "kfold", 3)
    expect_equal(r$n_blocks, length(bl))
    expect_equal(r$estimate, l1$prediction, tolerance = 1e-12)
    expect_equal(r$loco, loco_predictions(l0$predictors, l0$meta, l1$weights)$loco, tolerance = 1e-12)
  }
  expect_error(morie_regmlm(.rg_G, .rg_y[-1]), "same number of samples")
})
