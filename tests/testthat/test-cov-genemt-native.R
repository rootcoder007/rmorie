# Coverage tests for R/genemt_native.R (de Leeuw et al. 2015, MAGMA): LD
# principal components, the gene F statistic, gene-set regression with
# covariates and the conditional set test, checked against lm.

gm_G <- cbind(c(0, 1, 2, 1, 0, 2, 1, 1, 0, 2), c(0, 1, 2, 1, 1, 2, 1, 0, 0, 2), c(1, 0, 1, 2, 0, 1, 2, 1, 1, 0))
gm_y <- c(0.3, 1.1, 2.2, 1.4, 0.2, 2.5, 1.2, 0.6, -0.1, 2.1)

test_that("LD principal components of the standardised genotypes", {
  pc <- morie_genemt_ld_principal_components(gm_G, keep = 0.95)
  Z <- scale(gm_G)
  e <- eigen(cor(gm_G), symmetric = TRUE)
  k <- which(cumsum(e$values) / sum(e$values) >= 0.95)[1]
  expect_equal(pc$n_components, k)
  expect_equal(abs(pc$components), abs(unname(Z %*% e$vectors[, 1:k, drop = FALSE])), tolerance = 1e-10)
  expect_equal(pc$variance_explained, sum(e$values[1:k]) / 3, tolerance = 1e-12)
  G1 <- cbind(gm_G, 1)
  expect_equal(morie_genemt_ld_principal_components(G1)$n_markers, 4L)
  expect_identical(morie_genemt, morie_genemt_ld_principal_components)
})

test_that("gene statistic: F of the regression on the PCs and its normal transform", {
  g <- morie_genemt_gene_statistic(gm_y, gm_G, keep = 1)
  pc <- morie_genemt_ld_principal_components(gm_G, keep = 1)$components
  a <- anova(lm(gm_y ~ 1), lm(gm_y ~ pc))
  expect_equal(g$F, a$F[2], tolerance = 1e-8)
  expect_equal(c(g$df1, g$df2), c(3, 6))
  expect_equal(g$p, 1 - pnorm(sqrt(2 * g$F) - sqrt(2 * 3 - 1)), tolerance = 1e-12)
  expect_error(morie_genemt_gene_statistic(gm_y[-1], gm_G), "9 phenotypes but 10")
})

test_that("gene-set regression and the conditional test agree with lm", {
  z <- c(1.2, 0.3, 2.1, -0.4, 1.8, 0.1, 2.5, 0.2, -0.3, 1.1, 0.9, 0.4)
  a <- c(1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0)
  b <- c(1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0)
  len <- c(10, 22, 15, 8, 30, 12, 9, 25, 14, 11, 18, 20)
  # the package solves the normal equations with a 1e-8 ridge, which moves
  # the coefficients by ~1e-8 relative: compared at 1e-6
  r <- morie_genemt_gene_set_regression(z, a)
  f <- summary(lm(z ~ a))$coefficients
  expect_equal(unname(r$beta), f[2, 1], tolerance = 1e-6)
  expect_equal(r$se, f[2, 2], tolerance = 1e-6)
  expect_equal(unname(r$p), 1 - pnorm(f[2, 3]), tolerance = 1e-6)
  rc <- morie_genemt_gene_set_regression(z, a, covariates = cbind(log(len)))
  fc <- summary(lm(z ~ a + log(len)))$coefficients
  expect_equal(unname(rc$beta), fc[2, 1], tolerance = 1e-6)
  expect_equal(rc$se, fc[2, 2], tolerance = 1e-6)
  ct <- morie_genemt_conditional_set_test(z, a, b)
  fb <- summary(lm(z ~ a + b))$coefficients
  expect_equal(unname(ct$conditional_beta), fb[2, 1], tolerance = 1e-6)
  expect_equal(unname(ct$conditional_p), 1 - pnorm(fb[2, 3]), tolerance = 1e-6)
  expect_equal(unname(ct$attenuation), unname((r$beta - ct$conditional_beta) / r$beta), tolerance = 1e-12)
  expect_error(morie_genemt_gene_set_regression(z, a[-1]), "12 z-scores but 11")
  expect_error(morie_genemt_conditional_set_test(z, a, b[-1]), "differ in length")
})

test_that("gene size and density covariates", {
  cv <- morie_genemt_gene_covariates(c(10, 40, 5), c(1000, 2000, 500), ld_scores = c(1.2, 3.4, 0.8))
  expect_equal(cv$covariates, unname(cbind(log(c(10, 40, 5)), log(c(1000, 2000, 500)), log(c(0.01, 0.02, 0.01)), c(1.2, 3.4, 0.8))), ignore_attr = TRUE)
  expect_equal(cv$names[4], "ld_score")
  expect_error(morie_genemt_gene_covariates(c(1, 2), 3), "2 marker counts but 1")
  expect_error(morie_genemt_gene_covariates(0, 3), "positive")
  expect_match(morie_genemt_cheatsheet(), "MAGMA")
})
