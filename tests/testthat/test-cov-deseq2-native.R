# Coverage tests for R/deseq2_native.R (Love, Huber and Anders 2014):
# size factors, the Cox-Reid likelihood, the NB GLM (MASS), dispersion
# estimation and the Wald/BH pipeline.

ds_counts <- function() {
  g <- 1:24
  lapply(g, function(i) {
    base <- 20 + 7 * (i %% 5) + i
    fc <- if (i %% 4 == 0) 3 else 1
    round(c(base * c(1, 1.2, 0.9), base * fc * c(1.1, 0.95, 1.3)) * (1 + ((i * c(1, 3, 5, 7, 11, 13)) %% 7) / 20))
  })
}

test_that("median-of-ratios size factors", {
  K <- list(c(10, 20, 30), c(5, 5, 20), c(0, 4, 8), c(100, 150, 90))
  s <- size_factors(K)
  M <- do.call(rbind, K)[c(1, 2, 4), ]
  gm <- exp(rowMeans(log(M)))
  expect_equal(s, apply(M / gm, 2, median), tolerance = 1e-12)
  expect_error(size_factors(list(c(0, 1), c(1, 0))), "no gene has a positive count")
  expect_error(size_factors(list(c(1, -1))), "non-negative")
})

test_that("Cox-Reid adjusted NB likelihood", {
  K <- c(12, 30, 7, 22)
  mu <- c(10, 25, 9, 20)
  X <- cbind(1, c(0, 1, 0, 1))
  a <- 0.2
  W <- 1 / (1 / mu + a)
  ref <- sum(dnbinom(K, size = 1 / a, mu = mu, log = TRUE)) - 0.5 * log(det(t(X) %*% (X * W)))
  expect_equal(cox_reid_loglik(a, K, mu, X), ref, tolerance = 1e-10)
  expect_error(cox_reid_loglik(0, K, mu, X), "positive")
})

test_that("NB GLM with a fixed dispersion equals MASS negative.binomial with offsets", {
  skip_if_not_installed("MASS")
  K <- c(12, 30, 7, 22, 15, 40)
  x <- c(0, 1, 0, 1, 0, 1)
  s <- c(0.9, 1.1, 1, 1.2, 0.8, 1)
  X <- cbind(1, x)
  f <- nb_glm_fit(K, X, 0.15, s, tol = 1e-12)
  g <- glm(K ~ x + offset(log(s)), family = MASS::negative.binomial(1 / 0.15),
    control = glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(f$beta, unname(coef(g)), tolerance = 1e-8)
  expect_equal(f$mu, unname(fitted(g)), tolerance = 1e-8)
  expect_true(f$converged)
  # with a ridge penalty the fit is a fixed point of penalised IRLS
  fr <- nb_glm_fit(K, X, 0.15, s, lam = c(0, 2), tol = 1e-12)
  w <- 1 / (1 / fr$mu + 0.15)
  z <- log(fr$mu / s) + (K - fr$mu) / fr$mu
  expect_equal(as.numeric(solve(t(X) %*% (X * w) + diag(c(0, 2)), t(X) %*% (w * z))), fr$beta, tolerance = 1e-8)
  expect_lt(abs(fr$beta[2]), abs(f$beta[2]))
  expect_error(nb_glm_fit(K, cbind(1, x, x), 0.1), "singular")
})

test_that("gene-wise dispersion maximises the Cox-Reid likelihood; trend fit", {
  K <- c(12, 30, 7, 22, 15, 40)
  X <- cbind(1, c(0, 1, 0, 1, 0, 1))
  s <- rep(1, 6)
  d <- dispersion_gene_wise(K, X, s)
  obj <- function(a) cox_reid_loglik(a, K, d$mu0, X)
  expect_gte(obj(d$dispersion), obj(d$dispersion * exp(0.01)))
  expect_gte(obj(d$dispersion), obj(d$dispersion * exp(-0.01)))
  mu <- c(5, 10, 20, 50, 100, 300)
  tr <- dispersion_trend(mu, 2 / mu + 0.1)
  expect_equal(c(tr$a1, tr$a0), c(2, 0.1), tolerance = 1e-8)
  expect_equal(tr$fitted, 2 / mu + 0.1, tolerance = 1e-8)
  expect_error(dispersion_trend(c(1, 2), c(0.1, 0.2)), "too few genes")
})

test_that("DESeq2 pipeline: Wald statistics, BH adjustment and shrinkage", {
  K <- ds_counts()
  des <- rep(c("a", "b"), each = 3)
  r <- deseq2(K, des)
  expect_equal(r$size_factors, size_factors(K), tolerance = 1e-12)
  expect_equal(r$stat, r$estimate / r$lfc_se, tolerance = 1e-12)
  expect_equal(r$pvalue, 2 * pnorm(-abs(r$stat)), tolerance = 1e-9)
  expect_equal(r$padj, p.adjust(r$pvalue, "BH"), tolerance = 1e-12)
  expect_equal(r$base_mean, vapply(K, function(k) mean(k / r$size_factors), 0), tolerance = 1e-12)
  expect_true(all(abs(r$estimate) <= abs(r$lfc_mle) + 1e-9))
  up <- which(seq_along(K) %% 4 == 0)
  expect_true(all(r$estimate[up] > 1))
  X <- cbind(1, rep(0:1, each = 3))
  mle <- nb_glm_fit(K[[5]], X, r$dispersion[5], r$size_factors)
  expect_equal(r$lfc_mle[5], mle$beta[2] / log(2), tolerance = 1e-10)
  nb <- deseq2(K, des, beta_prior = FALSE, log2 = FALSE)
  expect_equal(nb$estimate, nb$lfc_mle, tolerance = 1e-12)
  expect_equal(nb$estimate[5], mle$beta[2], tolerance = 1e-10)
  expect_identical(deseq2_de, deseq2)
  expect_identical(deseq2_differential, deseq2)
  expect_identical(differential_expression, deseq2)
  expect_error(deseq2(K, rep("a", 6)), "only one group")
  expect_error(deseq2(K, des, contrast = c(1, 0, 0)), "one entry per coefficient")
})
