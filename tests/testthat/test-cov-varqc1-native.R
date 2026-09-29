# Variant quality filtering: GATK-style hard filters, the Gaussian
# mixture log density by Cholesky, EM for the VQSR mixtures, and VQSLOD
# tranches set on the training positives.

vq_fields <- c("QD", "QUAL", "FS", "SOR")
vq_recs <- rbind(c(10, 50, 1, 1), c(1.5, 50, 1, 1), c(5, 20, 70, 1), c(8, 100, 2, NA))

test_that("hard filters name every failed threshold", {
  thr <- list(c("QD", "lt", "2"), c("QUAL", "lt", "30"), c("FS", "gt", "60"), c("SOR", "gt", "3"),
              c("MQ", "lt", "40"))
  h <- morie_varqc1_hard(vq_recs, vq_fields, thr)
  expect_identical(h$filter, c("PASS", "QD<2", "QUAL<30;FS>60", "PASS"))
  expect_identical(h$counts[["PASS"]], 2L)
  r <- morie_varqc1(vq_recs, fields = vq_fields)
  expect_identical(r$filter, h$filter)
  expect_equal(r$estimate, 0.5)
  expect_equal(r$se, sqrt(0.25 / 4))
  expect_error(morie_varqc1(vq_recs), "fields")
  expect_error(morie_varqc1(vq_recs, fields = vq_fields, mode = "sv"), "snp or indel")
  expect_error(morie_varqc1(vq_recs, fields = vq_fields, method = "ml"), "method must be")
  expect_error(morie_varqc1(vq_recs, fields = vq_fields, method = "vqsr"), "training indices")
})

test_that("the mixture log density is log sum_k w_k N(x; mu_k, L_k L_k')", {
  S1 <- matrix(c(2, 0.3, 0.3, 1), 2)
  S2 <- matrix(c(0.5, -0.1, -0.1, 0.8), 2)
  x <- c(0.4, -1.2)
  mu <- list(c(0, 0), c(1, -1))
  w <- c(0.3, 0.7)
  dn <- function(x, m, S) {
    r <- x - m
    exp(-0.5 * sum(r * solve(S, r))) / sqrt(det(2 * pi * S))
  }
  ref <- log(w[1] * dn(x, mu[[1]], S1) + w[2] * dn(x, mu[[2]], S2))
  expect_equal(morie_varqc1_logpdf(x, w, mu, list(t(chol(S1)), t(chol(S2)))), ref, tolerance = 1e-13)
})

test_that("EM on the mixture raises the likelihood and reports the final one", {
  set.seed(9)
  X <- rbind(matrix(stats::rnorm(60, 0, 1), 30), matrix(stats::rnorm(60, 4, 0.7), 30))
  for (cv in c("full", "diagonal")) {
    f <- morie_varqc1_mixture(X, 2, n_iter = 30, covariance = cv, shrinkage = 0)
    expect_true(all(diff(f$loglik_trace) > -1e-8))
    ll <- sum(apply(X, 1, function(x) morie_varqc1_logpdf(x, f$weights, f$means, f$chols)))
    expect_equal(f$loglik, ll, tolerance = 1e-10)
    expect_equal(sum(f$weights), 1, tolerance = 1e-15)
    if (cv == "diagonal") {
      expect_equal(f$chols[[1]][2, 1], 0)
    }
  }
  expect_error(morie_varqc1_mixture(X, 2, covariance = "tied"), "covariance must be")
  expect_error(morie_varqc1_mixture(X[1:5, ], 2), "too few training variants")
  expect_error(morie_varqc1_mixture(X, 2, shrinkage = 1), "shrinkage")
})

test_that("VQSR scores the log10 likelihood ratio and cuts tranches on the positives", {
  set.seed(10)
  n <- 60
  good <- cbind(stats::rnorm(n, 20, 3), stats::rnorm(n, 60, 5))
  bad <- cbind(stats::rnorm(20, 5, 2), stats::rnorm(20, 25, 5))
  recs <- rbind(good, bad)
  pos <- 1:40
  neg <- 61:80
  r <- morie_varqc1(recs, fields = c("QD", "QUAL"), method = "vqsr", positive = pos, negative = neg,
                    n_components = 1, n_iter = 5, tranches = c(90, 99))
  g <- morie_varqc1_mixture(recs[pos, ], 1, 5)
  b <- morie_varqc1_mixture(recs[neg, ], 1, 5)
  lod <- apply(recs, 1, function(x) (morie_varqc1_logpdf(x, g$weights, g$means, g$chols) -
                                       morie_varqc1_logpdf(x, b$weights, b$means, b$chols)) / log(10))
  expect_equal(r$vqslod, lod, tolerance = 1e-12)
  ps <- sort(lod[pos], decreasing = TRUE)
  cut90 <- ps[ceiling(0.9 * 40)]
  cut99 <- ps[ceiling(0.99 * 40)]
  expect_equal(vapply(r$tranche_cuts, `[`, 1, 2), c(cut90, cut99), tolerance = 1e-15)
  expect_identical(r$tranche, ifelse(lod >= cut90, "90.0", ifelse(lod >= cut99, "99.0", "FAIL")))
  expect_identical(r$filter, ifelse(r$tranche != "FAIL", "PASS", "VQSRFail"))
  both <- morie_varqc1(recs, fields = c("QD", "QUAL"), method = "both", positive = pos,
                       negative = neg, n_components = 1, n_iter = 5, tranches = c(90, 99))
  expect_identical(both$filter[recs[, 1] < 2 & r$tranche != "FAIL"],
                   rep("QD<2", sum(recs[, 1] < 2 & r$tranche != "FAIL")))
  miss <- recs
  miss[3, 1] <- NA
  expect_error(morie_varqc1(miss, fields = c("QD", "QUAL"), method = "vqsr", positive = pos,
                            negative = neg), "missing annotation")
  expect_match(morie_varqc1_cheatsheet(), "hard, vqsr, both", fixed = TRUE)
})
