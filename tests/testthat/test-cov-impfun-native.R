# Coverage for IMPUTE2-style imputation (Li & Stephens 2003; Howie,
# Donnelly & Marchini 2009): panel merging by role (union scaffold vs
# intersection), the Li-Stephens copying HMM checked against brute-force
# enumeration of every template path (likelihood and smoothed posterior),
# posterior dosages and the variance-ratio info score.

test_that("panels merge by union; typed SNPs scaffold, the rest are targets", {
  m <- merge_panels(list(p1 = c("a", "b", "c"), p2 = c("b", "c", "d", "e")), c("b", "d", "x"))
  expect_identical(m$scaffold, c("b", "d"))
  expect_identical(m$targets, c("a", "c", "e"))
  expect_identical(m$intersection, c("b", "c"))
  expect_identical(m$gain, 3L)
  expect_error(merge_panels(list(), "a"), "no reference panels")
  expect_error(merge_panels(list(c("a")), "a"), "must have a name")
  expect_error(merge_panels(list(p = "a"), "z"), "nothing to align against")
})

test_that("the copying HMM matches enumeration over all template paths", {
  h <- c(1, 0, 1)
  R <- rbind(c(1, 0, 0), c(0, 0, 1))
  rho <- 0.2
  th <- 0.1
  cm <- copying_model(h, R, rho = rho, theta = th)
  paths <- as.matrix(expand.grid(1:2, 1:2, 1:2))
  em <- function(k, l) if (R[k, l] == h[l]) 1 - th else th
  tr <- function(a, b) (1 - rho) * (a == b) + rho / 2
  pp <- apply(paths, 1, function(z) 0.5 * em(z[1], 1) * tr(z[1], z[2]) * em(z[2], 2) * tr(z[2], z[3]) * em(z[3], 3))
  expect_equal(cm$log_likelihood, log(sum(pp)), tolerance = 1e-12)
  post <- t(vapply(1:3, function(l) vapply(1:2, function(k) sum(pp[paths[, l] == k]), 1) / sum(pp), numeric(2)))
  expect_equal(cm$posterior, post, tolerance = 1e-12)
  fwd1 <- vapply(1:2, function(k) em(k, 1), 1)
  expect_equal(cm$forward[1, ], fwd1 / sum(fwd1), tolerance = 1e-12)
  d <- impute_dosage(cm$posterior, R, 2)
  expect_equal(d$dosage, 0, tolerance = 1e-12)
  d3 <- impute_dosage(cm$posterior, R, 3)
  expect_equal(d3$allele_freq, post[3, 2], tolerance = 1e-12)
  expect_equal(d3$certainty, max(post[3, 2], 1 - post[3, 2]), tolerance = 1e-12)
  expect_identical(impute2, copying_model)
  expect_identical(genotype_imputation, copying_model)
  expect_identical(morie_impfun, copying_model)
  expect_error(copying_model(h, R[, 1:2]), "wrong length")
  expect_error(copying_model(h, R, theta = 0.5), "theta in \\(0,0.5\\)")
  expect_error(copying_model(integer(0), list()), "at least one reference")
  expect_error(impute_dosage(cm$posterior, R, 4), "site 4 is outside")
})

test_that("the info score is var(dosage) / (2 theta (1 - theta))", {
  d <- c(0.1, 1.2, 1.9, 0.8, 1, 0.3)
  th <- mean(d) / 2
  v <- mean((d - mean(d))^2)
  expect_equal(info_score(d)$info, v / (2 * th * (1 - th)), tolerance = 1e-12)
  expect_equal(info_score(c(0, 2, 0, 2))$info, 1)
  expect_identical(info_score(c(0, 0, 0))$info, 1)
  expect_error(info_score(1), "at least 2 individuals")
})
