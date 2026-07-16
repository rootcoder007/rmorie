# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 18 cross-validation: native psychometrics vs psych; native
# IRT vs mirt/ltm (reference packages allowed here only).
library(testthat)
library(rmorie)

set.seed(160)
n <- 600
f <- rnorm(n)
X <- sapply(1:6, function(j) 0.7 * f + rnorm(n, 0, 0.6))

test_that("native KMO equals psych::KMO", {
  skip_if_not_installed("psych")
  ref <- psych::KMO(cor(X))
  mine <- morie_psymet_kmo(X)
  expect_equal(mine$msa, unname(ref$MSA), tolerance = 1e-8)
  expect_equal(unname(mine$items), unname(as.numeric(ref$MSAi)),
               tolerance = 1e-8)
})

test_that("native omega tracks psych::omega on a one-factor scale", {
  skip_if_not_installed("psych")
  ref <- suppressWarnings(suppressMessages(
    psych::omega(X, nfactors = 1, plot = FALSE, flip = FALSE)))
  mine <- morie_psymet_omega(X)
  # Different extraction (PCA-style vs fa): agree to a few points.
  expect_equal(mine$total, unname(as.numeric(ref$omega.tot)),
               tolerance = 0.05)
})

test_that("native parallel analysis agrees with psych::fa.parallel", {
  skip_if_not_installed("psych")
  ref <- suppressWarnings(suppressMessages(
    psych::fa.parallel(X, n.iter = 100, plot = FALSE, fa = "pc",
                       error.bars = FALSE)))
  expect_equal(morie_psymet_parallel(X), max(as.integer(ref$ncomp), 1L))
})

test_that("native 2PL matches mirt item parameters", {
  skip_if_not_installed("mirt")
  set.seed(161)
  n <- 2000
  th <- rnorm(n)
  a_true <- c(0.9, 1.3, 1.7, 1.1)
  b_true <- c(-0.8, -0.2, 0.4, 1.0)
  R <- vapply(1:4, function(j)
    rbinom(n, 1, plogis(a_true[j] * (th - b_true[j]))), numeric(n))
  mine <- morie_irt_2pl(R)
  ref <- mirt::mirt(as.data.frame(R), 1, itemtype = "2PL",
                    verbose = FALSE)
  cf <- mirt::coef(ref, IRTpars = TRUE, simplify = TRUE)$items
  expect_equal(unname(mine$discrimination), unname(cf[, "a"]),
               tolerance = 0.1)
  expect_equal(unname(mine$difficulty), unname(cf[, "b"]),
               tolerance = 0.1)
})
