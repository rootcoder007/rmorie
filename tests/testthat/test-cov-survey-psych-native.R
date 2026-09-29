# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/survey_psych_native.R: Aitchison balances, the
# Astle-Balding genomic relationship matrix, balanced repeated
# replication (with the Fay adjustment) and Haebara / Stocking-Lord
# IRT linking. The BRR check uses the defining identity: with
# orthogonal Hadamard columns the replicate variance of a total is
# sum_h (y_h1 - y_h2)^2 exactly, for every Fay k.

test_that("aitchison_balance is sqrt(rs/(r+s)) log(g_num / g_den)", {
  X <- rbind(c(0.2, 0.3, 0.1, 0.4), c(0.25, 0.25, 0.25, 0.25))
  r <- morie_aitchison_balance(X, c(0, 1), 3)
  gn <- sqrt(X[, 1] * X[, 2])
  expect_equal(r$balance, sqrt(2 / 3) * log(gn / X[, 4]), tolerance = 1e-12)
  expect_equal(r$balance[2], 0)
  expect_error(morie_aitchison_balance(cbind(0, 1), 0, 1), "strictly positive")
  expect_error(morie_aitchison_balance(X, c(0, 1), 1), "disjoint")
  expect_error(morie_aitchison_balance(X, 0, 4), "0..3")
  expect_error(morie_aitchison_balance(X, integer(0), 1), "non-empty")
})

test_that("astle_balding_grm standardises each marker by 2p(1-p)", {
  G <- rbind(c(0, 1, 2, 1, 0), c(1, 1, 0, 2, 0), c(2, 0, 1, 1, 0), c(1, 2, 1, 0, 0))
  r <- morie_astle_balding_grm(G)
  p <- colMeans(G) / 2
  keep <- p > 0 & p < 1
  Z <- sweep(sweep(G[, keep], 2, 2 * p[keep]), 2, sqrt(2 * p[keep] * (1 - p[keep])), "/")
  expect_equal(r$G, Z %*% t(Z) / sum(keep), tolerance = 1e-12)
  expect_equal(r$n_dropped, 1L)
  expect_length(r$warnings, 2L)
  f <- c(0.4, 0.5, 0.5, 0.45, 0.1)
  rf <- morie_astle_balding_grm(G, freq = f)
  Zf <- sweep(sweep(G, 2, 2 * f), 2, sqrt(2 * f * (1 - f)), "/")
  expect_equal(rf$G, Zf %*% t(Zf) / 5, tolerance = 1e-12)
  expect_length(rf$warnings, 1L)
  expect_error(morie_astle_balding_grm(G + 1), "coded 0, 1 or 2")
  expect_error(morie_astle_balding_grm(G, freq = 0.5), "freq has 1 entries")
  expect_error(morie_astle_balding_grm(matrix(0, 3, 2)), "monomorphic")
})

test_that("brr_balanced builds orthogonal half-samples; brr_variance recovers sum d_h^2", {
  st <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  y <- c(3, 5, 2, 2.5, 7, 4, 1, 1.5, 6, 3)
  d2 <- sum((y[c(1, 3, 5, 7, 9)] - y[c(2, 4, 6, 8, 10)])^2)
  for (k in c(0, 0.5)) {
    b <- morie_brr_balanced(st, fay_k = k)
    expect_equal(b$n_replicates, 8L)
    expect_equal(crossprod(b$hadamard), diag(8, 5))
    expect_equal(rowSums(b$replicate_weights), rep(2 * 5, 8))
    est <- as.numeric(b$replicate_weights %*% y)
    v <- morie_brr_variance(est, full_estimate = sum(y), fay_k = k)
    expect_equal(v$variance, d2, tolerance = 1e-12)
    expect_equal(v$se, sqrt(d2), tolerance = 1e-12)
  }
  expect_equal(morie_brr_balanced(rep(1:3, each = 2))$n_replicates, 4L)
  v2 <- morie_brr_variance(c(1, 2, 3, 6))
  expect_equal(v2$estimate, 3)
  expect_equal(v2$variance, (4 + 1 + 0 + 9) / 4)
  expect_equal(v2$cv, sqrt(3.5) / 3)
  expect_error(morie_brr_balanced(c(1, 1, 1, 2, 2)), "has 3 PSUs")
  expect_error(morie_brr_balanced(st, fay_k = 1), "fay_k")
  expect_error(morie_brr_variance(1), "at least 2")
  expect_error(morie_brr_variance(1:3, fay_k = -1), "fay_k")
})

test_that("Haebara and Stocking-Lord recover a known linear transformation", {
  a_r <- c(1.2, 0.8, 1.5, 1, 0.6)
  b_r <- c(-1, 0.2, 0.9, -0.3, 1.4)
  A <- 1.3
  B <- -0.4
  a_f <- a_r * A
  b_f <- (b_r - B) / A
  for (fn in list(morie_equating_haebara, morie_equating_stocking_lord)) {
    r <- fn(a_r, b_r, a_f, b_f)
    # Nelder-Mead with reltol 1e-12 on a criterion that is quadratic at
    # its zero: the constants are recovered to about 1e-5
    expect_equal(c(r$A, r$B), c(A, B), tolerance = 1e-4)
    expect_lt(r$criterion, 1e-10)
    expect_equal(r$a_transformed, a_f / r$A)
    expect_equal(r$b_transformed, r$A * b_f + r$B)
  }
  expect_error(morie_equating_haebara(a_r, b_r, a_f[-1], b_f), "same length")
  expect_error(morie_equating_haebara(numeric(0), numeric(0), numeric(0), numeric(0)), "at least one anchor")
  expect_error(morie_equating_stocking_lord(-a_r, b_r, a_f, b_f), "positive")
})
