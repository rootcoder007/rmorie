# Coverage tests for R/hierF_native.R (Wickramasuriya, Athanasopoulos and
# Hyndman 2019, MinT reconciliation).

hf_S <- summing_matrix(list(0:3, 0:1, 2:3), 4)

test_that("summing matrix and coherence", {
  expect_equal(hf_S, rbind(c(1, 1, 1, 1), c(1, 1, 0, 0), c(0, 0, 1, 1), diag(4)))
  b <- c(2, 3, 1, 4)
  expect_true(is_coherent(as.numeric(hf_S %*% b), hf_S))
  expect_false(is_coherent(as.numeric(hf_S %*% b) + c(1, rep(0, 6)), hf_S))
  expect_error(summing_matrix(list(0:5), 4), "out of range")
})

test_that("Schafer-Strimmer shrinkage of the residual covariance", {
  E <- cbind(sin(1:10), cos(1:10), sin(2 * (1:10)))
  sc <- shrink_covariance(E)
  R <- sweep(E, 2, colMeans(E))
  S <- crossprod(R) / 9
  vs <- 0
  off <- 0
  for (i in 1:3) for (j in 1:3) if (i != j) {
    w <- R[, i] * R[, j]
    vs <- vs + sum((w - mean(w))^2) * 10 / 9^3
    off <- off + S[i, j]^2
  }
  lam <- min(1, max(0, vs / off))
  expect_equal(sc$lambda, lam, tolerance = 1e-12)
  expect_equal(sc$cov, (1 - lam) * S + lam * diag(diag(S)), tolerance = 1e-12)
  expect_equal(shrink_covariance(E, lam = 0.3)$cov, 0.7 * S + 0.3 * diag(diag(S)), tolerance = 1e-12)
  expect_error(shrink_covariance(E[1, , drop = FALSE]), "at least 2")
})

test_that("MinT projection: P = (S' W^-1 S)^-1 S' W^-1 with PS = I", {
  yb <- c(11, 5.5, 4.8, 2.1, 3.2, 2.2, 2.9)
  for (m in c("ols", "wls", "shrink")) {
    E <- cbind(sin(1:12), cos(1:12), sin(2 * (1:12)), cos(3 * (1:12)), sin(5 * (1:12)), cos(7 * (1:12)), sin(11 * (1:12)))
    r <- mint_reconcile(yb, hf_S, method = m, residuals = E)
    W <- switch(m, ols = diag(7), wls = diag(colMeans(E^2)), shrink = shrink_covariance(E)$cov)
    P <- solve(t(hf_S) %*% solve(W) %*% hf_S) %*% t(hf_S) %*% solve(W)
    expect_equal(r$P, P, tolerance = 1e-8, info = m)
    expect_true(r$coherent)
    expect_lt(r$ps_identity_error, 1e-8)
    expect_equal(r$reconciled, as.numeric(hf_S %*% P %*% yb), tolerance = 1e-8)
  }
  coh <- as.numeric(hf_S %*% c(1, 2, 3, 4))
  expect_equal(mint_reconcile(coh, hf_S, method = "ols")$reconciled, coh, tolerance = 1e-8)
  expect_equal(morie_hierF(yb, hf_S, method = "custom", W = diag(7))$P, mint_P(hf_S, method = "ols")$P, tolerance = 1e-12)
  expect_error(mint_reconcile(yb[-1], hf_S), "6 base forecasts for 7")
  expect_error(mint_P(hf_S, method = "wls"), "needs residuals")
  expect_error(mint_P(hf_S, method = "bu"), "method must be")
})
