# Network-dependent causal inference (van der Laan & Rose 2018, Chaps.
# 20-21): degree summaries, friend-exposure summaries, the community
# estimand, the network-corrected variance and longitudinal
# g-computation, recomputed with an adjacency matrix.

nt_fr <- list(c(2, 3), c(1), c(1, 4, 5), c(3), c(3, 5))
nt_adj <- function(fr) {
  N <- length(fr)
  M <- matrix(0, N, N)
  for (i in seq_len(N)) M[i, setdiff(fr[[i]], i)] <- 1
  M
}

test_that("network summary: degrees exclude self-loops; hubs are flagged", {
  s <- morie_tlnetlg_network_summary(nt_fr)
  expect_identical(s$degrees, as.integer(rowSums(nt_adj(nt_fr))))
  expect_identical(s$max_degree, 3L)
  expect_equal(s$max_share, 3 / 5)
  expect_false(s$sparse)
  expect_error(morie_tlnetlg_network_summary(list(1)), "at least 2")
})

test_that("exposure summaries are fraction, count or any treated friend", {
  A <- c(1, 0, 1, 1, 0)
  M <- nt_adj(nt_fr)
  cnt <- as.numeric(M %*% A)
  deg <- rowSums(M)
  for (k in c("fraction", "count", "any")) {
    ex <- morie_tlnetlg_exposure_summary(A, nt_fr, k)$summary
    ref <- switch(k, fraction = ifelse(deg > 0, cnt / deg, 0), count = cnt, any = as.numeric(cnt > 0))
    expect_equal(vapply(ex, `[[`, 1, 2), ref)
    expect_equal(vapply(ex, `[[`, 1, 1), A)
  }
  expect_error(morie_tlnetlg_exposure_summary(A[-1], nt_fr), "4 treatments but 5")
  expect_error(morie_tlnetlg_exposure_summary(A, nt_fr, "max"), "fraction, count or any")
})

test_that("the community estimand averages Q under the network policy", {
  W <- matrix(c(0.2, -0.5, 1, 0.3, 0.8), 5)
  pol <- function(i, rows) as.numeric(rows[i, 1] > 0)
  Q <- function(a, s, w) 0.5 + a - 0.3 * s + 0.1 * w
  r <- morie_tlnetlg_community_estimand(Q, nt_fr, W, pol)
  a <- as.numeric(W[, 1] > 0)
  M <- nt_adj(nt_fr)
  s <- as.numeric(M %*% a) / rowSums(M)
  expect_equal(r$assigned, a)
  expect_equal(r$individual, 0.5 + a - 0.3 * s + 0.1 * W[, 1], tolerance = 1e-15)
  expect_equal(r$psi, mean(r$individual), tolerance = 1e-15)
  expect_error(morie_tlnetlg_community_estimand(Q, nt_fr[-1], W, pol), "friend sets")
})

test_that("the variance adds the covariance of connected pairs", {
  ic <- c(0.4, -0.2, 1.1, -0.7, 0.3)
  v <- morie_tlnetlg_network_variance(ic, nt_fr)
  cen <- ic - mean(ic)
  M <- nt_adj(nt_fr)
  tot <- (mean(cen^2) + as.numeric(t(cen) %*% M %*% cen) / 5) / 5
  expect_equal(v$se, sqrt(max(tot, 0)), tolerance = 1e-15)
  expect_equal(v$se_naive, sqrt(mean(cen^2) / 5), tolerance = 1e-15)
  expect_identical(v$n_dependent_pairs, as.integer(sum(M)))
  expect_error(morie_tlnetlg_network_variance(ic[-1], nt_fr), "influence values")
})

test_that("longitudinal g-computation feeds each step's outcome forward as a covariate", {
  W <- matrix(c(0.2, -0.5, 1, 0.3, 0.8), 5)
  pol <- function(i, rows) as.numeric(rows[i, ncol(rows)] > 0)
  Qs <- list(function(a, s, w) 0.2 + a + 0.5 * s + 0.1 * sum(w),
             function(a, s, w) 0.1 * w[1] + a * s)
  r <- morie_tlnetlg_longitudinal_network_gcomp(Qs, nt_fr, W, pol, 2)
  s1 <- morie_tlnetlg_community_estimand(Qs[[1]], nt_fr, W, pol)
  s2 <- morie_tlnetlg_community_estimand(Qs[[2]], nt_fr, cbind(s1$individual, W), pol)
  expect_equal(r$path, c(s1$psi, s2$psi), tolerance = 1e-15)
  expect_equal(r$estimate, s2$psi)
  expect_identical(r$network$max_degree, 3L)
  expect_equal(morie_tlnetlg(Qs, nt_fr, W, pol, 2)$psi, r$psi)
  expect_error(morie_tlnetlg_longitudinal_network_gcomp(Qs, nt_fr, W, pol, 0), "at least one")
  expect_error(morie_tlnetlg_longitudinal_network_gcomp(Qs, nt_fr, W, pol, 3), "2 regressions for 3")
  expect_match(morie_tlnetlg_cheatsheet(), "CONNECTED pairs", fixed = TRUE)
})
