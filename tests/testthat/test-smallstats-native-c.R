# SPDX-License-Identifier: AGPL-3.0-or-later
# Wave C cross-validation: Sobol (vs randtoolbox), exact t-SNE (vs
# Rtsne, quality metrics), GEE Poisson exchangeable (vs geepack).

# Resolve package internals up front so the file works identically
# under pkgload::load_all, R CMD check's test_check, and plain
# testthat against an installed namespace (the CI R-tests harness).
.ss_ns <- environment(morie_hurst_r)
for (.nm in c(".morie_sobol", ".morie_knn_index", ".morie_gee_poisson_exch",
           ".morie_statcan_wds_table")) {
  assign(.nm, get(.nm, envir = .ss_ns))
}


test_that(".morie_sobol matches randtoolbox::sobol exactly", {
  s <- .morie_sobol(64L, 5L)
  expect_equal(dim(s), c(64L, 5L))
  expect_true(all(s >= 0 & s < 1))
  # Low-discrepancy sanity: mean of each dim near 0.5.
  expect_lt(max(abs(colMeans(s) - 0.5)), 0.05)

  skip_if_not_installed("randtoolbox")
  ref <- randtoolbox::sobol(n = 64L, dim = 5L, scrambling = 0L)
  if (!is.matrix(ref)) ref <- matrix(ref, ncol = 5L)
  expect_lt(max(abs(s - ref)), 1e-9)
})

test_that("sobls() runs on the native sequence and integrates", {
  r <- sobls(N = 256L, d = 2L, f = function(u) u[1] * u[2], seed = 0)
  # E[U1*U2] = 1/4; QMC at N=256 should be very close.
  expect_lt(abs(r$estimate - 0.25), 0.01)
})

test_that("native t-SNE separates well-separated clusters (Rtsne parity)", {
  set.seed(301)
  X <- rbind(matrix(rnorm(40 * 4), 40, 4),
             matrix(rnorm(40 * 4, mean = 8), 40, 4))
  lab <- rep(1:2, each = 40)
  out <- morie_tsne_reduction(X, n_components = 2L, perplexity = 10,
                              n_iter = 400L, seed = 1)
  emb <- out$embedding
  expect_equal(dim(emb), c(80L, 2L))
  expect_true(is.finite(out$kl_divergence))
  # Cluster recall: for each point, majority of its 5 embedding-space
  # NN must share its label.
  nn <- .morie_knn_index(emb, 5L)
  same <- vapply(seq_len(80L), function(i) {
    mean(lab[nn[i, ]] == lab[i])
  }, numeric(1))
  expect_gt(mean(same), 0.9)

  skip_if_not_installed("Rtsne")
  set.seed(1)
  ref <- Rtsne::Rtsne(X, dims = 2, perplexity = 10, max_iter = 400,
                      check_duplicates = FALSE, pca = TRUE)
  nn_r <- .morie_knn_index(ref$Y, 5L)
  same_r <- vapply(seq_len(80L), function(i) {
    mean(lab[nn_r[i, ]] == lab[i])
  }, numeric(1))
  # Same neighbourhood-recall quality (within 5 points).
  expect_gt(mean(same), mean(same_r) - 0.05)
})

test_that(".morie_gee_poisson_exch matches geepack::geeglm", {
  set.seed(302)
  n_cl <- 40L
  ni <- 5L
  id <- rep(seq_len(n_cl), each = ni)
  u <- rep(rnorm(n_cl, sd = 0.4), each = ni) # cluster effect
  x1 <- rnorm(n_cl * ni)
  x2 <- rbinom(n_cl * ni, 1, 0.5)
  y <- rpois(n_cl * ni, lambda = exp(0.3 + 0.5 * x1 - 0.4 * x2 + u))
  X <- cbind(1, x1, x2)
  fit <- .morie_gee_poisson_exch(X, y, id)
  expect_true(fit$converged)

  skip_if_not_installed("geepack")
  df <- data.frame(y = y, x1 = x1, x2 = x2, id = id)
  ref <- geepack::geeglm(y ~ x1 + x2, data = df, id = id,
                         family = stats::poisson(),
                         corstr = "exchangeable")
  expect_lt(max(abs(fit$coefficients - as.numeric(stats::coef(ref)))),
            1e-3)
  se_ours <- sqrt(diag(fit$vbeta))
  se_ref <- as.numeric(summary(ref)$coefficients[, "Std.err"])
  expect_lt(max(abs(se_ours - se_ref) / se_ref), 0.05)
})

test_that("StatCan pid normalization is correct (offline)", {
  # Exercise only the pure id-normalization logic; the network fetch
  # is covered by the opt-in live test below.
  expect_error(.morie_statcan_wds_table("bad-id"), "8-digit")
})

test_that("StatCan WDS fetch returns the raw CSV schema (live, opt-in)", {
  skip_if_not(identical(Sys.getenv("MORIE_RUN_LIVE"), "1"),
              "set MORIE_RUN_LIVE=1 for live StatCan fetch")
  df <- .morie_statcan_wds_table("35-10-0177")
  expect_true(is.data.frame(df))
  expect_true(all(c("REF_DATE", "GEO", "VALUE") %in% names(df)))
  expect_gt(nrow(df), 100)
})
