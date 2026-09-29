# MCMC diagnostics (Geyer 1992; Vehtari et al. 2021) and the
# non-overlapping block bootstrap: ACF and Ljung-Box against stats,
# ESS recomputed with direct-sum autocovariances and Geyer's initial
# positive sequence, rank-normalised bulk / indicator tail ESS, split
# R-hat, acceptance rates, and a replayed bootstrap.

mc_chains <- function() {
  set.seed(40)
  C <- matrix(0, 3, 200)
  for (j in 1:3) {
    e <- stats::rnorm(200)
    x <- numeric(200)
    for (t in 2:200) x[t] <- 0.6 * x[t - 1] + e[t]
    C[j, ] <- x + 0.1 * j
  }
  C
}
mc_ess <- function(C) {
  m <- nrow(C)
  n <- ncol(C)
  acov <- t(apply(C, 1, function(x) {
    xc <- x - mean(x)
    vapply(0:(n - 1), function(k) sum(xc[seq_len(n - k)] * xc[seq_len(n - k) + k]) / n, 1)
  }))
  W <- mean(acov[, 1] * n / (n - 1))
  vh <- if (m > 1) ((n - 1) * W + n * stats::var(rowMeans(C))) / n else W
  rho <- 1 - (W - colMeans(acov[, -1, drop = FALSE])) / vh
  tot <- 0
  t <- 0
  while (t + 1 < length(rho)) {
    pr <- rho[t + 1] + rho[t + 2]
    if (pr < 0) break
    tot <- tot + pr
    t <- t + 2
  }
  tau <- -1 + 2 * tot
  if (tau > 0) m * n / tau else m * n
}
mc_z <- function(C) {
  r <- order(order(as.vector(C)))
  matrix(stats::qnorm((r - 0.375) / (length(C) + 0.25)), nrow(C))
}

test_that("autocorrelation and Ljung-Box match stats::acf and Box.test", {
  set.seed(2)
  y <- stats::filter(stats::rnorm(120), 0.2, method = "recursive")
  r <- morie_autocorrelation(y, lag_max = 8)
  expect_equal(r$acf, as.numeric(stats::acf(y, lag.max = 8, plot = FALSE)$acf), tolerance = 1e-12)
  bt <- stats::Box.test(y, lag = 8, type = "Ljung-Box")
  expect_equal(r$ljung_box, unname(bt$statistic), tolerance = 1e-12)
  expect_equal(r$ljung_box_p, bt$p.value, tolerance = 1e-12)
  expect_equal(r$ci_bound, stats::qnorm(0.975) / sqrt(120), tolerance = 1e-15)
  expect_identical(length(morie_autocorrelation(y)$acf), as.integer(min(10 * log10(120), 119)) + 1L)
  expect_error(morie_autocorrelation(1:2), "at least 3")
  expect_error(morie_autocorrelation(y, lag_max = 200), "lag_max must be")
})

test_that("the acceptance rate is the share of moves, compared with the sampler target", {
  C <- rbind(c(1, 1, 2, 2, 2, 3), c(0, 1, 2, 2, 3, 3))
  r <- morie_acceptance_rate_diagnostic(C)
  expect_equal(r$per_chain, c(2, 3) / 5)
  expect_equal(r$acceptance_rate, 0.5)
  expect_equal(r$target, 0.234)
  expect_match(r$recommendation, "far above target")
  expect_identical(morie_acceptance_rate_diagnostic(C, kind = "hmc")$target, 0.8)
  low <- morie_acceptance_rate_diagnostic(c(1, 1, 1, 1, 2), kind = "mala")
  expect_match(low$recommendation, "far below")
  expect_error(morie_acceptance_rate_diagnostic(1), "at least 2 draws")
})

test_that("ESS uses Geyer's initial positive sequence over pooled autocorrelations", {
  C <- mc_chains()
  r <- morie_effective_sample_size_bayes(C)
  ess <- mc_ess(C)
  expect_equal(r$ess, ess, tolerance = 1e-9)
  expect_equal(r$mcse, stats::sd(as.vector(C)) / sqrt(ess), tolerance = 1e-9)
  # split R-hat on rank-normalised half chains
  S <- mc_z(rbind(C[, 1:100], C[, 101:200]))
  W <- mean(apply(S, 1, stats::var))
  B <- 100 * stats::var(rowMeans(S))
  expect_equal(r$rhat, sqrt((99 * W + B) / 100 / W), tolerance = 1e-12)
  one <- morie_effective_sample_size_bayes(C[1, ])
  expect_equal(one$ess, mc_ess(C[1, , drop = FALSE]), tolerance = 1e-9)
  expect_true(is.na(one$rhat))
  expect_error(morie_effective_sample_size_bayes(1:3), "at least 4")
})

test_that("bulk ESS rank-normalises; tail ESS is the smaller indicator ESS", {
  C <- mc_chains()
  b <- morie_effective_sample_size_bulk(C)
  expect_equal(b$ess_bulk, mc_ess(mc_z(C)), tolerance = 1e-9)
  expect_identical(b$sufficient, b$ess_bulk >= 300)
  tl <- morie_effective_sample_size_tail(C, prob = 0.1)
  q <- stats::quantile(as.vector(C), c(0.1, 0.9), names = FALSE)
  lo <- mc_ess(matrix(as.numeric(C < q[1]), 3))
  hi <- mc_ess(matrix(as.numeric(C > q[2]), 3))
  expect_equal(c(tl$ess_lower, tl$ess_upper), c(lo, hi), tolerance = 1e-9)
  expect_equal(tl$ess_tail, min(lo, hi), tolerance = 1e-9)
  expect_error(morie_effective_sample_size_tail(C, prob = 0.5), "prob must be")
  expect_error(morie_effective_sample_size_bulk(C[, 1:3]), "at least 4")
})

test_that("the block bootstrap resamples whole non-overlapping blocks", {
  x <- mc_chains()[2, 1:50]
  r <- morie_boot_nonoverlap_block(x, block_len = 5, B = 40, seed = 3)
  blocks <- matrix(x, 10, 5, byrow = TRUE)
  set.seed(3)
  reps <- vapply(1:40, function(b) mean(as.vector(t(blocks[sample.int(10, 10, replace = TRUE), ]))), 1)
  expect_equal(r$replicates, reps, tolerance = 1e-15)
  expect_equal(r$se, stats::sd(reps), tolerance = 1e-15)
  expect_equal(r$estimate, mean(x))
  expect_identical(r$n_blocks, 10L)
  md <- morie_boot_nonoverlap_block(x, block_len = 5, stat = stats::median, B = 10, seed = 3)
  expect_equal(md$estimate, stats::median(x))
  expect_identical(morie_boot_nonoverlap_block(x, B = 5)$block_len, as.integer(50^(1 / 3)))
  expect_error(morie_boot_nonoverlap_block(x, block_len = 30), "only 1 blocks")
  expect_error(morie_boot_nonoverlap_block(x, block_len = 0), "between 1 and 50")
  expect_error(morie_boot_nonoverlap_block(1), "at least 2")
})
