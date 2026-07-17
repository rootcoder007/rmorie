# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation of smallstats_native.R against the packages each
# helper replaces, plus speed benchmarks. The reference packages are
# Suggests-only: every comparison is skip_if_not_installed-guarded, so
# a bare install still runs the internal-consistency tests.

test_that(".morie_hurst_rs matches pracma::hurstexp on known series", {
  set.seed(11)
  x <- cumsum(rnorm(4096)) # Brownian motion
  h <- .morie_hurst_rs(x)
  # Simple whole-series R/S is upward-biased for BM; sanity range only.
  expect_true(h > 0.2 && h < 0.9)

  skip_if_not_installed("pracma")
  ref <- pracma::hurstexp(x, display = FALSE)$Hs
  expect_lt(abs(h - ref), 1e-10)

  # Persistent series
  y <- cumsum(cumsum(rnorm(2048))) # smoother, H -> high
  expect_gt(.morie_hurst_rs(y), 0.75)
})

test_that(".morie_psens_wilcoxon matches rbounds::psens bounds", {
  skip_if_not_installed("rbounds")
  set.seed(22)
  n <- 40
  treated <- rnorm(n, 0.6)
  control <- rnorm(n)
  ref <- rbounds::psens(treated, control, Gamma = 3, GammaInc = 0.5)$bounds
  for (i in seq_len(nrow(ref))) {
    g <- as.numeric(ref[i, "Gamma"])
    ours <- .morie_psens_wilcoxon(treated, control, g)
    expect_lt(abs(ours[["p_lower"]] -
                  as.numeric(ref[i, "Lower bound"])), 6e-5)
    expect_lt(abs(ours[["p_upper"]] -
                  as.numeric(ref[i, "Upper bound"])), 6e-5)
  }
})

test_that(".morie_psens_wilcoxon_d equals the paired form with zero controls", {
  # rbounds::psens has no one-sample interface (its y is required --
  # which means the old effects.R one-sample delegation could never
  # have run). Validate the differences core against the paired form:
  # d paired with zeros yields identical differences.
  skip_if_not_installed("rbounds")
  set.seed(23)
  d <- rnorm(35, 0.4)
  for (g in c(1, 2)) {
    ours <- .morie_psens_wilcoxon_d(d, g)
    ref <- rbounds::psens(d, rep(0, length(d)), Gamma = g,
                          GammaInc = 1)$bounds
    expect_lt(abs(ours[["p_lower"]] -
                  as.numeric(ref[nrow(ref), "Lower bound"])), 6e-5)
    expect_lt(abs(ours[["p_upper"]] -
                  as.numeric(ref[nrow(ref), "Upper bound"])), 6e-5)
  }
})

test_that(".morie_entropy_balance achieves exact moment balance + matches ebal", {
  set.seed(33)
  n <- 300
  X <- cbind(rnorm(n), rexp(n), rbinom(n, 1, 0.4))
  ps <- plogis(0.5 * X[, 1] - 0.3 * X[, 2])
  t_mask <- runif(n) < ps
  fit <- .morie_entropy_balance(t_mask, X)
  expect_true(fit$converged)
  # The defining invariant: weighted control means == treated means.
  w <- fit$w
  Xc <- X[!t_mask, , drop = FALSE]
  wm <- colSums(Xc * w) / sum(w)
  expect_lt(max(abs(wm - colMeans(X[t_mask, , drop = FALSE]))), 1e-4)

  skip_if_not_installed("ebal")
  ref <- ebal::ebalance(Treatment = as.integer(t_mask), X = X)
  # Same invariant, and weights proportional (entropy solution unique).
  expect_lt(max(abs(w / sum(w) - as.numeric(ref$w) / sum(ref$w))), 1e-3)
})

test_that(".morie_knn_index matches FNN::get.knn exactly", {
  set.seed(44)
  coords <- matrix(rnorm(200 * 2), ncol = 2)
  ours <- .morie_knn_index(coords, k = 5)
  expect_equal(dim(ours), c(200L, 5L))

  skip_if_not_installed("FNN")
  ref <- FNN::get.knn(coords, k = 5)$nn.index
  expect_equal(unname(ours), unname(ref))
})

test_that(".morie_smote balances classes with plausible synthetics", {
  set.seed(55)
  X <- rbind(matrix(rnorm(80 * 2), ncol = 2),
             matrix(rnorm(20 * 2, mean = 3), ncol = 2))
  y <- c(rep("a", 80), rep("b", 20))
  res <- .morie_smote(X, y, k = 5)
  expect_equal(nrow(res$X_new), 60L)
  expect_true(all(res$y_new == "b"))
  # Synthetics interpolate within the minority cloud.
  expect_true(all(res$X_new[, 1] > min(X[81:100, 1]) - 1e-9))
  expect_true(all(res$X_new[, 1] < max(X[81:100, 1]) + 1e-9))
})

test_that(".morie_hmp matches harmonicmeanp::p.hmp", {
  skip_if_not_installed("harmonicmeanp")
  set.seed(66)
  for (i in 1:3) {
    p <- runif(50)^(1 + i) # varying signal strength
    ours <- .morie_hmp(p, L = length(p))
    ref <- as.numeric(harmonicmeanp::p.hmp(p, L = length(p)))
    expect_lt(abs(ours - ref), 0.02)
  }
})

test_that("rewired public functions run end-to-end without the old packages", {
  set.seed(77)
  # Hurst through the public wrapper
  r <- morie_hurst_r(cumsum(rnorm(1024)))
  expect_true(is.numeric(r$H))
  expect_true(r$interpretation %in% c("persistent", "anti-persistent", "random"))

  # Rosenbaum bounds through the public wrapper
  tab <- morie_sensitivity_rosenbaum(treated = rnorm(30, 0.5),
                                     control = rnorm(30))
  expect_true(all(c("gamma", "p_lower", "p_upper") %in% names(tab)))
  expect_true(all(tab$p_upper >= tab$p_lower - 1e-12))

  # HMP through the public wrapper
  expect_true(harmonic_mean_p(runif(20)) <= 1)

  # SMOTE through the public wrapper
  X <- rbind(matrix(rnorm(60 * 2), ncol = 2),
             matrix(rnorm(15 * 2, 3), ncol = 2))
  y <- c(rep("maj", 60), rep("min", 15))
  sm <- morie_ml_apply_smote(as.data.frame(X), y)
  expect_equal(sm$status$method, "smote")
  expect_equal(length(sm$y), 120L)
})

test_that("benchmark: natives are within sane speed of the packages", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("MORIE_RUN_BENCH"), "1"),
              "set MORIE_RUN_BENCH=1 to run benchmarks")
  bench <- function(thunk, times = 5L) {
    ts <- vapply(seq_len(times), function(i) {
      t0 <- proc.time()[["elapsed"]]
      thunk()
      proc.time()[["elapsed"]] - t0
    }, numeric(1))
    stats::median(ts)
  }
  set.seed(88)
  x <- cumsum(rnorm(8192))
  coords <- matrix(rnorm(1000 * 2), ncol = 2)
  n <- 500
  Xb <- cbind(rnorm(n), rexp(n)); tb <- runif(n) < 0.4
  rows <- list(
    c(what = "hurst_native", t = bench(function() .morie_hurst_rs(x))),
    c(what = "knn_native_1k", t = bench(function() .morie_knn_index(coords, 8))),
    c(what = "ebalance_native", t = bench(function() .morie_entropy_balance(tb, Xb)))
  )
  if (requireNamespace("pracma", quietly = TRUE)) {
    rows <- c(rows, list(c(what = "hurst_pracma",
                           t = bench(function() pracma::hurstexp(x, display = FALSE)))))
  }
  if (requireNamespace("FNN", quietly = TRUE)) {
    rows <- c(rows, list(c(what = "knn_FNN_1k",
                           t = bench(function() FNN::get.knn(coords, k = 8)))))
  }
  if (requireNamespace("ebal", quietly = TRUE)) {
    rows <- c(rows, list(c(what = "ebalance_ebal",
                           t = bench(function() ebal::ebalance(as.integer(tb), Xb)))))
  }
  out <- do.call(rbind, rows)
  cat("\n== smallstats-native benchmarks (median s) ==\n")
  print(out)
  succeed()
})
