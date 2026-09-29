# Coverage tests for R/bndsmw_native.R (Andrews and Shi 2013): hypercube
# instruments, instrument-weighted moments, the S function, the
# Cramer-von Mises statistic, GMS bootstrap critical values and the
# inverted confidence set.

bw_X <- cbind(c(0.1, 0.4, 0.35, 0.8, 0.95, 0.6, 0.2, 0.7), c(1, 3, 2, 5, 4, 1.5, 4.5, 2.5))

test_that("hypercube instruments partition each level into cells", {
  inst <- morie_hypercube_instruments(bw_X, n_levels = 3)
  lo <- apply(bw_X, 2, min)
  U <- sweep(sweep(bw_X, 2, lo), 2, apply(bw_X, 2, max) - lo, "/")
  ref <- list()
  for (cells in c(2, 4)) {
    cell <- pmin(floor(U * cells), cells - 1)
    for (c_ in 0:(cells^2 - 1)) {
      g <- as.numeric(cell[, 1] == c_ %% cells & cell[, 2] == c_ %/% cells)
      if (sum(g) > 0) ref[[length(ref) + 1]] <- g
    }
  }
  expect_equal(inst$instruments, ref)
  expect_equal(inst$n_instruments, length(ref))
  expect_equal(Reduce(`+`, inst$instruments), rep(2, 8))
  expect_equal(morie_hypercube_instruments(bw_X, 1)$n_instruments, 0L)
  expect_error(morie_hypercube_instruments(matrix(0, 0, 2)), "no observations")
})

test_that("weighted moments and the S function", {
  M <- cbind(bw_X[, 1] - 0.3, 2 - bw_X[, 2])
  g <- c(1, 0, 1, 1, 0, 1, 0, 1)
  w <- morie_weighted_moments(M, g)
  expect_equal(w$mean, colMeans(M * g), tolerance = 1e-12)
  expect_equal(w$sd, apply(M * g, 2, sd), tolerance = 1e-12)
  expect_error(morie_weighted_moments(M[1, , drop = FALSE], 1), "at least 2")
  expect_error(morie_weighted_moments(M, g[-1]), "7 weights for 8")
  expect_error(morie_weighted_moments(M, -g), "non-negative")
  v <- c(-1, 2, -0.5, 3)
  expect_equal(morie_S_function(v), 1.25)
  expect_equal(morie_S_function(v, "qlr"), 1.25)
  expect_equal(morie_S_function(v, "max"), 1)
  expect_equal(morie_S_function(c(1, 2), "max"), 0)
  expect_equal(morie_S_function(v, n_equality = 1), 1.25 + 9)
  expect_equal(morie_S_function(v, n_equality = 4), sum(v^2))
  expect_error(morie_S_function(v, "min"), "form must be one of")
})

test_that("Cramer-von Mises statistic integrates S over the instruments", {
  M <- cbind(bw_X[, 1] - 0.5, bw_X[, 2] - 3)
  inst <- morie_hypercube_instruments(bw_X, 2)
  st <- vapply(inst$instruments, function(g) {
    Mg <- M * g
    morie_S_function(sqrt(8) * colMeans(Mg) / pmax(apply(Mg, 2, sd), 1e-12))
  }, 0)
  r <- morie_cvm_statistic(M, inst)
  expect_equal(r$per_instrument, st, tolerance = 1e-12)
  expect_equal(r$statistic, mean(st), tolerance = 1e-12)
  q <- seq_along(st) / sum(seq_along(st))
  expect_equal(morie_cvm_statistic(M, inst$instruments, weights = q)$statistic, sum(q * st), tolerance = 1e-12)
  expect_error(morie_cvm_statistic(M, list()), "instrument class is empty")
  expect_error(morie_cvm_statistic(M, inst, weights = 1), "1 measure weights")
  expect_error(morie_cvm_statistic(M, inst, weights = rep(1, length(st))), "sum to 1")
})

test_that("GMS bootstrap critical value", {
  M <- cbind(bw_X[, 1] - 0.2, 4 - bw_X[, 2])
  G <- morie_hypercube_instruments(bw_X, 2)$instruments
  r <- morie_gms_critical_value(M, G, level = 0.9, reps = 50, seed = 4)
  n <- 8
  kap <- sqrt(log(8))
  expect_equal(r$kappa, kap)
  e <- .ghc_rng(4)
  dr <- numeric(50)
  for (b in 1:50) {
    idx <- floor(.ghc_unif(e, n) * n) %% n + 1
    dr[b] <- mean(vapply(G, function(g) {
      m0 <- colMeans(M * g)
      s0 <- pmax(apply(M * g, 2, sd), 1e-12)
      mb <- colMeans(M[idx, ] * g[idx])
      xi <- sqrt(n) * m0 / s0
      morie_S_function(sqrt(n) * (mb - m0) / s0 + ifelse(xi <= kap, 0, 1e6))
    }, 0))
  }
  expect_equal(r$critical_value, unname(quantile(dr, 0.9, type = 1)), tolerance = 1e-12)
  expect_equal(morie_gms_critical_value(M, G, reps = 20, kappa = 0.5)$kappa, 0.5)
})

test_that("confidence set inverts the test over the grid", {
  L <- bw_X[, 1]
  Uu <- bw_X[, 1] + 0.5
  mf <- function(th) cbind(th - L, Uu - th)
  grid <- c(-0.5, 0.3, 0.7, 1.6)
  cs <- morie_confidence_set(mf, grid, bw_X, reps = 30, seed = 2)
  inst <- morie_hypercube_instruments(bw_X, 2)
  inside <- vapply(grid, function(th) {
    morie_cvm_statistic(mf(th), inst)$statistic <=
      morie_gms_critical_value(mf(th), inst, reps = 30, seed = 2)$critical_value
  }, TRUE)
  expect_equal(unlist(cs$set), grid[inside])
  expect_equal(cs$n_in_set, sum(inside))
  expect_true(0.7 %in% unlist(cs$set))
  expect_false(-0.5 %in% unlist(cs$set))
  expect_equal(cs$bounds, range(grid[inside]))
  expect_equal(cs$n_instruments, inst$n_instruments)
  expect_identical(morie_bndsmw, morie_confidence_set)
  expect_identical(morie_simulatedweightbound, morie_confidence_set)
  expect_identical(morie_bound_simul_weights, morie_confidence_set)
})
