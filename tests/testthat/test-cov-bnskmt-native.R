# Coverage for the Kolmogorov-Smirnov form of the conditional moment
# inequality test (Andrews & Shi 2013) with GMS critical values (Andrews &
# Soares 2010): the S function, instrument-weighted moments, the
# hypercube instrument class, the KS and CvM statistics (matrix
# recomputation), the bootstrap critical value (regenerated from the same
# seed) and the inverted confidence set.

.S <- function(t, ne = 0, form = "sum") {
  J <- length(t)
  neg <- pmin(t[seq_len(J - ne)], 0)^2
  (if (form == "max") max(c(neg, 0)) else sum(neg)) + sum(t[J - seq_len(ne) + 1]^2)
}
.tstat <- function(M, g) {
  V <- M * g
  sqrt(nrow(M)) * colMeans(V) / pmax(apply(V, 2, stats::sd), 1e-12)
}

test_that("S sums (or maxes) squared negative parts plus squared equalities", {
  v <- c(-1.5, 0.4, -0.2, 0.7)
  expect_equal(S_function(v), 1.5^2 + 0.2^2, tolerance = 1e-12)
  expect_equal(S_function(v, form = "max"), 1.5^2, tolerance = 1e-12)
  expect_equal(S_function(v, n_equality = 1), 1.5^2 + 0.2^2 + 0.49, tolerance = 1e-12)
  expect_equal(S_function(v, n_equality = 2), 1.5^2 + 0.2^2 + 0.49, tolerance = 1e-12)
  expect_identical(S_function(c(1, 2)), 0)
})

test_that("weighted moments are column means and sds of m_i g(X_i)", {
  M <- rbind(c(1, -2), c(0.5, 3), c(-1, 1), c(2, 0))
  g <- c(1, 0, 1, 0.5)
  w <- weighted_moments(M, g)
  expect_equal(w$mean, colMeans(M * g), tolerance = 1e-12)
  expect_equal(w$sd, apply(M * g, 2, stats::sd), tolerance = 1e-12)
  expect_equal(weighted_moments(split(M, row(M)), g)$mean, w$mean, tolerance = 1e-12)
  expect_error(weighted_moments(M[1, , drop = FALSE], 1), "at least 2")
  expect_error(weighted_moments(M, g[-1]), "3 weights for 4")
  expect_error(weighted_moments(M, -g), "non-negative")
})

test_that("hypercube instruments indicate the 2^l-cells at each level", {
  X <- cbind(c(0.05, 0.3, 0.55, 0.8, 1, 0.2), c(0.9, 0.1, 0.45, 0.7, 0, 0.6))
  h <- hypercube_instruments(X, n_levels = 3)
  lo <- apply(X, 2, min)
  pos <- sweep(sweep(X, 2, lo), 2, apply(X, 2, max) - lo, "/")
  ref <- list()
  for (lev in 1:2) {
    cells <- 2^lev
    cid <- pmin(floor(pos * cells), cells - 1)
    for (a in 0:(cells - 1)) for (b in 0:(cells - 1)) {
      g <- as.numeric(cid[, 1] == a & cid[, 2] == b)
      if (sum(g) > 0) ref[[length(ref) + 1]] <- g
    }
  }
  expect_equal(h$instruments, ref)
  expect_identical(h$n_instruments, length(ref))
  one <- hypercube_instruments(matrix(c(1, 2, 3, 4)), n_levels = 2)
  expect_equal(one$instruments, list(c(1, 1, 0, 0), c(0, 0, 1, 1)))
  expect_error(hypercube_instruments(1:3), "matrix or list")
  expect_error(hypercube_instruments(list()), "no observations")
})

test_that("KS is the maximum and CvM the Q-average of S over instruments", {
  set.seed(2)
  x <- stats::runif(30)
  M <- cbind(x - 0.3 + stats::rnorm(30, sd = 0.3), 0.2 - x^2 + stats::rnorm(30, sd = 0.2))
  inst <- hypercube_instruments(matrix(x), n_levels = 3)
  per <- vapply(inst$instruments, function(g) .S(.tstat(M, g)), 1)
  k <- ks_statistic(M, inst)
  expect_equal(k$per_instrument, per, tolerance = 1e-12)
  expect_equal(k$statistic, max(per), tolerance = 1e-12)
  expect_identical(k$argmax, which.max(per))
  expect_equal(ks_statistic(M, inst$instruments, form = "max")$statistic,
               max(vapply(inst$instruments, function(g) .S(.tstat(M, g), form = "max"), 1)), tolerance = 1e-12)
  cv <- cvm_statistic(M, inst)
  expect_equal(cv$statistic, mean(per), tolerance = 1e-12)
  q <- c(0.4, 0.1, 0.1, 0.1, 0.1, 0.2)
  expect_equal(cvm_statistic(M, inst, weights = q)$statistic, sum(q * per), tolerance = 1e-12)
  cf <- compare_forms(M, inst)
  expect_equal(cf$ratio_ks_over_cvm, max(per) / mean(per), tolerance = 1e-12)
  expect_identical(cf$argmax_instrument, k$argmax)
  # no violated instrument: the statistic is 0 and argmax is NA
  expect_true(is.na(ks_statistic(abs(M) + 1, inst)$argmax))
  expect_error(ks_statistic(M, list()), "instrument class is empty")
  expect_error(cvm_statistic(M, inst, weights = c(0.5, 0.5)), "2 measure weights for 6")
  expect_error(cvm_statistic(M, inst, weights = rep(0.2, 6)), "must sum to 1")
})

.gms_cv <- function(M, G, ne, level, reps, seed, kap) {
  n <- nrow(M)
  J <- ncol(M)
  set.seed(seed)
  d <- numeric(reps)
  for (r in seq_len(reps)) {
    i <- sample.int(n, n, replace = TRUE)
    d[r] <- max(0, vapply(G, function(g) {
      V <- M * g
      m0 <- colMeans(V)
      s0 <- pmax(apply(V, 2, stats::sd), 1e-12)
      xi <- sqrt(n) * m0 / s0
      tb <- sqrt(n) * (colMeans(V[i, , drop = FALSE]) - m0) / s0
      tb <- tb + ifelse(seq_len(J) <= J - ne & xi > kap, 1e6, 0)
      .S(tb, ne)
    }, 1))
  }
  sort(d)[max(1, floor(level * reps))]
}

test_that("GMS bootstrap critical value, with equality moments never slackened", {
  set.seed(6)
  x <- stats::runif(25)
  M <- cbind(x + stats::rnorm(25, sd = 0.2), 3 + stats::rnorm(25), stats::rnorm(25, mean = 1, sd = 0.5))
  inst <- hypercube_instruments(matrix(x), n_levels = 2)
  kap <- sqrt(log(25))
  c1 <- ks_critical_value(M, inst, reps = 40, seed = 3)
  expect_equal(c1$critical_value, .gms_cv(M, inst$instruments, 0, 0.95, 40, 3, kap), tolerance = 1e-12)
  expect_equal(c1$kappa, kap)
  # the third column is an equality moment: GMS must leave it alone, so
  # the critical value stays O(1) instead of jumping to ~1e12
  c2 <- ks_critical_value(M, inst, n_equality = 1, reps = 40, seed = 3, level = 0.9, kappa = 1)
  ref <- .gms_cv(M, inst$instruments, 1, 0.9, 40, 3, 1)
  expect_equal(c2$critical_value, ref, tolerance = 1e-12)
  expect_lt(c2$critical_value, 1e3)
})

test_that("the confidence set keeps theta with KS <= its critical value", {
  set.seed(9)
  x <- stats::runif(40)
  y <- 1 + x + stats::runif(40)
  fn <- function(th) cbind(y - th * x - 1, th * x + 2.1 - y)
  grid <- c(0.2, 1, 1.5, 3)
  r <- ks_confidence_set(fn, grid, matrix(x), reps = 30, seed = 1)
  inst <- hypercube_instruments(matrix(x), n_levels = 2)
  keep <- Filter(function(th) {
    ks_statistic(fn(th), inst)$statistic <= ks_critical_value(fn(th), inst, reps = 30, seed = 1)$critical_value
  }, grid)
  expect_equal(r$set, keep)
  expect_identical(r$n_in_set, length(keep))
  expect_equal(r$bounds, if (length(keep)) range(keep) else NULL)
  expect_false(3 %in% r$set)
  expect_identical(morie_bnskmt, ks_confidence_set)
  expect_identical(kernelmomentbound, ks_confidence_set)
  expect_identical(bound_kernel_moment, ks_confidence_set)
})
