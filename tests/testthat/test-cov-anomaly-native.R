# Coverage for the anomaly shelf: ABOD (Kriegel, Schubert & Zimek 2008,
# Definition 3, weighted angle variance), ECOD (Li et al. 2023), HBOS
# (Goldstein & Dengel 2012), isolation forest (Liu, Ting & Zhou 2008),
# LOF (Breunig et al. 2000), the MCD-based robust distance (Rousseeuw &
# Van Driessen 1999) and the rolling median/MAD series detector. Scores
# are recomputed from the definitions with independent loops.

.X <- function() {
  set.seed(3)
  rbind(matrix(stats::rnorm(40), 20), c(6, -5))
}

test_that("ABOD is the 1/(|AB||AC|)-weighted variance of <AB,AC>/(|AB|^2|AC|^2)", {
  X <- .X()
  n <- nrow(X)
  ref <- vapply(seq_len(n), function(a) {
    o <- setdiff(seq_len(n), a)
    t <- w <- numeric(0)
    for (i in seq_along(o)) for (j in seq_along(o)) if (i < j) {
      ab <- X[o[i], ] - X[a, ]
      ac <- X[o[j], ] - X[a, ]
      t <- c(t, sum(ab * ac) / (sum(ab^2) * sum(ac^2)))
      w <- c(w, 1 / sqrt(sum(ab^2) * sum(ac^2)))
    }
    sum(w * t^2) / sum(w) - (sum(w * t) / sum(w))^2
  }, 1)
  r <- morie_abod(X)
  expect_equal(r$abof, ref, tolerance = 1e-9)
  expect_equal(r$score, -r$abof)
  expect_identical(which(r$rank == 0L), 21L)
  expect_identical(r$mode, "exact")
  a <- morie_abod(X, k = 5)
  D <- as.matrix(stats::dist(X))
  diag(D) <- Inf
  nb <- order(D[21, ])[1:5]
  t <- w <- numeric(0)
  for (i in 1:4) for (j in (i + 1):5) {
    ab <- X[nb[i], ] - X[21, ]
    ac <- X[nb[j], ] - X[21, ]
    t <- c(t, sum(ab * ac) / (sum(ab^2) * sum(ac^2)))
    w <- c(w, 1 / sqrt(sum(ab^2) * sum(ac^2)))
  }
  expect_equal(a$abof[21], sum(w * t^2) / sum(w) - (sum(w * t) / sum(w))^2, tolerance = 1e-9)
  expect_identical(a$mode, "approximate")
  expect_error(morie_abod(X[1:2, ]), "at least 3")
  expect_error(morie_abod(X, k = 1), "between 2 and 20")
})

test_that("ECOD sums negative log tail probabilities per dimension", {
  X <- cbind(c(1, 2, 2, 3, 10), c(5, 4, 6, 5, 5))
  r <- morie_ecod(X)
  L <- apply(X, 2, function(v) vapply(v, function(x) mean(v <= x), 1))
  R <- apply(X, 2, function(v) vapply(v, function(x) mean(v >= x), 1))
  expect_equal(r$tail_left, L, tolerance = 1e-12)
  expect_equal(r$tail_right, R, tolerance = 1e-12)
  sk <- apply(X, 2, function(v) mean((v - mean(v))^3) / mean((v - mean(v))^2)^1.5)
  expect_equal(r$skewness, sk, tolerance = 1e-12)
  f <- function(P) rowSums(-log(pmax(P, 0.2)))
  auto <- f(sapply(1:2, function(j) if (sk[j] < 0) L[, j] else R[, j]))
  expect_equal(r$score, pmax(f(L), f(R), auto), tolerance = 1e-12)
  expect_identical(r$rank, as.integer(rank(-r$score, ties.method = "first") - 1))
  expect_identical(morie_ecod(t(c(1, 5, 2)))$n, 3L)
  expect_error(morie_ecod(matrix(1)), "at least 2")
})

test_that("HBOS sums negative log histogram densities", {
  X <- cbind(c(0, 1, 1.5, 2, 2.2, 9), c(3, 3, 3, 3, 3, 3))
  r <- morie_hbos(X, bins = 3)
  e <- seq(0, 9, length.out = 4)
  cnt <- c(5, 0, 1)
  d1 <- cnt[pmin(findInterval(X[, 1], e), 3)] / (6 * 3)
  d2 <- rep(6 / (6 * 1 / 3), 6)
  expect_equal(r$densities, cbind(d1, d2), tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(r$score, -log(d1) - log(d2), tolerance = 1e-12)
  dy <- morie_hbos(X[, 1, drop = FALSE], bins = 2, mode = "dynamic")
  q <- stats::quantile(X[, 1], c(0, 0.5, 1), names = FALSE)
  expect_equal(dy$bin_edges[[1]], q)
  expect_equal(dy$densities[, 1], 3 / (6 * diff(q))[pmin(findInterval(X[, 1], q), 2)], tolerance = 1e-12)
  expect_error(morie_hbos(X, bins = 0), "at least 1")
})

test_that("isolation forest: score 2^(-E h / c(psi)), outlier first, seeded", {
  X <- .X()
  r <- morie_isolation_forest(X, n_trees = 50, sample_size = 16, seed = 4)
  cn <- 2 * (log(15) + 0.5772156649) - 2 * 15 / 16
  expect_equal(r$score, 2^(-r$path_length / cn), tolerance = 1e-12)
  expect_identical(which(r$rank == 0L), 21L)
  expect_true(all(r$path_length <= ceiling(log2(16)) + cn + 1e-12))
  set.seed(99)
  u <- stats::runif(1)
  set.seed(99)
  r2 <- morie_isolation_forest(X, n_trees = 50, sample_size = 16, seed = 4)
  expect_identical(stats::runif(1), u)
  expect_identical(r2$score, r$score)
  expect_error(morie_isolation_forest(X, n_trees = 0), "n_trees")
  expect_error(morie_isolation_forest(X[1, , drop = FALSE]), "sample_size")
})

test_that("LOF follows Breunig et al.'s reachability definitions", {
  X <- .X()
  n <- nrow(X)
  k <- 4
  D <- as.matrix(stats::dist(X))
  N <- lapply(1:n, function(i) order(replace(D[i, ], i, Inf))[1:k])
  kd <- vapply(1:n, function(i) D[i, N[[i]][k]], 1)
  lrd <- vapply(1:n, function(i) 1 / mean(pmax(kd[N[[i]]], D[i, N[[i]]])), 1)
  lof <- vapply(1:n, function(i) mean(lrd[N[[i]]]) / lrd[i], 1)
  r <- morie_local_outlier_factor(X, k = k)
  expect_equal(r$lrd, lrd, tolerance = 1e-12)
  expect_equal(r$lof, lof, tolerance = 1e-12)
  expect_equal(r$k_distance, kd, tolerance = 1e-12)
  expect_identical(which(r$rank == 0L), 21L)
  expect_error(morie_local_outlier_factor(X, k = 21), "between 1 and 20")
})

test_that("MCD distances: classical at h = n, robust with contamination", {
  set.seed(8)
  X <- rbind(matrix(stats::rnorm(80), 40), matrix(stats::rnorm(8, 8), 4))
  full <- morie_mcd_outlier(X, support_fraction = 1, n_trials = 2)
  S <- stats::cov(X)
  expect_equal(full$location, colMeans(X), tolerance = 1e-12)
  expect_equal(full$covariance, S + 1e-9 * diag(2), tolerance = 1e-12)
  expect_equal(full$classical_distance, sqrt(stats::mahalanobis(X, colMeans(X), S)), tolerance = 1e-9)
  expect_equal(full$cutoff, sqrt(stats::qchisq(0.975, 2)))
  r <- morie_mcd_outlier(X, n_trials = 20, seed = 1)
  expect_identical(r$h, 33L)
  expect_true(all(r$outlier[41:44]))
  expect_equal(r$distance, sqrt(stats::mahalanobis(X, r$location, r$covariance)), tolerance = 1e-9)
  expect_identical(r$n_outliers, sum(r$distance > r$cutoff))
  expect_error(morie_mcd_outlier(X[1:2, ]), "more observations than dimensions")
  expect_error(morie_mcd_outlier(X, support_fraction = 0.5), "\\(0.5, 1\\]")
})

test_that("the series detector scores |y - rolling median| / (1.4826 MAD)", {
  y <- c(1, 2, 1.5, 1.8, 12, 1.7, 1.6, 2.1, 1.9, 1.4, 1.8)
  r <- morie_joseph_ts_outlier_detection(y, W = 2, threshold = 3.5)
  win <- lapply(seq_along(y), function(i) y[max(1, i - 2):min(11, i + 2)])
  md <- vapply(win, stats::median, 1)
  ma <- vapply(seq_along(y), function(i) stats::median(abs(win[[i]] - md[i])), 1)
  sc <- ifelse(ma > 0, abs(y - md) / (1.4826 * ma), 0)
  expect_equal(r$score, sc, tolerance = 1e-12)
  expect_equal(r$rolling_mad, ma, tolerance = 1e-12)
  expect_identical(which(r$outlier), 5L)
  expect_identical(morie_joseph_ts_outlier_detection(rep(1, 5), W = 2)$score, rep(0, 5))
  expect_error(morie_joseph_ts_outlier_detection(y, W = 0), "at least 1")
  expect_error(morie_joseph_ts_outlier_detection(1:4, W = 2), "too short")
})
