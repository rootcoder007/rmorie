# Coverage for the tail-2 batch t01: Cohen's kappa and weighted kappa
# (against irr::kappa2 where present), Cook's distance (against
# stats::cooks.distance), PNA aggregation (Corso et al. 2020, eqs. 5-7)
# and the Cori et al. (2013) instantaneous reproduction number.

test_that("Cohen's kappa and its large-sample SE", {
  r1 <- c("a", "b", "a", "c", "b", "a", "c", "c", "a", "b")
  r2 <- c("a", "b", "b", "c", "b", "a", "a", "c", "a", "c")
  k <- KappaCoh(r1, r2)
  tab <- table(factor(r1, c("a", "b", "c")), factor(r2, c("a", "b", "c")))
  po <- sum(diag(tab)) / 10
  pe <- sum(rowSums(tab) * colSums(tab)) / 100
  expect_equal(k$kappa, (po - pe) / (1 - pe), tolerance = 1e-12)
  expect_equal(k$se, sqrt(po * (1 - po) / 10) / (1 - pe), tolerance = 1e-12)
  expect_equal(k$z, k$kappa / k$se, tolerance = 1e-12)
  expect_identical(KappaCoh(rep(1, 3), rep(1, 3))$kappa, 0)
  expect_error(KappaCoh(1:3, 1:2), "equal length")
  skip_if_not_installed("irr")
  expect_equal(k$kappa, irr::kappa2(cbind(r1, r2))$value, tolerance = 1e-12)
})

test_that("weighted kappa is 1 - sum(W O) / sum(W E)", {
  r1 <- c(1, 2, 3, 3, 2, 1, 4, 4, 2, 3, 1, 4)
  r2 <- c(1, 3, 3, 2, 2, 2, 4, 3, 1, 3, 1, 4)
  lin <- KappaWt(r1, r2)
  O <- table(factor(r1, 1:4), factor(r2, 1:4)) / 12
  E <- outer(rowSums(O), colSums(O))
  W <- abs(outer(1:4, 1:4, "-"))
  expect_equal(lin$kappa, 1 - sum(W * O) / sum(W * E), tolerance = 1e-12)
  q <- KappaWt(r1, r2, "quadratic")
  expect_equal(q$kappa, 1 - sum(W^2 * O) / sum(W^2 * E), tolerance = 1e-12)
  M <- 1 - diag(4)
  expect_equal(KappaWt(r1, r2, M)$kappa, KappaCoh(r1, r2)$kappa, tolerance = 1e-12)
  expect_error(KappaWt(r1, r2, "cubic"), "must be 'linear'")
  expect_error(KappaWt(r1, r2, diag(3)), "k x k")
  expect_error(KappaWt(rep(1, 3), rep(1, 3)), "expected disagreement is zero")
  skip_if_not_installed("irr")
  expect_equal(lin$kappa, irr::kappa2(cbind(r1, r2), weight = "equal")$value, tolerance = 1e-12)
  expect_equal(q$kappa, irr::kappa2(cbind(r1, r2), weight = "squared")$value, tolerance = 1e-12)
})

test_that("Cook's distance matches stats::cooks.distance", {
  set.seed(3)
  x <- stats::rnorm(15)
  y <- 1 + 2 * x + stats::rnorm(15)
  y[4] <- y[4] + 5
  X <- cbind(1, x)
  f <- stats::lm(y ~ x)
  cd <- CooksD(y, X)
  expect_equal(cd$d, unname(stats::cooks.distance(f)), tolerance = 1e-10)
  expect_equal(cd$leverage, unname(stats::hatvalues(f)), tolerance = 1e-10)
  expect_equal(cd$std_residual, unname(stats::rstandard(f)), tolerance = 1e-10)
  expect_identical(cd$argmax_d, as.integer(which.max(stats::cooks.distance(f)) - 1))
  expect_equal(cd$threshold, 4 / 15)
  expect_error(CooksD(1:2, cbind(1, 1:2)), "more observations than parameters")
  expect_error(CooksD(1:3, cbind(1, 1:4)), "same length")
})

test_that("PNA aggregates neighbours and scales by log-degree", {
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 1), c(1, 1, 0, 0), c(0, 1, 0, 0))
  X <- cbind(c(1, 2, 3, 4), c(0, 1, 0, 2))
  p <- PnaAgg(A, X)
  deg <- rowSums(A)
  delta <- mean(log(deg + 1))
  expect_equal(p$delta, delta, tolerance = 1e-12)
  nb2 <- X[c(1, 3, 4), ]
  sd_pop <- function(v) sqrt(mean(v^2) - mean(v)^2 + 1e-5)
  expect_equal(p$aggregated[[2]][[1]], colMeans(nb2), tolerance = 1e-12)
  expect_equal(p$aggregated[[2]][[2]], apply(nb2, 2, sd_pop), tolerance = 1e-12)
  expect_equal(p$aggregated[[2]][[3]], apply(nb2, 2, max))
  s <- log(deg + 1) / delta
  expect_equal(p$scale, unname(cbind(1, s, 1 / s)), tolerance = 1e-12)
  row2 <- unlist(lapply(1:3, function(si) unlist(p$aggregated[[2]]) * p$scale[2, si]))
  expect_equal(p$out[2, ], row2, tolerance = 1e-12)
  expect_identical(p$n_columns, 24L)
  q <- PnaAgg(A, X, aggregators = "min", scalers = "attenuation")
  expect_equal(q$out[4, ], X[2, ] / s[4], tolerance = 1e-12)
  expect_error(PnaAgg(A[, 1:3], X), "square adjacency")
  expect_error(PnaAgg(A, X[1:3, ]), "one row per node")
  expect_error(PnaAgg(A, X, aggregators = "sum"), "unknown aggregator")
  expect_error(PnaAgg(A, X, scalers = "log"), "unknown scaler")
  expect_error(PnaAgg(matrix(0, 2, 2), X[1:2, ]), "no edges")
})

test_that("Cori R_t: Gamma(a + sum I, 1/(1/b + sum Lambda)) posterior", {
  inc <- c(2, 3, 5, 8, 10, 14, 15, 20, 22, 25)
  w <- c(0, 0.3, 0.4, 0.2, 0.1)
  r <- RtSi(inc, w, window = 3)
  lam <- vapply(1:10, function(t) {
    k <- 0:min(t - 1, 4)
    sum(w[k + 1] * inc[t - k])
  }, 1)
  lam[1] <- 0
  expect_equal(r$lambda, lam, tolerance = 1e-12)
  ends <- 3:9
  a <- 1 + vapply(ends, function(e) sum(inc[(e - 2):e + 1]), 1)
  b <- 1 / (1 / 5 + vapply(ends, function(e) sum(lam[(e - 2):e + 1]), 1))
  expect_equal(r$r_mean, a * b, tolerance = 1e-12)
  expect_equal(r$r_std, sqrt(a) * b, tolerance = 1e-12)
  expect_identical(r$t_end, ends)
  full <- RtSi(inc, w, window = 10)
  expect_identical(full$n_windows, 0L)
  expect_error(RtSi(inc, w, window = 11), "window must lie")
  expect_error(RtSi(numeric(0), w), "incidence is empty")
  expect_error(RtSi(inc, numeric(0)), "serial_interval is empty")
})
