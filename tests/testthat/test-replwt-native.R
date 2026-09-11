# Anchors for replicate-weight variance estimation.
#
# The module exists to avoid writing a variance formula down, which means
# it can be checked against the one case where the formula IS known: the
# variance of a weighted total under a stratified design with PSUs is the
# ultimate-cluster estimator
#
#   V = sum_h n_h/(n_h - 1) sum_i (t_hi - tbar_h)^2
#
# and with two PSUs per stratum that collapses to sum_h (t_h1 - t_h2)^2.
# Both the jackknife and BRR must reproduce it exactly. Nothing was
# checking any of this: 29.6% coverage, no test naming any function here.

hmat <- function(k) do.call(rbind, .replwt_hadamard(k))

# the ultimate-cluster variance of a weighted total
uc_variance <- function(w, y, strata, psu) {
  t_hi <- tapply(w * y, psu, sum)
  h_of <- tapply(strata, psu, function(z) z[1])
  sum(vapply(unique(strata), function(h) {
    tt <- t_hi[h_of == h]
    nh <- length(tt)
    nh / (nh - 1) * sum((tt - mean(tt))^2)
  }, numeric(1)))
}

total <- function(wt, values) sum(wt * values)

test_that("the Hadamard construction is orthogonal at every order", {
  for (k in c(1, 2, 4, 8, 16)) {
    H <- hmat(k)
    n <- nrow(H)
    expect_identical(dim(H), c(n, n))
    expect_true(all(H %in% c(-1L, 1L)))
    # H'H = nI is what makes the half-samples balanced
    expect_equal(crossprod(H), diag(n) * n, ignore_attr = TRUE)
    expect_equal(tcrossprod(H), diag(n) * n, ignore_attr = TRUE)
  }
  # this construction only reaches powers of two, and says so
  for (bad in c(3, 5, 6, 7, 12)) {
    expect_error(.replwt_hadamard(bad), "power of two")
  }
  expect_error(.replwt_hadamard(0), "power of two")
})

test_that("the design records the strata and PSU structure", {
  w <- c(10, 10, 20, 20, 30, 30)
  st <- c("A", "A", "A", "A", "B", "B")
  ps <- c(1, 1, 2, 2, 3, 4)
  d <- .replwt_design(w, st, ps)
  expect_equal(d$weights, w)
  expect_identical(d$n, 6L)
  expect_setequal(d$stratum_order, c("A", "B"))
  # two PSUs in A, two in B
  expect_length(d$stratum_psus, 2L)
  expect_length(d$psu_units, 4L)
  # every unit belongs to exactly one PSU, and the PSUs partition the units
  expect_identical(sort(unlist(d$psu_units)), 1:6)
})

test_that("the jackknife variance of a total is the ultimate-cluster formula", {
  set.seed(5)
  strata <- rep(c("A", "B", "C"), each = 20)
  psu <- rep(1:12, each = 5)
  w <- rep(c(10, 12, 15), each = 20)
  y <- rnorm(60, 100, 20)
  d <- .replwt_design(w, strata, psu)
  jk <- .replwt_jackknife_weights(d)
  v <- .replwt_replicate_variance(total, d, jk, values = y)
  expect_equal(as.numeric(v$variance), uc_variance(w, y, strata, psu),
               tolerance = 1e-9)
  # the standard error is the root of the variance, and theta the estimate
  expect_equal(as.numeric(v$std_error), sqrt(as.numeric(v$variance)),
               tolerance = 1e-12)
  expect_equal(as.numeric(v$theta), sum(w * y), tolerance = 1e-9)
  # one replicate per PSU dropped
  expect_identical(as.integer(v$n_replicates), 12L)
  expect_length(v$replicates, 12L)
})

test_that("the jackknife drops one PSU and rescales its stratum", {
  w <- rep(10, 8)
  strata <- rep(c("A", "B"), each = 4)
  psu <- rep(1:4, each = 2)
  d <- .replwt_design(w, strata, psu)
  jk <- .replwt_jackknife_weights(d)
  expect_length(jk$weights, 4L)
  zeroed <- integer(0)
  for (r in seq_along(jk$weights)) {
    wr <- jk$weights[[r]]
    expect_length(wr, 8L)
    # exactly one PSU's units are zeroed, which here is two units
    z <- which(wr == 0)
    expect_length(z, 2L)
    zeroed <- c(zeroed, z)
    # the stratum that lost a PSU keeps its total weight, because the
    # survivor is scaled up by n_h/(n_h - 1)
    for (h in c("A", "B")) {
      inh <- which(strata == h)
      expect_equal(sum(wr[inh]), sum(w[inh]), tolerance = 1e-9)
    }
    # the stratum that lost nothing is untouched
    h_dropped <- strata[z[1]]
    other <- which(strata != h_dropped)
    expect_equal(wr[other], w[other], tolerance = 1e-12)
    # and no weight goes negative
    expect_true(all(wr >= 0))
  }
  # over the four replicates every unit is dropped exactly once
  expect_identical(sort(zeroed), 1:8)
})

test_that("BRR reproduces the closed form, whatever the Fay coefficient", {
  set.seed(6)
  strata <- rep(c("A", "B", "C", "D"), each = 10)
  psu <- rep(1:8, each = 5)
  w <- rep(c(10, 12, 15, 9), each = 10)
  y <- rnorm(40, 50, 10)
  d <- .replwt_design(w, strata, psu)
  want <- uc_variance(w, y, strata, psu)
  # the Fay adjustment shrinks the perturbation and divides it back out,
  # so the variance it reports does not depend on the coefficient
  for (fay in c(0, 0.3, 0.5, 0.7)) {
    br <- .replwt_brr_weights(d, fay = fay)
    v <- .replwt_replicate_variance(total, d, br, values = y)
    expect_equal(as.numeric(v$variance), want, tolerance = 1e-8)
  }
  # and the jackknife agrees with it on the same design
  jk <- .replwt_jackknife_weights(d)
  expect_equal(
    as.numeric(.replwt_replicate_variance(total, d, jk, values = y)$variance),
    want, tolerance = 1e-8)
})

test_that("BRR needs exactly two PSUs per stratum", {
  # the half-sample is formed by taking one of a pair, so the design has
  # to be paired
  strata <- rep(c("A", "B"), each = 15)
  psu <- rep(1:6, each = 5)
  d <- .replwt_design(rep(10, 30), strata, psu)
  expect_error(.replwt_brr_weights(d), "two PSUs per strat")
})

test_that("the bootstrap converges on the closed form as R grows", {
  set.seed(6)
  strata <- rep(c("A", "B", "C", "D"), each = 10)
  psu <- rep(1:8, each = 5)
  w <- rep(c(10, 12, 15, 9), each = 10)
  y <- rnorm(40, 50, 10)
  d <- .replwt_design(w, strata, psu)
  want <- uc_variance(w, y, strata, psu)
  v200 <- as.numeric(.replwt_replicate_variance(
    total, d, .replwt_bootstrap_weights(d, R = 200, seed = 1), values = y)$variance)
  v800 <- as.numeric(.replwt_replicate_variance(
    total, d, .replwt_bootstrap_weights(d, R = 800, seed = 1), values = y)$variance)
  # within a quarter at 200 replicates, and closer at 800
  expect_lt(abs(v200 / want - 1), 0.25)
  expect_lt(abs(v800 / want - 1), 0.1)
  # and the draw is seeded
  a <- .replwt_bootstrap_weights(d, R = 50, seed = 2)
  b <- .replwt_bootstrap_weights(d, R = 50, seed = 2)
  expect_equal(a$weights, b$weights, tolerance = 1e-15)
  c2 <- .replwt_bootstrap_weights(d, R = 50, seed = 3)
  expect_false(isTRUE(all.equal(a$weights, c2$weights)))
})

test_that("the replicate variance works for a ratio, not just a total", {
  # the point of the method: an estimator with no easy variance formula
  set.seed(8)
  strata <- rep(c("A", "B", "C"), each = 20)
  psu <- rep(1:12, each = 5)
  w <- rep(c(10, 12, 15), each = 20)
  num <- rnorm(60, 100, 20)
  den <- runif(60, 5, 15)
  d <- .replwt_design(w, strata, psu)
  ratio <- function(wt, values) sum(wt * values$num) / sum(wt * values$den)
  v <- .replwt_replicate_variance(ratio, d, .replwt_jackknife_weights(d),
                                  values = list(num = num, den = den))
  expect_equal(as.numeric(v$theta), sum(w * num) / sum(w * den),
               tolerance = 1e-10)
  expect_gt(as.numeric(v$variance), 0)
  expect_true(is.finite(as.numeric(v$std_error)))
})

test_that("a constant response has no sampling variance", {
  strata <- rep(c("A", "B"), each = 10)
  psu <- rep(1:4, each = 5)
  w <- rep(10, 20)
  d <- .replwt_design(w, strata, psu)
  # every PSU total identical, so the between-PSU spread is zero
  y <- rep(3, 20)
  v <- .replwt_replicate_variance(total, d, .replwt_jackknife_weights(d),
                                  values = y)
  expect_equal(as.numeric(v$variance), 0, tolerance = 1e-9)
  expect_equal(as.numeric(v$std_error), 0, tolerance = 1e-9)
})

test_that("the entry point builds weights for each method it offers", {
  strata <- rep(c("A", "B", "C", "D"), each = 10)
  psu <- rep(1:8, each = 5)
  d <- .replwt_design(rep(10, 40), strata, psu)
  jk <- morie_replwt(d, method = "jkn")
  expect_identical(as.integer(jk$n_replicates), 8L)
  expect_length(jk$weights, 8L)
  br <- morie_replwt(d, method = "brr")
  expect_gte(as.integer(br$n_replicates), 4L)
  expect_equal(br$fay, 0)
  bs <- morie_replwt(d, method = "bootstrap", R = 30, seed = 1)
  expect_identical(as.integer(bs$n_replicates), 30L)
  expect_equal(bs$seed, 1)
  # every replicate weight vector is as long as the sample
  for (obj in list(jk, br, bs)) {
    for (wr in obj$weights) expect_length(wr, 40L)
    expect_true(all(vapply(obj$weights, function(x) all(x >= 0), logical(1))))
  }
})
