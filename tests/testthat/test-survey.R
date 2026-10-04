# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for R/survey.R -- design constructor, HT total, Hajek mean, ratio,
# post-stratify, raking calibrate, subpop, complex GLM.

.make_survey_df <- function(n = 100L, seed = 1L) {
  set.seed(seed)
  data.frame(
    id     = seq_len(n),
    y      = rnorm(n, 10, 2),
    x      = rnorm(n, 5, 1),
    w      = runif(n, 0.5, 1.5),
    s      = sample(c("A", "B"), n, replace = TRUE),
    cl     = sample(seq_len(10), n, replace = TRUE),
    fpc    = rep(1000L, n)
  )
}

# ---------------------------------------------------------------------------
# morie_survey_design
# ---------------------------------------------------------------------------

test_that("morie_survey_design is native whatever is installed", {
  df <- .make_survey_df()
  d <- morie_survey_design(df, weights_col = "w")
  expect_s3_class(d, "morie_survey_design")
  expect_equal(d$weights, df$w)
  expect_equal(unique(d$n_psu), nrow(df))
})

test_that("morie_survey_design errors on missing weights column", {
  df <- .make_survey_df()
  expect_error(morie_survey_design(df, weights_col = "nope"),
               regexp = "not in data")
})

test_that("morie_survey_design with stratum + cluster builds object", {
  df <- .make_survey_df()
  d <- morie_survey_design(df, weights_col = "w",
                           strata_col = "s", cluster_col = "cl",
                           nest = TRUE)
  expect_s3_class(d, "morie_survey_design")
  # PSUs counted within each stratum
  for (h in c("A", "B")) {
    expect_equal(unique(d$n_psu[d$strata == h]), length(unique(df$cl[df$s == h])))
  }
  # shared cluster ids across strata need nest = TRUE
  expect_error(morie_survey_design(df, "w", strata_col = "s", cluster_col = "cl"),
               "not nested in strata")
})

test_that("design-based SE of the mean equals the stratified cluster formula", {
  # two strata, two/three PSUs each, an fpc in stratum A
  df <- data.frame(y = c(1, 3, 2, 6, 5, 4, 8, 7), w = c(2, 2, 1, 1, 3, 3, 1, 2),
                   s = c("A", "A", "A", "A", "B", "B", "B", "B"),
                   psu = c(1, 1, 2, 2, 3, 4, 5, 5), N = c(10, 10, 10, 10, Inf, Inf, Inf, Inf))
  df$N[df$s == "B"] <- 1e12
  d <- morie_survey_design(df, "w", strata_col = "s", cluster_col = "psu", fpc_col = "N")
  r <- morie_survey_mean(d, "y")
  m <- sum(df$w * df$y) / sum(df$w)
  z <- df$w * (df$y - m) / sum(df$w)
  v <- 0
  for (h in c("A", "B")) {
    t <- tapply(z[df$s == h], df$psu[df$s == h], sum)
    nh <- length(t)
    f <- (df$N[df$s == h][1] - nh) / df$N[df$s == h][1]
    v <- v + f * nh / (nh - 1) * sum((t - mean(t))^2)
  }
  expect_equal(r$mean, m, tolerance = 1e-14)
  expect_equal(r$se, sqrt(v), tolerance = 1e-14)
  # a stratum with one PSU cannot carry a variance
  df1 <- df[df$psu %in% c(1, 3, 4), ]
  expect_error(morie_survey_mean(morie_survey_design(df1, "w", strata_col = "s", cluster_col = "psu"), "y"),
               "only one PSU")
})

# ---------------------------------------------------------------------------
# morie_survey_ht_total
# ---------------------------------------------------------------------------

test_that("morie_survey_ht_total: equal pi recovers sum(y)/pi", {
  y <- c(1, 2, 3, 4, 5)
  pi <- rep(0.5, 5)
  res <- morie_survey_ht_total(y, pi)
  expect_equal(res$total, sum(y) / 0.5, tolerance = 1e-3)
  expect_true(res$se >= 0)
  expect_true(res$ci_lower < res$total && res$ci_upper > res$total)
})

test_that("morie_survey_ht_total has zero SE when pi == 1", {
  res <- morie_survey_ht_total(c(2, 4, 6), c(1, 1, 1))
  expect_equal(res$total, 12, tolerance = 1e-3)
  expect_equal(res$se, 0, tolerance = 1e-6)
})

test_that("morie_survey_ht_total rejects invalid inclusion probs", {
  expect_error(morie_survey_ht_total(c(1, 2), c(0, 1)),
               regexp = "inclusion_probs")
  expect_error(morie_survey_ht_total(c(1, 2), c(0.5, 1.5)),
               regexp = "inclusion_probs")
  expect_error(morie_survey_ht_total(c(1, 2), c(0.5)),
               regexp = "same length")
})

# ---------------------------------------------------------------------------
# morie_survey_hajek_mean
# ---------------------------------------------------------------------------

test_that("morie_survey_hajek_mean: equal weights == simple mean", {
  y <- c(2, 4, 6, 8)
  w <- rep(1, 4)
  res <- morie_survey_hajek_mean(y, w)
  expect_equal(res$mean, mean(y), tolerance = 1e-3)
})

test_that("morie_survey_hajek_mean: weighted mean of c(2,4) w=c(1,3) = 3.5", {
  res <- morie_survey_hajek_mean(c(2, 4), c(1, 3))
  expect_equal(res$mean, 3.5, tolerance = 1e-3)
})

test_that("morie_survey_hajek_mean rejects bad input", {
  expect_error(morie_survey_hajek_mean(c(1, 2), c(1, -1)),
               regexp = "must be > 0")
  expect_error(morie_survey_hajek_mean(c(1, 2, 3), c(1, 1)),
               regexp = "same length")
  expect_error(morie_survey_hajek_mean(c(1), c(1)),
               regexp = ">=")
})

# ---------------------------------------------------------------------------
# morie_survey_mean
# ---------------------------------------------------------------------------

test_that("morie_survey_mean works on fallback design", {
  # build a fallback object directly by skipping survey check via class fallback
  df <- .make_survey_df(50)
  design_fb <- structure(list(data = df, weights = df$w),
                         class = "morie_survey_design_fallback")
  res <- morie_survey_mean(design_fb, "y")
  expect_true(is.numeric(res$mean))
  expect_true(res$se >= 0)
})

test_that("morie_survey_mean wraps svymean for survey designs", {
  df <- .make_survey_df()
  des <- morie_survey_design(df, weights_col = "w")
  res <- morie_survey_mean(des, "y")
  expect_true(is.numeric(res$mean))
})

# ---------------------------------------------------------------------------
# morie_survey_ratio
# ---------------------------------------------------------------------------

test_that("morie_survey_ratio recovers known ratio for proportional data", {
  set.seed(1)
  x <- runif(100, 1, 10)
  y <- 3 * x  # exact ratio = 3
  w <- rep(1, 100)
  res <- morie_survey_ratio(y, x, w, X_population_total = sum(x) * 2)
  expect_equal(res$ratio, 3, tolerance = 1e-3)
  expect_equal(res$total_estimate, 3 * sum(x) * 2, tolerance = 1e-3)
})

test_that("morie_survey_ratio rejects invalid inputs", {
  expect_error(morie_survey_ratio(c(1, 2), c(1, 2), c(1, -1), 10),
               regexp = "must be > 0")
  expect_error(morie_survey_ratio(c(1), c(1, 2), c(1, 1), 10),
               regexp = "same length")
  expect_error(morie_survey_ratio(c(1, 2), c(1, 2), c(1, 1), 0),
               regexp = "must be > 0")
  expect_error(morie_survey_ratio(c(1, 2), c(0, 0), c(1, 1), 10),
               regexp = "zero")
})

# ---------------------------------------------------------------------------
# morie_survey_poststratify
# ---------------------------------------------------------------------------

test_that("morie_survey_poststratify gives weights summing close to n", {
  df <- data.frame(s = rep(c("a", "b"), each = 5))
  pop <- list(a = 100, b = 200)
  w <- morie_survey_poststratify(df, "s", pop)
  expect_length(w, nrow(df))
  # Group means: a-stratum weight, b-stratum weight
  expect_equal(unique(w[df$s == "a"]),
               (100 / 300) / (5 / 10), tolerance = 1e-3)
  expect_equal(unique(w[df$s == "b"]),
               (200 / 300) / (5 / 10), tolerance = 1e-3)
})

test_that("morie_survey_poststratify errors on missing stratum in pop counts", {
  df <- data.frame(s = c("a", "b", "c"))
  pop <- list(a = 10, b = 20)
  expect_error(morie_survey_poststratify(df, "s", pop),
               regexp = "present in sample")
})

test_that("morie_survey_poststratify errors on missing column", {
  df <- data.frame(s = c("a", "b"))
  expect_error(morie_survey_poststratify(df, "nope", list(a = 1)),
               regexp = "not in df")
})

# ---------------------------------------------------------------------------
# morie_survey_calibrate (single-var raking)
# ---------------------------------------------------------------------------

test_that("morie_survey_calibrate matches target totals", {
  set.seed(1)
  df <- data.frame(x = rep(1, 50))
  w <- morie_survey_calibrate(df, "x", list(x = 100), max_iter = 20)
  expect_equal(sum(w * df$x), 100, tolerance = 1e-3)
})

test_that("morie_survey_calibrate errors on missing aux var", {
  df <- data.frame(x = rep(1, 5))
  expect_error(morie_survey_calibrate(df, "nope", list(nope = 1)),
               regexp = "not in df")
  expect_error(morie_survey_calibrate(df, "x", list()),
               regexp = "missing")
})

# ---------------------------------------------------------------------------
# morie_survey_subpop
# ---------------------------------------------------------------------------

test_that("morie_survey_subpop returns weighted domain mean", {
  set.seed(1)
  df <- data.frame(d = c(1, 1, 0, 0, 1),
                   y = c(10, 20, 30, 40, 30),
                   w = c(1, 1, 1, 1, 1))
  res <- morie_survey_subpop(df, "d", 1, "y", "w")
  expect_equal(res$mean, mean(c(10, 20, 30)), tolerance = 1e-3)
  expect_equal(res$n_domain, 3)
})

test_that("morie_survey_subpop errors when domain empty", {
  df <- data.frame(d = c(0, 0), y = c(1, 2), w = c(1, 1))
  expect_error(morie_survey_subpop(df, "d", 99, "y", "w"),
               regexp = "match")
})

# ---------------------------------------------------------------------------
# morie_survey_glm / morie_survey_complex_glm
# ---------------------------------------------------------------------------

test_that("morie_survey_complex_glm fits the weighted least-squares coefficients", {
  df <- .make_survey_df(80)
  fit <- morie_survey_complex_glm(df, y ~ x, weight_col = "w",
                                  family = "gaussian", cluster_col = "cl", strata_col = "s", nest = TRUE)
  X <- cbind(1, df$x)
  b <- solve(crossprod(X, X * df$w), crossprod(X, df$w * df$y))
  expect_equal(unname(fit$coefficients[, "Estimate"]), as.numeric(b), tolerance = 1e-10)
  expect_true(all(fit$coefficients[, "Std. Error"] > 0))
})

test_that("morie_survey_complex_glm rejects non-positive weights", {
  df <- .make_survey_df(20)
  df$w[1] <- -1
  expect_error(morie_survey_complex_glm(df, y ~ x, weight_col = "w"),
               regexp = "> 0")
})

test_that("survey estimators match the survey package", {
  y <- c(5.1, 6.3, 4.8, 7.2, 5.9, 6.6, 5.4, 6.0, 5.5, 6.8, 4.2, 5.0)
  x <- c(2.1, 2.9, 2.0, 3.3, 2.6, 3.0, 2.4, 2.7, 2.5, 3.1, 1.9, 2.2)
  w <- c(10, 12, 8, 15, 9, 11, 14, 10, 13, 9, 12, 16)
  pi <- c(0.1, 0.08, 0.12, 0.07, 0.11, 0.09, 0.07, 0.1, 0.08, 0.11, 0.09, 0.06)
  d <- data.frame(y, x, w, dom = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1),
                  x2 = c(1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0))
  # svymean, svyratio (SE x 70), svymean on subset(dom == 1), svytotal under
  # poisson_sampling(pi); calibrate(calfun = "raking")
  expect_equal(morie_survey_hajek_mean(y, w)$se, 0.2735903604313098, tolerance = 1e-13)
  expect_equal(morie_survey_ratio(y, x, w, 70)$se, 1.3338586248673094, tolerance = 1e-13)
  expect_equal(morie_survey_subpop(d, "dom", 1, "y", "w")$se, 0.35636988931716962, tolerance = 1e-13)
  expect_equal(morie_survey_ht_total(y, pi)$se, 227.5546234351398, tolerance = 1e-13)
  cw <- morie_survey_calibrate(d, c("x", "x2"), list(x = 40, x2 = 7.5), tol = 1e-13)
  expect_equal(cw[c(1, 4, 12)], c(1.2170070612763382, 1.4083631497992353, 1.2564438996747997), tolerance = 1e-11)
  expect_equal(sum(cw * x), 40, tolerance = 1e-12)
})

test_that("morie_survey_design validates its inputs with worded errors", {
  df <- .make_survey_df(30)
  expect_error(morie_survey_design(as.list(df), "w"), "must be a data frame")
  expect_error(morie_survey_design(df, "w", strata_col = "nope"), "column .nope. not in data")
  bad <- df; bad$w[2] <- NA
  expect_error(morie_survey_design(bad, "w"), "sampling weights must be numeric")
  bad <- df; bad$s[3] <- NA
  expect_error(morie_survey_design(bad, "w", strata_col = "s"), "missing values in the strata")
  bad <- df; bad$cl[4] <- NA
  expect_error(morie_survey_design(bad, "w", cluster_col = "cl"), "missing values in the cluster")
  bad <- df; bad$fpc[1] <- -1
  expect_error(morie_survey_design(bad, "w", fpc_col = "fpc"), "fpc column must be positive")
  bad <- df; bad$fpc[1] <- 999
  expect_error(morie_survey_design(bad, "w", fpc_col = "fpc"), "same for every unit of a stratum")
  bad <- df; bad$fpc <- 5
  expect_error(morie_survey_design(bad, "w", fpc_col = "fpc"), "population smaller than the sample")
  # values <= 1 are sampling fractions: N = n / f
  frac <- df; frac$fpc <- 0.25
  d <- morie_survey_design(frac, "w", fpc_col = "fpc")
  expect_equal(unique(d$popsize), nrow(df) / 0.25)
})

test_that("a census stratum (fpc = sample size) adds no variance", {
  df <- data.frame(y = c(1, 4, 2, 8, 3, 9), w = 1, s = rep(c("A", "B"), each = 3), N = c(3, 3, 3, 100, 100, 100))
  d <- morie_survey_design(df, "w", strata_col = "s", fpc_col = "N")
  r <- morie_survey_mean(d, "y")
  m <- mean(df$y)
  z <- (df$y - m) / nrow(df)
  zb <- z[4:6]
  expect_equal(r$se, sqrt((100 - 3) / 100 * 3 / 2 * sum((zb - mean(zb))^2)), tolerance = 1e-14)
})

test_that("designs are read from older fallback lists and survey-shaped objects", {
  df <- .make_survey_df(24)
  old <- structure(list(data = df, weights = df$w), class = "morie_survey_design_fallback")
  ref <- morie_survey_mean(morie_survey_design(df, "w"), "y")
  expect_equal(morie_survey_mean(old, "y"), ref)
  # the fields of a survey::svydesign object, built by hand so no survey install is needed
  sv <- structure(list(variables = df, prob = 1 / df$w,
                       strata = data.frame(s = rep(1, 24)), cluster = data.frame(id = seq_len(24)),
                       fpc = list(sampsize = matrix(24, 24, 1), popsize = NULL)),
                  class = c("survey.design2", "survey.design"))
  expect_equal(morie_survey_mean(sv, "y"), ref)
  sv$fpc$popsize <- matrix(Inf, 24, 1)
  expect_equal(morie_survey_mean(sv, "y"), ref)
  two <- sv
  two$cluster <- data.frame(a = rep(1:12, 2), b = seq_len(24))
  two$fpc$popsize <- cbind(rep(100, 24), rep(50, 24))
  expect_error(morie_survey_mean(two, "y"), "below the first stage")
  expect_error(morie_survey_mean(list(a = 1), "y"), "must come from morie_survey_design")
})

test_that("morie_survey_mean names a bad variable and returns NA on missing values", {
  df <- .make_survey_df(20)
  d <- morie_survey_design(df, "w")
  expect_error(morie_survey_mean(d, "nope"), "must name one column")
  df$y[5] <- NA
  expect_identical(morie_survey_mean(morie_survey_design(df, "w"), "y"), list(mean = NA_real_, se = NA_real_))
})

test_that("morie_survey_glm takes a family object and a formula string", {
  df <- .make_survey_df(40)
  d <- morie_survey_design(df, "w")
  a <- morie_survey_glm(d, "y ~ x", family = stats::gaussian())
  b <- morie_survey_glm(d, y ~ x)
  expect_equal(a$coefficients, b$coefficients)
  expect_equal(nrow(a$vcov), 2L)
})

test_that("survey-shaped designs: a finite population size and PSUs missing from the rows", {
  df <- .make_survey_df(24)
  base <- structure(list(variables = df, prob = 1 / df$w,
                         strata = data.frame(s = rep(1, 24)), cluster = data.frame(id = seq_len(24)),
                         fpc = list(sampsize = matrix(24, 24, 1), popsize = matrix(1000, 24, 1))),
                    class = c("survey.design2", "survey.design"))
  m <- sum(df$w * df$y) / sum(df$w)
  z <- df$w * (df$y - m) / sum(df$w)
  expect_equal(morie_survey_mean(base, "y")$se,
               sqrt((1000 - 24) / 1000 * 24 / 23 * sum((z - mean(z))^2)), tolerance = 1e-14)
  # a subset design keeps the PSU count of the full design: the absent PSUs enter as zero totals
  sub <- base
  sub$fpc$sampsize <- matrix(30, 24, 1)
  zp <- c(z, rep(0, 6))
  expect_equal(morie_survey_mean(sub, "y")$se,
               sqrt((1000 - 30) / 1000 * 30 / 29 * sum((zp - mean(zp))^2)), tolerance = 1e-14)
})

test_that("morie_survey_glm: rows with a missing covariate keep a zero score in the variance", {
  df <- .make_survey_df(40)
  df$x[c(2, 9, 31)] <- NA
  fit <- morie_survey_glm(morie_survey_design(df, "w"), y ~ x)
  cc <- stats::complete.cases(df)
  X <- cbind(1, df$x[cc]); w <- df$w[cc] / mean(df$w); y <- df$y[cc]
  b <- solve(crossprod(X, X * w), crossprod(X, w * y))
  U <- matrix(0, nrow(df), 2)
  U[cc, ] <- X * as.numeric(w * (y - X %*% b))
  A <- solve(crossprod(X, X * w))
  n <- nrow(df)
  V <- A %*% (crossprod(sweep(U, 2, colMeans(U))) * n / (n - 1)) %*% A
  expect_equal(unname(fit$coefficients[, "Estimate"]), as.numeric(b), tolerance = 1e-10)
  expect_equal(unname(fit$vcov), V, tolerance = 1e-10)
  # t tests on the rows in the fit: n_kept - 1 PSU degrees of freedom less the slope
  expect_equal(unname(fit$coefficients[, 4]),
               2 * stats::pt(-abs(as.numeric(b) / sqrt(diag(V))), df = sum(cc) - 2), tolerance = 1e-10)
})
