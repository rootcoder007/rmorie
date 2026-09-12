# Gibbons & Chakraborti, Nonparametric Statistical Inference (5th ed.),
# Ch. 2: the empirical distribution function, order statistics, sample
# quantiles, exceedance statistics and the quantile procedures.
#
# Anchors outside the module: base R's binomial and beta distributions
# (dbinom, pbinom, dbeta, pbeta), stats::ecdf, the uniform order-statistic
# moments E[U_(r)] = r/(n+1) and Cov = r(n-s+1)/((n+1)^2 (n+2)), and for
# the exceedance statistic an exhaustive enumeration of every arrangement
# of the two samples -- Exceed(i, m, n) is the number of Y values (sample
# size m) exceeding the i-th largest of the n X values, so its whole
# distribution is a count over choose(m + n, n) equally likely rank
# orderings.

test_that("the EDF count is binomial (Theorem 2.3.1)", {
  n <- 12L
  fx <- 0.3
  for (i in 0:n) {
    r <- rmorie:::Edfbinom(n, fx, i)
    expect_equal(r$pmf, stats::dbinom(i, n, fx))
    expect_equal(r$cdf, stats::pbinom(i, n, fx))
  }
  # n F_n(x) counts how many observations fall at or below x, so its mean
  # and variance are the binomial ones
  r <- rmorie:::Edfbinom(n, fx)
  expect_equal(r$mean, n * fx)
  expect_equal(r$var, n * fx * (1 - fx))
  # with no index asked for, no point probability is invented
  expect_true(is.nan(r$pmf))
  expect_true(is.nan(r$cdf))
  # the degenerate tails are exact
  expect_equal(rmorie:::Edfbinom(5, 0, 0)$cdf, 1)
  expect_equal(rmorie:::Edfbinom(5, 1, 5)$pmf, 1)
  expect_equal(rmorie:::Edfbinom(5, 0)$var, 0)
  expect_equal(rmorie:::Edfbinom(1, 0.5, 1)$pmf, 0.5)

  expect_error(rmorie:::Edfbinom(0, 0.5), "at least 1")
  expect_error(rmorie:::Edfbinom(5, 1.2), "\\[0, 1\\]")
  expect_error(rmorie:::Edfbinom(5, -0.1), "\\[0, 1\\]")
  expect_error(rmorie:::Edfbinom(5, 0.5, 6), "0\\.\\.n")
  expect_error(rmorie:::Edfbinom(5, 0.5, -1), "0\\.\\.n")
})

test_that("the EDF is the step function stats::ecdf builds", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  t <- c(0, 1, 1.5, 4, 9, 10)
  r <- rmorie:::Edfstep(x, t)
  expect_equal(r$edf, as.numeric(stats::ecdf(x)(t)))
  expect_equal(r$count, as.numeric(vapply(t, function(v) sum(x <= v), 0)))
  expect_equal(r$n, 8L)
  # the sorted sample is reported, ties kept
  expect_equal(r$sorted, sort(x))
  # the EDF is right-continuous and jumps by 1/n at each distinct value,
  # by 2/n at the tied value
  expect_equal(rmorie:::Edfstep(x, 1)$edf, 2 / 8)
  expect_equal(rmorie:::Edfstep(x, 0.999)$edf, 0)
  expect_equal(rmorie:::Edfstep(x, 100)$edf, 1)
  # a single observation gives the unit step
  expect_equal(rmorie:::Edfstep(7, c(6, 7, 8))$edf, c(0, 1, 1))
  expect_error(rmorie:::Edfstep(numeric(0), 1), "non-empty")
})

test_that("the r-th order statistic CDF is the binomial tail (eq. 2.4.1)", {
  n <- 10L
  p <- 0.4
  for (r in seq_len(n)) {
    got <- rmorie:::Ostatcdf(0, r, n, p)
    # at least r of the n observations fall at or below t
    expect_equal(got$cdf, 1 - stats::pbinom(r - 1L, n, p))
    # equivalently the incomplete beta, since U_(r) ~ Beta(r, n - r + 1)
    expect_equal(got$cdf, stats::pbeta(p, r, n - r + 1))
    expect_equal(got$sf, 1 - got$cdf)
    expect_equal(got$fx, p)
  }
  # the CDF is passed either as a value or as a function of t
  expect_equal(rmorie:::Ostatcdf(qnorm(0.4), 3, n, stats::pnorm)$cdf,
               rmorie:::Ostatcdf(0, 3, n, 0.4)$cdf)
  # the minimum and the maximum are the two closed forms
  expect_equal(rmorie:::Ostatcdf(0, 1L, n, p)$cdf, 1 - (1 - p)^n)
  expect_equal(rmorie:::Ostatcdf(0, n, n, p)$cdf, p^n)
  # the r-th order statistic is stochastically larger as r grows
  cdfs <- vapply(seq_len(n), function(r) rmorie:::Ostatcdf(0, r, n, p)$cdf, 0)
  expect_true(all(diff(cdfs) < 0))

  expect_error(rmorie:::Ostatcdf(0, 0L, n, p), "1 <= r <= n")
  expect_error(rmorie:::Ostatcdf(0, 11L, n, p), "1 <= r <= n")
  expect_error(rmorie:::Ostatcdf(0, 3L, n, 1.5), "must lie in \\[0, 1\\]")
})

test_that("the order-statistic density is the beta density under uniform", {
  n <- 8L
  for (r in seq_len(n)) {
    for (x in c(0.1, 0.5, 0.9)) {
      # on the uniform, F(x) = x and f(x) = 1, so eq. (2.4.4) collapses to
      # the Beta(r, n - r + 1) density
      got <- rmorie:::Ostatpdf(x, r, n, cdf = x, pdf = 1)
      expect_equal(got$pdf, stats::dbeta(x, r, n - r + 1))
      expect_equal(got$coef, factorial(n) /
                     (factorial(r - 1) * factorial(n - r)))
    }
  }
  # the density is F and f evaluated at x, however they are supplied
  got <- rmorie:::Ostatpdf(0.3, 2L, n, cdf = function(z) z^2,
                           pdf = function(z) 2 * z)
  expect_equal(got$fx, 0.09)
  expect_equal(got$dx, 0.6)
  expect_equal(got$pdf, got$coef * 0.09^1 * (1 - 0.09)^(n - 2) * 0.6)
  # the normal median density at the population median
  m <- rmorie:::Ostatpdf(0, 3L, 5L, stats::pnorm, stats::dnorm)
  expect_equal(m$pdf, 30 * 0.5^2 * 0.5^2 * stats::dnorm(0))
  expect_error(rmorie:::Ostatpdf(0.5, 9L, n, 0.5, 1), "1 <= r <= n")
})

test_that("the probability-integral transform is Beta(r, n - r + 1)", {
  n <- 7L
  for (r in seq_len(n)) {
    for (u in c(0.05, 0.5, 0.95)) {
      b <- rmorie:::Ostatbeta(u, r, n)
      expect_equal(b$alpha, r)
      expect_equal(b$beta, n - r + 1)
      expect_equal(b$pdf, stats::dbeta(u, r, n - r + 1))
      expect_equal(b$cdf, stats::pbeta(u, r, n - r + 1))
      expect_equal(b$mean, r / (n + 1))
      expect_equal(b$var, r * (n - r + 1) / ((n + 1)^2 * (n + 2)))
    }
  }
  # the sample median of an odd sample is centred on 1/2
  expect_equal(rmorie:::Ostatbeta(0.5, 4L, 7L)$mean, 0.5)
  # the endpoints of the support
  expect_equal(rmorie:::Ostatbeta(0, 1L, n)$cdf, 0)
  expect_equal(rmorie:::Ostatbeta(1, n, n)$cdf, 1)
  expect_error(rmorie:::Ostatbeta(0.5, 8L, n), "1 <= r <= n")
  expect_error(rmorie:::Ostatbeta(1.5, 1L, n), "must lie in \\[0, 1\\]")
})

test_that("the sample-quantile variance is the asymptotic p(1-p)/(n f^2)", {
  a <- rmorie:::Ostatasymp(0.5, 100L, 0, stats::dnorm(0))
  expect_equal(a$mean, 0)
  expect_equal(a$var, 0.25 / (100 * stats::dnorm(0)^2))
  expect_equal(a$se, sqrt(a$var))
  # the classic result: the normal median has asymptotic variance pi/(2n)
  expect_equal(a$var, pi / 200)
  # the uniform median has 1/(4n), since its density is one
  expect_equal(rmorie:::Ostatasymp(0.5, 64L, 0.5, 1)$var, 1 / 256)
  # and the variance falls like 1/n
  expect_equal(rmorie:::Ostatasymp(0.5, 200L, 0, 1)$var,
               rmorie:::Ostatasymp(0.5, 100L, 0, 1)$var / 2)
  # a flatter density at the quantile makes it harder to pin down
  expect_true(rmorie:::Ostatasymp(0.5, 50L, 0, 0.1)$var >
                rmorie:::Ostatasymp(0.5, 50L, 0, 0.4)$var)
  expect_error(rmorie:::Ostatasymp(0, 10L, 0, 1), "strictly inside")
  expect_error(rmorie:::Ostatasymp(1, 10L, 0, 1), "strictly inside")
  expect_error(rmorie:::Ostatasymp(0.5, 0L, 0, 1), "at least 1")
  expect_error(rmorie:::Ostatasymp(0.5, 10L, 0, 0), "strictly positive")
})

test_that("uniform order-statistic moments are the beta moments", {
  n <- 9L
  for (r in seq_len(n)) {
    m <- rmorie:::Ostatmom(r, n, k = 1L)
    expect_equal(m$moment, r / (n + 1))
    expect_equal(m$mean, r / (n + 1))
    expect_equal(m$var, r * (n - r + 1) / ((n + 1)^2 * (n + 2)))
    # the k-th raw moment of a Beta(r, n - r + 1) variate
    for (k in 1:4) {
      expect_equal(rmorie:::Ostatmom(r, n, k)$moment,
                   prod((r + 0:(k - 1)) / (n + 1 + 0:(k - 1))))
      expect_equal(rmorie:::Ostatmom(r, n, k)$moment,
                   beta(r + k, n - r + 1) / beta(r, n - r + 1))
    }
    # and the variance is the second moment less the squared first
    expect_equal(m$var, rmorie:::Ostatmom(r, n, 2L)$moment - m$mean^2)
  }
  # the order statistics divide (0, 1) into n + 1 equal expected gaps
  means <- vapply(seq_len(n), function(r) rmorie:::Ostatmom(r, n)$mean, 0)
  expect_equal(diff(means), rep(1 / (n + 1), n - 1))
  expect_error(rmorie:::Ostatmom(10L, n), "1 <= r <= n")
  expect_error(rmorie:::Ostatmom(1L, n, 0L), "at least 1")
})

test_that("order statistics are positively correlated, decaying with the gap", {
  n <- 10L
  for (r in 1:4) {
    for (s in (r + 1):n) {
      cv <- rmorie:::Ostatcov(r, s, n)
      den <- (n + 1)^2 * (n + 2)
      expect_equal(cv$cov, r * (n - s + 1) / den)
      expect_equal(cv$var_r, r * (n - r + 1) / den)
      expect_equal(cv$var_s, s * (n - s + 1) / den)
      expect_equal(cv$corr, cv$cov / sqrt(cv$var_r * cv$var_s))
      expect_true(cv$cov > 0)
      expect_true(cv$corr > 0 && cv$corr <= 1)
    }
  }
  # r = s is the variance, so the correlation is exactly one
  expect_equal(rmorie:::Ostatcov(3L, 3L, n)$corr, 1)
  expect_equal(rmorie:::Ostatcov(3L, 3L, n)$cov,
               rmorie:::Ostatmom(3L, n)$var)
  # the correlation decays as the two ranks move apart
  cors <- vapply(2:n, function(s) rmorie:::Ostatcov(1L, s, n)$corr, 0)
  expect_true(all(diff(cors) < 0))
  # the minimum and the maximum are the least dependent pair
  expect_equal(rmorie:::Ostatcov(1L, n, n)$cov, 1 / ((n + 1)^2 * (n + 2)))
  expect_equal(rmorie:::Ostatcov(1L, n, n)$cov,
               min(vapply(2:n, function(s) rmorie:::Ostatcov(1L, s, n)$cov, 0)))
  # and the uniform order statistics are mirror-symmetric about 1/2, so
  # rank r and rank n - r + 1 have the same variance
  for (r in seq_len(n)) {
    expect_equal(rmorie:::Ostatcov(r, r, n)$var_r,
                 rmorie:::Ostatcov(n - r + 1L, n - r + 1L, n)$var_r)
  }
  expect_error(rmorie:::Ostatcov(5L, 3L, n), "1 <= r <= s <= n")
  expect_error(rmorie:::Ostatcov(1L, 11L, n), "1 <= r <= s <= n")
})

test_that("the joint density of two order statistics integrates to one", {
  # eq. (2.4.6) on the uniform: the coefficient times x^(r-1) (y-x)^(s-r-1)
  # (1-y)^(n-s). For n = 2, r = 1, s = 2 that is the constant 2 on the
  # triangle x < y, whose area is 1/2.
  j <- rmorie:::Ostatjoint(0.3, 0.8, 1L, 2L, 2L, cdf = function(z) z,
                           pdf = function(z) 1)
  expect_equal(j$pdf, 2)
  expect_equal(j$coef, 2)
  # the density vanishes off the ordered region, and reports no evaluation
  off <- rmorie:::Ostatjoint(0.8, 0.3, 1L, 2L, 2L, cdf = function(z) z,
                             pdf = function(z) 1)
  expect_equal(off$pdf, 0)
  expect_true(is.nan(off$fx))
  expect_equal(rmorie:::Ostatjoint(0.5, 0.5, 1L, 2L, 2L,
                                   function(z) z, function(z) 1)$pdf, 0)

  # a larger case, integrated numerically over the triangle
  n <- 5L
  r <- 2L
  s <- 4L
  dens <- function(x, y) {
    rmorie:::Ostatjoint(x, y, r, s, n, function(z) z, function(z) 1)$pdf
  }
  inner <- function(y) {
    stats::integrate(function(xs) vapply(xs, dens, 0, y = y), 0, y)$value
  }
  total <- stats::integrate(function(ys) vapply(ys, inner, 0), 0, 1)$value
  expect_equal(total, 1, tolerance = 1e-6)
  # and against the closed form written out directly
  expect_equal(dens(0.25, 0.7),
               factorial(n) / (factorial(r - 1) * factorial(s - r - 1) *
                                 factorial(n - s)) *
                 0.25^(r - 1) * (0.7 - 0.25)^(s - r - 1) * (1 - 0.7)^(n - s))
  expect_error(rmorie:::Ostatjoint(0.2, 0.5, 2L, 2L, n, function(z) z,
                                   function(z) 1), "1 <= r < s <= n")
})

test_that("the joint density of the whole ordered sample is n! prod f", {
  f <- function(z) stats::dexp(z, rate = 2)
  x <- c(0.2, 0.5, 1.1)
  j <- rmorie:::Ostatjall(x, f)
  expect_equal(j$coef, factorial(3))
  expect_equal(j$prod, prod(f(x)))
  expect_equal(j$pdf, factorial(3) * prod(f(x)))
  expect_equal(j$ordered, 1L)
  # an unordered argument is outside the support
  un <- rmorie:::Ostatjall(c(0.5, 0.2, 1.1), f)
  expect_equal(un$pdf, 0)
  expect_equal(un$ordered, 0L)
  # so are ties, since the ordered region is open
  expect_equal(rmorie:::Ostatjall(c(0.2, 0.2), f)$pdf, 0)
  # the uniform case is the constant n! on the ordered simplex, whose
  # volume is 1/n!
  expect_equal(rmorie:::Ostatjall(c(0.1, 0.4, 0.9), function(z) 1)$pdf,
               factorial(3))
  expect_equal(rmorie:::Ostatjall(0.5, function(z) 1)$pdf, 1)
  expect_error(rmorie:::Ostatjall(numeric(0), f), "non-empty")
})

test_that("the sample quantile is the ceiling order statistic", {
  x <- c(5, 1, 4, 2, 3)
  for (p in c(0.05, 0.2, 0.35, 0.5, 0.75, 0.95)) {
    q <- rmorie:::Sampquant(x, p)
    expect_equal(q$r, as.integer(floor(5 * p)) + 1L)
    expect_equal(q$estimate, sort(x)[q$r])
    # the rank's beta moments come along for the confidence statement
    expect_equal(q$u_mean, q$r / 6)
    expect_equal(q$u_var, q$r * (5 - q$r + 1) / (36 * 7))
  }
  # the median of an odd sample is the middle observation
  expect_equal(rmorie:::Sampquant(x, 0.5)$estimate, 3)
  # a quantile past the last rank is held at the maximum rather than NA
  expect_equal(rmorie:::Sampquant(x, 0.999)$r, 5L)
  expect_equal(rmorie:::Sampquant(x, 0.999)$estimate, 5)
  expect_equal(rmorie:::Sampquant(x, 1e-9)$estimate, 1)
  expect_error(rmorie:::Sampquant(numeric(0), 0.5), "non-empty")
  expect_error(rmorie:::Sampquant(x, 0), "strictly inside")
  expect_error(rmorie:::Sampquant(x, 1), "strictly inside")
})

test_that("the exceedance distribution matches an exhaustive enumeration", {
  # With m Y values and n X values all distinct and exchangeable, every one
  # of the choose(m + n, n) rank arrangements is equally likely. Count the
  # Y values above the i-th largest X in each.
  enumerate <- function(i, m, n) {
    idx <- utils::combn(m + n, n)
    cnt <- integer(m + 1L)
    for (c in seq_len(ncol(idx))) {
      xr <- idx[, c]
      yr <- setdiff(seq_len(m + n), xr)
      k <- sum(yr > sort(xr, decreasing = TRUE)[i])
      cnt[k + 1L] <- cnt[k + 1L] + 1L
    }
    cnt / ncol(idx)
  }
  for (m in 2:4) {
    for (n in 2:4) {
      for (i in seq_len(n)) {
        e <- rmorie:::Exceed(i, m, n)
        expect_equal(e$pmf, enumerate(i, m, n))
        expect_equal(sum(e$pmf), 1)
        # the closed-form moments of the negative hypergeometric
        expect_equal(e$mean, m * i / (n + 1))
        expect_equal(e$var, m * i * (n - i + 1) * (m + n + 1) /
                       ((n + 1)^2 * (n + 2)))
      }
    }
  }
  # the individual point and tail probabilities are read off the same pmf
  e <- rmorie:::Exceed(2L, 5L, 4L)
  for (j in 0:5) {
    expect_equal(rmorie:::Exceed(2L, 5L, 4L, j)$pmf_j, e$pmf[j + 1L])
    expect_equal(rmorie:::Exceed(2L, 5L, 4L, j)$cdf_j, sum(e$pmf[1:(j + 1L)]))
  }
  expect_equal(rmorie:::Exceed(2L, 5L, 4L, 5L)$cdf_j, 1)
  # with no index asked for, nothing is invented
  expect_true(is.nan(e$pmf_j))
  expect_true(is.nan(e$cdf_j))
  # more Y values exceed a higher X threshold rank
  means <- vapply(1:4, function(i) rmorie:::Exceed(i, 5L, 4L)$mean, 0)
  expect_true(all(diff(means) > 0))

  expect_error(rmorie:::Exceed(0L, 3L, 4L), "1 <= i <= n")
  expect_error(rmorie:::Exceed(5L, 3L, 4L), "1 <= i <= n")
  expect_error(rmorie:::Exceed(1L, 0L, 4L), "at least 1")
  expect_error(rmorie:::Exceed(1L, 3L, 4L, 4L), "0\\.\\.m")
})

test_that("a distribution-free quantile interval has binomial coverage", {
  set.seed(2)
  x <- sort(stats::rnorm(20))
  for (p in c(0.25, 0.5, 0.75)) {
    for (r in c(1L, 5L)) {
      for (s in c(10L, 20L)) {
        ci <- rmorie:::Quantci(x, p, r, s)
        expect_equal(ci$lower, x[r])
        expect_equal(ci$upper, x[s])
        # the interval covers the p-th quantile exactly when between r and
        # s - 1 of the observations fall below it
        expect_equal(ci$coverage,
                     stats::pbinom(s - 1L, 20L, p) -
                       stats::pbinom(r - 1L, 20L, p))
        expect_equal(ci$alpha, 1 - ci$coverage)
      }
    }
  }
  # the whole sample range is the widest available statement
  full <- rmorie:::Quantci(x, 0.5, 1L, 20L)
  expect_equal(full$coverage, 1 - 2 * 0.5^20)
  # and widening the interval can only raise the coverage
  cov5 <- rmorie:::Quantci(x, 0.5, 5L, 16L)$coverage
  cov7 <- rmorie:::Quantci(x, 0.5, 7L, 14L)$coverage
  expect_true(cov5 > cov7)
  expect_error(rmorie:::Quantci(x[1], 0.5, 1L, 2L), "at least 2")
  expect_error(rmorie:::Quantci(x, 0.5, 5L, 5L), "1 <= r < s <= n")
  expect_error(rmorie:::Quantci(x, 0.5, 1L, 21L), "1 <= r < s <= n")
  expect_error(rmorie:::Quantci(x, 1, 1L, 5L), "strictly inside")
})

test_that("the quantile test is the binomial sign test on the threshold", {
  x <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  n <- 10L
  q0 <- 9
  k <- sum(x <= q0)
  expect_equal(k, 4L)
  lo <- rmorie:::Quanttest(x, q0, alternative = "less")
  hi <- rmorie:::Quanttest(x, q0, alternative = "greater")
  two <- rmorie:::Quanttest(x, q0)
  expect_equal(lo$statistic, k)
  # the one-sided p-values are the two binomial tails at k
  expect_equal(lo$p_value, stats::pbinom(k, n, 0.5))
  expect_equal(hi$p_value, 1 - stats::pbinom(k - 1L, n, 0.5))
  # the two-sided p-value doubles the smaller tail, capped at one
  expect_equal(two$p_value, min(1, 2 * min(lo$p_value, hi$p_value)))
  expect_equal(two$mean, n * 0.5)
  expect_equal(two$var, n * 0.25)
  # a threshold at the sample median cannot be rejected
  expect_equal(rmorie:::Quanttest(x, 11)$p_value, 1)
  # a threshold outside the sample is as extreme as the test can report
  expect_equal(rmorie:::Quanttest(x, 0, alternative = "less")$p_value, 0.5^n)
  expect_equal(rmorie:::Quanttest(x, 100, alternative = "greater")$p_value,
               0.5^n)
  # testing a quantile other than the median shifts the null binomial
  q3 <- rmorie:::Quanttest(x, 15, p = 0.75)
  expect_equal(q3$statistic, 7L)
  expect_equal(q3$mean, 7.5)
  expect_equal(q3$p_value,
               min(1, 2 * min(stats::pbinom(7, n, 0.75),
                              1 - stats::pbinom(6, n, 0.75))))
  expect_error(rmorie:::Quanttest(numeric(0), 1), "non-empty")
  expect_error(rmorie:::Quanttest(x, 1, p = 0), "strictly inside")
  expect_error(rmorie:::Quanttest(x, 1, alternative = "bigger"),
               "two-sided, less or greater")
})
