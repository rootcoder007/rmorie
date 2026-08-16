# Native distribution functions: d/p/q/r without stats::.
#
# The Python arm (morie.fn._rrng_core + _stats_core) is the reference
# implementation; this file is its R mirror.  qnorm routes to the AS 241
# already in aaa_rng_native.R.  erf/erfc use the Maclaurin series for
# small |x| and the Abramowitz & Stegun 7.1.14 continued fraction for the
# tail -- exact expansions with no fitted coefficients, evaluated by
# modified Lentz.  The incomplete gamma (series / Lentz CF split at
# x = a + 1) and incomplete beta (Lentz CF with the symmetry flip at
# x = (a+1)/(a+b+2)) are ports of _stats_core, which is equivalence-tested
# against scipy on the Python side.  Everything here is tested against
# R's own d/p/q/r at 1e-13 or better in tests/testthat/test-dist-native.R.


#' Neumaier compensated summation: the R-side counterpart of Python\'s
#'
#' math.fsum (Shewchuk).  On ARM64 R\'s long double IS double, so base
#' sum() accumulates rounding error this does not; the two language arms
#' then agree to the last bit instead of to the platform.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
.morie_fsum <- function(x) {
  # Neumaier compensated summation: the R-side counterpart of Python's
  # math.fsum (Shewchuk).  On ARM64 R's long double IS double, so base
  # sum() accumulates rounding error this does not; the two language arms
  # then agree to the last bit instead of to the platform.
  s <- 0
  comp <- 0
  for (v in x) {
    t <- s + v
    comp <- comp + (if (abs(s) >= abs(v)) (s - t) + v else (v - t) + s)
    s <- t
  }
  s + comp
}

#' Regularized UPPER incomplete gamma Q(a, x), computed directly so the
#'
#' far tail never passes through 1 - P and lose its digits.
#'
#' @param a See Usage.
#' @param x See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_gammainc_q <- function(a, x) {
  # regularized UPPER incomplete gamma Q(a, x), computed directly so the
  # far tail never passes through 1 - P and lose its digits.
  if (x <= 0) {
    return(1)
  }
  if (x < a + 1) {
    1 - .morie_gammainc_p(a, x)
  } else {
    tiny <- 1e-300
    b <- x + 1 - a
    c_ <- 1 / tiny
    d <- 1 / b
    h <- d
    for (i in 1:1000) {
      an <- -i * (i - a)
      b <- b + 2
      d <- an * d + b
      if (abs(d) < tiny) d <- tiny
      c_ <- b + an / c_
      if (abs(c_) < tiny) c_ <- tiny
      d <- 1 / d
      delta <- d * c_
      h <- h * delta
      if (abs(delta - 1) < 1e-16) break
    }
    exp(-x + a * log(x) - lgamma(a)) * h
  }
}

#' Exact identity erfc(v) = Q(1/2, v^2): no transcribed continued
#'
#' fraction of its own, and the tail comes straight from the CF branch
#' of the incomplete gamma, which computes Q without forming 1 - P.
#'
#' @param x See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_erfc <- function(x) {
  # exact identity erfc(v) = Q(1/2, v^2): no transcribed continued
  # fraction of its own, and the tail comes straight from the CF branch
  # of the incomplete gamma, which computes Q without forming 1 - P.
  vapply(x, function(v) {
    if (v < 0) {
      2 - .morie_erfc(-v)
    } else {
      .morie_gammainc_q(0.5, v * v)
    }
  }, numeric(1))
}

#' .morie_erf
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_erf <- function(x) {
  vapply(x, function(v) {
    if (v < 0) {
      return(-.morie_erf(-v))
    }
    .morie_gammainc_p(0.5, v * v)
  }, numeric(1))
}

#' Regularized lower incomplete gamma P(a, x): series for x < a + 1,
#'
#' Lentz continued fraction for the complement otherwise.
#'
#' @param a See Usage.
#' @param x See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_gammainc_p <- function(a, x) {
  # regularized lower incomplete gamma P(a, x): series for x < a + 1,
  # Lentz continued fraction for the complement otherwise.
  if (x <= 0) {
    return(0)
  }
  if (x < a + 1) {
    ap <- a
    term <- 1 / a
    s <- term
    for (i in 1:1000) {
      ap <- ap + 1
      term <- term * x / ap
      s <- s + term
      if (abs(term) < abs(s) * 1e-16) break
    }
    s * exp(-x + a * log(x) - lgamma(a))
  } else {
    tiny <- 1e-300
    b <- x + 1 - a
    c_ <- 1 / tiny
    d <- 1 / b
    h <- d
    for (i in 1:1000) {
      an <- -i * (i - a)
      b <- b + 2
      d <- an * d + b
      if (abs(d) < tiny) d <- tiny
      c_ <- b + an / c_
      if (abs(c_) < tiny) c_ <- tiny
      d <- 1 / d
      delta <- d * c_
      h <- h * delta
      if (abs(delta - 1) < 1e-16) break
    }
    1 - exp(-x + a * log(x) - lgamma(a)) * h
  }
}

#' .morie_betacf
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param x See Usage.
#' @return The value of \code{h}, as built in the body.
#' @export
.morie_betacf <- function(a, b, x) {
  tiny <- 1e-300
  qab <- a + b
  qap <- a + 1
  qam <- a - 1
  c_ <- 1
  d <- 1 - qab * x / qap
  if (abs(d) < tiny) d <- tiny
  d <- 1 / d
  h <- d
  for (m in 1:500) {
    m2 <- 2 * m
    aa <- m * (b - m) * x / ((qam + m2) * (a + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    c_ <- 1 + aa / c_
    if (abs(c_) < tiny) c_ <- tiny
    d <- 1 / d
    h <- h * d * c_
    aa <- -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    c_ <- 1 + aa / c_
    if (abs(c_) < tiny) c_ <- tiny
    d <- 1 / d
    delta <- d * c_
    h <- h * delta
    if (abs(delta - 1) < 1e-16) break
  }
  h
}

#' Regularized incomplete beta I_x(a, b)
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param x See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_betainc <- function(a, b, x) {
  # regularized incomplete beta I_x(a, b)
  if (x <= 0) {
    return(0)
  }
  if (x >= 1) {
    return(1)
  }
  lbeta_ <- lgamma(a + b) - lgamma(a) - lgamma(b)
  front <- exp(lbeta_ + a * log(x) + b * log1p(-x))
  if (x < (a + 1) / (a + b + 2)) {
    front * .morie_betacf(a, b, x) / a
  } else {
    1 - exp(lbeta_ + b * log1p(-x) + a * log(x)) *
      .morie_betacf(b, a, 1 - x) / b
  }
}

#' .morie_bisect_q
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param cdf See Usage.
#' @param p See Usage.
#' @param lo See Usage.
#' @param hi See Usage.
#' @param tol Defaults to \code{1e-13}.
#' @return The value of \code{x}, as built in the body.
#' @export
.morie_bisect_q <- function(cdf, p, lo, hi, tol = 1e-13) {
  if (p <= 0) {
    return(lo)
  }
  if (p >= 1) {
    return(hi)
  }
  while (cdf(hi) < p) hi <- hi * 2 + 1
  while (cdf(lo) > p) lo <- lo * 2 - 1
  for (i in 1:200) {
    mid <- (lo + hi) / 2
    if (cdf(mid) < p) lo <- mid else hi <- mid
    if (abs(hi - lo) <= tol * max(1, abs(hi))) break
  }
  x <- (lo + hi) / 2
  # Newton polish: bisection converges on the interval, not the value
  for (i in 1:3) {
    fx <- cdf(x) - p
    h <- 1e-6 * max(1, abs(x))
    d <- (cdf(x + h) - cdf(x - h)) / (2 * h)
    if (!is.finite(d) || d <= 0) break
    x <- x - fx / d
  }
  if (abs(x) < 1e-11) x <- 0
  x
}

# ---- normal ----------------------------------------------------------

#' Dnorm
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param mean Defaults to \code{0}.
#' @param sd Defaults to \code{1}.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dnorm <- function(x, mean = 0, sd = 1, log = FALSE) {
  if (sd <= 0) stop("sd must be positive")
  z <- (x - mean) / sd
  lg <- -0.5 * z * z - log(sd) - 0.5 * log(2 * pi)
  if (log) lg else exp(lg)
}

#' Pnorm
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param mean Defaults to \code{0}.
#' @param sd Defaults to \code{1}.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pnorm <- function(q, mean = 0, sd = 1, lower_tail = TRUE) {
  if (sd <= 0) stop("sd must be positive")
  z <- (q - mean) / sd
  # the tail comes from erfc directly: 1 - cdf has no digits left once
  # the cdf rounds to 1
  if (lower_tail) {
    0.5 * .morie_erfc(-z / sqrt(2))
  } else {
    0.5 * .morie_erfc(z / sqrt(2))
  }
}

#' Qnorm
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param mean Defaults to \code{0}.
#' @param sd Defaults to \code{1}.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return A numeric value.
#' @export
Qnorm <- function(p, mean = 0, sd = 1, lower_tail = TRUE) {
  if (sd <= 0) stop("sd must be positive")
  pp <- if (lower_tail) p else 1 - p
  if (any(pp <= 0 | pp >= 1)) stop("p must lie strictly inside (0, 1)")
  mean + sd * .morie_normal_quantile(pp)
}

#' Inversion of the Philox uniform stream: draw k depends only on
#'
#' uniform k, so the stream is stable when n changes
#'
#' @param n See Usage.
#' @param mean Defaults to \code{0}.
#' @param sd Defaults to \code{1}.
#' @param seed Defaults to \code{0}.
#' @param stream Defaults to \code{0}.
#' @return The value of \code{Qnorm}.
#' @export
Rnorm <- function(n, mean = 0, sd = 1, seed = 0, stream = 0) {
  # inversion of the Philox uniform stream: draw k depends only on
  # uniform k, so the stream is stable when n changes
  u <- .morie_random_uniform(n, seed = seed, stream = stream)
  u <- pmin(pmax(u, 1e-300), 1 - 1e-16)
  Qnorm(u, mean, sd)
}

# ---- exponential -----------------------------------------------------

#' Dexp
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param rate Defaults to \code{1}.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dexp <- function(x, rate = 1, log = FALSE) {
  if (rate <= 0) stop("rate must be positive")
  lg <- ifelse(x < 0, -Inf, log(rate) - rate * x)
  if (log) lg else exp(lg)
}

#' Pexp
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param rate Defaults to \code{1}.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pexp <- function(q, rate = 1, lower_tail = TRUE) {
  if (rate <= 0) stop("rate must be positive")
  p <- ifelse(q < 0, 0, -expm1(-rate * q))
  if (lower_tail) p else ifelse(q < 0, 1, exp(-rate * q))
}

#' Qexp
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param rate Defaults to \code{1}.
#' @return A numeric value.
#' @export
Qexp <- function(p, rate = 1) {
  if (rate <= 0) stop("rate must be positive")
  if (any(p < 0 | p >= 1)) stop("p must lie in [0, 1)")
  -log1p(-p) / rate
}

#' Rexp
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @param rate Defaults to \code{1}.
#' @param seed Defaults to \code{0}.
#' @param stream Defaults to \code{0}.
#' @return The value of \code{Qexp}.
#' @export
Rexp <- function(n, rate = 1, seed = 0, stream = 0) {
  u <- .morie_random_uniform(n, seed = seed, stream = stream)
  Qexp(pmin(u, 1 - 1e-16), rate)
}

# ---- gamma / chi-square ---------------------------------------------

#' Dgamma
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param shape See Usage.
#' @param rate Defaults to \code{1}.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dgamma <- function(x, shape, rate = 1, log = FALSE) {
  if (shape <= 0 || rate <= 0) stop("shape and rate must be positive")
  lg <- ifelse(x <= 0, -Inf,
    shape * log(rate) + (shape - 1) * log(x) - rate * x -
      lgamma(shape)
  )
  if (log) lg else exp(lg)
}

#' Pgamma
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param shape See Usage.
#' @param rate Defaults to \code{1}.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pgamma <- function(q, shape, rate = 1, lower_tail = TRUE) {
  p <- vapply(q, function(v) {
    if (v <= 0) 0 else .morie_gammainc_p(shape, rate * v)
  }, numeric(1))
  if (lower_tail) p else 1 - p
}

#' Qgamma
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param shape See Usage.
#' @param rate Defaults to \code{1}.
#' @return A vector, from \code{vapply}.
#' @export
Qgamma <- function(p, shape, rate = 1) {
  vapply(
    p, function(pp) {
      .morie_bisect_q(function(v) Pgamma(v, shape, rate), pp, 0, 1)
    },
    numeric(1)
  )
}

#' Dchisq
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param df See Usage.
#' @param log Defaults to \code{FALSE}.
#' @return The value of \code{Dgamma}.
#' @export
Dchisq <- function(x, df, log = FALSE) Dgamma(x, df / 2, 0.5, log)
#' Pchisq
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param df See Usage.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return The value of \code{Pgamma}.
#' @export
Pchisq <- function(q, df, lower_tail = TRUE) {
  Pgamma(q, df / 2, 0.5, lower_tail)
}
#' Qchisq
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param df See Usage.
#' @return The value of \code{Qgamma}.
#' @export
Qchisq <- function(p, df) Qgamma(p, df / 2, 0.5)

# ---- Poisson / binomial ---------------------------------------------

#' Dpois
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param lambda See Usage.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dpois <- function(x, lambda, log = FALSE) {
  if (lambda < 0) stop("lambda must be non-negative")
  k <- round(x)
  lg <- ifelse(k < 0, -Inf,
    if (lambda > 0) {
      k * log(lambda) - lambda - lgamma(k + 1)
    } else {
      ifelse(k == 0, 0, -Inf)
    }
  )
  if (log) lg else exp(lg)
}

#' P(X <= k) = Q(k+1, lambda), the UPPER regularized incomplete gamma
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param lambda See Usage.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Ppois <- function(q, lambda, lower_tail = TRUE) {
  # P(X <= k) = Q(k+1, lambda), the UPPER regularized incomplete gamma
  p <- vapply(q, function(v) {
    k <- floor(v)
    if (k < 0) 0 else 1 - .morie_gammainc_p(k + 1, lambda)
  }, numeric(1))
  if (lower_tail) p else 1 - p
}

#' Smallest k with cdf(k) >= p, as R defines it
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param lambda See Usage.
#' @return A vector, from \code{vapply}.
#' @export
Qpois <- function(p, lambda) {
  # smallest k with cdf(k) >= p, as R defines it
  vapply(p, function(pp) {
    if (pp < 0 || pp > 1) stop("p must lie in [0, 1]")
    k <- 0
    while (Ppois(k, lambda) < pp - 1e-10) k <- k + 1
    k
  }, numeric(1))
}

#' Dbinom
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param size See Usage.
#' @param prob See Usage.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dbinom <- function(x, size, prob, log = FALSE) {
  if (prob < 0 || prob > 1) stop("prob must lie in [0, 1]")
  k <- round(x)
  lg <- vapply(k, function(kk) {
    if (kk < 0 || kk > size) {
      return(-Inf)
    }
    if (prob == 0) {
      return(if (kk == 0) 0 else -Inf)
    }
    if (prob == 1) {
      return(if (kk == size) 0 else -Inf)
    }
    lgamma(size + 1) - lgamma(kk + 1) - lgamma(size - kk + 1) +
      kk * log(prob) + (size - kk) * log1p(-prob)
  }, numeric(1))
  if (log) lg else exp(lg)
}

#' P(X <= k) = I_{1-p}(n - k, k + 1)
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param size See Usage.
#' @param prob See Usage.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pbinom <- function(q, size, prob, lower_tail = TRUE) {
  # P(X <= k) = I_{1-p}(n - k, k + 1)
  p <- vapply(q, function(v) {
    k <- floor(v)
    if (k < 0) {
      0
    } else if (k >= size) {
      1
    } else {
      .morie_betainc(size - k, k + 1, 1 - prob)
    }
  }, numeric(1))
  if (lower_tail) p else 1 - p
}

#' Qbinom
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param size See Usage.
#' @param prob See Usage.
#' @return A vector, from \code{vapply}.
#' @export
Qbinom <- function(p, size, prob) {
  vapply(p, function(pp) {
    if (pp < 0 || pp > 1) stop("p must lie in [0, 1]")
    k <- 0
    while (k < size && Pbinom(k, size, prob) < pp - 1e-10) k <- k + 1
    k
  }, numeric(1))
}

# ---- beta / t / F ----------------------------------------------------

#' Dbeta
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param shape1 See Usage.
#' @param shape2 See Usage.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dbeta <- function(x, shape1, shape2, log = FALSE) {
  if (shape1 <= 0 || shape2 <= 0) stop("shape parameters must be positive")
  lg <- ifelse(x <= 0 | x >= 1, -Inf,
    (shape1 - 1) * log(x) + (shape2 - 1) * log1p(-x) +
      lgamma(shape1 + shape2) - lgamma(shape1) - lgamma(shape2)
  )
  if (log) lg else exp(lg)
}

#' Pbeta
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param shape1 See Usage.
#' @param shape2 See Usage.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pbeta <- function(q, shape1, shape2, lower_tail = TRUE) {
  p <- vapply(q, function(v) .morie_betainc(shape1, shape2, v), numeric(1))
  if (lower_tail) p else 1 - p
}

#' Qbeta
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param shape1 See Usage.
#' @param shape2 See Usage.
#' @return A vector, from \code{vapply}.
#' @export
Qbeta <- function(p, shape1, shape2) {
  vapply(
    p, function(pp) {
      .morie_bisect_q(function(v) {
        Pbeta(min(max(v, 0), 1), shape1, shape2)
      }, pp, 0, 1)
    },
    numeric(1)
  )
}

#' Dt
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param df See Usage.
#' @param log Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
Dt <- function(x, df, log = FALSE) {
  if (df <= 0) stop("df must be positive")
  lg <- lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * log(df * pi) -
    (df + 1) / 2 * log1p(x * x / df)
  if (log) lg else exp(lg)
}

#' Pt
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param df See Usage.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pt <- function(q, df, lower_tail = TRUE) {
  p <- vapply(q, function(v) {
    xb <- df / (df + v * v)
    half <- 0.5 * .morie_betainc(df / 2, 0.5, xb)
    if (v <= 0) half else 1 - half
  }, numeric(1))
  if (lower_tail) p else 1 - p
}

#' The t is symmetric: qt(p) = -qt(1 - p) and qt(0.5) = 0 exactly
#'
#' Without this, bisection lands on the cdf plateau around zero --
#' betainc\'s 1 - xb underflows for |v| < ~1e-8, the cdf sits at exactly
#' 0.5 there, and both bisection and Newton are blind inside it.
#'
#' @param p See Usage.
#' @param df See Usage.
#' @return A vector, from \code{vapply}.
#' @export
Qt <- function(p, df) {
  # The t is symmetric: qt(p) = -qt(1 - p) and qt(0.5) = 0 exactly.
  # Without this, bisection lands on the cdf plateau around zero --
  # betainc's 1 - xb underflows for |v| < ~1e-8, the cdf sits at exactly
  # 0.5 there, and both bisection and Newton are blind inside it.
  vapply(p, function(pp) {
    if (pp == 0.5) {
      return(0)
    }
    if (pp < 0.5) {
      return(-.morie_bisect_q(function(v) Pt(v, df), 1 - pp, -1, 1))
    }
    .morie_bisect_q(function(v) Pt(v, df), pp, -1, 1)
  }, numeric(1))
}

#' Pf
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param q See Usage.
#' @param df1 See Usage.
#' @param df2 See Usage.
#' @param lower_tail Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
Pf <- function(q, df1, df2, lower_tail = TRUE) {
  p <- vapply(q, function(v) {
    if (v <= 0) {
      0
    } else {
      .morie_betainc(df1 / 2, df2 / 2, df1 * v / (df1 * v + df2))
    }
  }, numeric(1))
  if (lower_tail) p else 1 - p
}

#' Qf
#'
#' Part of the dist_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param df1 See Usage.
#' @param df2 See Usage.
#' @return A vector, from \code{vapply}.
#' @export
Qf <- function(p, df1, df2) {
  vapply(p, function(pp) {
    .morie_bisect_q(function(v) Pf(v, df1, df2), pp, 0, 1)
  }, numeric(1))
}

# canonical names are capitalized so library(morie) never masks
# stats:: -- morie::Dnorm, or bare Dnorm after attach.  The old
# morie_ spellings stay as aliases per ledger/NAMING.md.
morie_dbeta <- Dbeta # alias: pre-policy spelling
morie_dbinom <- Dbinom # alias: pre-policy spelling
morie_dchisq <- Dchisq # alias: pre-policy spelling
morie_dexp <- Dexp # alias: pre-policy spelling
morie_dgamma <- Dgamma # alias: pre-policy spelling
morie_dnorm <- Dnorm # alias: pre-policy spelling
morie_dpois <- Dpois # alias: pre-policy spelling
morie_dt <- Dt # alias: pre-policy spelling
morie_pbeta <- Pbeta # alias: pre-policy spelling
morie_pbinom <- Pbinom # alias: pre-policy spelling
morie_pchisq <- Pchisq # alias: pre-policy spelling
morie_pexp <- Pexp # alias: pre-policy spelling
morie_pf <- Pf # alias: pre-policy spelling
morie_pgamma <- Pgamma # alias: pre-policy spelling
morie_pnorm <- Pnorm # alias: pre-policy spelling
morie_ppois <- Ppois # alias: pre-policy spelling
morie_pt <- Pt # alias: pre-policy spelling
morie_qbeta <- Qbeta # alias: pre-policy spelling
morie_qbinom <- Qbinom # alias: pre-policy spelling
morie_qchisq <- Qchisq # alias: pre-policy spelling
morie_qexp <- Qexp # alias: pre-policy spelling
morie_qf <- Qf # alias: pre-policy spelling
morie_qgamma <- Qgamma # alias: pre-policy spelling
morie_qnorm <- Qnorm # alias: pre-policy spelling
morie_qpois <- Qpois # alias: pre-policy spelling
morie_qt <- Qt # alias: pre-policy spelling
morie_rexp <- Rexp # alias: pre-policy spelling
morie_rnorm <- Rnorm # alias: pre-policy spelling
