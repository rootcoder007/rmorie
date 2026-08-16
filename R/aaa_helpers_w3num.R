# SPDX-License-Identifier: AGPL-3.0-or-later
# Numerics shared by the wave-3 modules -- the R mirror of
# src/morie/fn/_w3num.py, function for function, with .w3_ in place of
# the leading underscore.
#
# Nothing here is novel. It exists because the obvious call in each
# language is a DIFFERENT function from the obvious call in the other,
# and a module that reaches for the obvious one cannot then be held to
# its Python arm at the twelfth digit:
#
#   sum()        R accumulates in 80-bit long double; CPython 3.12+
#                applies Neumaier compensation. Two good answers, not
#                the same answer.
#   %*%          goes through BLAS, free to reassociate and use FMA.
#                Written out here instead.
#   ^ vs **      R special-cases an integer exponent to repeated
#                squaring and 0.5 to sqrt(); Python calls libm pow().
#                Never used here for anything iterated.
#   pnorm, pt,   separate implementations in the two languages, agreeing
#   lgamma, erf  to about 1e-15 and no further. Written out so both arms
#                run the SAME algorithm.

# Neumaier-compensated sum. Not sum(): see the header.
#' Neumaier-compensated sum. Not sum(): see the header
#'
#' A step of the helpers_w3num implementation. Called by \code{.alfrf2_centre}, \code{.baysmplr_build_tree}, \code{.blinkg_corr} and 101 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v See Usage.
#' @return A numeric value.
#' @export
.w3_csum <- function(v) {
  s <- 0; cc <- 0
  for (t in v) {
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t) else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

# Compensated dot product. Not sum(a * b), same reason.
#' Compensated dot product. Not sum(a * b), same reason
#'
#' A step of the helpers_w3num implementation. Called by \code{.baysmplr_build_tree}, \code{.chemsc_angle}, \code{.hyper2_elliptical} and 21 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.w3_dot <- function(a, b) {
  n <- length(a)
  if (n == 0L) return(0)
  s <- 0; cc <- 0
  for (i in seq_len(n)) {
    t <- a[i] * b[i]
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t) else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

# log(sum(exp(v))), shifted by the maximum so it cannot overflow.
#' Log(sum(exp(v))), shifted by the maximum so it cannot overflow
#'
#' A step of the helpers_w3num implementation. Called by \code{morie_snpest}, \code{morie_varqc1_logpdf}, \code{morie_varqc1_mixture}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
.w3_logsumexp <- function(v) {
  if (length(v) == 0L) return(-Inf)
  m <- max(v)
  if (m == -Inf) return(m)
  m + log(.w3_csum(exp(v - m)))
}

# Lower Cholesky factor L with A = L L'. Explicit, not chol().
#' Lower Cholesky factor L with A = L L\'. Explicit, not chol()
#'
#' A step of the helpers_w3num implementation. Called by \code{.w3_ols}, \code{morie_cypin_fit}, \code{morie_hyper2} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A matrix; indexed by row and column.
#' @return The value of \code{lo}, as built in the body.
#' @export
.w3_chol <- function(a) {
  p <- nrow(a)
  lo <- matrix(0, p, p)
  for (i in seq_len(p)) {
    for (j in seq_len(i)) {
      s <- a[i, j] - if (j > 1L) .w3_dot(lo[i, seq_len(j - 1L)],
                                         lo[j, seq_len(j - 1L)]) else 0
      if (i == j) {
        if (s <= 0) stop("matrix is not positive definite")
        lo[i, j] <- sqrt(s)
      } else lo[i, j] <- s / lo[j, j]
    }
  }
  lo
}

# Solve L L' x = b by forward then back substitution.
#' Solve L L\' x = b by forward then back substitution
#'
#' A step of the helpers_w3num implementation. Called by \code{.w3_inv_from_chol}, \code{.w3_ols}, \code{morie_cypin_fit} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param lo A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
.w3_solve_chol <- function(lo, b) {
  p <- nrow(lo)
  z <- numeric(p)
  for (i in seq_len(p)) {
    acc <- if (i > 1L) .w3_dot(lo[i, seq_len(i - 1L)], z[seq_len(i - 1L)]) else 0
    z[i] <- (b[i] - acc) / lo[i, i]
  }
  x <- numeric(p)
  for (i in seq(p, 1L)) {
    acc <- if (i < p) .w3_csum(vapply((i + 1L):p, function(k) lo[k, i] * x[k],
                                      numeric(1))) else 0
    x[i] <- (z[i] - acc) / lo[i, i]
  }
  x
}

# (L L')^-1, column by column from the factor.
#' (L L\')^-1, column by column from the factor
#'
#' A step of the helpers_w3num implementation. Called by \code{.w3_ols}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param lo A matrix; passed to \code{nrow}.
#' @return The value of \code{out}, as built in the body.
#' @export
.w3_inv_from_chol <- function(lo) {
  p <- nrow(lo)
  cols <- lapply(seq_len(p), function(j) {
    e <- numeric(p); e[j] <- 1
    .w3_solve_chol(lo, e)
  })
  out <- matrix(0, p, p)
  for (i in seq_len(p)) for (j in seq_len(p)) out[i, j] <- cols[[j]][i]
  out
}

# Least squares by the normal equations.
#' Least squares by the normal equations
#'
#' A step of the helpers_w3num implementation. Called by \code{morie_blinkg_scan}, \code{morie_blinkg_select}, \code{morie_sdcfst}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param design A matrix; indexed by row and column.
#' @return A list with \code{beta}, \code{rss}, \code{df}, \code{sigma2}, \code{xtx_inv}, \code{fitted}, \code{chol}.
#' @export
.w3_ols <- function(y, design) {
  n <- length(y)
  p <- ncol(design)
  xtx <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p))
    xtx[a, b] <- .w3_csum(design[, a] * design[, b])
  xty <- vapply(seq_len(p), function(a) .w3_csum(design[, a] * y), numeric(1))
  lo <- .w3_chol(xtx)
  beta <- .w3_solve_chol(lo, xty)
  fitted <- vapply(seq_len(n), function(i) .w3_dot(design[i, ], beta), numeric(1))
  rss <- .w3_csum((y - fitted) * (y - fitted))
  df <- n - p
  list(beta = beta, rss = rss, df = df,
       sigma2 = if (df > 0L) rss / df else NaN,
       xtx_inv = .w3_inv_from_chol(lo), fitted = fitted, chol = lo)
}

.W3_LG <- c(76.18009172947146, -86.50532032941677, 24.01409824083091,
            -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5)

# Lanczos log-gamma. Not lgamma(): Python's is a different routine.
#' Lanczos log-gamma. Not lgamma(): Python\'s is a different routine
#'
#' A step of the helpers_w3num implementation. Called by \code{.bnppvl_log_beta}, \code{.w3_betainc}, \code{.w3_gammcf} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.w3_lgamma <- function(z) {
  x <- z
  tmp <- x + 5.5
  tmp <- tmp - (x + 0.5) * log(tmp)
  ser <- 1.000000000190015
  for (j in 1:6) {
    x <- x + 1
    ser <- ser + .W3_LG[j] / x
  }
  -tmp + log(2.5066282746310005 * ser / z)
}

# Q(a, x) by Lentz's continued fraction.
#' Q(a, x) by Lentz\'s continued fraction
#'
#' A step of the helpers_w3num implementation. Called by \code{.w3_gammp}, \code{.w3_gammq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param x Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.w3_gammcf <- function(a, x) {
  tiny <- 1e-300
  b <- x + 1 - a
  cc <- 1 / tiny
  d <- 1 / b
  h <- d
  for (i in 1:500) {
    an <- -i * (i - a)
    b <- b + 2
    d <- an * d + b
    if (abs(d) < tiny) d <- tiny
    cc <- b + an / cc
    if (abs(cc) < tiny) cc <- tiny
    d <- 1 / d
    de <- d * cc
    h <- h * de
    if (abs(de - 1) < 3e-16) break
  }
  exp(-x + a * log(x) - .w3_lgamma(a)) * h
}

# Regularised lower incomplete gamma P(a, x). Series below a+1, the
# continued fraction for the complement above: the crossover is where
# each is the convergent one, and using the wrong side is where a
# hand-rolled version loses its digits.
#' Regularised lower incomplete gamma P(a, x). Series below a+1, the
#'
#' continued fraction for the complement above: the crossover is where
#' each is the convergent one, and using the wrong side is where a
#' hand-rolled version loses its digits.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param x Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.w3_gammp <- function(a, x) {
  if (x < 0 || a <= 0) stop("gammp: need a > 0 and x >= 0")
  if (x == 0) return(0)
  if (x < a + 1) {
    ap <- a
    s <- 1 / a
    d <- s
    for (i in 1:500) {
      ap <- ap + 1
      d <- d * x / ap
      s <- s + d
      if (abs(d) < abs(s) * 3e-16) break
    }
    return(s * exp(-x + a * log(x) - .w3_lgamma(a)))
  }
  1 - .w3_gammcf(a, x)
}

# Regularised upper incomplete gamma Q(a, x). Computed without forming
# 1 - P when x is large, so the far tail keeps its significant digits
# instead of cancelling against 1.
#' Regularised upper incomplete gamma Q(a, x). Computed without forming
#'
#' 1 - P when x is large, so the far tail keeps its significant digits
#' instead of cancelling against 1.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param x Passed to \code{.w3_gammp}.
#' @return The value of \code{.w3_gammcf}.
#' @export
.w3_gammq <- function(a, x) {
  if (x < 0 || a <= 0) stop("gammq: need a > 0 and x >= 0")
  if (x == 0) return(1)
  if (x < a + 1) return(1 - .w3_gammp(a, x))
  .w3_gammcf(a, x)
}

# Standard normal CDF via the incomplete gamma.
# Phi(z) = 1/2 (1 + sign(z) P(1/2, z^2/2)) is an identity, not an
# approximation, so this is exactly as accurate as gammp -- and it runs
# the same series as the Python arm rather than R's own pnorm.
#' Standard normal CDF via the incomplete gamma
#'
#' Phi(z) = 1/2 (1 + sign(z) P(1/2, z^2/2)) is an identity, not an
#' approximation, so this is exactly as accurate as gammp -- and it runs
#' the same series as the Python arm rather than R\'s own pnorm.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.w3_ncdf <- function(z) {
  if (z == 0) return(0.5)
  p <- .w3_gammp(0.5, 0.5 * z * z)
  if (z > 0) 0.5 * (1 + p) else 0.5 * (1 - p)
}

#' .w3_npdf
#'
#' A step of the helpers_w3num implementation. Called by \code{morie_chemsc_smooth_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.w3_npdf <- function(z) exp(-0.5 * z * z) / sqrt(2 * pi)

# Standard normal quantile by bisection on the CDF: slower than a
# rational approximation and exactly as accurate as the CDF it inverts,
# which is the property that matters here.
#' Standard normal quantile by bisection on the CDF: slower than a
#'
#' rational approximation and exactly as accurate as the CDF it inverts,
#' which is the property that matters here.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @param lo Passed to \code{.w3_bisect}. Defaults to \code{-40}.
#' @param hi Passed to \code{.w3_bisect}. Defaults to \code{40}.
#' @return The value of \code{.w3_bisect}.
#' @export
.w3_nppf <- function(p, lo = -40, hi = 40) {
  if (!(p > 0 && p < 1)) stop("nppf: p must lie strictly inside (0, 1)")
  .w3_bisect(function(z) .w3_ncdf(z) - p, lo, hi)
}

#' .w3_betacf
#'
#' A step of the helpers_w3num implementation. Called by \code{.w3_betainc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param x Numeric; combined arithmetically in the body.
#' @return The value of \code{h}, as built in the body.
#' @export
.w3_betacf <- function(a, b, x) {
  tiny <- 1e-30
  qab <- a + b; qap <- a + 1; qam <- a - 1
  cc <- 1
  d <- 1 - qab * x / qap
  if (abs(d) < tiny) d <- tiny
  d <- 1 / d
  h <- d
  for (m in 1:300) {
    m2 <- 2 * m
    aa <- m * (b - m) * x / ((qam + m2) * (a + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    cc <- 1 + aa / cc
    if (abs(cc) < tiny) cc <- tiny
    d <- 1 / d
    # h * (d * cc), NOT (h * d) * cc: the Python arm writes `h *= d * c`
    # and the two associations differ in the last bit, which a long
    # continued fraction then carries into the answer.
    h <- h * (d * cc)
    aa <- -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    cc <- 1 + aa / cc
    if (abs(cc) < tiny) cc <- tiny
    d <- 1 / d
    de <- d * cc
    h <- h * de
    if (abs(de - 1) < 3e-16) break
  }
  h
}

# Regularised incomplete beta I_x(a, b).
#' Regularised incomplete beta I_x(a, b)
#'
#' A step of the helpers_w3num implementation. Called by \code{.w3_t_sf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param x Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.w3_betainc <- function(a, b, x) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  lb <- .w3_lgamma(a) + .w3_lgamma(b) - .w3_lgamma(a + b)
  front <- exp(a * log(x) + b * log(1 - x) - lb)
  if (x < (a + 1) / (a + b + 2)) return(front * .w3_betacf(a, b, x) / a)
  1 - exp(b * log(1 - x) + a * log(x) - lb) * .w3_betacf(b, a, 1 - x) / b
}

# Upper tail of Student's t.
#' Upper tail of Student\'s t
#'
#' A step of the helpers_w3num implementation. Called by \code{morie_blinkg_scan}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Numeric; combined arithmetically in the body.
#' @param df Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.w3_t_sf <- function(t, df) 0.5 * .w3_betainc(df / 2, 0.5, df / (df + t * t))

# Root of f on a bracketing interval, by plain bisection. A fixed
# iteration count rather than a tolerance loop: the two arms then take
# exactly the same number of steps and end at exactly the same point,
# which a tolerance test cannot guarantee.
#' Root of f on a bracketing interval, by plain bisection. A fixed
#'
#' iteration count rather than a tolerance loop: the two arms then take
#' exactly the same number of steps and end at exactly the same point,
#' which a tolerance test cannot guarantee.
#'
#' @param f See Usage.
#' @param lo Numeric; combined arithmetically in the body.
#' @param hi Numeric; combined arithmetically in the body.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.
#' @return A numeric value.
#' @export
.w3_bisect <- function(f, lo, hi, iters = 200L) {
  flo <- f(lo); fhi <- f(hi)
  if (flo == 0) return(lo)
  if (fhi == 0) return(hi)
  if ((flo > 0) == (fhi > 0))
    stop("bisect: the interval does not bracket a root")
  for (i in seq_len(iters)) {
    mid <- 0.5 * (lo + hi)
    fm <- f(mid)
    if (fm == 0) return(mid)
    if ((fm > 0) == (flo > 0)) { lo <- mid; flo <- fm } else hi <- mid
  }
  0.5 * (lo + hi)
}

# Composite Simpson on n panels (rounded up to an even number). Fixed
# panels, not adaptive: an adaptive rule branches on a tolerance test and
# the two arms would take different numbers of subdivisions on the very
# integrals where it matters.
#' Composite Simpson on n panels (rounded up to an even number). Fixed
#'
#' panels, not adaptive: an adaptive rule branches on a tolerance test
#' and the two arms would take different numbers of subdivisions on the
#' very integrals where it matters.
#'
#' @param f See Usage.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param n Numeric; combined arithmetically in the body. Defaults to \code{200L}.
#' @return A numeric value.
#' @export
.w3_simpson <- function(f, a, b, n = 200L) {
  n <- as.integer(n)
  if (n %% 2L == 1L) n <- n + 1L
  h <- (b - a) / n
  terms <- c(f(a), f(b))
  odd <- seq(1L, n - 1L, by = 2L)
  even <- if (n > 2L) seq(2L, n - 2L, by = 2L) else integer(0)
  terms <- c(terms, vapply(odd, function(k) 4 * f(a + h * k), numeric(1)))
  terms <- c(terms, vapply(even, function(k) 2 * f(a + h * k), numeric(1)))
  h / 3 * .w3_csum(terms)
}

# Nelder-Mead simplex minimisation. Derivative-free and deterministic
# given the starting point, which is what a likelihood with a
# step-function selection term needs -- the objective is not
# differentiable at the cutoffs, so a gradient method would be answering
# a question the model does not pose. The initial simplex is the standard
# one: x0 plus a step along each coordinate, scaled by the coordinate
# when it is non-zero so the simplex is not tiny in a large parameter and
# huge in a small one. A fixed iteration count keeps the arms in step.
#' Nelder-Mead simplex minimisation. Derivative-free and deterministic
#'
#' given the starting point, which is what a likelihood with a
#' step-function selection term needs -- the objective is not
#' differentiable at the cutoffs, so a gradient method would be
#' answering a question the model does not pose. The initial simplex is
#' the standard one: x0 plus a step along each coordinate, scaled by the
#' coordinate when it is non-zero so the simplex is not tiny in a large
#' parameter and huge in a small one. A fixed iteration count keeps the
#' arms in step.
#'
#' @param f See Usage.
#' @param x0 A vector; its length is taken.
#' @param step Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param iters Defaults to \code{400L}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @param rho Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param sigma Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @return A list with \code{x}, \code{value}.
#' @export
.w3_nelder_mead <- function(f, x0, step = 0.1, iters = 400L, alpha = 1,
                            gamma = 2, rho = 0.5, sigma = 0.5) {
  n <- length(x0)
  pts <- list(as.numeric(x0))
  for (i in seq_len(n)) {
    p <- as.numeric(x0)
    p[i] <- p[i] + if (p[i] != 0) step * p[i] else step
    pts[[i + 1L]] <- p
  }
  vals <- vapply(pts, f, numeric(1))
  for (it in seq_len(as.integer(iters))) {
    # order(): ties broken by index, matching Python's stable sort on
    # (value, index).
    o <- order(vals, seq_len(n + 1L))
    pts <- pts[o]; vals <- vals[o]
    cen <- vapply(seq_len(n), function(j)
      .w3_csum(vapply(seq_len(n), function(i) pts[[i]][j], numeric(1))) / n,
      numeric(1))
    xr <- cen + alpha * (cen - pts[[n + 1L]])
    fr <- f(xr)
    if (fr < vals[1]) {
      xe <- cen + gamma * (xr - cen)
      fe <- f(xe)
      if (fe < fr) { pts[[n + 1L]] <- xe; vals[n + 1L] <- fe }
      else { pts[[n + 1L]] <- xr; vals[n + 1L] <- fr }
      next
    }
    if (fr < vals[n]) { pts[[n + 1L]] <- xr; vals[n + 1L] <- fr; next }
    shrink <- TRUE
    if (fr < vals[n + 1L]) {
      xc <- cen + rho * (xr - cen)
      fc <- f(xc)
      if (fc <= fr) { pts[[n + 1L]] <- xc; vals[n + 1L] <- fc; shrink <- FALSE }
    } else {
      xc <- cen + rho * (pts[[n + 1L]] - cen)
      fc <- f(xc)
      if (fc < vals[n + 1L]) { pts[[n + 1L]] <- xc; vals[n + 1L] <- fc; shrink <- FALSE }
    }
    if (!shrink) next
    for (i in 2:(n + 1L)) {
      pts[[i]] <- pts[[1L]] + sigma * (pts[[i]] - pts[[1L]])
      vals[i] <- f(pts[[i]])
    }
  }
  b <- order(vals, seq_len(n + 1L))[1]
  list(x = pts[[b]], value = vals[b])
}
