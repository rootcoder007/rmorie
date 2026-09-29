# SPDX-License-Identifier: AGPL-3.0-or-later
# Analytic and symbolic-numeric tools.
# Identical to the Python arm morie.fn.analytic.

#' Analytic tools: Laplace transform and inversion, Laurent coefficients, Walsh-Hadamard, polynomials, heat equation
#'
#' \code{LaplaceTransformNum}: \code{int_0^inf f(t) exp(-s t) dt} by the
#' exp-sinh double-exponential rule. \code{TalbotInverse}: fixed Talbot
#' inversion (Abate and Valko 2004), \code{F} must accept complex arguments.
#' \code{StehfestInverse}: Gaver-Stehfest inversion with weights \code{V_k}.
#' \code{LaurentCoefficients}: \code{a_k} by the trapezoidal Cauchy integral on
#' the circle \code{|z - c| = radius}. \code{HadamardTransform} and
#' \code{HadamardInverse}: orthonormal fast Walsh-Hadamard transform (its own
#' inverse). \code{PolyExpand}: product of polynomial powers (ascending
#' coefficients). \code{PolyFactorModP}: Cantor-Zassenhaus factorisation over
#' GF(p) for an odd prime \code{p < 2^26} (square-free, distinct-degree and
#' equal-degree steps, Philox random splitting). \code{RationalCancel}: lowest
#' terms of an integer rational function by the primitive pseudo-remainder gcd.
#' \code{HeatEquationSeries}: separation of variables for \code{u_t = alpha u_xx}
#' with zero boundaries and Simpson Fourier-sine coefficients.
#' \code{QuadraticRoots}: cancellation-free roots. \code{FunctionalNorm}:
#' trapezoidal L2 norm and scaled function.
#'
#' @param f A function (of real, or complex for \code{LaurentCoefficients}, argument).
#' @param s Laplace variable(s), real and positive.
#' @param h Double-exponential step.
#' @param F Laplace-domain function.
#' @param t Time point(s), or the grid for \code{FunctionalNorm}.
#' @param M Number of Talbot nodes.
#' @param N Number of Stehfest terms (even).
#' @param c Expansion centre (complex).
#' @param orders Integer orders of the coefficients.
#' @param radius Contour radius.
#' @param n Number of contour points.
#' @param x Numeric vector (length a power of two), or evaluation points.
#' @param y Transformed vector.
#' @param factors List of coefficient vectors.
#' @param powers Integer powers, or NULL for ones.
#' @param coeffs Ascending integer coefficients.
#' @param p Odd prime.
#' @param seed Philox seed for the random splitting.
#' @param num,den Integer numerator and denominator coefficients.
#' @param length Rod length.
#' @param alpha Diffusivity.
#' @param n_terms Number of modes.
#' @param n_quad Simpson panels.
#' @param a,b Quadratic coefficients (with \code{c}).
#' @return A vector or list.
#' @references Takahasi, H. and Mori, M. (1974). Publ. RIMS 9, 721-741. Abate,
#'   J. and Valko, P. P. (2004). Int. J. Numer. Meth. Engng 60, 979-993.
#'   Stehfest, H. (1970). Comm. ACM 13, 47-49. Trefethen, L. N. and Weideman,
#'   J. A. C. (2014). SIAM Review 56, 385-458. Pratt, W. K. et al. (1969). Proc.
#'   IEEE 57, 58-68. Cantor, D. G. and Zassenhaus, H. (1981). Math. Comp. 36,
#'   587-592. Cohen, H. (1996). A Course in Computational Algebraic Number
#'   Theory. Collins, G. E. (1967). J. ACM 14, 128-142. Haberman, R. (2013).
#'   Applied Partial Differential Equations. Press, W. H. et al. (2007).
#'   Numerical Recipes, 3rd ed. Ramsay, J. O. and Silverman, B. W. (2005).
#'   Functional Data Analysis.
#' @examples
#' TalbotInverse(function(s) 1 / (s + 1), 1)
#' PolyFactorModP(c(-1, 0, 0, 1), 7)$factors
#' QuadraticRoots(1, -3, 2)$real
#' @export
LaplaceTransformNum <- function(f, s, h = 1 / 128) {
  vapply(as.numeric(s), function(sv) {
    .an_exp_sinh(function(t) if (sv * t < 745) f(t) * exp(-sv * t) else 0, h)
  }, 0)
}

.an_exp_sinh <- function(g, h, tmax = 4) {
  n <- round(tmax / h)
  s <- 0
  for (k in -n:n) {
    t <- k * h
    u <- exp(pi / 2 * sinh(t))
    if (u == 0 || is.infinite(u)) next
    v <- g(u)
    if (v != 0) s <- s + v * u * pi / 2 * cosh(t)
  }
  s * h
}

#' @rdname LaplaceTransformNum
#' @export
TalbotInverse <- function(F, t, M = 24) {
  vapply(as.numeric(t), function(tv) {
    r <- 2 * M / (5 * tv)
    acc <- 0.5 * Re(F(complex(real = r, imaginary = 0)) * exp(r * tv))
    for (k in seq_len(M - 1)) {
      th <- k * pi / M
      ct <- cos(th) / sin(th)
      d <- complex(real = r * th * ct, imaginary = r * th)
      g <- complex(real = 1, imaginary = th * (1 + ct * ct) - ct)
      acc <- acc + Re(exp(tv * d) * F(d) * g)
    }
    r / M * acc
  }, 0)
}

#' @rdname LaplaceTransformNum
#' @export
StehfestInverse <- function(F, t, N = 16) {
  hh <- N %/% 2
  V <- vapply(seq_len(N), function(k) {
    s <- 0
    for (j in ((k + 1) %/% 2):min(k, hh)) {
      s <- s + j^hh * factorial(2 * j) /
        (factorial(hh - j) * factorial(j) * factorial(j - 1) * factorial(k - j) * factorial(2 * j - k))
    }
    (-1)^(k + hh) * s
  }, 0)
  vapply(as.numeric(t), function(tv) {
    s <- 0
    for (k in seq_len(N)) s <- s + V[k] * F(k * log(2) / tv)
    log(2) / tv * s
  }, 0)
}

#' @rdname LaplaceTransformNum
#' @export
LaurentCoefficients <- function(f, c, orders, radius = 1, n = 256) {
  pts <- radius * exp(2i * pi * (0:(n - 1)) / n)
  vals <- vapply(pts, function(z) as.complex(f(c + z)), 0i)
  a <- vapply(orders, function(k) {
    s <- 0i
    for (j in seq_len(n)) s <- s + vals[j] * pts[j]^(-k)
    s / n
  }, 0i)
  list(orders = orders, real = Re(a), imag = Im(a))
}

#' @rdname LaplaceTransformNum
#' @export
HadamardTransform <- function(x) {
  y <- as.numeric(x)
  d <- length(y)
  if (bitwAnd(d, d - 1) != 0) stop("length must be a power of two")
  h <- 1
  while (h < d) {
    for (i in seq(0, d - 1, by = 2 * h)) for (j in i:(i + h - 1)) {
      a <- y[j + 1]
      b <- y[j + h + 1]
      y[j + 1] <- a + b
      y[j + h + 1] <- a - b
    }
    h <- h * 2
  }
  y / sqrt(d)
}

#' @rdname LaplaceTransformNum
#' @export
HadamardInverse <- function(y) HadamardTransform(y)

.an_pmul <- function(a, b) {
  if (!length(a) || !length(b)) return(numeric(0))
  out <- numeric(length(a) + length(b) - 1)
  for (i in seq_along(a)) for (j in seq_along(b)) out[i + j - 1] <- out[i + j - 1] + a[i] * b[j]
  out
}

#' @rdname LaplaceTransformNum
#' @export
PolyExpand <- function(factors, powers = NULL) {
  out <- 1
  for (i in seq_along(factors)) {
    e <- if (is.null(powers)) 1 else powers[i]
    for (r in seq_len(e)) out <- .an_pmul(out, as.numeric(factors[[i]]))
  }
  out
}

.an_trim <- function(a) {
  n <- length(a)
  while (n > 0 && a[n] == 0) n <- n - 1
  a[seq_len(n)]
}

.an_mulmod <- function(a, b, p) (a * b) %% p

.an_inv <- function(a, p) {
  out <- 1
  base <- a %% p
  e <- p - 2
  while (e > 0) {
    if (e %% 2 == 1) out <- .an_mulmod(out, base, p)
    base <- .an_mulmod(base, base, p)
    e <- e %/% 2
  }
  out
}

.an_gsub <- function(a, b, p) {
  n <- max(length(a), length(b))
  a <- c(a, numeric(n - length(a)))
  b <- c(b, numeric(n - length(b)))
  .an_trim((a - b) %% p)
}

.an_gmul <- function(a, b, p) {
  if (!length(a) || !length(b)) return(numeric(0))
  out <- numeric(length(a) + length(b) - 1)
  for (i in seq_along(a)) for (j in seq_along(b)) out[i + j - 1] <- (out[i + j - 1] + .an_mulmod(a[i], b[j], p)) %% p
  .an_trim(out)
}

.an_gdivmod <- function(a, b, p) {
  q <- numeric(max(length(a) - length(b) + 1, 1))
  il <- .an_inv(b[length(b)], p)
  while (length(a) >= length(b) && length(a)) {
    k <- length(a) - length(b)
    cc <- .an_mulmod(a[length(a)], il, p)
    q[k + 1] <- cc
    idx <- seq_along(b) + k
    a[idx] <- (a[idx] - .an_mulmod(cc, b, p)) %% p
    a <- .an_trim(a)
  }
  list(q = .an_trim(q), r = a)
}

.an_monic <- function(a, p) .an_mulmod(a, .an_inv(a[length(a)], p), p)

.an_ggcd <- function(a, b, p) {
  while (length(b)) {
    r <- .an_gdivmod(a, b, p)$r
    a <- b
    b <- r
  }
  if (length(a)) .an_monic(a, p) else a
}

.an_gpowmod <- function(a, e, f, p) {
  out <- 1
  base <- .an_gdivmod(a, f, p)$r
  while (e > 0) {
    if (e %% 2 == 1) out <- .an_gdivmod(.an_gmul(out, base, p), f, p)$r
    base <- .an_gdivmod(.an_gmul(base, base, p), f, p)$r
    e <- e %/% 2
  }
  out
}

.an_sff <- function(f, p) {
  if (length(f) <= 1) return(list())
  out <- list()
  d <- if (length(f) > 1) .an_trim((seq_len(length(f) - 1) * f[-1]) %% p) else numeric(0)
  if (length(d)) {
    cc <- .an_ggcd(f, d, p)
    w <- .an_gdivmod(f, cc, p)$q
    i <- 1
    while (length(w) > 1) {
      y <- .an_ggcd(w, cc, p)
      fac <- .an_gdivmod(w, y, p)$q
      if (length(fac) > 1) out <- c(out, list(list(f = fac, m = i)))
      i <- i + 1
      w <- y
      cc <- .an_gdivmod(cc, y, p)$q
    }
    if (length(cc) > 1) {
      for (g in .an_sff(cc[seq(1, length(cc), by = p)], p)) out <- c(out, list(list(f = g$f, m = g$m * p)))
    }
  } else {
    for (g in .an_sff(f[seq(1, length(f), by = p)], p)) out <- c(out, list(list(f = g$f, m = g$m * p)))
  }
  out
}

.an_ddf <- function(f, p) {
  out <- list()
  i <- 1
  fs <- f
  h <- c(0, 1)
  while (length(fs) - 1 >= 2 * i) {
    h <- .an_gpowmod(h, p, fs, p)
    g <- .an_ggcd(fs, .an_gsub(h, c(0, 1), p), p)
    if (length(g) > 1) {
      out <- c(out, list(list(f = g, d = i)))
      fs <- .an_gdivmod(fs, g, p)$q
      h <- .an_gdivmod(h, fs, p)$r
    }
    i <- i + 1
  }
  if (length(fs) > 1) out <- c(out, list(list(f = fs, d = length(fs) - 1)))
  out
}

.an_edf <- function(f, d, p, seed, env) {
  n <- length(f) - 1
  parts <- list(f)
  while (length(parts) < n %/% d) {
    u <- .morie_random_uniform(n, seed = seed, stream = env$counter)
    env$counter <- env$counter + 1
    h <- .an_trim(floor(u * p))
    if (length(h) < 2) next
    g <- 1
    hp <- h
    for (j in seq_len(d) - 1) {
      if (j > 0) hp <- .an_gpowmod(hp, p, f, p)
      g <- .an_gdivmod(.an_gmul(g, hp, p), f, p)$r
    }
    g <- .an_gsub(.an_gpowmod(g, (p - 1) %/% 2, f, p), 1, p)
    nw <- list()
    for (q in parts) {
      if (length(q) - 1 > d) {
        r <- if (length(g)) .an_ggcd(q, .an_gdivmod(g, q, p)$r, p) else q
        if (length(r) > 1 && length(r) < length(q)) {
          nw <- c(nw, list(r, .an_gdivmod(q, r, p)$q))
          next
        }
      }
      nw <- c(nw, list(q))
    }
    parts <- nw
  }
  parts
}

.an_less <- function(a, b) {
  if (length(a$f) != length(b$f)) return(length(a$f) < length(b$f))
  ra <- rev(a$f)
  rb <- rev(b$f)
  for (i in seq_along(ra)) if (ra[i] != rb[i]) return(ra[i] < rb[i])
  a$m < b$m
}

#' @rdname LaplaceTransformNum
#' @export
PolyFactorModP <- function(coeffs, p, seed = 0) {
  f <- .an_trim(as.numeric(coeffs) %% p)
  unit <- f[length(f)]
  f <- .an_monic(f, p)
  env <- new.env()
  env$counter <- 0
  res <- list()
  for (g in .an_sff(f, p)) for (q in .an_ddf(g$f, p)) {
    pieces <- if (length(q$f) - 1 > q$d) .an_edf(q$f, q$d, p, seed, env) else list(q$f)
    for (r in pieces) res <- c(res, list(list(f = r, m = g$m)))
  }
  if (length(res) > 1) for (i in 2:length(res)) {
    j <- i
    while (j > 1 && .an_less(res[[j]], res[[j - 1]])) {
      tmp <- res[[j]]
      res[[j]] <- res[[j - 1]]
      res[[j - 1]] <- tmp
      j <- j - 1
    }
  }
  list(unit = unit, factors = lapply(res, `[[`, "f"), multiplicities = vapply(res, `[[`, 0, "m"))
}

.an_igcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b != 0) {
    r <- a %% b
    a <- b
    b <- r
  }
  a
}

.an_content <- function(a) {
  g <- 0
  for (v in a) g <- .an_igcd(g, v)
  g
}

.an_prem <- function(a, b) {
  lb <- b[length(b)]
  while (length(a) >= length(b) && length(a)) {
    k <- length(a) - length(b)
    la <- a[length(a)]
    a <- a * lb
    idx <- seq_along(b) + k
    a[idx] <- a[idx] - la * b
    a <- .an_trim(a)
  }
  a
}

.an_zdiv <- function(a, b) {
  q <- numeric(length(a) - length(b) + 1)
  while (length(a) >= length(b) && length(a)) {
    k <- length(a) - length(b)
    cc <- a[length(a)] / b[length(b)]
    q[k + 1] <- cc
    idx <- seq_along(b) + k
    a[idx] <- a[idx] - cc * b
    a <- .an_trim(a)
  }
  q
}

#' @rdname LaplaceTransformNum
#' @export
RationalCancel <- function(num, den) {
  a <- .an_trim(as.numeric(num))
  b <- .an_trim(as.numeric(den))
  v <- c(a, b)
  if (any(v != floor(v)) || any(abs(v) >= 2^53)) stop("coefficients must be integers below 2^53")
  if (length(a) >= length(b)) {
    x <- a
    y <- b
  } else {
    x <- b
    y <- a
  }
  x <- x / .an_content(x)
  y <- y / .an_content(y)
  while (length(y)) {
    r <- .an_prem(x, y)
    x <- y
    y <- if (length(r)) r / .an_content(r) else numeric(0)
  }
  g <- x / .an_content(x)
  if (g[length(g)] < 0) g <- -g
  n2 <- .an_zdiv(a, g)
  d2 <- .an_zdiv(b, g)
  cc <- .an_igcd(.an_content(n2), .an_content(d2))
  n2 <- n2 / cc
  d2 <- d2 / cc
  if (d2[length(d2)] < 0) {
    n2 <- -n2
    d2 <- -d2
  }
  list(numerator = n2, denominator = d2, gcd = g)
}

#' @rdname LaplaceTransformNum
#' @export
HeatEquationSeries <- function(f, length, alpha, x, t, n_terms = 50, n_quad = 1000) {
  L <- length
  m <- n_quad + n_quad %% 2
  hq <- L / m
  xs <- (0:m) * hq
  fv <- vapply(xs, f, 0)
  w <- ifelse(0:m %in% c(0, m), 1, ifelse((0:m) %% 2 == 1, 4, 2))
  b <- vapply(seq_len(n_terms), function(n) 2 / L * sum(w * fv * sin(n * pi * xs / L)) * hq / 3, 0)
  u <- matrix(0, base::length(x), base::length(t))
  for (i in seq_along(x)) for (j in seq_along(t)) {
    nn <- seq_len(n_terms)
    u[i, j] <- sum(b * sin(nn * pi * x[i] / L) * exp(-alpha * (nn * pi / L)^2 * t[j]))
  }
  list(u = u, b = b)
}

#' @rdname LaplaceTransformNum
#' @export
QuadraticRoots <- function(a, b, c) {
  disc <- b * b - 4 * a * c
  if (disc >= 0) {
    sq <- sqrt(disc)
    q <- -0.5 * (b + if (b >= 0) sq else -sq)
    return(list(real = c(q / a, if (q != 0) c / q else 0), imag = c(0, 0), discriminant = disc))
  }
  im <- sqrt(-disc) / (2 * a)
  list(real = rep(-b / (2 * a), 2), imag = c(im, -im), discriminant = disc)
}

#' @rdname LaplaceTransformNum
#' @export
FunctionalNorm <- function(t, f) {
  tt <- as.numeric(t)
  ff <- as.numeric(f)
  n <- base::length(tt)
  nrm <- sqrt(sum(diff(tt) * (ff[-n]^2 + ff[-1]^2) / 2))
  list(norm = nrm, scaled = ff / nrm)
}
