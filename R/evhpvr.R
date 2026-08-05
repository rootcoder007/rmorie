# SPDX-License-Identifier: AGPL-3.0-or-later

# Objective at b with a, mu and sigma all profiled out.  For fixed b the
# residual variance is a quadratic in a, so the minimising a is the
# ordinary-least-squares slope of u = y/x^b on v = x^(1-b) about their
# means.  Profiling a out ANALYTICALLY is what makes the two language
# arms agree: the likelihood is flat along the (a, b) ridge, and a
# numerical search there decides its steps on the last bits, which
# walked the arms 1e-8 apart.  The sums are accumulated in loops for the
# same reason -- R sums in long double, Python in double.
.ht_prof <- function(xv, yv, b) {
  n <- length(xv)
  p <- xv^b
  u <- yv / p
  v <- xv / p
  mu_u <- 0; mu_v <- 0
  for (i in seq_len(n)) { mu_u <- mu_u + u[i]; mu_v <- mu_v + v[i] }
  mu_u <- mu_u / n; mu_v <- mu_v / n
  svv <- 0; suv <- 0
  for (i in seq_len(n)) {
    dv <- v[i] - mu_v
    svv <- svv + dv * dv
    suv <- suv + dv * (u[i] - mu_u)
  }
  a <- if (svv > 0) suv / svv else 0
  s2 <- 0
  for (i in seq_len(n)) {
    r <- u[i] - a * v[i]
    s2 <- s2 + r * r
  }
  m <- mu_u - a * mu_v
  s2 <- s2 / n - m * m
  if (s2 <= 0) return(list(f = Inf, a = a, mu = m, sd = 0))
  slx <- 0
  for (i in seq_len(n)) slx <- slx + log(xv[i])
  list(f = 0.5 * n * log(s2) + b * slx, a = a, mu = m, sd = sqrt(s2))
}

#' Heffernan-Tawn conditional extremes model
#'
#' Formula: Y_j | X = x  =  a_j x + x^(b_j) Z_j   for x > u
#'
#' Fitted by Gaussian likelihood on the exceedance set.  The slope a and
#' the residual mean and sd are profiled out in closed form at every b,
#' leaving a single bounded search for b: its stationary point is
#' bracketed by a sign change of the profile derivative on a fixed grid
#' and then bisected.
#'
#' @param X An n x 2 matrix; column 1 conditions, on a standard Laplace
#'   or exponential-type scale.
#' @param u Conditioning threshold applied to column 1.
#' @return List with \code{a}, \code{b}, \code{mu_z}, \code{sigma_z},
#'   \code{estimate}, \code{nll}, \code{n_exceed}, \code{n},
#'   \code{method}.
#' @references Heffernan & Tawn (2004), JRSS B 66(3):497-546.
#' @export
Evhpvr <- function(X, u) {
  M <- .s03mat(X)
  n <- nrow(M)
  if (n == 0L) stop("empty input: X has no rows")
  if (ncol(M) != 2L) stop("X must have exactly two columns")
  u <- as.numeric(u)
  keep <- M[, 1] > u
  xv <- M[keep, 1]; yv <- M[keep, 2]
  k <- length(xv)
  if (k < 3L) stop("fewer than three exceedances of u; nothing to fit")
  if (any(xv <= 0)) stop("the conditioning variable must be positive above u")
  b_lo <- 0; b_hi <- 0.999
  dg <- function(b) {
    h <- 1e-4
    lo <- if (b - h > b_lo) b - h else b_lo
    hi <- if (b + h < b_hi) b + h else b_hi
    (.ht_prof(xv, yv, hi)$f - .ht_prof(xv, yv, lo)$f) / (hi - lo)
  }
  grid <- 200L
  prev_b <- b_lo
  prev <- dg(prev_b)
  b <- NA_real_
  for (i in seq_len(grid)) {
    cb <- b_lo + (b_hi - b_lo) * i / grid
    cur <- dg(cb)
    if ((prev <= 0 && 0 <= cur) || (cur <= 0 && 0 <= prev)) {
      lo <- prev_b; hi <- cb; flo <- prev
      for (it in seq_len(100)) {
        mid <- 0.5 * (lo + hi)
        fm <- dg(mid)
        if ((flo <= 0) == (fm <= 0)) { lo <- mid; flo <- fm } else hi <- mid
      }
      b <- 0.5 * (lo + hi)
      break
    }
    prev_b <- cb; prev <- cur
  }
  if (is.na(b)) {
    bf <- Inf; b <- b_lo
    for (i in 0:grid) {
      cb <- b_lo + (b_hi - b_lo) * i / grid
      f <- .ht_prof(xv, yv, cb)$f
      if (f < bf) { bf <- f; b <- cb }
    }
  }
  r <- .ht_prof(xv, yv, b)
  .t1_result(a = r$a, b = b, mu_z = r$mu, sigma_z = r$sd, estimate = r$a,
             nll = r$f, n_exceed = k, n = n,
             method = "Heffernan-Tawn conditional extremes model")
}
