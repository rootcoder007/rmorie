# SPDX-License-Identifier: AGPL-3.0-or-later
#' Huber Proposal 2: simultaneous location and scale
#'
#' Each sweep Winsorises at mu +/- k s, takes the mean of the Winsorised values
#' as mu and sqrt(sum (y - mu)^2 / ((n-1) beta)) as s, with
#' beta = th + k^2 (1 - th) - 2 k dnorm(k), th = 2 pnorm(k) - 1.  Source
#' consulted: Huber (1981), Robust Statistics, section 6.7.  MASS::hubers stops
#' at tol = 1e-6 within 30 sweeps and returns the PRE-update iterate; the
#' default here reproduces that exactly, and the true fixed point (about 1e-3
#' away on the example) is returned as mu_refined/scale_refined.
#'
#' @param x sample.
#' @param k Huber tuning constant.
#' @param tol relative convergence tolerance.
#' @param maxit sweep cap.
#' @return list: estimate, scale, mu_refined, scale_refined, se, beta,
#'   iterations, k, n, method.
#' @keywords internal
#' @examples
#' locS(c(2.1, 3.4, 1.9, 5.6, 2.8, 3.1, 9.9, 2.5, 3.3, 2.7))$estimate
#' @export
locS <- function(x, k = 1.5, tol = 1e-6, maxit = 30L) {
  v <- as.numeric(x); n <- length(v); kk <- as.numeric(k)
  th <- 2 * stats::pnorm(kk) - 1
  beta <- th + kk * kk * (1 - th) - 2 * kk * stats::dnorm(kk)
  mu0 <- stats::median(v)
  s0 <- 1.4826 * stats::median(abs(v - mu0))
  if (s0 == 0) {
    return(list(estimate = mu0, scale = 0, mu_refined = mu0, scale_refined = 0,
                se = 0, beta = beta, iterations = 0L, k = kk, n = n,
                method = "Huber Proposal 2 simultaneous location and scale (Huber 1981, sec. 6.7)"))
  }
  sweep1 <- function(tolv, mx) {
    a <- mu0; b <- s0; it <- 0L
    for (i in seq_len(as.integer(mx))) {
      it <- i
      yy <- pmin(pmax(a - kk * b, v), a + kk * b)
      mu1 <- sum(yy) / n
      s1 <- sqrt((sum((yy - mu1)^2) / (n - 1)) / beta)
      if (abs(a - mu1) < tolv * b && abs(b - s1) < tolv * b) break
      a <- mu1; b <- s1
    }
    list(mu = a, s = b, it = it)
  }
  f <- sweep1(as.numeric(tol), maxit)
  g <- sweep1(1e-13, 500L)
  list(estimate = f$mu, scale = f$s, mu_refined = g$mu, scale_refined = g$s,
       se = f$s / sqrt(n), beta = beta, iterations = as.integer(f$it), k = kk,
       n = n,
       method = "Huber Proposal 2 simultaneous location and scale (Huber 1981, sec. 6.7)")
}

# CANONICAL TEST
# r <- locS(c(2.1,3.4,1.9,5.6,2.8,3.1,9.9,2.5,3.3,2.7))
# stopifnot(abs(r$estimate - 3.18126476231742) < 1e-12)

#' @rdname locS
#' @keywords internal
#' @export
morie_locS <- locS
