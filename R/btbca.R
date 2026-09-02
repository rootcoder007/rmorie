# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bias-corrected and accelerated (BCa) bootstrap confidence interval
#'
#' Source read as rendered page images: Davison and Hinkley (1997), Bootstrap
#' Methods and their Application, Cambridge University Press.  Page 204,
#' equations (5.20)-(5.21), theta_alpha = t*_((R+1) alpha~) with
#' alpha~ = Phi(w + (w + z_alpha)/(1 - a(w + z_alpha))); page 205, equation
#' (5.22), the bias correction in simulation terms,
#' w = Phi^-1(#\{t*_r <= t\}/(R + 1)); and page 209, equation (5.27), the
#' nonparametric acceleration from the empirical influence values,
#' a = (1/6) sum l_j^3 / (sum l_j^2)^(3/2).
#'
#' The empirical influence values are the jackknife ones,
#' l_j = (n - 1)(t - t_(-j)); for the sample mean that reduces to
#' l_j = y_j - ybar exactly as the book states in Example 5.8.  That example
#' prints a = 0.0938 for the air-conditioning data
#' (3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487), and that printed number
#' is the anchor.
#'
#' Both conventions are returned: lo/hi use type-7 quantiles at alpha~,
#' matching the rest of this shelf, and lo_order/hi_order use the book's
#' ((R+1) alpha~)-th order statistic.  a = w = 0 collapses both to the plain
#' percentile interval.
#'
#' @param theta_hat the estimate on the original data.
#' @param theta_b the R bootstrap replicates.
#' @param x the original sample, used only for the jackknife.
#' @param stat the statistic, called on the leave-one-out samples.
#' @param alpha two-sided error rate.
#' @return list: lo, hi, lo_order, hi_order, alpha_lo, alpha_hi, estimate, z0,
#'   accel, B, n, method.
#' @keywords internal
#' @examples
#' Btbca(3, c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), mean)$accel
#' @export
Btbca <- function(theta_hat, theta_b, x, stat, alpha = 0.05) {
  v <- .s03vec(theta_b)
  R <- length(v)
  if (R == 0L) stop("boot_bca_ci: no bootstrap replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_bca_ci: alpha must lie strictly between 0 and 1")
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_bca_ci: need at least two observations for the jackknife")
  t <- as.numeric(theta_hat)
  cnt <- 0L
  for (u in v) if (u <= t) cnt <- cnt + 1L
  p <- cnt / (R + 1)
  if (p <= 0) p <- 0.5 / (R + 1)
  if (p >= 1) p <- 1 - 0.5 / (R + 1)
  w <- .s03qnorm(p)
  s2 <- 0; s3 <- 0
  for (j in seq_len(n)) {
    tj <- as.numeric(stat(xx[-j]))
    lj <- (n - 1) * (t - tj)
    s2 <- s2 + lj * lj
    s3 <- s3 + lj * lj * lj
  }
  acc <- if (s2 > 0) s3 / (6 * s2^1.5) else 0
  sv <- sort(v)
  res <- list()
  for (nm in c("lo", "hi")) {
    q <- if (nm == "lo") a / 2 else 1 - a / 2
    z <- .s03qnorm(q)
    den <- 1 - acc * (w + z)
    if (den == 0) stop("boot_bca_ci: the acceleration makes the BCa transform singular")
    at <- .s03pnorm(w + (w + z) / den)
    res[[paste0(nm, "_alpha")]] <- at
    res[[nm]] <- .s03quantile7(v, at)
    res[[paste0(nm, "_order")]] <- .btbca_order(sv, (R + 1) * at)
  }
  list(lo = res$lo, hi = res$hi, lo_order = res$lo_order, hi_order = res$hi_order,
       alpha_lo = res$lo_alpha, alpha_hi = res$hi_alpha, estimate = res$hi - res$lo,
       z0 = w, accel = acc, B = R, n = n,
       method = "Davison and Hinkley (1997) eqs. (5.21), (5.22), (5.27)")
}

#' @noRd
.btbca_order <- function(sv, r) {
  R <- length(sv)
  if (r <= 1) return(sv[1])
  if (r >= R) return(sv[R])
  lo <- floor(r)
  fr <- r - lo
  sv[lo] + fr * (sv[lo + 1L] - sv[lo])
}
