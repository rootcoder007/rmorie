# SPDX-License-Identifier: AGPL-3.0-or-later

# Inverse Laplace CDF at u in (0, 1), location zero.
#' Inverse Laplace CDF at u in (0, 1), location zero
#'
#' Part of the Dpvar implementation; see the file header for the source
#' it follows.
#'
#' @param u See Usage.
#' @param scale See Usage.
#' @return A numeric value.
#' @export
.dpvar_laplace <- function(u, scale) {
  d <- u - 0.5
  s <- if (d >= 0) 1 else -1
  t <- 1 - 2 * abs(d)
  if (t < 1e-300) t <- 1e-300
  -scale * s * log(t)
}

#' Differentially private variance of bounded data
#'
#' The Laplace mechanism releases \code{f(x) + Lap(GS_f / eps)} where
#' \code{GS_f} is the global sensitivity of \code{f}; the guarantee
#' requires the data to lie in a known range, so points are clamped to
#' \code{[a, b]} first.  The variance is released by composing two
#' Laplace queries on the clamped data, each spending half the budget:
#' the mean, of sensitivity \code{(b - a)/n}, and the second raw moment,
#' of sensitivity \code{(sup t^2 - inf t^2)/n} over \code{t in [a, b]}.
#' The result is clipped into \code{[0, ((b - a)/2)^2]}, Popoviciu's
#' bound, which is post-processing and costs no privacy.  The noise is
#' drawn deterministically by inverting the Laplace CDF at van der
#' Corput points in bases 2 and 3 so both language arms agree; a real
#' deployment must use a cryptographic source.
#'
#' @param x Raw data.
#' @param a,b Clamping bounds, \code{a < b}.
#' @param epsilon Total privacy budget, split evenly between the two
#'   queries.
#' @param seed Index into the deterministic low-discrepancy stream.
#' @return List with \code{estimate}, \code{var_dp}, \code{mean_dp},
#'   \code{m2_dp}, \code{var_true}, \code{mean_true}, \code{sens_mean},
#'   \code{sens_m2}, \code{scale_mean}, \code{scale_m2},
#'   \code{n_clamped}, \code{epsilon}, \code{n}.
#' @references Karwa, V. and Vadhan, S. (2018). Finite sample
#'   differentially private confidence intervals. ITCS 2018;
#'   arXiv:1711.03908, Sections 1.5 and 2.  Dwork, C., McSherry, F.,
#'   Nissim, K. and Smith, A. (2006). TCC, LNCS 3876, 265-284.
#' @export
Dpvar <- function(x, a, b, epsilon, seed = 42L) {
  xv <- .s03vec(x); n <- length(xv)
  if (n == 0L) stop("Dpvar: empty input, x has no observations")
  a <- as.numeric(a); b <- as.numeric(b)
  if (!(a < b)) stop("Dpvar: bounds must satisfy a < b")
  eps <- as.numeric(epsilon)
  if (!(eps > 0)) stop("Dpvar: epsilon must be strictly positive")
  si <- as.integer(seed)
  if (si < 1L) stop("Dpvar: seed must be at least 1")
  cl <- pmin(pmax(xv, a), b)
  nclamp <- sum(cl != xv)
  mean_true <- .s03mean(cl)
  m2_true <- .s03mean(cl * cl)
  hi2 <- max(a * a, b * b)
  lo2 <- if (a <= 0 && 0 <= b) 0 else min(a * a, b * b)
  sens_mean <- (b - a) / n
  sens_m2 <- (hi2 - lo2) / n
  half <- eps / 2
  scale_mean <- sens_mean / half
  scale_m2 <- sens_m2 / half
  mean_dp <- mean_true + .dpvar_laplace(.s03vdc(si, 2L), scale_mean)
  m2_dp <- m2_true + .dpvar_laplace(.s03vdc(si, 3L), scale_m2)
  var_dp <- m2_dp - mean_dp * mean_dp
  cap <- ((b - a) / 2)^2
  if (var_dp < 0) var_dp <- 0 else if (var_dp > cap) var_dp <- cap
  .t1_result(estimate = var_dp, var_dp = var_dp, mean_dp = mean_dp,
             m2_dp = m2_dp, var_true = m2_true - mean_true * mean_true,
             mean_true = mean_true, sens_mean = sens_mean,
             sens_m2 = sens_m2, scale_mean = scale_mean,
             scale_m2 = scale_m2, n_clamped = nclamp, epsilon = eps,
             n = n, method = "DP variance (bounded)")
}
