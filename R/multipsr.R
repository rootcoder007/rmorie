# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-stage sample: PSUs drawn, then elements within each PSU.
#'
#' The within term carries the factor m/M, so it vanishes when every PSU
#' is taken -- the check that the bookkeeping is right.
#'
#' Formula: Yhat = sum_l N_l ybar_l / sum_l N_l;
#'   V = (M/N)^2 \[ ((M-m)/M)/(m(m-1)) * sum (N_l ybar_l - N_l Yhat)^2
#'     + (1/(mM)) * sum N_l^2 ((N_l-n_l)/N_l) s_l^2 / n_l ]
#'
#' @param Y List of observation vectors, one per sampled PSU.
#' @param Nl Population size of each sampled PSU.
#' @param M Number of PSUs in the population.
#' @param N Number of elements in the population.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{psu_mean}, \code{psu_var},
#'   \code{between_term}, \code{within_term}, \code{m}, \code{M},
#'   \code{N}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   10 (subsampling / two-stage sampling). Chapter 10 was NOT in the
#'   scanned excerpt available to this batch, so the estimator and
#'   variance are taken from the reference implementation in the CRAN
#'   package samplingbook 1.2.4, function submean with method = "ratio".
#' @export
Twostage <- function(Y, Nl, M, N, level = 0.95) {
  m <- length(Y)
  if (m < 2L) stop("at least two PSUs are needed for a variance")
  Nl <- .t1_vec(Nl)
  if (length(Nl) != m) stop("Nl must have one entry per sampled PSU")
  M <- as.numeric(M); N <- as.numeric(N)
  if (M < m) stop("M must be at least the number of sampled PSUs")
  yb <- s2 <- nl <- numeric(m)
  for (i in seq_len(m)) {
    yi <- .t1_vec(Y[[i]]); k <- length(yi)
    if (k < 2L) stop("every sampled PSU needs at least two elements")
    if (k > Nl[i]) stop("a PSU sample cannot exceed its population")
    nl[i] <- k; yb[i] <- mean(yi); s2[i] <- stats::var(yi)
  }
  est <- sum(Nl * yb) / sum(Nl)
  f1 <- (M - m) / M
  VB <- sum((Nl * yb - Nl * est)^2)
  between <- f1 / (m * (m - 1)) * VB
  within <- sum(Nl^2 * ((Nl - nl) / Nl) * s2 / nl) / (m * M)
  var <- (M / N)^2 * (between + within)
  se <- sqrt(var)
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = est, se = se, ci_lower = est - z * se,
             ci_upper = est + z * se, psu_mean = yb, psu_var = s2,
             between_term = (M / N)^2 * between,
             within_term = (M / N)^2 * within, m = m, M = M, N = N,
             method = "Two-stage sampling, ratio form")
}
