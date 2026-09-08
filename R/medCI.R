# SPDX-License-Identifier: AGPL-3.0-or-later
#' Confidence limits for a product, without pretending it is normal
#'
#' The product of two normal estimates is not normal: it is skewed and
#' sharply peaked, so the usual estimate-plus-or-minus-Sobel interval is
#' mis-centred and too short, and the deficit is worst exactly where
#' mediation studies live -- small \code{a} or small \code{b}. Taking the
#' quantiles of the product distribution itself fixes the shape.
#'
#' Formula: draw \code{a* ~ N(ahat, sa^2)} and \code{b* ~ N(bhat, sb^2)}
#' independently, form \code{a* b*}, and read off its \code{alpha/2} and
#' \code{1 - alpha/2} quantiles -- MacKinnon, Lockwood and Williams (2004)
#' Section 3. The draws are a two-dimensional Halton sequence (van der
#' Corput in bases 2 and 3, pushed through AS 241), so the interval is the
#' same number every time and in both language arms.
#'
#' @param a,b Path coefficients.
#' @param sa,sb Their standard errors, strictly positive.
#' @param n_sim Number of deterministic draws.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{ci_lo}, \code{ci_hi},
#'   \code{se_mc}, \code{sobel_se}, \code{sobel_lo}, \code{sobel_hi},
#'   \code{asymmetry}, \code{n_sim}.
#' @references MacKinnon, D. P., Lockwood, C. M. and Williams, J. (2004).
#'   Multivariate Behavioral Research 39(1):99-128.
#'   \doi{10.1207/s15327906mbr3901_4}.
#' @export
MedCI <- function(a, b, sa, sb, n_sim = 20000, level = 0.95) {
  av <- as.numeric(a)
  bv <- as.numeric(b)
  sav <- as.numeric(sa)
  sbv <- as.numeric(sb)
  if (sav <= 0 || sbv <= 0)
    stop("standard errors must be strictly positive")
  n <- as.integer(n_sim)
  if (n < 2L) stop("n_sim must be at least two")
  lv <- as.numeric(level)
  if (lv <= 0 || lv >= 1)
    stop("level must lie strictly between 0 and 1")
  za <- .s03normdraws(n, 2L)
  zb <- .s03normdraws(n, 3L)
  prod <- sort((av + sav * za) * (bv + sbv * zb))
  alo <- (1 - lv) / 2
  lo <- .s03quantile7(prod, alo)
  hi <- .s03quantile7(prod, 1 - alo)
  est <- av * bv
  sob <- sqrt(av^2 * sbv^2 + bv^2 * sav^2)
  z <- .s03qnorm(1 - alo)
  .t1_result(estimate = est, ci_lo = lo, ci_hi = hi,
             se_mc = .s03sd(prod, 1L), sobel_se = sob,
             sobel_lo = est - z * sob, sobel_hi = est + z * sob,
             asymmetry = (hi - est) - (est - lo), n_sim = n,
             method = "Distribution-of-the-product confidence limits")
}
