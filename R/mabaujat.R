# SPDX-License-Identifier: AGPL-3.0-or-later
#' Baujat plot coordinates for a meta-analysis
#'
#' Formula: x_i = w_i (y_i - theta_FE)^2; y_i = (theta_FE - theta_FE(-i))^2 / var(theta_FE(-i))
#'
#' @param yi Effect estimates.
#' @param vi Their sampling variances.

#' @param yi See Usage.
#' @param vi See Usage.
#' @return List with ``x`` (contribution to Q), ``y`` (influence on the pooled estimate), ``theta_fe``, ``theta_loo``, ``k``.
#' @references Baujat, Mahe, Pignon and Hill (2002), A graphical method for exploring heterogeneity in meta-analyses, Statistics in Medicine 21:2641-2652. Paywalled; the two axes are as documented by metafor::baujat, the reference implementation -- the x-axis is each study's contribution to the Q statistic and the y-axis the standardised squared difference between the overall estimate with and without that study.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Baujat(V, V)
Baujat <- function(yi, vi) {
  y <- .t1_vec(yi)
  v <- .t1_vec(vi)
  k <- length(y)
  if (any(v <= 0)) stop("variances must be positive")
  w <- 1 / v
  sw <- sum(w)
  th <- sum(w * y) / sw
  xs <- w * (y - th)^2
  sw_i <- sw - w
  th_i <- (sum(w * y) - w * y) / sw_i
  ys <- ifelse(sw_i > 0, (th - th_i)^2 * sw_i, NA_real_)
  .t1_result(x = xs, y = ys, theta_fe = th,
             theta_loo = ifelse(sw_i > 0, th_i, NA_real_), k = k,
             method = "Baujat plot coordinates")
}
