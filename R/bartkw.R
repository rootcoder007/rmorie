# SPDX-License-Identifier: AGPL-3.0-or-later
#' Triangular lag weights that keep a long-run variance non-negative
#'
#' Truncating an autocovariance sum with equal weights can produce a
#' negative variance estimate -- a structural problem, not a rounding
#' one. The Bartlett taper is the Fourier transform of a non-negative
#' kernel, so the estimator is positive semi-definite by construction.
#'
#' Formula: \code{w_k = 1 - k/(M + 1)} for \code{k <= M}, else 0.
#'
#' @param lags Number of lags, or the lag indices themselves.
#' @param M Bandwidth; the largest lag index by default.
#' @return List with \code{w}, \code{estimate}, \code{M}, \code{n}.
#' @references Newey, W. K. & West, K. D. (1987). Econometrica
#'   55:703-708; Bartlett, M. S. (1950) Biometrika 37:1-16.
#' @export
Bartkw <- function(lags, M = NULL) {
  ks <- if (length(lags) == 1L) as.numeric(0:as.integer(lags)) else as.numeric(lags)
  Mv <- if (is.null(M)) max(ks) else as.numeric(M)
  w <- pmax(1 - ks / (Mv + 1), 0)
  .t1_result(w = w, estimate = sum(w), M = Mv, n = length(w),
             method = "Bartlett kernel lag weights")
}
