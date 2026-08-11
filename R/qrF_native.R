# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Quantile (pinball) loss for point forecasts (Qrf).
# Bit-identical mirror of src/morie/fn/qrF.py. Anchored on the optimal
# point forecast property: the sample tau-quantile minimizes the mean
# score (Gneiting 2011, Theorem 9), and on hand-computed values.

#' Quantile (pinball) scoring function for point forecasts
#'
#' For forecast x, realization y and level tau in (0, 1) the score is
#' \eqn{S(x, y) = (1\{x \ge y\} - \tau)(x - y)}, the generalized
#' piecewise-linear scoring function of order tau with identity g
#' (Gneiting 2011). Negatively oriented; the tau-quantile of the
#' predictive distribution is the optimal point forecast. Equivalent
#' to the Koenker-Bassett check function
#' \eqn{\rho_\tau(u) = u(\tau - 1\{u < 0\})} with \eqn{u = y - x}.
#'
#' @param y Realizations.
#' @param y_hat Forecasts (scalar or vector of the same length).
#' @param tau Quantile level in (0, 1).
#' @return List with \code{estimate} (mean score), \code{scores},
#'   \code{n}, \code{tau}, \code{method}.
#' @references Gneiting, T. (2011), Making and evaluating point
#'   forecasts, Journal of the American Statistical Association
#'   106(494), 746-762; arXiv:0912.0902, sec. 3.3 and Theorem 9
#'   (source library/pdf/fetched-wave3/
#'   gneiting-2011-quantiles-point-forecasts.pdf); Koenker, R. and
#'   Bassett, G. (1978), Regression quantiles, Econometrica 46(1),
#'   33-50.
#' @export
Qrf <- function(y, y_hat, tau) {
  tau <- as.numeric(tau)
  if (!(tau > 0 && tau < 1)) stop("tau must be in (0, 1)", call. = FALSE)
  y <- as.numeric(y)
  x <- as.numeric(y_hat)
  if (length(x) == 1L && length(y) > 1L) x <- rep(x, length(y))
  if (length(x) != length(y)) stop("y and y_hat must have equal length", call. = FALSE)
  ind <- as.numeric(x >= y)
  scores <- (ind - tau) * (x - y)
  list(
    estimate = mean(scores),
    scores = scores,
    n = length(y),
    tau = tau,
    method = "Quantile (pinball) loss"
  )
}
