# SPDX-License-Identifier: AGPL-3.0-or-later
#' Huber weight function
#'
#' The weight is the ratio psi(r)/r of Huber's score function to the
#' residual, so an IRLS step with these weights solves the Huber
#' M-estimating equation.  Weights are scale-equivariant.
#'
#' Formula: w(r) = 1 if |r| <= k, else k/|r|.
#'
#' @param y Residuals, already scaled if a scale estimate is used.
#' @param k Tuning constant; 1.345 gives 95 percent normal efficiency.
#' @return List with \code{estimate} (mean weight), \code{weights},
#'   \code{psi}, \code{n_downweighted}, \code{k}, \code{n},
#'   \code{method}.
#' @references Huber (1964), Robust estimation of a location parameter,
#'   Annals of Mathematical Statistics 35(1):73-101.
#'   \doi{10.1214/aoms/1177703732}
#' @export
Huberw <- function(y, k = 1.345) {
  r <- .s03vec(y)
  if (length(r) == 0L) stop("huber_weight: y is empty")
  kv <- as.numeric(k)
  if (kv <= 0) stop("huber_weight: k must be positive")
  w <- ifelse(abs(r) <= kv, 1, kv / abs(r))
  psi <- pmax(-kv, pmin(kv, r))
  .t1_result(estimate = mean(w), weights = w, psi = psi,
             n_downweighted = sum(abs(r) > kv), k = kv, n = length(r),
             method = "w(r) = min(1, k/|r|), Huber (1964)")
}
