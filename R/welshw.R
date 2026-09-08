# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian-shaped weight that decays smoothly and never reaches zero
#'
#' Unlike the biweight there is no hard cutoff -- a residual ten times the
#' tuning constant still counts, just by \code{e^-100}. The objective is
#' smooth everywhere, so gradient methods behave where the biweight kink
#' at c can stall them.
#'
#' Formula: \code{w(r) = exp(-(r/c)^2)},
#' \code{rho(r) = (c^2/2)\[1 - exp(-(r/c)^2)\]}, \code{psi = r w(r)}.
#'
#' @param y Scaled residuals.
#' @param c Tuning constant; 2.9846 gives 95 percent Gaussian efficiency.
#' @return List with \code{estimate}, \code{w}, \code{rho}, \code{psi}, \code{n}.
#' @references Dennis, J. E. & Welsch, R. E. (1978). Commun Statist B
#'   7:345-359.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Welshw(V)
Welshw <- function(y, c = 2.9846) {
  v <- as.numeric(unlist(y))
  c <- as.numeric(c)
  w <- exp(-((v / c)^2))
  rho <- (c * c / 2) * (1 - w)
  .t1_result(estimate = sum(rho), w = w, rho = rho, psi = v * w, n = length(v),
             method = "Welsch robust weight")
}
