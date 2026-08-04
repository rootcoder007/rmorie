# SPDX-License-Identifier: AGPL-3.0-or-later
#' The biweight loss and its derivative, evaluated pointwise
#'
#' \code{rho} is the loss an M-estimator minimises, \code{psi} its
#' derivative, \code{w} the IRLS weight \code{psi(r)/r}. rho is flat
#' past c, so a residual beyond the tuning constant costs a constant and
#' the fit stops chasing it.
#'
#' Formula: \code{rho(r) = (c^2/6)(1 - [1 - (r/c)^2]^3)} for
#' \code{|r| <= c}, else \code{c^2/6}; \code{psi(r) = r[1 - (r/c)^2]^2}
#' inside and 0 outside.
#'
#' @param r Scaled residuals.
#' @param c Tuning constant.
#' @return List with \code{estimate}, \code{rho}, \code{psi}, \code{w}, \code{n}.
#' @references Beaton, A. E. & Tukey, J. W. (1974). Technometrics
#'   16:147-185.
#' @export
Tukrho <- function(r, c = 4.685) {
  v <- as.numeric(unlist(r)); c <- as.numeric(c)
  cap <- c * c / 6
  u <- v / c
  inside <- abs(u) <= 1
  rho <- ifelse(inside, cap * (1 - (1 - u * u)^3), cap)
  psi <- ifelse(inside, v * (1 - u * u)^2, 0)
  w <- ifelse(inside, (1 - u * u)^2, 0)
  .t1_result(estimate = sum(rho), rho = rho, psi = psi, w = w, n = length(v),
             method = "Tukey biweight rho, psi and weight")
}
