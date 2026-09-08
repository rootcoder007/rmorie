# Borusyak-Jaravel-Spiess event-study imputation estimator, exposed
# under its wave3 module name.  Source: Borusyak, K., Jaravel, X. and
# Spiess, J. (2024), Revisiting event study designs: robust and
# efficient estimation, Review of Economic Studies 91(6), 3253-3285
# (arXiv 2108.12419), Theorem 2.  Native implementation mirroring
# Python morie.fn.causdidev, which is the same binding onto
# morie.fn.boryis rather than a second implementation.

#' Event-study imputation estimator (module-name entry point)
#'
#' Entry point onto \code{\link{morie_boryis}}; see there for the
#' three-step imputation procedure of Borusyak, Jaravel and Spiess
#' (2024), Theorem 2.
#'
#' @inheritParams morie_boryis
#' @return The list returned by \code{\link{morie_boryis}}.
#' @references Borusyak, K., Jaravel, X. and Spiess, J. (2024).
#'   Revisiting event study designs: robust and efficient estimation.
#'   Review of Economic Studies, 91(6), 3253-3285.
#' @export
#' @examples
#' set.seed(1)
#' nu <- 6; T <- 5
#' unit <- rep(1:nu, each = T)
#' time <- rep(1:T, nu)
#' ft <- rep(c(Inf, Inf, Inf, 4, 4, 3), each = T)
#' D <- as.integer(time >= ft)
#' y <- rnorm(nu * T) + D * 1.5
#' morie_causdidev(y, D, unit, time)
morie_causdidev <- function(y, D, unit, time, X = NULL, weights = NULL) {
  morie_boryis(y, D, unit, time, X = X, weights = weights)
}
