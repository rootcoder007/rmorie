# Callaway-Sant'Anna staggered DiD, exposed under its wave3 module
# name.  Source: Callaway, B. and Sant'Anna, P. H. C. (2021),
# Difference-in-differences with multiple time periods, Journal of
# Econometrics 225(2), 200-230.  Native implementation mirroring
# Python morie.fn.causdidcs, which is the same binding onto
# morie.fn.cssant rather than a second implementation.

#' Callaway-Sant'Anna staggered DiD (module-name entry point)
#'
#' Entry point onto \code{\link{morie_cssant}}; see there for the full
#' description of the group-time ATT(g, t) estimator, its clean
#' comparison groups and its aggregations.
#'
#' @inheritParams morie_cssant
#' @return The list returned by \code{\link{morie_cssant}}.
#' @references Callaway, B. and Sant'Anna, P. H. C. (2021).
#'   Difference-in-differences with multiple time periods. Journal of
#'   Econometrics, 225(2), 200-230.
#' @export
#' @examples
#' set.seed(1)
#' nu <- 6; T <- 5
#' unit <- rep(1:nu, each = T)
#' time <- rep(1:T, nu)
#' ft <- rep(c(Inf, Inf, Inf, 4, 4, 3), each = T)
#' D <- as.integer(time >= ft)
#' y <- rnorm(nu * T) + D * 1.5
#' morie_causdidcs(y, D, unit, time)
morie_causdidcs <- function(y, D, unit, time, cohort = NULL,
                            control = "notyet") {
  morie_cssant(y, D, unit, time, cohort = cohort, control = control)
}
