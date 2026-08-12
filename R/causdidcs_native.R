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
morie_causdidcs <- function(y, D, unit, time, cohort = NULL,
                            control = "notyet") {
  morie_cssant(y, D, unit, time, cohort = cohort, control = control)
}
