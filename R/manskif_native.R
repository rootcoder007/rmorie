# Manski worst-case bounds, full interface (partially observed mean
# OR average treatment effect).  Source: Manski, C. F. (2007),
# Identification for Prediction and Decision, Harvard University
# Press, Sec. 2.1 eq (2.4) and Sec. 7.1 eqs (7.5)-(7.7).  Native
# implementation mirroring Python morie.fn.manskif, which forwards to
# morie.fn.bndest.  Both routes the source gives are reachable here:
# leave `treatment` NULL for the missing-outcome mean bound, or pass a
# binary treatment for the ATE bound.

#' Manski worst-case bounds (full interface)
#'
#' Alias of \code{\link{morie_bndest}} exposing both the
#' partially-observed-mean and the average-treatment-effect routes.
#' Mirrors Python \code{morie.fn.manskif}.
#'
#' @inheritParams morie_bndest
#' @return The list returned by \code{\link{morie_bndest}}.
#' @references Manski, C. F. (2007). Identification for Prediction and
#'   Decision. Harvard University Press.
#' @export
morie_manskif <- function(y, observed, support, treatment = NULL) {
  morie_bndest(y, observed, support, treatment = treatment)
}
