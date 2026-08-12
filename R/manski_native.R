# Manski (1990) no-assumption bounds on the average treatment
# effect.  Source: Manski, C. F. (1990), Nonparametric bounds on
# treatment effects, American Economic Review Papers and Proceedings
# 80(2), 319-323; Manski, C. F. (2007), Identification for Prediction
# and Decision, Harvard University Press, Sec. 7.1, eqs (7.5)-(7.7).
# Native implementation mirroring Python morie.fn.manski, which is the
# same call onto morie.fn.bndest with the support given as two
# scalars.

#' Manski no-assumption bounds on the average treatment effect
#'
#' Convenience form of \code{\link{morie_bndest}} taking the support
#' as two scalars.  Mirrors Python \code{morie.fn.manski}.
#'
#' @param y Outcomes.
#' @param D Binary 0/1 treatment.
#' @param y_min,y_max Assumed outcome support.
#' @return The ATE list of \code{\link{morie_bndest}}.
#' @references Manski, C. F. (1990). Nonparametric bounds on treatment
#'   effects. American Economic Review, 80(2), 319-323.
#' @export
morie_manski <- function(y, D, y_min, y_max) {
  morie_bndest(y, NULL, c(y_min, y_max), treatment = D)
}
