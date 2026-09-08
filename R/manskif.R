# SPDX-License-Identifier: AGPL-3.0-or-later

#' Manski worst-case bounds on a partially observed mean
#'
#' \code{Manskif} and \code{morie_bnd_manski} document the SAME method:
#' the mean of a partially observed outcome is bounded by filling the
#' unseen part with the support endpoints; with \code{treatment} given,
#' the same construction bounds each potential-outcome mean and the ATE.
#' Rather than carry a second implementation -- which would agree with
#' the first at 1e-9 forever while doubling the surface -- this function
#' forwards to \code{\link{morie_bnd_manski}}.
#'
#' @param y Outcome; entries where \code{observed} is FALSE are ignored.
#' @param observed Logical vector, whether the outcome was seen; ignored
#'   when \code{treatment} is given (pass NULL then).
#' @param support Numeric length-2, the logical range of the outcome.
#' @param treatment Optional 0/1 treatment indicator for the ATE case.
#' @return Whatever \code{\link{morie_bnd_manski}} returns.
#' @references Manski, C. F. (1989), Anatomy of the Selection Problem,
#'   Journal of Human Resources 24(3):343-360. Manski, C. F. (1990),
#'   Nonparametric Bounds on Treatment Effects, American Economic Review
#'   Papers and Proceedings 80(2):319-323. Molinari, F. (2021),
#'   Microeconometrics with Partial Identification, Handbook of
#'   Econometrics 7A, eq. (2.11) (arXiv:2004.11751).
#' @export
Manskif <- function(y, observed, support, treatment = NULL) {
  morie_bnd_manski(y, observed, support, treatment = treatment)
}
