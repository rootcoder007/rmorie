# SPDX-License-Identifier: AGPL-3.0-or-later

#' Cumulative incidence function (alias of Crrcim)
#'
#' Formula: F_k(t) = integral_0^t S(u-) lambda_k(u) du
#'
#' \code{cumcif} and \code{crrcim} document the SAME Aalen-Johansen
#' estimator.  A second implementation would agree with the first at
#' 1e-9 forever and establish nothing, so this function forwards.  Here
#' \code{cause} carries the per-subject event-type vector, matching this
#' module's own stub signature.
#'
#' @param time Follow-up time per subject.
#' @param cause 0 for censored, otherwise the cause label, per subject.
#' @param event_type Cause of interest, or NULL for 1.
#' @return Whatever \code{\link{Crrcim}} returns.
#' @references Aalen & Johansen (1978), Scand. J. Statist.
#'   5(3):141-150.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Cumcif(V, V)
Cumcif <- function(time, cause, event_type = NULL) {
  k <- if (is.null(event_type)) 1 else event_type
  Crrcim(time, cause, k)
}
