# SPDX-License-Identifier: AGPL-3.0-or-later
#' Critical vaccination threshold for herd immunity.
#'
#' Formula: p_c = 1 - 1/R0; with vaccine efficacy e the coverage needed is p_c/e
#'
#' @param R0 Basic reproduction number, greater than 1.
#' @param efficacy Vaccine efficacy in (0, 1].

#' @return List with ``threshold``, ``coverage``, ``feasible``, ``R0``, ``efficacy``.
#' @references Anderson and May (1991), Infectious Diseases of Humans: Dynamics and Control, Oxford University Press. Not held locally; p_c = 1 - 1/R0 is the standard published result and is stated in the same form in every open source consulted.
#' @export
Vaccthresh <- function(R0, efficacy = 1) {
  r0 <- .t1_vec(R0); e <- as.numeric(efficacy)
  if (e <= 0 || e > 1) stop("efficacy must be in (0, 1]")
  if (any(r0 <= 0)) stop("R0 must be positive")
  pc <- 1 - 1 / r0
  .t1_result(threshold = pc, coverage = pc / e, feasible = (pc / e) <= 1,
             R0 = r0, efficacy = e, method = "Critical vaccination threshold")
}
