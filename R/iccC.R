# SPDX-License-Identifier: AGPL-3.0-or-later
#' Agreement up to a constant offset per rater
#'
#' The consistency form does not penalise a rater who is systematically
#' high, because the column mean square is left out of the denominator.
#' Right when only the ordering matters, wrong when the absolute number
#' is the point.
#'
#' Formula: \code{ICC(C,1) = (MS_R - MS_E)/(MS_R + (k-1) MS_E)}.
#'
#' @param y Ratings.
#' @param subject Subject label.
#' @param rater Rater label.
#' @return List with \code{estimate}, \code{ms_r}, \code{ms_c},
#'   \code{ms_e}, \code{k}, \code{n_subjects}.
#' @references Shrout & Fleiss (1979) Psychol Bull 86:420-428; McGraw &
#'   Wong (1996) Psychol Methods 1:30-46.
#' @export
#' @examples
#' IccC(y = c(1, 2, 3, 4, 5, 6, 7, 8), subject = c(1, 2, 3, 4, 5, 6, 7, 8), rater = c(1, 2, 3, 4, 5, 6, 7, 8))
IccC <- function(y, subject, rater) {
  ms <- .s4_icc_ms(y, subject, rater)
  den <- ms$ms_r + (ms$k - 1) * ms$ms_e
  .t1_result(estimate = if (den != 0) (ms$ms_r - ms$ms_e) / den else NaN,
             ms_r = ms$ms_r, ms_c = ms$ms_c, ms_e = ms$ms_e, k = ms$k,
             n_subjects = ms$n, method = "Intraclass correlation ICC(C,1)")
}
