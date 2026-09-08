# SPDX-License-Identifier: AGPL-3.0-or-later
#' Agreement that counts a systematic rater offset as disagreement
#'
#' The extra \code{k (MS_C - MS_E)/n} term in the denominator is the whole
#' difference from the consistency form: it charges for between-rater
#' bias. Two raters who agree on the ordering but differ by a constant
#' score high on consistency and low here.
#'
#' Formula: \code{ICC(A,1) = (MS_R - MS_E) /
#' \[MS_R + (k-1) MS_E + k (MS_C - MS_E)/n\]}.
#'
#' @param y Ratings.
#' @param subject Subject label.
#' @param rater Rater label.
#' @return List with \code{estimate}, \code{ms_r}, \code{ms_c},
#'   \code{ms_e}, \code{k}, \code{n_subjects}.
#' @references McGraw & Wong (1996) Psychol Methods 1:30-46, table 4;
#'   Shrout & Fleiss (1979) Psychol Bull 86:420-428.
#' @export
#' @examples
#' IccA(y = c(1, 2, 3, 4, 5, 6, 7, 8), subject = c(1, 2, 3, 4, 5, 6, 7, 8), rater = c(1,
#' 2, 3, 4, 5, 6, 7, 8))
IccA <- function(y, subject, rater) {
  ms <- .s4_icc_ms(y, subject, rater)
  den <- ms$ms_r + (ms$k - 1) * ms$ms_e + ms$k * (ms$ms_c - ms$ms_e) / ms$n
  .t1_result(estimate = if (den != 0) (ms$ms_r - ms$ms_e) / den else NaN,
             ms_r = ms$ms_r, ms_c = ms$ms_c, ms_e = ms$ms_e, k = ms$k,
             n_subjects = ms$n, method = "Intraclass correlation ICC(A,1)")
}
