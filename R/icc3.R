# SPDX-License-Identifier: AGPL-3.0-or-later
#' ICC(3,1) two-way mixed-effects, single rater, consistency
#'
#' Shrout, P. E. and Fleiss, J. L. (1979), "Intraclass correlations: uses in
#' assessing rater reliability", \emph{Psychological Bulletin} 86(2), 420-428,
#' doi:10.1037/0033-2909.86.2.420, is the primary source; it is closed access
#' with no open copy in any repository (Unpaywall reports oa_status "closed").
#' The two-way ANOVA is the one printed in Hedderich, J., Sachs, L. and
#' Reynarowych, Z., \emph{Applied Statistics: Methods Using R}, Springer,
#' Section 6.16, pp. 427-428, "ANOVA according to Shrout-Fleiss"; that book
#' stops at types 1 and 2 and does not print type 3, so the type-3 ratio here
#'
#' \deqn{ICC(3,1) = (BMS - EMS)/(BMS + (k-1)EMS)}{ICC(3,1) = (BMS - EMS)/(BMS + (k-1) EMS)}
#'
#' is the type-2 ratio of p. 428 with the rater-variance term k(JMS - EMS)/n
#' dropped, which is what treating the k raters as fixed rather than sampled
#' does to the denominator.  Two consequences are used as checks instead of a
#' printed number, since none was available: ICC(3,1) equals ICC(2,1) exactly
#' when JMS = EMS, and ICC(3,1) = 1 exactly when the raters differ only by an
#' additive constant, where ICC(2,1) is strictly smaller.
#'
#' @param y Ratings in long format.
#' @param subject Subject of each rating.
#' @param rater Rater of each rating; the design must be complete and balanced.
#' @return list: estimate (ICC(3,1)), bms, wms, jms, ems, n, k, method.
#' @keywords internal
#' @examples
#' Icc3(c(1, 2, 3, 4, 5, 6), c(1, 1, 2, 2, 3, 3), c(1, 2, 1, 2, 1, 2))$estimate
#' @export
Icc3 <- function(y, subject, rater) {
  b <- .icc_balanced(y, subject, "icc_two_way_mixed")
  n <- b$n
  k <- b$k
  rs <- .s03vec(rater)
  if (length(rs) != n * k) {
    stop("icc_two_way_mixed: rater must have one entry per rating")
  }
  if (length(unique(rs)) != k) {
    stop("icc_two_way_mixed: the number of raters must match the ratings per subject")
  }
  ms <- .icc_mean_squares(b$rows, n, k)
  den <- ms$bms + (k - 1) * ms$ems
  if (den == 0) stop("icc_two_way_mixed: the ratings carry no variance")
  list(estimate = (ms$bms - ms$ems) / den, bms = ms$bms, wms = ms$wms,
       jms = ms$jms, ems = ms$ems, n = n, k = k,
       method = "ICC(3,1) two-way mixed single rater (consistency)")
}
