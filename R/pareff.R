# SPDX-License-Identifier: AGPL-3.0-or-later
#' Population attributable fraction (Levin)
#'
#' DUPLICATE of the Python module \code{attfr}, which has no R arm; the
#' formula is one line and is written here rather than duplicated twice.
#'
#' Formula: \code{PAF = pe (RR - 1) / (pe (RR - 1) + 1)}.
#'
#' @param pe Prevalence of exposure in the population, in \[0, 1\].
#' @param RR Relative risk, strictly positive.
#' @param se_RR Optional standard error of \code{RR}; supplying it adds
#'   a delta-method confidence interval.
#' @param alpha Two-sided CI level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{pe}, \code{RR}.
#' @references Levin, M. L. (1953). The occurrence of lung cancer in
#'   man. Acta Unio Internationalis Contra Cancrum, 9(3), 531-541.
#' @export
Pareff <- function(pe, RR, se_RR = NULL, alpha = 0.05) {
  p <- as.numeric(pe); r <- as.numeric(RR)
  if (r <= 0) stop("Pareff: RR must be positive")
  if (!(p >= 0 && p <= 1)) stop("Pareff: pe must lie in [0, 1]")
  paf <- p * (r - 1) / (p * (r - 1) + 1)
  se <- NaN; lo <- NaN; hi <- NaN
  if (!is.null(se_RR) && as.numeric(se_RR) > 0) {
    dp <- p / (p * (r - 1) + 1)^2
    se <- abs(dp) * as.numeric(se_RR)
    z <- stats::qnorm(1 - as.numeric(alpha) / 2)
    lo <- paf - z * se; hi <- paf + z * se
  }
  .t1_result(estimate = paf, se = se, ci_lower = lo, ci_upper = hi,
             pe = p, RR = r,
             method = "Levin population attributable fraction")
}
