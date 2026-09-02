# SPDX-License-Identifier: AGPL-3.0-or-later
#' Prevalence ratio
#'
#' Formula: PR = p_e / p_u; se(log PR) = sqrt((1-p_e)/(p_e n_e) + (1-p_u)/(p_u n_u))
#'
#' @param prev_exposed Prevalence in the exposed group.
#' @param prev_unexposed Prevalence in the unexposed group.
#' @param n_exposed Size of the exposed group.
#' @param n_unexposed Size of the unexposed group.
#' @param alpha Two-sided significance level.

#' @param prev_exposed See Usage.
#' @param prev_unexposed See Usage.
#' @param n_exposed See Usage.
#' @param n_unexposed See Usage.
#' @param alpha See Usage.
#' @return List with ``pr``, ``log_pr``, ``se_log``, ``ci_lower``, ``ci_upper``.
#' @references Barros and Hirakata (2003), Alternatives for logistic regression in cross-sectional studies: an empirical comparison of models that directly estimate the prevalence ratio, BMC Medical Research Methodology 3:21. Open access; the delta-method standard error for log PR used here is the standard binomial one.
#' @export
Prevratio <- function(prev_exposed, prev_unexposed, n_exposed = NULL, n_unexposed = NULL, alpha = 0.05) {
  pe <- as.numeric(prev_exposed)
  pu <- as.numeric(prev_unexposed)
  if (pe <= 0 || pe >= 1 || pu <= 0 || pu >= 1)
    stop("prevalences must be strictly between 0 and 1")
  pr <- pe / pu
  se <- NA_real_
  lo <- NA_real_
  hi <- NA_real_
  if (!is.null(n_exposed) && !is.null(n_unexposed)) {
    se <- sqrt((1 - pe) / (pe * as.numeric(n_exposed)) +
               (1 - pu) / (pu * as.numeric(n_unexposed)))
    z <- stats::qnorm(1 - alpha / 2)
    lo <- exp(log(pr) - z * se)
    hi <- exp(log(pr) + z * se)
  }
  .t1_result(pr = pr, log_pr = log(pr), se_log = se, ci_lower = lo,
             ci_upper = hi, method = "Prevalence ratio")
}
