# SPDX-License-Identifier: AGPL-3.0-or-later
#' Worst-case bias bound under local misspecification.
#'
#' |bias| <= c ||s||_2 by Cauchy-Schwarz, attained at
#' gamma = c s / ||s||, and the conservative interval is
#' estimate +/- (c ||s|| + z_{1-alpha/2} se).
#'
#' @param estimate Point estimate under the baseline model.
#' @param sensitivity Derivative of the estimate w.r.t. the perturbation.
#' @param c Radius of the misspecification neighbourhood.
#' @param se Standard error under the baseline model.
#' @param conf Nominal confidence level.
#'
#' @return List with bias, lower, upper, halfwidth, worstgamma,
#'   normsens, z, c.
#' @references Bias bound by Cauchy-Schwarz; the bias-aware interval is
#'   Armstrong and Kolesar (2021), Quantitative Economics 12(1), 77-108,
#'   Sect. 2.  The worklist attributed this row to Andrews and Kasy
#'   (2019), which is about publication bias and is therefore not cited
#'   as the source.  The Quantitative Economics article is not in the
#'   local corpus and was not read.
#' @export
#' @examples
#' Misspecbd(estimate = 0.5, sensitivity = c(0.1, 0.2), c = 0.3, se = 0.1)
Misspecbd <- function(estimate, sensitivity, c, se, conf = 0.95) {
  s <- .t1_vec(sensitivity); c <- as.numeric(c)
  if (c < 0) stop("c must be non-negative")
  se <- as.numeric(se)
  if (se < 0) stop("se must be non-negative")
  ns <- sqrt(sum(s^2))
  bias <- c * ns
  z <- stats::qnorm(0.5 + 0.5 * as.numeric(conf))
  hw <- bias + z * se
  est <- as.numeric(estimate)
  .t1_result(bias = bias, lower = est - hw, upper = est + hw,
             halfwidth = hw,
             worstgamma = if (ns == 0) rep(0, length(s)) else c * s / ns,
             normsens = ns, z = z, c = c,
             method = "Conservative bias-aware interval under local misspecification")
}
