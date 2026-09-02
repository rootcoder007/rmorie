# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effective number of parameters from a deviance sample
#'
#' Spiegelhalter, Best, Carlin and van der Linde (2002), JRSS-B 64(4),
#' 583-639, equation (9): p_D = Dbar - D(thetabar).  The companion measure
#' p_V = var(D)/2 (Gelman, Carlin, Stern and Rubin, Bayesian Data
#' Analysis, 2nd ed., section 6.7) needs only the deviance sample and is
#' the fallback when D(thetabar) is unavailable.  Both papers are
#' paywalled; the two expressions are quoted in their standard published
#' form.
#'
#' @param deviance posterior draws of the deviance.
#' @param d_at_mean optional D(thetabar).
#' @return list: estimate, p_d, p_v, dbar, d_hat, variant, n, method.
#' @keywords internal
#' @examples
#' Pdic(c(10, 12, 11, 13, 9), 10.2)$estimate
#' @export
Pdic <- function(deviance, d_at_mean = NULL) {
  d <- .s03vec(deviance)
  dbar <- .s03mean(d)
  pv <- 0.5 * .s03var(d, 1L)
  if (is.null(d_at_mean)) {
    pd <- NaN
    dhat <- NaN
    est <- pv
    variant <- "p_V"
  } else {
    dhat <- as.numeric(d_at_mean)
    pd <- dbar - dhat
    est <- pd
    variant <- "p_D"
  }
  list(estimate = est, p_d = pd, p_v = pv, dbar = dbar, d_hat = dhat,
       variant = variant, n = length(d),
       method = "Effective parameters from the deviance sample (p_D, p_V)")
}
