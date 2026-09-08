# SPDX-License-Identifier: AGPL-3.0-or-later
#' Deviance information criterion
#'
#' Spiegelhalter, Best, Carlin and van der Linde (2002), Bayesian measures
#' of model complexity and fit, JRSS-B 64(4), 583-639, equations (9) and
#' (10): p_D = Dbar - D(thetabar) and DIC = Dbar + p_D.  The paper is
#' paywalled, so both equations are quoted in their standard published
#' form.  When D(thetabar) is not supplied the alternative complexity
#' measure p_V = var(D)/2 is used and reported as such.
#'
#' @param deviance posterior draws of the deviance.
#' @param d_at_mean optional D(thetabar).
#' @return list: estimate (DIC), dbar, p_d, d_hat, variant, n, method.
#' @keywords internal
#' @examples
#' Dic(c(10, 12, 11, 13, 9), 10.2)$estimate
#' @export
Dic <- function(deviance, d_at_mean = NULL) {
  d <- .s03vec(deviance)
  dbar <- .s03mean(d)
  if (is.null(d_at_mean)) {
    pd <- 0.5 * .s03var(d, 1L)
    dhat <- dbar - pd
    variant <- "p_V"
  } else {
    dhat <- as.numeric(d_at_mean)
    pd <- dbar - dhat
    variant <- "p_D"
  }
  list(estimate = dbar + pd, dbar = dbar, p_d = pd, d_hat = dhat,
       variant = variant, n = length(d),
       method = "Spiegelhalter et al (2002) deviance information criterion")
}
