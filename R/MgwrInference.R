#' Inference for (multiscale) geographically weighted regression
#'
#' \code{MgwrLocalT}: local t-values with the da Silva-Fotheringham corrected
#' critical value \eqn{t_{1-\alpha/(2 ENP), df}}. \code{BandwidthConfidenceInterval}:
#' interval of candidate bandwidths carrying the requested cumulative
#' Akaike weight (Li et al. 2020). Identical to the Python arm
#' \code{morie.fn.mgwrinfer}.
#'
#' @param coef,se Local coefficients and standard errors.
#' @param enp Effective number of parameters of the surface.
#' @param alpha Family-wise level.
#' @param df Degrees of freedom (default n - enp).
#' @param bandwidths,aicc Candidate bandwidths and their AICc.
#' @param level Cumulative Akaike weight.
#' @return List.
#' @references da Silva, A. R. and Fotheringham, A. S. (2016). The multiple
#'   testing issue in geographically weighted regression. Geographical
#'   Analysis 48, 233-247.
#'
#'   Li, Z., Fotheringham, A. S., Oshan, T. M. and Wolf, L. J. (2020).
#'   Measuring bandwidth uncertainty in multiscale geographically weighted
#'   regression using Akaike weights. Annals of the American Association of
#'   Geographers 110, 1500-1520.
#' @examples
#' MgwrLocalT(c(0.5, -0.2, 1.1), c(0.2, 0.25, 0.3), 2, df = 50)$significant
#' BandwidthConfidenceInterval(c(40, 50, 60, 70, 80), c(310, 302, 300, 301, 306))
#' @export
MgwrLocalT <- function(coef, se, enp, alpha = 0.05, df = NULL) {
  t <- coef / se
  a <- alpha / enp
  d <- if (is.null(df)) length(coef) - enp else df
  crit <- qt(1 - a / 2, d)
  list(t = t, alpha_adjusted = a, critical = crit, significant = abs(t) > crit)
}

#' @rdname MgwrLocalT
#' @export
BandwidthConfidenceInterval <- function(bandwidths, aicc, level = 0.95) {
  e <- exp(-(aicc - min(aicc)) / 2)
  tot <- 0
  for (v in e) tot <- tot + v
  w <- e / tot
  ord <- order(-w, seq_along(w))
  cum <- 0
  inc <- integer(0)
  for (k in ord) {
    inc <- c(inc, k)
    cum <- cum + w[k]
    if (cum >= level) break
  }
  list(best = bandwidths[ord[1]], lower = min(bandwidths[inc]), upper = max(bandwidths[inc]), weights = w,
       coverage = cum)
}
