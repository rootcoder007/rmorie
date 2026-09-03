# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bucher adjusted indirect comparison
#'
#' Formula: d_AC = d_AB - d_CB; var(d_AC) = v_AB + v_CB
#'
#' @param d_AB Direct estimate of A versus B.
#' @param v_AB Its variance.
#' @param d_CB Direct estimate of C versus B.
#' @param v_CB Its variance.
#' @param alpha Two-sided significance level.

#' @param d_AB See Usage.
#' @param v_AB See Usage.
#' @param d_CB See Usage.
#' @param v_CB See Usage.
#' @param alpha See Usage.
#' @return List with ``estimate``, ``variance``, ``se``, ``z``, ``p_value``,
#' ``ci_lower``, ``ci_upper``.
#' @references Bucher, Guyatt, Griffith and Walter (1997), The results of direct and
#' indirect treatment comparisons in meta-analysis of randomized controlled trials,
#' Journal of Clinical Epidemiology 50:683-691. Paywalled; d_AC = d_AB - d_CB with
#' variances added is the standard published form, restated identically in every network
#' meta-analysis source consulted.
#' @export
Bucherind <- function(d_AB, v_AB, d_CB, v_CB, alpha = 0.05) {
  d <- as.numeric(d_AB) - as.numeric(d_CB)
  var <- as.numeric(v_AB) + as.numeric(v_CB)
  if (var <= 0) stop("variances must be positive")
  se <- sqrt(var)
  z <- d / se
  zc <- stats::qnorm(1 - alpha / 2)
  .t1_result(estimate = d, variance = var, se = se, z = z,
             p_value = 2 * stats::pnorm(-abs(z)),
             ci_lower = d - zc * se, ci_upper = d + zc * se,
             method = "Bucher adjusted indirect comparison")
}
