# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cox model with a beta-process baseline
#'
#' lambda(t | z) = lambda_0(t) exp(beta z) with lambda_0 ~ BP and beta
#' normal.  Because the covariate acts multiplicatively on the hazard,
#' the ratio of cumulative hazards between two covariate values is
#' exp(beta ) FOR EVERY t -- proportional hazards holds by construction,
#' independently of whatever the baseline turns out to be, which is why
#' the baseline can be left nonparametric.
#'
#' Formula: H(t | z) = H0(t) exp(beta z); H(t | z2) / H(t | z1)
#'   = exp(beta (z2 - z1)).
#'
#' @param beta Log hazard ratio.
#' @param z Two covariate values.
#' @param t Time point.
#' @param c Beta-process concentration (does not enter the mean ratio).
#' @return List with \code{estimate} (the hazard ratio),
#'   \code{cum_hazards}, \code{proportional}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.6.
#' @export
#' @examples
#' Ghosalcoxmodel()
Ghosalcoxmodel <- function(beta = 0.7, z = c(0, 1), t = 1, c = 2) {
  zs <- as.numeric(z)
  if (length(zs) != 2L) stop("z must have exactly two covariate values")
  H <- t * exp(beta * zs)
  if (H[1] == 0) stop("the first cumulative hazard is zero; the ratio is undefined")
  ratio <- H[2] / H[1]
  .t1_result(estimate = ratio, cum_hazards = H,
             proportional = abs(ratio - exp(beta)) < 1e-12,
             method = "Cox with BP baseline (GvdV 2017 sec. 13.6)")
}
