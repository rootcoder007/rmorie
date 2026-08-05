# SPDX-License-Identifier: AGPL-3.0-or-later
#' ZAP random-forest prediction.
#'
#' Formula: Y-hat = (1 - theta-hat) mu-hat / (1 - exp(-mu-hat)) (eq. 15.3):
#' under ZAP_RF the prediction is the mean of the zero-altered Poisson model.
#'
#' Book erratum: eq. (15.3) as printed, and the E(Y) line on p.651, both give
#' the numerator as (1 - theta) exp(-mu), dropping the mu factor. The ZAP
#' probability mass function printed directly above E(Y), the Var(Y)
#' expression printed on the next line, and the p.652 estimating equation for
#' mu all carry the mu. The printed form is also not a count: it decreases
#' towards zero as mu grows. The internally consistent formula is implemented.
#'
#' @param theta_hat Estimated probability of an altered zero.
#' @param mu_hat Estimated Poisson rate.
#' @return List with estimate, y_hat, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (15.3) p.652. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm327 <- function(theta_hat, mu_hat) {
  v <- .gpzappredict(theta_hat, mu_hat)
  list(estimate = v, y_hat = v, method = "ZAP_RF prediction (MVSML 2022 eq. 15.3)")
}
