# SPDX-License-Identifier: AGPL-3.0-or-later
#' ZAPC random-forest prediction.
#'
#' Formula: Y-hat = 0 when theta-hat > 0.5 and mu-hat otherwise (eq. 15.4):
#' ZAPC_RF converts the probability to a zero rather than to a binary label,
#' and the 0.5 threshold is used because it assumes no prior information.
#'
#' @param theta_hat Estimated probability of an altered zero.
#' @param mu_hat Estimated Poisson rate.
#' @param threshold Decision threshold on theta-hat.
#' @return List with estimate, y_hat, is_zero, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (15.4) p.652. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm329(theta_hat = 5L, mu_hat = c(1, 2, 3, 4, 5, 6, 7, 8))
Msm329 <- function(theta_hat, mu_hat, threshold = 0.5) {
  v <- .gpzapcpredict(theta_hat, mu_hat, threshold = threshold)
  list(estimate = v, y_hat = v, is_zero = v == 0,
       method = "ZAPC_RF prediction (MVSML 2022 eq. 15.4)")
}
