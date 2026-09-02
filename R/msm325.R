# SPDX-License-Identifier: AGPL-3.0-or-later
#' Zero-truncated Poisson splitting criterion.
#'
#' Formula: LL+ = -N+ log(1 - exp(-mu)) + log(mu) sum_i Y_i+ - N+ mu
#' - sum_i log(Y_i+!) (eq. 15.2), the zero-truncated Poisson log-likelihood
#' used as the splitting criterion in the truncated part of the forest. With
#' \code{x} the best split is searched, taking the one that maximizes
#' LL+(left) + LL+(right).
#'
#' @param y_positive Strictly positive counts in the node.
#' @param mu Rate; the zero-truncated MLE is used when NULL.
#' @param x Candidate splitting covariate; no split is searched when NULL.
#' @return List with estimate, loglik, mu, split, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (15.2) p.651 and p.652. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Msm325(V)
Msm325 <- function(y_positive, mu = NULL, x = NULL) {
  if (is.null(mu)) mu <- .gpztpmle(y_positive)
  ll <- .gpztploglik(y_positive, mu)
  list(estimate = ll, loglik = ll, mu = mu,
       split = if (is.null(x)) NULL else .gpzapbestsplit(y_positive, x),
       method = "zero-truncated Poisson criterion (MVSML 2022 eq. 15.2)")
}
