# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bias-variance decomposition of the expected prediction error
#'
#' Formula: E(y - fhat)^2 = Var(e) + Bias\[fhat\]^2 + Var(fhat)
#'
#' @param F One row per replicate fit, one column per evaluation point: the predictions fhat(x_j) from replicate r.
#' @param f The true f(x_j) at the same n evaluation points.
#' @param sigma2 Irreducible error Var(e); must be non-negative.
#'
#' @return List with ``bias2``, ``variance``, ``irreducible``, ``total``, ``bias2_point``, ``variance_point``, ``R``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 4, Sect. 4.2 p. 113, which reproduces the decomposition of Hastie, Tibshirani and Friedman (2008) p. 223: the expected prediction error under quadratic loss splits into Var(e), the squared bias of fhat and the variance of fhat.  Averaged over the evaluation points here.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' set.seed(1)
#' F <- matrix(rnorm(50), 5, 10)
#' f <- rnorm(10)
#' Biasvardec(F, f, sigma2 = 1)
Biasvardec <- function(F, f, sigma2) {
  F <- .t1_mat(F)
  f <- .t1_vec(f)
  s2 <- as.numeric(sigma2)
  R <- nrow(F)
  n <- ncol(F)
  if (R == 0L) stop("F must have at least one replicate row")
  if (n != length(f)) stop("F must have one column per entry of f")
  if (s2 < 0) stop("sigma2 must be non-negative")
  m <- colMeans(F)
  b2 <- (m - f)^2
  vv <- colMeans(sweep(F, 2, m, "-")^2)
  bias2 <- mean(b2)
  var <- mean(vv)
  .t1_result(bias2 = bias2, variance = var, irreducible = s2,
             total = s2 + bias2 + var, bias2_point = b2, variance_point = vv,
             R = R, n = n,
             method = "Bias-variance decomposition, MVSML Sect. 4.2")
}
