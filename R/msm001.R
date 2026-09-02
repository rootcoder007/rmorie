# SPDX-License-Identifier: AGPL-3.0-or-later
#' Statistical learning model, systematic plus random part.
#'
#' Formula: y_i = f(x_i) + eps_i, i = 1..n (eq. 1.1). The systematic part f is
#' determined by the predictors; eps has mean zero.
#'
#' @param x Predictor values.
#' @param f Systematic function; identity when NULL.
#' @param noise Error terms; zero when NULL.
#' @return List with estimate, y, systematic, mean_error, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (1.1) p.8. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Msm001(V)
Msm001 <- function(x, f = NULL, noise = NULL) {
  xs <- .gpflat(x)
  if (is.null(f)) f <- function(v) v
  sys_part <- vapply(xs, function(v) as.numeric(f(v)), 0)
  eps <- if (is.null(noise)) rep(0, length(xs)) else .gpflat(noise)
  y <- sys_part + eps
  list(estimate = y[1L], y = y, systematic = sys_part,
       mean_error = sum(eps) / length(eps),
       method = "model = systematic + random (MVSML 2022 eq. 1.1)")
}
