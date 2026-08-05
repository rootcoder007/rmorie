# SPDX-License-Identifier: AGPL-3.0-or-later
#' Compound-outcome worst-case bound
#'
#' A composite endpoint is a fixed linear functional of its components, so
#' the identification problem is the ordinary one for the scalar
#' \code{sum_k w_k y_k}. The support of the composite is read off the
#' realised composite values rather than combined component-wise, which
#' keeps the interval sharp for the data at hand.
#'
#' Formula: \code{c_i = sum_k w_k y_ik}, then the worst-case bound of
#' Molinari (2021) equation (2.11) applied to \code{c}.
#'
#' @param y_components Numeric matrix of outcome components, one row per
#'   unit.
#' @param D Binary treatment indicator, coded 0/1.
#' @param X Component weights, length equal to the number of columns.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{k}, \code{n}.
#' @references Manski, C. F. (2003). Partial Identification of Probability
#'   Distributions. Springer, New York. Worst-case form as equation (2.11)
#'   of Molinari, F. (2021), Handbook of Econometrics 7A
#'   (arXiv:2004.11751 p. 17).
#' @export
Bnscbo <- function(y_components, D, X) {
  M <- as.matrix(y_components)
  n <- nrow(M)
  if (n == 0L) stop("Bnscbo: y_components is empty")
  k <- ncol(M)
  w <- as.numeric(unlist(X))
  if (length(w) != k)
    stop("Bnscbo: X must give one weight per component")
  comp <- as.numeric(M %*% w)
  z <- .bnd_yd(comp, D, "Bnscbo")
  y0 <- min(z$y)
  y1 <- max(z$y)
  b <- .bnd_wc_ate(z$y, z$d, y0, y1)
  .t1_result(lower = b[1], upper = b[2], width = b[2] - b[1],
             estimate = 0.5 * (b[1] + b[2]), k = k, n = n,
             method = "Compound-outcome bound")
}
