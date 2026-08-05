# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nadaraya-Watson kernel regression (alias of hrzk2)
#'
#' This module is an ALIAS. The estimator is implemented once, in
#' \code{hrzk2}; this entry point supplies the classical argument
#' spelling (bandwidth \code{h}) and delegates.
#'
#' \code{m_hat(x) = sum_i K_h(x - X_i) Y_i / sum_i K_h(x - X_i)} with a
#' Gaussian kernel and, when \code{h} is omitted, Silverman's
#' rule-of-thumb bandwidth.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric response vector.
#' @param h Optional bandwidth (Silverman default).
#' @param grid Optional evaluation grid (defaults to \code{x}).
#' @return List with estimate, se, bandwidth, n, method.
#' @references Nadaraya (1964), Theory Probab. Appl. 9(1), 141-142,
#'   \doi{10.1137/1109020}; Watson (1964), Sankhya A 26(4), 359-372.
#' @export
Naday <- function(x, y, h = NULL, grid = NULL) {
  hrzk2(x, y, bandwidth = h, grid = grid)
}
