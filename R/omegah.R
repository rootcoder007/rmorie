# SPDX-License-Identifier: AGPL-3.0-or-later
#' Share of total score variance due to the single general factor
#'
#' Cronbach alpha is routinely read as evidence that a scale measures one
#' thing, and it is not: alpha is high whenever the items intercorrelate
#' at all. Omega hierarchical answers the question alpha is mistaken for,
#' putting only the general-factor loadings in the numerator.
#'
#' Formula: \code{omega_h = (sum lambda_g)^2 / Var(T)},
#' \code{Var(T) = (sum lambda_g)^2 + (sum lambda_s)^2 + sum psi}.
#'
#' @param X Item scores; used only for the reported observed variance.
#' @param loadings_g General-factor loadings.
#' @param loadings_specific Group-factor loadings, optional.
#' @return List with \code{estimate}, \code{omega_total},
#'   \code{var_total}, \code{uniqueness}, \code{p}.
#' @references Zinbarg, Revelle, Yovel & Li (2005) Psychometrika
#'   70:123-133; McDonald, R. P. (1999), Test Theory, ch 6.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Omegah(V, V)
Omegah <- function(X, loadings_g, loadings_specific = NULL) {
  lg <- as.numeric(loadings_g)
  p <- length(lg)
  ls <- if (is.null(loadings_specific)) rep(0, p) else as.numeric(loadings_specific)
  psi <- 1 - lg^2 - ls^2
  sg2 <- sum(lg)^2
  ss2 <- sum(ls)^2
  var_t <- sg2 + ss2 + sum(psi)
  .t1_result(estimate = if (var_t != 0) sg2 / var_t else NaN,
             omega_total = if (var_t != 0) (sg2 + ss2) / var_t else NaN,
             var_total = var_t, uniqueness = psi, p = p,
             method = "McDonald omega hierarchical")
}
