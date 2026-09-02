# SPDX-License-Identifier: AGPL-3.0-or-later
#' Frechet-Hoeffding bounds on a joint distribution
#'
#' For any joint distribution with the given margins the copula lies
#' between the two bounds, and both are attained: the upper by
#' comonotone variables, the lower by countermonotone ones.  Both bounds
#' are themselves copulas in two dimensions, so they satisfy
#' C(u, 1) = u exactly -- which is what the tests check, along with the
#' bracketing of the independence copula uv.
#'
#' Formula: max(u + v - 1, 0) <= C(u, v) <= min(u, v).
#'
#' @param F_0,F_1 Marginal CDF values in \[0, 1\], of equal length.
#' @param joint Optional candidate joint CDF at the same points.
#' @return List with \code{estimate}, \code{lower}, \code{upper},
#'   \code{independence}, \code{width}, \code{respects_bounds},
#'   \code{n_violations}, \code{n}, \code{method}.
#' @references Frechet (1951), Sur les tableaux de correlation dont les
#'   marges sont donnees, Annales de l'Universite de Lyon A 14:53-77;
#'   Hoeffding (1940), Masstabinvariante Korrelationstheorie, Schriften
#'   des Mathematischen Instituts der Universitat Berlin 5:181-233.
#' @export
#' @examples
#' Frdbnd(F_0 = c(0.2, 0.5, 0.8), F_1 = c(0.3, 0.6, 0.9))
Frdbnd <- function(F_0, F_1, joint = NULL) {
  u <- .s03vec(F_0)
  v <- .s03vec(F_1)
  if (length(u) == 0L) stop("frechet_hoeffding_bounds: F_0 is empty")
  if (length(v) != length(u)) stop("frechet_hoeffding_bounds: F_0 and F_1 have different lengths")
  if (any(c(u, v) < 0 | c(u, v) > 1)) stop("frechet_hoeffding_bounds: marginal probabilities must lie in [0, 1]")
  lo <- pmax(u + v - 1, 0)
  hi <- pmin(u, v)
  viol <- 0L
  if (is.null(joint)) {
    okv <- 1L
  } else {
    jv <- .s03vec(joint)
    if (length(jv) != length(u)) stop("frechet_hoeffding_bounds: joint and F_0 have different lengths")
    viol <- sum(jv < lo - 1e-12 | jv > hi + 1e-12)
    okv <- as.integer(viol == 0L)
  }
  .t1_result(estimate = hi[1], lower = lo, upper = hi, independence = u * v,
             width = hi - lo, respects_bounds = okv, n_violations = viol,
             n = length(u),
             method = "max(u + v - 1, 0) <= C(u, v) <= min(u, v), Frechet (1951); Hoeffding (1940)")
}
