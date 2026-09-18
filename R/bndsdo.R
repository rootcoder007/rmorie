# SPDX-License-Identifier: AGPL-3.0-or-later
#' ATE bound under monotone treatment selection
#'
#' Monotone treatment selection says the treated would have done at least
#' as well as the untreated under either treatment,
#' \code{E[y(t) | D = 1] >= E[y(t) | D = 0]}. The consequence is one line
#' of algebra: \code{E[y(1)] = E[y | D = 1] P(D = 1) +
#' E[y(1) | D = 0] P(D = 0) <= E[y | D = 1]}, and symmetrically
#' \code{E[y(0)] >= E[y | D = 0]}, so the naive observed difference, which
#' without the assumption bounds nothing, becomes an exact upper bound on
#' the ATE. The other end is left at the worst case.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @param skew Direction of the selection: positive for selection on gains
#'   (the default), negative for the reverse.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{naive}, \code{wc_lower}, \code{wc_upper},
#'   \code{p_treated}, \code{n}.
#' @references Manski, C. F. and Pepper, J. V. (2000). Monotone
#'   instrumental variables, with an application to the returns to
#'   schooling. Econometrica 68(4), 997-1010. \doi{10.1111/1468-0262.00144}.
#'   The worst-case end is equation (2.11) of Molinari, F. (2021),
#'   Handbook of Econometrics 7A (arXiv:2004.11751 p. 17). The derivation
#'   above is written out because the paper was not accessible: what is
#'   cited to Manski and Pepper is the assumption, not a copied formula.
#' @export
#' @examples
#' set.seed(1)
#' r <- Bndsdo(y = rnorm(10), D = rbinom(10, 1, 0.5)); TRUE
Bndsdo <- function(y, D, skew = 1) {
  z <- .bnd_yd(y, D, "Bndsdo")
  cm <- .bnd_cellmeans(z$y, z$d)
  if (cm$p1 <= 0 || cm$p0 <= 0)
    stop("Bndsdo: both treatment arms must be non-empty")
  y0 <- min(z$y)
  y1 <- max(z$y)
  w <- .bnd_wc_ate(z$y, z$d, y0, y1)
  naive <- cm$m1 - cm$m0
  s <- as.numeric(skew)[1]
  if (s == 0) stop("Bndsdo: skew must be non-zero")
  lo <- if (s > 0) w[1] else naive
  hi <- if (s > 0) naive else w[2]
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), naive = naive,
             wc_lower = w[1], wc_upper = w[2], p_treated = cm$p1,
             n = length(z$y), method = "Skewed-outcome bound")
}
