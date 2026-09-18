# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intersection of the instrument bound and the shape bound
#'
#' The two assumptions restrict different things -- the instrument
#' restricts selection, monotone response restricts the outcome -- so
#' their identified sets intersect rather than nest, and the joint bound
#' can be strictly tighter than either. Reporting the two components
#' alongside the intersection shows which assumption is doing the work,
#' and an empty intersection means the two are jointly refuted.
#'
#' Formula: instrument bound from Molinari (2021) equation (2.15), shape
#' bound \code{[0, upper]} from equation (2.13), intersected.
#'
#' @param y Observed outcome.
#' @param D Binary treatment, coded 0/1.
#' @param Z Discrete instrument, one value per unit.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{iv_lower}, \code{iv_upper}, \code{mtr_lower},
#'   \code{mtr_upper}, \code{refuted}, \code{n}.
#' @references Mogstad, M., Santos, A. and Torgovitsky, A. (2018). Using
#'   instrumental variables for inference about policy relevant treatment
#'   parameters. Econometrica 86(5), 1589-1619. \doi{10.3982/ECTA15463} --
#'   the stub's attribution; their general linear-programming machinery
#'   already ships as \code{morie_bnd_lp} and is not duplicated here. The
#'   component bounds are equations (2.15) and (2.13) of Molinari, F.
#'   (2021), Handbook of Econometrics 7A (arXiv:2004.11751 pp. 18-19).
#' @export
#' @examples
#' set.seed(1)
#' r <- Bnsipv(y = rnorm(10), D = rbinom(10, 1, 0.5), Z = rnorm(10)); TRUE
Bnsipv <- function(y, D, Z) {
  z <- .bnd_yd(y, D, "Bnsipv")
  zv <- unlist(Z)
  n <- length(z$y)
  if (length(zv) != n) stop("Bnsipv: Z must have one value per unit")
  y0 <- min(z$y)
  y1 <- max(z$y)
  iv <- .bnd_wc_intersect(z$y, z$d, zv, y0, y1)
  iv_lo <- iv[1] - iv[4]
  iv_hi <- iv[2] - iv[3]
  cm <- .bnd_cellmeans(z$y, z$d)
  mtr_lo <- 0
  mtr_hi <- (cm$m1 * cm$p1 + y1 * cm$p0) - (cm$m0 * cm$p0 + y0 * cm$p1)
  lo <- max(iv_lo, mtr_lo)
  hi <- min(iv_hi, mtr_hi)
  refuted <- if (lo > hi || iv[1] > iv[2] || iv[3] > iv[4]) 1 else 0
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), iv_lower = iv_lo,
             iv_upper = iv_hi, mtr_lower = mtr_lo, mtr_upper = mtr_hi,
             refuted = refuted, n = n,
             method = "Partial IV bound under one-sided compliance")
}
