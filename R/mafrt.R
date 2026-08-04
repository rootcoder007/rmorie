# SPDX-License-Identifier: AGPL-3.0-or-later
#' Variance-stabilising double arcsine transform of a proportion
#'
#' A single arcsine blows up at zero and one; the double arcsine
#' averages two neighbouring single arcsines so a study with no events
#' still contributes, which is the case that matters in rare-event
#' synthesis. The variance depends only on n, so the weights are fixed
#' rather than estimated.
#'
#' Formula: \code{FT = asin sqrt(x/(n+1)) + asin sqrt((x+1)/(n+1))},
#' \code{Var(FT) = 1/(n + 1/2)}.
#'
#' @param x Event counts.
#' @param n Sample sizes, elementwise against x.
#' @return List with \code{ft}, \code{var}, \code{se}, \code{k}.
#' @references Freeman, M. F. & Tukey, J. W. (1950). Ann Math Statist
#'   21:607-611; Miller, J. J. (1978). Amer Statist 32:138.
#' @export
Mafrt <- function(x, n) {
  xv <- as.numeric(x); nv <- as.numeric(n)
  if (length(nv) == 1L) nv <- rep(nv, length(xv))
  ft <- asin(sqrt(xv / (nv + 1))) + asin(sqrt((xv + 1) / (nv + 1)))
  vr <- 1 / (nv + 0.5)
  .t1_result(ft = ft, var = vr, se = sqrt(vr), k = length(ft),
             method = "Freeman-Tukey double arcsine transform")
}
