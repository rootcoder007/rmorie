# SPDX-License-Identifier: AGPL-3.0-or-later
#' Monotone treatment selection bounds (alias of Mtsbound)
#'
#' This module is an ALIAS. The bound is implemented once, in
#' \code{Mtsbound} (module \code{bdmnsl}); this entry point only supplies
#' the argument spelling used by the selection-bounds literature and the
#' direction switch, then delegates.
#'
#' Under monotone treatment selection,
#' \code{E[Y(d)] <= P(X<=d) E[Y|X=d] + P(X>d) ymax} and
#' \code{E[Y(d)] >= P(X<d) ymin + P(X>=d) E[Y|X=d]}.
#' \code{monotone = FALSE} reverses the selection inequality, which is the
#' same bound on \code{-Y} over the reflected support with the sides
#' swapped back.
#'
#' @param Y Observed outcomes.
#' @param X Observed (selected) treatment levels.
#' @param monotone TRUE if higher selection implies weakly higher outcomes.
#' @param d Level whose mean counterfactual is bounded; defaults to
#'   \code{max(X)}.
#' @param ymin,ymax A priori outcome support; default to the observed range.
#' @return List with lower, upper, width, condmean, pbelow, pat, pabove,
#'   n, d.
#' @references Manski and Pepper (2000), Econometrica 68(4), 997-1010.
#'   Standard published form; see \code{bdmnsl} for the availability note.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Mtsbnd(V, V)
Mtsbnd <- function(Y, X, monotone = TRUE, d = NULL, ymin = NULL, ymax = NULL) {
  y <- .t1_vec(Y); x <- .t1_vec(X)
  if (length(y) == 0L) stop("Y is empty")
  if (length(x) != length(y)) stop("Y and X must have the same length")
  lo <- if (is.null(ymin)) min(y) else as.numeric(ymin)
  hi <- if (is.null(ymax)) max(y) else as.numeric(ymax)
  lev <- if (is.null(d)) max(x) else as.numeric(d)
  if (isTRUE(monotone)) return(Mtsbound(y, x, lev, lo, hi))
  r <- Mtsbound(-y, x, lev, -hi, -lo)
  .t1_result(lower = -r$upper, upper = -r$lower, width = r$width,
             condmean = -r$condmean, pbelow = r$pbelow, pat = r$pat,
             pabove = r$pabove, n = r$n, d = r$d,
             method = "Monotone treatment selection bounds (Manski-Pepper 2000)")
}
