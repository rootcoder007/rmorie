# SPDX-License-Identifier: AGPL-3.0-or-later
#' Manski-Pepper monotone treatment response bounds, either direction
#'
#' This module is an ALIAS. The increasing-response bound is implemented
#' once, in \code{Mtrbound} (module \code{bdmnto}); this entry point adds
#' the direction switch and delegates.
#'
#' Under monotone treatment response \code{Y_i(d) >= y_i} when
#' \code{d >= z_i} and \code{Y_i(d) <= y_i} when \code{d <= z_i}, giving
#' \code{L_i = y_i if z_i <= d else ymin}, \code{U_i = y_i if z_i >= d
#' else ymax} and \code{E[Y(d)] in [mean(L), mean(U)]}.
#'
#' \code{direction = "decreasing"} asserts the reverse monotonicity, which
#' is the same statement about the level scale run backwards and is
#' obtained by negating the treatment levels before delegating.
#'
#' @param y Observed outcomes.
#' @param D Observed treatment levels on an ordered scale.
#' @param direction Either "increasing" or "decreasing".
#' @param d Level whose mean counterfactual is bounded; defaults to
#'   \code{max(D)}.
#' @param y_min,y_max A priori outcome support; default the observed range.
#' @return List with lower, upper, width, nfixed, n, d.
#' @references Manski (1997), Econometrica 65(6), 1311-1334; Manski and
#'   Pepper (2000), Econometrica 68(4), 997-1010. Standard published
#'   form; see \code{bdmnto} for the availability note.
#' @export
Mtrbnd <- function(y, D, direction = "increasing", d = NULL,
                   y_min = NULL, y_max = NULL) {
  yv <- .t1_vec(y); z <- .t1_vec(D)
  if (length(yv) == 0L) stop("y is empty")
  if (length(z) != length(yv)) stop("y and D must have the same length")
  if (!direction %in% c("increasing", "decreasing")) {
    stop("direction must be 'increasing' or 'decreasing'")
  }
  lo <- if (is.null(y_min)) min(yv) else as.numeric(y_min)
  hi <- if (is.null(y_max)) max(yv) else as.numeric(y_max)
  lev <- if (is.null(d)) max(z) else as.numeric(d)
  r <- if (direction == "increasing") {
    Mtrbound(yv, z, lev, lo, hi)
  } else {
    Mtrbound(yv, -z, -lev, lo, hi)
  }
  .t1_result(lower = r$lower, upper = r$upper, width = r$width,
             nfixed = r$nfixed, n = r$n, d = lev,
             method = "Monotone treatment response bounds (Manski 1997)")
}
