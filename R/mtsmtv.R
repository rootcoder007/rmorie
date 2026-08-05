# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intersection of the MTS and the MTR bound on E[Y(d)]
#'
#' Monotone treatment selection and monotone treatment response are
#' separate assumptions, each of which alone identifies an interval
#' containing \code{E[Y(d)]}. Maintaining both means the truth lies in
#' both intervals, so the joint bound is their intersection:
#' \code{lower = max(lower_MTS, lower_MTR)} and
#' \code{upper = min(upper_MTS, upper_MTR)}.
#'
#' Because both components are valid under their own assumption, the
#' intersection is non-empty whenever the two assumptions are jointly
#' consistent with the data; an empty result is evidence against the pair
#' and is flagged rather than returned as a negative width.
#'
#' The components are not recomputed here: MTS is \code{Mtsbound} and MTR
#' is \code{Mtrbound}.
#'
#' @param y Observed outcomes.
#' @param D Observed treatment levels on an ordered scale.
#' @param y_min,y_max A priori outcome support.
#' @param d Level whose mean counterfactual is bounded; defaults to
#'   \code{max(D)}.
#' @return List with lower, upper, width, mts_lower, mts_upper, mtr_lower,
#'   mtr_upper, empty, n, d.
#' @references Manski and Pepper (2000), Econometrica 68(4), 997-1010.
#'   Standard published form; see \code{bdmnsl} for the availability note.
#' @export
Mtsmtv <- function(y, D, y_min, y_max, d = NULL) {
  yv <- .t1_vec(y); z <- .t1_vec(D)
  if (length(yv) == 0L) stop("y is empty")
  if (length(z) != length(yv)) stop("y and D must have the same length")
  lo <- as.numeric(y_min); hi <- as.numeric(y_max)
  if (lo > hi) stop("y_min must not exceed y_max")
  lev <- if (is.null(d)) max(z) else as.numeric(d)
  s <- Mtsbound(yv, z, lev, lo, hi)
  r <- Mtrbound(yv, z, lev, lo, hi)
  lb <- max(s$lower, r$lower)
  ub <- min(s$upper, r$upper)
  .t1_result(lower = lb, upper = ub, width = ub - lb,
             mts_lower = s$lower, mts_upper = s$upper,
             mtr_lower = r$lower, mtr_upper = r$upper,
             empty = if (lb > ub) 1 else 0, n = length(yv), d = lev,
             method = "Combined MTS+MTR bounds (Manski-Pepper 2000)")
}
