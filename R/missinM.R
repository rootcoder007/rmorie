# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pattern-mixture sensitivity analysis over a shift parameter delta
#'
#' Missingness at random is not testable from the observed data: any
#' value of \code{E\[Y | R = 0\]} is equally consistent with what was seen.
#' A pattern-mixture sensitivity analysis parameterises the departure
#' instead of assuming it away,
#' \code{E\[Y | R = 0\] = E\[Y | R = 1\] + delta}, so the marginal mean traced
#' over the grid is
#' \code{E[Y](delta) = E\[Y|R=1\] + (1 - P(R=1)) delta}.
#'
#' \code{delta = 0} is the MAR answer, recovered exactly. The mean is
#' linear in delta with slope equal to the missingness rate: the analysis
#' is fragile only in proportion to how much data is absent, and with
#' nothing missing no delta can move it.
#'
#' The TIPPING POINT is the delta at which the mean crosses
#' \code{reference}, \code{delta* = (reference - E\[Y|R=1\]) / P(R=0)}, so
#' the conclusion can be stated as "the missing units would have to
#' differ by delta* for this to reverse". With nothing missing there is
#' no such delta and the field is \code{NaN}, not infinity: the
#' conclusion cannot be tipped at all.
#'
#' @param Y Outcomes; entries where \code{R} is 0 are ignored and may be
#'   any placeholder, including \code{NA}.
#' @param R Response indicator, 1 where \code{Y} is observed.
#' @param delta_grid Shift values to trace.
#' @param reference Value whose crossing defines the tipping point.
#' @return List with estimate (mean at delta = 0), means, delta_grid,
#'   mar_mean, p_observed, tipping_delta, n_observed, n.
#' @references Daniels and Hogan (2008), Missing Data in Longitudinal
#'   Studies, Chapman and Hall/CRC. The book was not in the local corpus
#'   and could not be obtained; the mean and tipping point above are
#'   elementary consequences of the shift equation and are stated in full.
#' @export
MissinM <- function(Y, R, delta_grid, reference = 0) {
  r <- .t1_vec(R)
  n <- length(r)
  if (n == 0L) stop("R is empty")
  yraw <- unlist(Y)
  if (length(yraw) != n) stop("Y and R must have the same length")
  if (any(r != 0 & r != 1)) stop("R must be binary 0/1")
  obs <- which(r == 1)
  if (length(obs) == 0L) stop("no observed outcome; nothing to shift from")
  yo <- as.numeric(yraw[obs])
  if (any(is.na(yo))) stop("an outcome marked observed is NaN")
  m1 <- mean(yo)
  p1 <- length(obs) / n
  p0 <- 1 - p1
  grid <- .t1_vec(delta_grid)
  if (length(grid) == 0L) stop("delta_grid is empty")
  means <- m1 + p0 * grid
  tip <- if (p0 > 0) (as.numeric(reference) - m1) / p0 else NaN
  .t1_result(estimate = m1, means = means, delta_grid = grid,
             mar_mean = m1, p_observed = p1, tipping_delta = tip,
             n_observed = length(obs), n = n,
             method = "Delta-shift NMAR sensitivity (pattern mixture)")
}
