# SPDX-License-Identifier: AGPL-3.0-or-later
#' Put every study on the same vertical scale before comparing them
#'
#' A forest plot draws each study at its own precision, so the eye
#' compares intervals of different widths and cannot see heterogeneity.
#' Dividing by the standard error puts every point on a unit-variance
#' axis: under homogeneity the points scatter with unit standard deviation
#' about a line through the origin, so a study more than two units off the
#' line is visibly discrepant regardless of its size.
#'
#' Formula: \code{z_i = y_i/se_i} against \code{x_i = 1/se_i}; the
#' through-the-origin slope \code{sum(z_i x_i)/sum(x_i^2)} is exactly the
#' inverse-variance pooled estimate -- Galbraith (1988).
#'
#' @param yi Study effect estimates.
#' @param se_i Their standard errors, strictly positive.
#' @return List with \code{z}, \code{x}, \code{slope}, \code{resid},
#'   \code{n_outside_2}, \code{k}.
#' @references Galbraith, R. F. (1988). Statistics in Medicine
#'   7(8):889-894. \doi{10.1002/sim.4780070807}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Magal(V, V)
Magal <- function(yi, se_i) {
  y <- as.numeric(yi); s <- as.numeric(se_i); k <- length(y)
  if (k == 0L) stop("no studies")
  if (length(s) != k)
    stop("effects and standard errors must have equal length")
  if (any(s <= 0)) stop("standard errors must be strictly positive")
  z <- y / s; x <- 1 / s
  slope <- sum(z * x) / sum(x^2)
  resid <- z - slope * x
  .t1_result(z = z, x = x, slope = slope, resid = resid,
             n_outside_2 = sum(abs(resid) > 2), k = k,
             method = "Galbraith (radial) plot")
}
