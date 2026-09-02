# SPDX-License-Identifier: AGPL-3.0-or-later
#' The coordinates of a funnel plot, and the funnel itself
#'
#' A funnel plot is only interpretable against its own null: without the
#' contour that a no-bias world would produce, an eye reading the scatter
#' is reading noise. The contour is the fixed-effect summary plus and
#' minus a critical value times each study's own standard error, which is
#' why the plot is a funnel rather than a band.
#'
#' Formula: points \code{(y_i, se_i)}; contour \code{theta_FE +-
#' z_{1-alpha/2} se} with \code{theta_FE = sum(y_i/v_i)/sum(1/v_i)} --
#' Light and Pillemer (1984), Chapter 3.
#'
#' @param yi Study effect estimates.
#' @param se_i Their standard errors, strictly positive.
#' @param level Contour level.
#' @return List with \code{x_funnel}, \code{y_funnel}, \code{precision},
#'   \code{center}, \code{ci_lo}, \code{ci_hi}, \code{k}.
#' @references Light, R. J. and Pillemer, D. B. (1984). Summing Up: The
#'   Science of Reviewing Research. Harvard University Press, Chapter 3.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Mafnpr(V, V)
Mafnpr <- function(yi, se_i, level = 0.95) {
  y <- as.numeric(yi)
  s <- as.numeric(se_i)
  k <- length(y)
  if (k == 0L) stop("no studies")
  if (length(s) != k)
    stop("effects and standard errors must have equal length")
  if (any(s <= 0)) stop("standard errors must be strictly positive")
  w <- 1 / s^2
  sw <- sum(w)
  center <- sum(w * y) / sw
  z <- .s03qnorm(1 - (1 - as.numeric(level)) / 2)
  .t1_result(x_funnel = y, y_funnel = s, precision = 1 / s, center = center,
             ci_lo = center - z * s, ci_hi = center + z * s,
             se_center = sqrt(1 / sw), z_crit = z, k = k,
             method = "Funnel-plot coordinates with pseudo-confidence contours")
}
