# SPDX-License-Identifier: AGPL-3.0-or-later
#' Siegel repeated-median line: the 50 percent breakdown robust regression
#'
#' Siegel, A. F. (1982), "Robust regression using repeated medians", Biometrika
#' 69(1), 242-244, doi:10.1093/biomet/69.1.242.
#'
#' CITATION LIMIT, stated rather than papered over. The Biometrika article is
#' closed access (Semantic Scholar and OpenAlex both report no open-access
#' location) and the Princeton technical-report version, DTIC ADA092660,
#' returns 403 to every fetch tried. The estimator is therefore taken from a
#' source that states it in full and cites Siegel for it: Borowski, M. and
#' Fried, R. (2011), "Robust repeated median regression in moving windows with
#' data-adaptive width selection", SFB 823 Discussion Paper 28/2011, TU
#' Dortmund University, doi:10.17877/de290r-13059, Section 2, equation (3),
#' page 3, read off a rendered page image. That equation is
#' \code{beta = med_i med_{i' != i} (y_i - y_i')/(x_i - x_i')} and
#' \code{level = med_i (y_i - beta x_i)}, and the same page states the property
#' that makes the estimator worth having: a finite-sample replacement breakdown
#' point of \code{floor(n/2)/n}, about 50 percent, which Davies and Gather
#' (2005) show is the maximum possible for a regression-equivariant estimator.
#'
#' The doubled median is the whole idea. Theil-Sen takes ONE median over all
#' n(n-1)/2 pairwise slopes and breaks down at about 29 percent; taking a median
#' within each point first, and then across points, means a minority of
#' arbitrarily corrupted observations cannot move either level of the
#' calculation. Half the sample can be replaced by nonsense and the line does
#' not move to infinity.
#'
#' Points sharing an x value with no other point contribute no inner median and
#' are skipped; if no pair of distinct x values exists at all, no slope is
#' defined and that is an error rather than a number.
#'
#' @param x,y Predictor and response, same length, at least two points with two
#'   distinct x values.
#' @return List with \code{estimate} (the slope), \code{slope},
#'   \code{intercept}, \code{fitted}, \code{residuals}, \code{n},
#'   \code{n_used}, \code{breakdown_point}, \code{method}.
#' @references Siegel, A. F. (1982), Biometrika 69(1):242-244,
#'   doi:10.1093/biomet/69.1.242; the form used is equation (3) of Borowski and
#'   Fried (2011), doi:10.17877/de290r-13059.
#' @examples
#' Sieg(1:7, 3 * (1:7) - 2)$estimate  # exactly 3
#' @export
Sieg <- function(x, y) {
  xv <- .s03vec(x)
  yv <- .s03vec(y)
  if (length(xv) != length(yv)) {
    stop("siegel_repeated: x and y must have the same length")
  }
  n <- length(xv)
  if (n < 2L) stop("siegel_repeated: need at least two points")
  inner <- numeric(0)
  for (i in seq_len(n)) {
    keep <- xv != xv[i]
    if (any(keep)) {
      inner <- c(inner, median((yv[keep] - yv[i]) / (xv[keep] - xv[i])))
    }
  }
  if (length(inner) == 0L) {
    stop("siegel_repeated: every x is identical, so no pairwise slope exists ",
         "and no line is defined")
  }
  slope <- median(inner)
  intercept <- median(yv - slope * xv)
  fitted <- intercept + slope * xv
  list(estimate = slope, slope = slope, intercept = intercept,
       fitted = fitted, residuals = yv - fitted,
       n = as.integer(n), n_used = as.integer(length(inner)),
       breakdown_point = (n %/% 2L) / n,
       method = "Siegel (1982) repeated-median line, breakdown floor(n/2)/n")
}
