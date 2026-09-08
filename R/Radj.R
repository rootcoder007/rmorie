# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adjusted coefficient of determination for the linear model
#'
#' Source READ FROM THE CORPUS PDF, page rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.2.7 "Variable Selection Method", printed page 813,
#' equation (8.38):
#' \code{R2_a = 1 - \[RSS / (n - (p + 1))\] / \[SSY / (n - 1)\]
#' = 1 - (n - 1)/(n - (p + 1)) (1 - R2)}, with p the number of
#' independent variables excluding the intercept.  The point the book
#' makes in giving it: R2 never decreases when a variable is added, so
#' models of different size must be compared on R2_a.
#'
#' @param n Number of observations.
#' @param p Number of independent variables, excluding the intercept.
#' @param r2 Unadjusted coefficient of determination.
#' @param rss,ssy Residual and total sum of squares, used when r2 is not
#'   given; ssy must be positive.
#' @return list: radj, r2, n, p, df_resid.
#' @examples
#' Radj(20, 2, r2 = 0.8)$radj
#' @export
Radj <- function(n, p, r2 = NULL, rss = NULL, ssy = NULL) {
  n <- as.integer(n)[1]
  p <- as.integer(p)[1]
  if (is.na(p) || p < 0L) stop("p must be non-negative")
  if (is.na(n) || n - (p + 1L) < 1L) stop("n - (p + 1) must be at least 1")
  if (is.null(r2)) {
    if (is.null(rss) || is.null(ssy)) stop("supply either r2, or both rss and ssy")
    rss <- as.numeric(rss)[1]
    ssy <- as.numeric(ssy)[1]
    if (!is.finite(rss) || rss < 0) stop("rss must be finite and non-negative")
    if (!is.finite(ssy) || ssy <= 0) stop("ssy must be finite and positive")
    r2 <- 1 - rss / ssy
  } else {
    r2 <- as.numeric(r2)[1]
    if (!is.finite(r2)) stop("r2 must be finite")
  }
  val <- 1 - (n - 1) / (n - (p + 1)) * (1 - r2)
  list(radj = val, r2 = r2, n = n, p = p, df_resid = n - (p + 1L))
}
