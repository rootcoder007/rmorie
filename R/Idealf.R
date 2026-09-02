#' Ideal fourths
#'
#' The lower and upper ideal-fourth estimates of the quartiles and the
#' resulting interquartile range.  With the observations in ascending
#' order, let \code{j} be the integer part of \code{n/4 + 5/12} and
#' \code{h = n/4 + 5/12 - j}.  Then
#' \code{q1 = (1 - h) X\[j\] + h X\[j + 1\]} and, with \code{k = n - j + 1},
#' \code{q2 = (1 - h) X\[k\] + h X\[k - 1\]}.
#'
#' Note the descending indexing in the upper fourth: it interpolates from
#' \code{X\[k\]} back towards \code{X\[k - 1\]}, so \code{q2} mirrors
#' \code{q1}.  Writing \code{X\[k + 1\]} instead breaks that symmetry.
#'
#' @param x Numeric vector; at least three observations, otherwise the
#'   index \code{j} falls below one.
#' @return A list with components \code{q1}, \code{q2}, \code{iqr},
#'   \code{j}, \code{h}, \code{n}, \code{estimate} (= \code{q1}) and
#'   \code{method}.
#' @references
#' Wilcox, R. R. (2017). \emph{Modern Statistics for the Social and
#' Behavioral Sciences: A Practical Introduction}, 2nd edn. CRC Press,
#' section 2.4.3, equations (2.6)-(2.8), p.27.
#' @examples
#' x <- c(-29.6, -20.9, -19.7, -15.4, -12.3, -8.0,
#'        -4.3, 0.8, 2.0, 6.2, 11.2, 25.0)
#' Idealf(x)$q1
#' @export
Idealf <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 3L) stop("Idealf: need at least 3 observations")
  if (anyNA(x)) stop("Idealf: x contains a missing value")
  xs <- sort(x)
  g <- n / 4 + 5 / 12
  j <- as.integer(floor(g))
  h <- g - j
  kk <- n - j + 1L
  q1 <- (1 - h) * xs[j] + h * xs[j + 1L]
  q2 <- (1 - h) * xs[kk] + h * xs[kk - 1L]
  list(q1 = q1, q2 = q2, iqr = q2 - q1, j = j, h = h, n = n,
       estimate = q1,
       method = "Wilcox (2017) ideal fourths, eq. (2.6)-(2.8)")
}
