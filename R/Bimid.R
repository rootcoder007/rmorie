#' Biweight midvariance
#'
#' A robust measure of dispersion.  With \code{M} the sample median and
#' \code{MAD} the unscaled median absolute deviation, set
#' \code{Y = (x - M) / (9 MAD)} and \code{a = 1} when \code{|Y| < 1} and
#' \code{0} otherwise.  Then
#' \code{zeta = sqrt(n) sqrt(sum(a (x - M)^2 (1 - Y^2)^4)) /
#' |sum(a (1 - Y^2)(1 - 5 Y^2))|}, and the biweight midvariance is
#' \code{zeta^2}.
#'
#' The \code{MAD} entering \code{Y} is the raw median absolute deviation,
#' not the rescaled \code{MADN}; the 9 is the tuning constant that decides
#' which points receive weight zero.
#'
#' @param x Numeric vector; at least two observations, with a non-zero
#'   median absolute deviation.  A sample more than half of whose values
#'   are tied at the median has \code{MAD = 0} and no biweight
#'   midvariance.
#' @return A list with components \code{estimate} (= \code{zeta^2}),
#'   \code{zeta}, \code{med}, \code{mad}, \code{n_used}, \code{n} and
#'   \code{method}.
#' @references
#' Wilcox, R. R. (2017). \emph{Modern Statistics for the Social and
#' Behavioral Sciences: A Practical Introduction}, 2nd edn. CRC Press,
#' Box 2.1, equation (2.12), p.31.
#' @examples
#' Bimid(c(-1, 0, 1))$zeta
#' @export
Bimid <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("Bimid: need at least 2 observations")
  if (anyNA(x)) stop("Bimid: x contains a missing value")
  med <- stats::median(x)
  mad <- stats::median(abs(x - med))
  if (!(mad > 0)) stop("Bimid: the median absolute deviation is zero")
  y <- (x - med) / (9 * mad)
  a <- abs(y) < 1
  y2 <- y * y
  num <- sum(((x - med)^2 * (1 - y2)^4)[a])
  den <- abs(sum(((1 - y2) * (1 - 5 * y2))[a]))
  if (!(den > 0)) stop("Bimid: the biweight denominator vanished")
  zeta <- sqrt(n) * sqrt(num) / den
  list(estimate = zeta * zeta, zeta = zeta, med = med, mad = mad,
       n_used = sum(a), n = n,
       method = "Wilcox (2017) biweight midvariance, eq. (2.12)")
}
