#' Lilliefors test for normality with the Dallal-Wilkinson p-value
#'
#' The Kolmogorov-Smirnov distance to the normal distribution function with
#' the sample mean and standard deviation plugged in, \eqn{D = \max(D^+,
#' D^-)}. Because the two parameters are estimated, the null distribution of
#' \eqn{D} is not Kolmogorov's; the p-value is the Dallal and Wilkinson (1986)
#' analytic approximation below 0.1 and Stephens' (1974) modified statistic
#' \eqn{(\sqrt n - 0.01 + 0.85/\sqrt n) D} above it, as in
#' \code{nortest::lillie.test}. Identical to the Python arm
#' \code{morie.fn.lilef.lilef}.
#'
#' @param x Numeric sample (at least four values). For a matrix the first
#'   slice along \code{axis} is tested.
#' @param axis 0 tests the first row of a matrix, 1 the first column.
#' @param cdf Ignored; kept for signature parity with the Python arm.
#' @return A list with \code{statistic}, \code{p_value},
#'   \code{critical_value} (Lilliefors' large-sample 5 percent point
#'   0.886/sqrt(n)), \code{interpretation} (\code{"reject"} when the p-value
#'   is below 0.05), \code{mean} and \code{std}.
#' @references Lilliefors, H. W. (1967). On the Kolmogorov-Smirnov test for
#'   normality with mean and variance unknown. Journal of the American
#'   Statistical Association 62, 399-402.
#'
#'   Dallal, G. E. and Wilkinson, L. (1986). An analytic approximation to the
#'   distribution of Lilliefors's test statistic for normality. The American
#'   Statistician 40, 294-296.
#'
#'   Stephens, M. A. (1974). EDF statistics for goodness of fit and some
#'   comparisons. Journal of the American Statistical Association 69,
#'   730-737.
#' @examples
#' lilef(c(2.1, 3.4, 1.9, 5.6, 2.8, 3.1, 9.9, 2.5, 3.3, 2.7))$p_value
#' @export
lilef <- function(x, axis = 0, cdf = NULL) {
  if (is.matrix(x)) x <- if (axis == 0) x[1, ] else x[, 1]
  v <- as.numeric(x)
  n <- length(v)
  if (n < 4) stop("Sample size must be at least 4 for Lilliefors test")
  m <- sum(v) / n
  s <- sqrt(sum((v - m)^2) / (n - 1))
  if (!(s > 0)) stop("Sample has zero variance; the normal fit is degenerate")
  f <- stats::pnorm(sort((v - m) / s))
  i <- seq_len(n)
  d <- max(pmax(i / n - f, f - (i - 1) / n))
  p <- .lillie_p_norm(d, n)
  list(statistic = d, p_value = p, critical_value = 0.886 / sqrt(n),
       interpretation = if (p < 0.05) "reject" else "not reject",
       mean = m, std = s)
}
