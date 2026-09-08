# Rao-Scott corrected chi-square for complex surveys.
# Source: Rao & Scott (1981), JASA 76(374), 221-230, Secs. 2-3
# (fetched-wave3/The Analysis of Categorical Data from Complex
# Sample Surveys...pdf).  Mirrors Python morie.fn.raoscot exactly.

#' Rao-Scott first-order corrected goodness-of-fit chi-square
#'
#' X^2 / lambda_bar treated as chi-square_\{k-1\}, with lambda_bar the
#' mean generalized design effect: trace-based when the covariance V
#' of the estimated proportions is supplied, or the paper's cell-deff
#' estimate sum_i (1 - p0_i) d_i/(k - 1).  If V = c P0 (uniform
#' clustering), every eigenvalue is c and X^2/c is exactly
#' chi-square_\{k-1\}.
#'
#' @param p_hat Estimated cell proportions (sum to 1).
#' @param p0 Hypothesized positive proportions (sum to 1).
#' @param n Sample size.
#' @param V Optional n * covariance matrix of p_hat.
#' @param deffs Optional positive cell design effects.
#' @return A list with elements \code{statistic}, \code{corrected},
#'   \code{lambda_bar}, \code{df}, \code{p_value}, \code{method}.
#' @references Rao, J. N. K. and Scott, A. J. (1981). The analysis of
#'   categorical data from complex sample surveys. JASA, 76(374),
#'   221-230.
#' @export
morie_raoscot <- function(p_hat, p0, n, V = NULL, deffs = NULL) {
  ph <- as.numeric(p_hat)
  p0v <- as.numeric(p0)
  k <- length(ph)
  if (length(p0v) != k || k < 2) stop("p_hat and p0 must be paired, k >= 2")
  if (any(p0v <= 0)) stop("p0 must be positive")
  if (abs(sum(ph) - 1) > 1e-6 || abs(sum(p0v) - 1) > 1e-6) {
    stop("proportions must sum to 1")
  }
  n <- as.integer(n)
  if (n < 2) stop("n must be at least 2")
  x2 <- n * sum((ph - p0v)^2 / p0v)
  df <- k - 1
  if (!is.null(V)) {
    Vm <- as.matrix(V)
    tr <- sum(diag(Vm) / p0v) - sum(Vm)
    lam <- tr / df
  } else if (!is.null(deffs)) {
    dv <- as.numeric(deffs)
    if (length(dv) != k || any(dv <= 0)) stop("need k positive cell deffs")
    lam <- sum((1 - p0v) * dv) / df
  } else {
    lam <- 1
  }
  if (lam <= 0) stop("estimated mean design effect is not positive")
  xc <- x2 / lam
  list(statistic = x2, corrected = xc, lambda_bar = lam, df = df,
       p_value = pchisq(xc, df, lower.tail = FALSE),
       method = "Rao-Scott (1981) first-order corrected chi-square")
}
