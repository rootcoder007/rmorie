# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized ESD test for outliers (Rosner 1983)
#'
#' Source FETCHED: NIST/SEMATECH e-Handbook of Statistical Methods,
#' section 1.3.5.17.3, which states Rosner (1983, Technometrics 25,
#' 165-172) in full: \code{R_i = max |x_i - xbar| / s} recomputed after
#' each removal, with critical values
#' \code{lambda_i = (n-i) t_{p,n-i-1} / sqrt((n-i-1+t^2)(n-i+1))} and
#' \code{p = 1 - alpha / (2(n-i+1))}.  The number of outliers is the
#' largest \code{i} with \code{R_i > lambda_i}.  The handbook worked
#' example (Rosner 54-point data set, r = 10, alpha = 0.05) gives
#' \code{R_1 = 3.118}, \code{lambda_1 = 3.158} and three outliers.
#'
#' @param x Numeric vector, approximately normal under H0.
#' @param alpha Significance level.  Default 0.05.
#' @param r Upper bound on the number of outliers.  Default
#'   \code{max(1, n \%/\% 10)}.
#' @return list: n_outliers, outlier_index, R, lam, alpha, r, n, method.
#' @examples
#' Gesd(c(1:20, 40), 0.05, 3)$n_outliers
#' @export
Gesd <- function(x, alpha = 0.05, r = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(r)) r <- max(1, n %/% 10)
  r <- as.integer(r)
  if (n < 3 || r < 1 || r > n - 2) stop("need n>=3 and 1<=r<=n-2")
  alpha <- as.numeric(alpha)
  keep <- seq_len(n)
  R <- numeric(r)
  removed <- integer(r)
  for (i in seq_len(r)) {
    v <- x[keep]
    dev <- abs(v - mean(v))
    s <- stats::sd(v)
    j <- which.max(dev)
    R[i] <- if (s > 0) dev[j] / s else Inf
    removed[i] <- keep[j]
    keep <- keep[-j]
  }
  lam <- numeric(r)
  for (i in seq_len(r)) {
    p <- 1 - alpha / (2 * (n - i + 1))
    nu <- n - i - 1
    tq <- stats::qt(p, nu)
    lam[i] <- (n - i) * tq / sqrt((nu + tq^2) * (n - i + 1))
  }
  nout <- 0L
  for (i in seq_len(r)) if (R[i] > lam[i]) nout <- i
  list(
    n_outliers = nout,
    outlier_index = if (nout > 0) removed[seq_len(nout)] else integer(0),
    R = R, lam = lam, alpha = alpha, r = r, n = n,
    method = "Generalized ESD test (Rosner 1983)"
  )
}
