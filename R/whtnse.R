# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: Gamma_l = n^-1 sum_{t=l+1}^{n} e_t e_{t-l}'.
.whtnse_autocov <- function(E, lag) {
  n <- nrow(E)
  if (lag == 0L) return(crossprod(E) / n)
  crossprod(E[(lag + 1L):n, , drop = FALSE],
            E[1L:(n - lag), , drop = FALSE]) / n
}

#' Hosking's multivariate portmanteau test for white noise
#'
#' Hosking (1980) generalised the univariate portmanteau statistic to
#' several series at once. His modified form is
#'
#' \deqn{\tilde Q_m = n^2 \sum_{\ell=1}^{m} (n-\ell)^{-1}
#'       \hat r_\ell' (\hat R_0^{-1} \otimes \hat R_0^{-1}) \hat r_\ell}
#'
#' with \eqn{\hat r_\ell = vec(\hat R_\ell')},
#' \eqn{\hat R_\ell = \hat L' \hat\Gamma_\ell \hat L},
#' \eqn{\hat L \hat L' = \hat\Gamma_0^{-1}}, and
#' \eqn{\hat\Gamma_\ell = n^{-1} \sum_{t=\ell+1}^{n} e_t e_{t-\ell}'}.
#'
#' The Kronecker form collapses. Because
#' \eqn{\hat L \hat L' = \hat\Gamma_0^{-1}}, the standardised lag-zero
#' matrix \eqn{\hat R_0 = \hat L' \hat\Gamma_0 \hat L} is the identity, so
#' the quadratic form is \eqn{tr(\hat R_\ell' \hat R_\ell)}. Substituting
#' back gives the trace form computed here,
#'
#' \deqn{\tilde Q_m = n^2 \sum_{\ell=1}^{m} (n-\ell)^{-1}
#'       tr(\hat\Gamma_\ell' \hat\Gamma_0^{-1} \hat\Gamma_\ell
#'          \hat\Gamma_0^{-1})}
#'
#' which never forms the \eqn{k^2 \times k^2} Kronecker product.
#'
#' Under the null the statistic is asymptotically chi-squared on
#' \eqn{k^2(m - fitdf)} degrees of freedom.
#'
#' Mirrors \code{morie.fn.whtnse} on the Python side; both agree with
#' \code{portes::Hosking} to every printed digit.
#'
#' @param x Numeric matrix (n x k) to test, usually residuals from a
#'   fitted model. A transposed panel is detected. Columns are demeaned.
#' @param lags Number of lags \code{m} in the sum. Default 10. Must be
#'   smaller than the series length.
#' @param cdf Optional function giving the null CDF of the statistic,
#'   replacing the asymptotic chi-square. Use it to supply a Monte Carlo
#'   null, which is more accurate at the small \code{n} and large
#'   \code{lags} where the asymptotic approximation is known to be poor.
#' @param fitdf Parameters already estimated from the series, \code{p + q}
#'   for a fitted VARMA(p, q). Zero when testing a raw series. The
#'   degrees of freedom lose \code{k^2 * fitdf}.
#' @param modified Use the \code{(n - lag)} weighting of Hosking's
#'   equation (9). \code{FALSE} gives the unmodified statistic, which
#'   weights every lag by \code{n}.
#' @return Named list with \code{statistic}, \code{p_value}, \code{df},
#'   \code{lags}, \code{n}, \code{k}, \code{fitdf}, \code{method}.
#' @references Hosking JRM (1980). The multivariate portmanteau
#'   statistic. \emph{Journal of the American Statistical Association},
#'   75(371), 602-608.
#'
#'   Mahdi E (2020). portes: an R package for portmanteau tests in time
#'   series models. arXiv:2005.00931, equation (9).
#' @examples
#' set.seed(1)
#' morie_portmanteau_hosking(matrix(rnorm(600), 200, 3), lags = 5)$p_value
#' @export
morie_portmanteau_hosking <- function(x, lags = 10L, cdf = NULL,
                                      fitdf = 0L, modified = TRUE) {
  E <- as.matrix(x)
  if (nrow(E) < ncol(E)) E <- t(E)
  n <- nrow(E)
  k <- ncol(E)
  m <- as.integer(lags)
  if (m < 1L) stop("lags must be at least 1, got ", m, ".", call. = FALSE)
  if (m >= n) {
    stop("lags must be smaller than the series length; got lags=", m,
         ", n=", n, ".", call. = FALSE)
  }
  if (n <= k) {
    stop("Need more observations than variables, got n=", n, ", k=", k, ".",
         call. = FALSE)
  }
  fitdf <- as.integer(fitdf)
  if (fitdf < 0L) {
    stop("fitdf must not be negative, got ", fitdf, ".", call. = FALSE)
  }
  if (m <= fitdf) {
    stop("lags must exceed fitdf, else the test has no degrees of freedom; ",
         "got lags=", m, ", fitdf=", fitdf, ".", call. = FALSE)
  }

  E <- sweep(E, 2L, colMeans(E), "-")
  G0 <- .whtnse_autocov(E, 0L)
  G0_inv <- tryCatch(solve(G0), error = function(e) NULL)
  if (is.null(G0_inv)) {
    stop("Lag-zero autocovariance is singular; the columns are collinear.",
         call. = FALSE)
  }

  total <- 0
  for (lag in seq_len(m)) {
    Gl <- .whtnse_autocov(E, lag)
    term <- sum(diag(t(Gl) %*% G0_inv %*% Gl %*% G0_inv))
    total <- total + if (modified) term / (n - lag) else term
  }
  statistic <- if (modified) n * n * total else n * total

  df <- k * k * (m - fitdf)
  p <- if (!is.null(cdf)) 1 - cdf(statistic) else stats::pchisq(statistic, df, lower.tail = FALSE)

  list(
    statistic = statistic,
    p_value = p,
    df = df,
    lags = m,
    n = n, k = k,
    fitdf = fitdf,
    method = if (modified) {
      "Hosking (1980) modified multivariate portmanteau"
    } else {
      "Hosking (1980) multivariate portmanteau"
    }
  )
}
