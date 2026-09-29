#' James-Stein shrinkage of a vector of normal means
#'
#' Positive-part James-Stein estimator \eqn{\hat\theta_i = t + c (X_i - t)},
#' \eqn{c = \max(0, 1 - k \sigma^2 / \sum_i (X_i - t)^2)}, with \eqn{k = p - 2}
#' for a fixed target \eqn{t} and \eqn{k = p - 3} when the target is the
#' grand mean estimated from the same data (Efron and Morris). Identical to
#' the Python arm \code{morie.fn.jamste.james_stein}.
#'
#' @param x Numeric vector of observed means, length at least 3.
#' @param target Fixed shrinkage target, or \code{NULL} for the grand mean.
#' @param sigma2 Known sampling variance of each coordinate.
#' @return A list with \code{value} (the shrinkage factor), \code{p},
#'   \code{target}, \code{k}, \code{sigma2}, \code{shrinkage_factor},
#'   \code{js_estimates}, \code{original_means} and
#'   \code{mse_reduction_bound}.
#' @references James, W. and Stein, C. (1961). Estimation with quadratic loss.
#'   Proceedings of the Fourth Berkeley Symposium 1, 361-379.
#'
#'   Efron, B. and Morris, C. (1975). Data analysis using Stein's estimator
#'   and its generalizations. Journal of the American Statistical
#'   Association 70, 311-319.
#' @examples
#' james_stein(c(10, -5, 3, 0.1, -2))$value
#' @export
james_stein <- function(x, target = NULL, sigma2 = 1) {
  x <- as.numeric(x)
  p <- length(x)
  if (p < 3) stop("James-Stein requires >= 3 means (Stein's paradox)")
  if (!(sigma2 > 0)) stop("sigma2 must be positive")
  if (is.null(target)) {
    tgt <- sum(x) / p
    k <- p - 3
  } else {
    tgt <- as.numeric(target)
    k <- p - 2
  }
  d <- x - tgt
  ss <- sum(d * d)
  c0 <- if (ss < 1e-30) 0 else max(0, 1 - k * sigma2 / ss)
  list(value = c0, p = p, target = tgt, k = k, sigma2 = sigma2,
       shrinkage_factor = c0, js_estimates = tgt + c0 * d, original_means = x,
       mse_reduction_bound = if (c0 < 1) round(1 - c0^2, 4) else 0)
}
