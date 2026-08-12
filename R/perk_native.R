# Periodic covariance kernel.
# Source: MacKay (1998), Introduction to Gaussian processes
# (fetched-wave3/mackay-1998-gp-intro.pdf; the (cos, sin) warping);
# Rasmussen & Williams (2006), GPML, Sec. 4.2.3
# (fetched-wave3/rasmussen-williams-2006-gpml-ch4.pdf):
#   k(x, x') = sigma^2 exp(-2 sin^2(pi (x - x')/p) / l^2).
# Mirrors Python morie.fn.perK exactly.

#' Periodic kernel matrix
#'
#' MacKay's warping construction u(x) = (cos, sin)(2 pi x / p) with a
#' squared-exponential kernel in u-space, giving
#' k(x, x') = sigma^2 exp(-2 sin^2(pi (x - x')/p)/l^2): positive
#' definite, period p in x - x', maximum sigma^2 on the diagonal.
#'
#' @param x1 Numeric vector of inputs.
#' @param x2 Optional second input vector (defaults to \code{x1}).
#' @param period Period p > 0.
#' @param lengthscale Lengthscale l > 0.
#' @param variance Signal variance sigma^2 > 0.
#' @return A list with elements \code{K} (matrix), \code{shape},
#'   \code{period}, \code{lengthscale}, \code{variance},
#'   \code{diag_is_variance}, \code{method}.
#' @references MacKay, D. J. C. (1998). Introduction to Gaussian
#'   processes. In Bishop (ed.), Neural Networks and Machine
#'   Learning, Springer.  Rasmussen, C. E. and Williams, C. K. I.
#'   (2006). Gaussian Processes for Machine Learning, MIT Press,
#'   Sec. 4.2.3.
#' @export
morie_perk <- function(x1, x2 = NULL, period = 1, lengthscale = 1,
                       variance = 1) {
  a <- as.numeric(x1)
  b <- if (is.null(x2)) a else as.numeric(x2)
  p <- as.numeric(period)
  l <- as.numeric(lengthscale)
  s2 <- as.numeric(variance)
  if (p <= 0 || l <= 0 || s2 <= 0) {
    stop("period, lengthscale, variance must be positive")
  }
  K <- outer(a, b, function(xa, xb) {
    s <- sin(pi * (xa - xb) / p)
    s2 * exp(-2 * s * s / (l * l))
  })
  diag_ok <- is.null(x2) && all(abs(diag(K) - s2) < 1e-15)
  list(K = K,
       shape = dim(K),
       period = p, lengthscale = l, variance = s2,
       diag_is_variance = diag_ok,
       method = "periodic kernel (MacKay 1998; R&W 2006 Sec. 4.2.3)")
}
