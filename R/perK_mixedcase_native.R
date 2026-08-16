# morie native arm -- perK
# Periodic covariance kernel.
#
#   k(x, x') = sigma^2 exp( -2 sin^2( pi (x - x') / p ) / l^2 )
#
# MacKay's warping construction: map x to (cos, sin)(2 pi x / p) and
# apply the squared exponential in that space. Period p in x - x',
# maximum sigma^2 exactly at lag in p Z, and locally squared-
# exponential for |x - x'| << p.
#
# MacKay, D. J. C. (1998) Introduction to Gaussian processes;
# Rasmussen & Williams (2006) GPML Sec. 4.2.3.

#' morie_perK
#'
#' A step of the perK_mixedcase_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param x1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param x2 Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param period Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param lengthscale Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param variance Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{K}, \code{shape}, \code{period}, \code{lengthscale}, \code{variance}, \code{diag_is_variance}, \code{method}.
#' @export
morie_perK <- function(x1, x2 = NULL, period = 1, lengthscale = 1,
                       variance = 1) {
  a <- as.numeric(x1)
  same <- is.null(x2)
  b <- if (same) a else as.numeric(x2)
  p <- as.numeric(period)
  l <- as.numeric(lengthscale)
  s2 <- as.numeric(variance)
  if (p <= 0 || l <= 0 || s2 <= 0) {
    stop("period, lengthscale, variance must be positive")
  }
  s <- sin(pi * outer(a, b, "-") / p)
  K <- s2 * exp(-2 * s * s / (l * l))
  diag_ok <- same && length(a) > 0 &&
    all(abs(diag(K) - s2) < 1e-15)
  list(
    K = K,
    shape = c(length(a), length(b)),
    period = p, lengthscale = l, variance = s2,
    diag_is_variance = diag_ok,
    method = "periodic kernel (MacKay 1998; R&W 2006 Sec. 4.2.3)"
  )
}

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_perk <- morie_perK
