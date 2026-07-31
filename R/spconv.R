# SPDX-License-Identifier: AGPL-3.0-or-later
#' Convolution representation of a stationary random field.
#'
#' Convolving a kernel K with a white-noise field gives a second-order
#' stationary process whose covariance is the convolution of the kernel
#' with itself, Cov[Z(s), Z(s+h)] = sigma_x^2 int K(u) K(u+h) du. The
#' construction is useful because ANY kernel yields a valid covariance --
#' positive definiteness is automatic rather than something to check.
#'
#' The book's own worked case: a uniform (boxcar) kernel on the line
#' convolves to a TENT correlation function, which is also the d = 1
#' member of the spherical family.
#'
#' @param kernel Function K(u); the boxcar on [-1/2, 1/2] by default.
#' @param h Lags at which to evaluate the covariance.
#' @param sigma2_x White-noise variance, positive.
#' @param half_width,n Quadrature half-width and node count.
#' @return Named list: h, covariance, correlation, variance.
#' @references Schabenberger & Gotway (2005), Sec 2.4.2; the boxcar case
#'   reappears at Sec 4.3.3, p. 146.
#' @examples
#' spconv(h = seq(0, 1, length.out = 5))$correlation
#' @export
spconv <- function(kernel = NULL, h = NULL, sigma2_x = 1, half_width = 5,
                   n = 40001) {
  if (is.null(kernel)) kernel <- function(u) as.numeric(abs(u) <= 0.5)
  if (!is.function(kernel)) stop("`kernel` must be a function K(u)")
  if (sigma2_x <= 0) stop("`sigma2_x` must be > 0")
  if (is.null(h)) h <- seq(0, 2, length.out = 41)
  h <- as.numeric(h)
  u <- seq(-half_width, half_width, length.out = n)
  ku <- as.numeric(kernel(u))
  trap <- function(y, x) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
  cov <- vapply(h, function(hh) sigma2_x * trap(ku * as.numeric(kernel(u + hh)), u),
                numeric(1))
  c0 <- sigma2_x * trap(ku * ku, u)
  list(h = h, covariance = cov,
       correlation = if (c0 > 0) cov / c0 else cov * NA_real_, variance = c0)
}
