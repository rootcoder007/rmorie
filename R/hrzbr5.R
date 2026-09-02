# SPDX-License-Identifier: AGPL-3.0-or-later
#' Higher-order kernels: the bias-reduction device of deconvolution
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer.  Section 5.2.4, page 153 (volume \[Pages 135-188\],
#' read as a rendered page image) is the bias-reduction section this function
#' is filed under; the correction printed there is the explicit smoothing
#' correction (5.39), f_hat_n_eps(z) = f_n_eps(z) minus
#' (1/2) v_n_eps^2 times the second derivative of f_n_eps times sigma_zeta^2,
#' which removes the O(v^2) term by subtracting an estimate of it.  The device
#' implemented here is the other one named in the specification and used
#' throughout the appendix (Section A.1, p. 236, volume \[Pages 233-255\]): a
#' kernel of order r, that is one with integral K(u) du = 1,
#' integral u^j K(u) du = 0 for j = 1, ..., r - 1 and integral u^r K(u) du
#' nonzero, which reduces the leading smoothing bias from O(h^2) to O(h^r).
#'
#' The order-r Gaussian kernel is built here in the classical Hermite form
#' K_r(u) = phi(u) sum_{j=0}^{r/2 - 1} (-1)^j / (2^j j!) He_{2j}(u), phi the
#' standard normal density and He the probabilists Hermite polynomials.
#' r = 2 recovers phi itself, r = 4 gives phi(u)(3 - u^2)/2 and r = 6 gives
#' phi(u)(15 - 10 u^2 + u^4)/8.  The moments returned are computed exactly, by
#' expanding the polynomial factor in powers of u and using
#' integral u^m phi(u) du = (m-1)!! for even m and 0 for odd m; no quadrature
#' is involved, so moments 1 through r-1 come back as exact zeros.
#'
#' @param bandwidth The smoothing parameter h; only its bias order h^r is
#'   reported.
#' @param kernel_order r, an even integer at least 2.
#' @return list: estimate, reduced_bias_estimate, bias_order, kernel_order,
#'   bandwidth, coefficients, moments, leading_moment, n, method.
#' @keywords internal
#' @examples
#' Hrzbr5(0.5, 4)$moments
#' @export
Hrzbr5 <- function(bandwidth, kernel_order) {
  h <- as.numeric(bandwidth)
  r <- as.integer(kernel_order)
  if (r < 2L || r %% 2L != 0L) {
    stop("horowitz_bias_reduction_deconv: kernel_order must be an even integer >= 2")
  }
  if (h <= 0) stop("horowitz_bias_reduction_deconv: bandwidth must be positive")
  hermite <- function(m) {
    prev <- 1
    if (m == 0L) return(prev)
    cur <- c(0, 1)
    k <- 1L
    while (k < m) {
      nxt <- numeric(k + 2L)
      for (i in seq_along(cur)) nxt[i + 1L] <- nxt[i + 1L] + cur[i]
      for (i in seq_along(prev)) nxt[i] <- nxt[i] - k * prev[i]
      prev <- cur
      cur <- nxt
      k <- k + 1L
    }
    cur
  }
  gmom <- function(m) {
    if (m %% 2L == 1L) return(0)
    v <- 1
    k <- m - 1L
    while (k > 1L) {
      v <- v * k
      k <- k - 2L
    }
    v
  }
  poly <- numeric(r - 1L)
  for (j in seq_len(r %/% 2L) - 1L) {
    w <- ((-1)^j) / ((2^j) * factorial(j))
    hc <- hermite(2L * j)
    for (i in seq_along(hc)) poly[i] <- poly[i] + w * hc[i]
  }
  moments <- numeric(r + 1L)
  for (k in seq_len(r + 1L) - 1L) {
    s <- 0
    for (i in seq_along(poly)) {
      if (poly[i] != 0) s <- s + poly[i] * gmom(k + i - 1L)
    }
    moments[k + 1L] <- s
  }
  list(estimate = h^r, reduced_bias_estimate = h^r, bias_order = r,
       kernel_order = r, bandwidth = h, coefficients = poly,
       moments = moments, leading_moment = moments[r + 1L], n = r,
       method = paste0("Horowitz (2009) Sec. A.1 p.236 order-r kernel; ",
                       "Hermite construction on the Gaussian"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzbr5
#' @keywords internal
#' @export
morie_horowitz_bias_reduction_deconv <- Hrzbr5
