# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias and variance of the raw gamma-kernel function A_h (Theorem 1.1)
#'
#' `A_h` is the raw gamma-kernel function of Eq. (1.9). Remark 1.1 is
#' explicit that it is NOT the proposed estimator, only the object the
#' proposed one is extrapolated from. Its moments are
#' \deqn{\mathrm{Bias}\[A_h(x)\] = (f'(x) + x^2 f''(x)/2)\sqrt{h} + o(\sqrt{h}),}{Bias\[A_h(x)\] = (f'(x) + x^2 f''(x)/2) sqrt(h) + o(sqrt(h)),}
#' \deqn{\mathrm{Var}\[A_h(x)\] = \frac{R^2(h^{-1/2}-1) f(x)}{2(x+\sqrt{h})\sqrt{\pi}(1-\sqrt{h})R(2h^{-1/2}-2) n h^{1/4}},}{Var\[A_h(x)\] = R^2(h^-1/2 - 1) f(x) / (2(x+sqrt(h)) sqrt(pi) (1-sqrt(h)) R(2h^-1/2 - 2) n h^(1/4)),}
#' in the interior, and the same with `(x+sqrt(h))` replaced by
#' `(c sqrt(h) + 1)` and `h^(1/4)` by `h^(3/4)` in the boundary region.
#' `R` is Eq. (1.12).
#'
#' The bias is O(sqrt(h)) -- WORSE than Chen's O(h). That is the tension of
#' Sec. 1.2: fixing the gamma shape at `h^(-1/2)` and moving the scale buys
#' a variance of order `n^(-1) h^(-1/4)` instead of Chen's
#' `n^(-1) h^(-1/2)`, and pays for it in bias. Theorem 1.2 buys the bias
#' back by geometric extrapolation.
#'
#' @param x Evaluation point, `x >= 0`.
#' @param h Bandwidth, `h > 0`.
#' @param n Sample size.
#' @param fp,fpp,f The values `f'(x)`, `f''(x)` and `f(x)`.
#' @param boundary Logical; use the boundary branch of (1.11).
#' @param c The constant in `x/h -> c`; required when `boundary` is TRUE.
#' @return Named list: bias, variance, mse, rnum, rden, region, h, n, method.
#' @references Fauzi and Maesono (2023), Theorem 1.1, Eqs. (1.9)-(1.12).
#' @examples
#' Gkrawbv(x = 1, h = 0.01, n = 100, fp = 0.1, fpp = -0.2, f = 0.3)
#' @export
Gkrawbv <- function(x, h, n, fp, fpp, f, boundary = FALSE, c = NULL) {
  x <- as.numeric(x)
  h <- as.numeric(h)
  n <- as.integer(n)
  if (h <= 0) stop("bandwidth must be positive.")
  if (n < 1L) stop("sample size must be at least 1.")
  if (x < 0) stop("gamma kernels need x >= 0.")
  rh <- sqrt(h)
  bias <- (fp + 0.5 * x * x * fpp) * rh
  rnum <- .morie_fauzi_rratio(1 / rh - 1)
  rden <- .morie_fauzi_rratio(2 / rh - 2)
  if (isTRUE(boundary)) {
    if (is.null(c)) stop("the boundary branch of (1.11) needs c.")
    scl <- c * rh + 1
    pw <- h^0.75
    region <- "boundary"
  } else {
    scl <- x + rh
    pw <- h^0.25
    region <- "interior"
  }
  v <- (rnum^2 * f) / (2 * scl * sqrt(pi) * (1 - rh) * rden * n * pw)
  list(
    bias = bias, variance = v, mse = bias * bias + v,
    rnum = rnum, rden = rden, region = region, h = h, n = n,
    method = "gamma-kernel A_h bias and variance (Theorem 1.1)"
  )
}

# CANONICAL TEST
# r <- Gkrawbv(x = 1, h = 0.01, n = 100, fp = 0.1, fpp = -0.2, f = 0.3)
# stopifnot(abs(r$bias) < 1e-15)

#' @rdname Gkrawbv
#' @keywords internal
#' @export
morie_fauzi_thm1_1_bias_mgkde <- Gkrawbv
