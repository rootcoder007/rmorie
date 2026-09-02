# SPDX-License-Identifier: AGPL-3.0-or-later

#' MISE of the bias-reduced KDFE (Theorem 2.4)
#'
#' Theorem 2.4:
#' \deqn{\mathrm{MISE}(\tilde F_X) = h^8 a^4\int\Big\[\frac{b_2^2 - 2b_4F}{2F}\Big\]^2 dx + \frac1n\int F(1-F)dx - \frac hn\Big\[\frac{2(a^4+1)}{(a^2-1)^2}r_1 + r_2\Big\] + o(h^8 + h/n).}{MISE(Ftilde) = h^8 a^4 int \[(b2^2 - 2 b4 F)/(2F)\]^2 dx + (1/n) int F(1-F) dx - (h/n)\[2(a^4+1)/(a^2-1)^2 r1 + r2\] + o(h^8 + h/n).}
#'
#' Compare the plain KDFE's MISE from Sec. 2.1, which leads with
#' `h^4 mu2^2 R(f')/4`: the bias term has gone from `h^4` to `h^8` while the
#' variance terms are unchanged in order. That is the claim in one line.
#'
#' The third term has lost its `f(x)` factor relative to (2.3): integrating
#' `f` over the line gives 1. So the variance gain from smoothing is a pure
#' constant, independent of `F` -- which is why Sec. 2.1 can assert dominance
#' over the empirical df for EVERY `F_X`.
#'
#' The integrals are the caller's to supply: they depend on the unknown `F_X`,
#' and estimating them here would silently turn an exact theoretical quantity
#' into a plug-in with its own error.
#'
#' @param n Sample size.
#' @param h Bandwidth.
#' @param a Second smoothing parameter.
#' @param biasint `int \[(b2^2 - 2 b4 F)/(2 F)\]^2 dx`.
#' @param varint `int F(x)(1 - F(x)) dx`.
#' @param r1,r2 Kernel constants; default to Gaussian `r1` and `r2(a)`.
#' @return Named list with ``mise``, ``biasterm``, ``varterm``, ``smoothgain``, ``h``, ``a``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 2.4.
#' @examples
#' Gekdfmise(n = 100, h = 0.2, a = 2, biasint = 1, varint = 0.5)
#' @export
Gekdfmise <- function(n, h, a, biasint, varint, r1 = NULL, r2 = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (a <= 0 || abs(a - 1) < 1e-6) stop("a must be positive and not close to 1.")
  if (is.null(r1)) r1 <- Kdfr1()$estimate
  if (is.null(r2)) r2 <- Kdfr2(a = a)$estimate
  biasterm <- h^8 * a^4 * biasint
  varterm <- varint / n
  gain <- h / n * (2 * (a^4 + 1) / (a * a - 1)^2 * r1 + r2)
  list(mise = biasterm + varterm - gain, biasterm = biasterm,
       varterm = varterm, smoothgain = gain, h = h, a = a,
       method = "MISE of the bias-reduced KDFE (Theorem 2.4)")
}

# CANONICAL TEST
# r <- Gekdfmise(n = 100, h = 0.2, a = 2, biasint = 1, varint = 0.5)
# stopifnot(abs(r$mise - (r$biasterm + r$varterm - r$smoothgain)) < 1e-18)

#' @rdname Gekdfmise
#' @keywords internal
#' @export
morie_fauzi_thm2_4_mise_brdkdfe <- Gekdfmise
