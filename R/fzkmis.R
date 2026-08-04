# SPDX-License-Identifier: AGPL-3.0-or-later

#' MISE of the standard kernel distribution function estimator
#'
#' The MISE display of Sec. 2.1, assembled from (2.3) and (2.4):
#' \deqn{\mathrm{MISE}(\hat F_h) = \frac{h^4}{4}\Big[\int z^2K(z)dz\Big]^2\int [f'(x)]^2dx + \frac1n\int F(1-F)dx - \frac{2h}{n}r_1 + o(h^4 + h/n).}{MISE(Fhat_h) = (h^4/4)[int z^2 K dz]^2 int [f'(x)]^2 dx + (1/n) int F(1-F) dx - (2h/n) r1 + o(h^4 + h/n).}
#'
#' Differentiating in `h` and setting to zero gives
#' `hopt = (2 r1 / (n mu2^2 R(f')))^(1/3)` -- a CUBE root, the reason this
#' suite's bandwidth rule is `n^(-1/3)` and not the density estimator's
#' `n^(-1/5)`. For a Gaussian kernel against a normal reference it collapses
#' to `4^(1/3) sigma n^(-1/3)`, which is what `.morie_kdfe_h` returns and what
#' Sec. 5.3.2 attributes to Azzalini.
#'
#' `hopt` comes back alongside the MISE so the two cannot drift apart. When
#' `rfp` is zero the bias term vanishes and no optimum exists; `hopt` is then
#' `NA`, not an infinity dressed up as a number.
#'
#' @param n Sample size.
#' @param h Bandwidth.
#' @param rfp `int [f'(x)]^2 dx`, the roughness of the density.
#' @param varint `int F(x)(1 - F(x)) dx`.
#' @param mu2 `int z^2 K(z) dz`.
#' @param r1 Kernel constant (2.9); defaults to the Gaussian value.
#' @return Named list with ``mise``, ``biasterm``, ``varterm``, ``smoothgain``, ``hopt``, ``r1``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 2.1, Eqs. (2.3)-(2.4) and the MISE display.
#' @examples
#' Kdfmise(n = 100, h = 0.3, rfp = 0.2, varint = 0.5)
#' @export
Kdfmise <- function(n, h, rfp, varint, mu2 = 1, r1 = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (h <= 0) stop("bandwidth must be positive.")
  if (is.null(r1)) r1 <- Kdfr1()$estimate
  biasterm <- h^4 / 4 * mu2^2 * rfp
  varterm <- varint / n
  gain <- 2 * h / n * r1
  hopt <- if (rfp > 0) (2 * r1 / (n * mu2^2 * rfp))^(1 / 3) else NA_real_
  list(mise = biasterm + varterm - gain, biasterm = biasterm,
       varterm = varterm, smoothgain = gain, hopt = hopt, r1 = r1,
       method = "MISE of the standard KDFE (Sec. 2.1)")
}

# CANONICAL TEST
# r <- Kdfmise(n = 100, h = 0.3, rfp = 0.2, varint = 0.5)
# stopifnot(abs(r$hopt - (2 * r$r1 / (100 * 0.2))^(1/3)) < 1e-12)

#' @rdname Kdfmise
#' @keywords internal
#' @export
morie_fauzi_kdfe_mise <- Kdfmise
