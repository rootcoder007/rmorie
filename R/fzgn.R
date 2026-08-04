# SPDX-License-Identifier: AGPL-3.0-or-later

#' Edgeworth expansion function G_n for the kernel quantile estimator
#'
#' Theorem 3.1, the function `G_n` of (3.14):
#' \deqn{G_n(x) = \Phi(x) - \phi(x)\Big\{\frac{x^2-1}{6n^{1/2}\sigma_n^3}\Big(e_{1n} + \frac{3e_{2n}}{h}\Big) + \frac{1}{nh^2}\Big[\frac{x}{4\sigma_n^2}(4e_{5n}+e_{6n}) + \frac{x^3-3x}{6\sigma_n^4}(3e_{3n}+e_{4n}) + \frac{x^5-10x^3+15x}{8\sigma_n^6}e_{2n}^2\Big]\Big\}.}{G_n(x) = Phi(x) - phi(x){(x^2-1)/(6 n^1/2 s^3)(e1n + 3 e2n/h) + (1/(n h^2))[x/(4 s^2)(4 e5n + e6n) + (x^3-3x)/(6 s^4)(3 e3n + e4n) + (x^5-10x^3+15x)/(8 s^6) e2n^2]}.}
#'
#' Every bracket is a Hermite polynomial -- `He2 = x^2-1`, `He3 = x^3-3x`,
#' `He5 = x^5-10x^3+15x` -- and the `He5` term carries `e2n` SQUARED, the
#' standard Edgeworth structure in which the fifth-order term is the square of
#' the third-order one.
#'
#' The book's (3.14) prints `{3 e2n + e4n}` in the `He3` bracket. The primary
#' source -- Maesono, Y. and Penev, S. (2011), "Edgeworth expansion for the
#' kernel quantile estimator", Annals of the Institute of Statistical
#' Mathematics 63(3):617-644, Theorem 1 -- prints `{3 e3n + e4n}`, and is
#' followed by default. The tell that the book has a typo is internal: it
#' defines `e3n` immediately below the display and then never uses it. Pass
#' `book = TRUE` to reproduce the book's spelling.
#'
#' @param x Argument of the expansion.
#' @param n Sample size.
#' @param h Bandwidth.
#' @param sigma `sigma_n`, the standardising scale.
#' @param e1,e2,e3,e4,e5,e6 The moment functionals `e1n` ... `e6n`.
#' @param delta The shift `delta/(sigma sqrt(n))` of (3.14); 0 gives `G_n(x)`.
#' @param book Logical; use the book's `3 e2n + e4n` instead of the primary
#'   source's `3 e3n + e4n`.
#' @return Named list with ``estimate``, ``normal``, ``correction``, ``book``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 3.1, Eq. (3.14); Maesono and Penev (2011), AISM 63:617-644, Theorem 1.
#' @examples
#' Qedgew(x = 0, n = 100, h = 0.1, sigma = 1, e1 = 0, e2 = 0, e3 = 0, e4 = 0, e5 = 0, e6 = 0)
#' @export
Qedgew <- function(x, n, h, sigma, e1, e2, e3, e4, e5, e6, delta = 0, book = FALSE) {
  if (n < 1) stop("sample size must be at least 1.")
  if (h <= 0) stop("bandwidth must be positive.")
  if (sigma <= 0) stop("sigma_n must be positive.")
  xv <- as.numeric(x) - delta / (sigma * sqrt(n))
  phi <- stats::dnorm(xv); base <- stats::pnorm(xv)
  he2 <- xv^2 - 1
  he3 <- xv^3 - 3 * xv
  he5 <- xv^5 - 10 * xv^3 + 15 * xv
  term1 <- he2 / (6 * sqrt(n) * sigma^3) * (e1 + 3 * e2 / h)
  mid <- if (isTRUE(book)) e2 else e3
  inner <- xv / (4 * sigma^2) * (4 * e5 + e6) +
    he3 / (6 * sigma^4) * (3 * mid + e4) +
    he5 / (8 * sigma^6) * e2^2
  corr <- phi * (term1 + inner / (n * h * h))
  list(estimate = base - corr, normal = base, correction = corr,
       book = isTRUE(book),
       method = "Edgeworth function G_n for the kernel quantile estimator (3.14)")
}

# CANONICAL TEST
# r <- Qedgew(x = 0, n = 100, h = 0.1, sigma = 1, e1 = 0, e2 = 0, e3 = 0, e4 = 0, e5 = 0, e6 = 0)
# stopifnot(abs(r$estimate - 0.5) < 1e-15)

#' @rdname Qedgew
#' @keywords internal
#' @export
morie_fauzi_gn_edgeworth_correction <- Qedgew
