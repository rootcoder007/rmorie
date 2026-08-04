# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias of the modified gamma kernel density estimator (Theorem 1.3)
#'
#' Theorem 1.3, Eq. (1.15):
#' \deqn{\mathrm{Bias}[\tilde f_X(x)] = -2(b(x) - a^2(x)/(2 f_X(x)))h + o(h) + O(n^{-1}h^{-1/4}),}{Bias[ftilde(x)] = -2(b(x) - a^2(x)/(2 f(x))) h + o(h) + O(n^-1 h^-1/4),}
#' with `a` and `b` from (1.16) and (1.17). The order is back to `h` -- the
#' same as Chen's -- while the variance keeps the smaller order of
#' Theorem 1.1, which is the point of the chapter.
#'
#' Two notes on (1.15)-(1.17), stated plainly.
#'
#' (i) In the extracted text of (1.15) the superscript on `a` is lost; the
#' proof of Theorem 1.2 writes `a^2(x)/(2 f(x))` three times, and Eq. (2.8) of
#' Fauzi's doctoral thesis prints the square, so it is certain.
#'
#' (ii) (1.17) prints `b(x) = x + f''(x)/2 + x^2(x/3 + 1/2) f3(x)`, with a bare
#' ADDITIVE `x`. That reading is confirmed against the primary source, where
#' the prime marks are legible: Fauzi, R. R. (2020), "Bias Reduction of
#' Kernel-Type Estimators without Boundary Problems", doctoral thesis, Kyushu
#' University (institutional repository, `math0257`), Eq. (2.10). It is what
#' both sources print, not a transcription artefact.
#'
#' It is nevertheless dimensionally impossible. `b(x)` multiplies `h` to give a
#' density, so `b` carries the units of `f''`; a bare `x` carries units of
#' length and cannot be added to it. Carrying the book's own Taylor argument
#' through gives the `h` coefficient as
#' `(x + 1/2) f''(x) + x^2(x/3 + 1/2) f3(x)` -- the `x` MULTIPLYING `f''`,
#' dimensionally consistent and differing from the printed form only by a
#' missing pair of brackets.
#'
#' `book = TRUE` (default) reproduces what both published sources print;
#' `book = FALSE` uses the derived form. They agree only when `f''(x) = 1`.
#'
#' @param x Evaluation point, `x >= 0`.
#' @param h Bandwidth.
#' @param f,fp,fpp,fppp `f(x)` and its first three derivatives at `x`.
#' @param book Logical; reproduce (1.17) as printed (default) or use the
#'   derived form.
#' @return Named list with ``bias``, ``a``, ``b``, ``h``, ``book``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 1.3, Eqs. (1.15)-(1.17).
#' @examples
#' Mgkbias(x = 1, h = 0.01, f = 0.5, fp = 0, fpp = 0, fppp = 0)
#' @export
Mgkbias <- function(x, h, f, fp, fpp, fppp, book = TRUE) {
  if (h <= 0) stop("bandwidth must be positive.")
  if (f == 0) stop("(1.15) divides by f(x); it must be non-zero.")
  a <- fp + 0.5 * x * x * fpp
  b <- if (isTRUE(book)) {
    x + 0.5 * fpp + x * x * (x / 3 + 0.5) * fppp
  } else {
    (x + 0.5) * fpp + x * x * (x / 3 + 0.5) * fppp
  }
  list(bias = -2 * (b - a^2 / (2 * f)) * h, a = a, b = b, h = h,
       book = isTRUE(book),
       method = "modified gamma KDE bias (Theorem 1.3)")
}

# CANONICAL TEST
# r <- Mgkbias(x = 1, h = 0.01, f = 0.5, fp = 0, fpp = 0, fppp = 0)
# stopifnot(abs(r$bias + 0.02) < 1e-15)

#' @rdname Mgkbias
#' @keywords internal
#' @export
morie_fauzi_thm1_3_mise_mgkde <- Mgkbias
