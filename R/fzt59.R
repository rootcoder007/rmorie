# SPDX-License-Identifier: AGPL-3.0-or-later

#' Edgeworth expansions for the smoothed sign and Wilcoxon tests (Theorem 5.9)
#'
#' Theorem 5.9. Under the conditions of Theorem 5.8, with a symmetric kernel,
#' `|f''(x)| <= M`, `int |u^4 k(u)| du < Inf` and `h = c n^(-d)`,
#' `1/4 < d < 1/2`:
#' \deqn{P_0\Big(\frac{\tilde S - E_0(\tilde S)}{\sqrt{V_0(\tilde S)}}\le y\Big) =
#' \Phi(y) - \frac{1}{24n}(y^3-3y)\phi(y) + o(n^{-1}),}{P((Stilde - E Stilde)/sqrt(V
#' Stilde) <= y) = Phi(y) - (1/(24n))(y^3 - 3y) phi(y) + o(1/n),}
#' \deqn{P_0\Big(\frac{\tilde W - E_0(\tilde W)}{\sqrt{V_0(\tilde W)}}\le y\Big) =
#' \Phi(y) - \frac{1}{20n}(y^3-3y)\phi(y) + o(n^{-1}).}{P((Wtilde - E Wtilde)/sqrt(V
#' Wtilde) <= y) = Phi(y) - (1/(20n))(y^3 - 3y) phi(y) + o(1/n).}
#'
#' Both corrections are the same Hermite polynomial `H3(y) = y^3 - 3y`; only
#' the constant differs, 1/24 against 1/20. Neither depends on `F` -- the payoff
#' of the whole section, holding because a symmetric fourth-order kernel is used.
#'
#' The book's printed Wilcoxon line is
#' `Phi(y) - (7/20 y^3 - 21/20 y) phi(y) + o(1/n)`, i.e. `(7/20) H3(y)` with NO
#' `n` in the denominator. That cannot be right as printed: a correction that
#' does not shrink with `n` is not `o(1/n)`, and it would not match the sign
#' test's `1/(24n)` in form. The primary source -- Maesono, Y., Moriyama, T.
#' and Lu, M. (2018), "Smoothed nonparametric tests and their properties",
#' Annals of the Institute of Statistical Mathematics 70(5):969-982
#' (arXiv:1610.02145), Theorems 3 and 5 -- gives `Phi(y) - phi(y) H3(y)/(20n)`,
#' which is used here. `book = TRUE` reproduces the book's printed coefficients.
#'
#' @param y Argument of the expansion.
#' @param n Sample size.
#' @param which Either `"sign"` or `"wilcoxon"`.
#' @param book Logical; reproduce the book's printed Wilcoxon coefficients
#'   `7/20` and `21/20` with no `n`. No effect for the sign test, where the
#'   book and the primary source agree.
#' @return Named list with ``estimate``, ``normal``, ``correction``, ``coef``, ``which``,
#' ``book``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.9; Maesono, Moriyama and Lu (2018),
#' AISM 70:969-982, Theorems 3 and 5.
#' @examples
#' Smthedge(y = 0, n = 100, which = "wilcoxon")
#' @export
Smthedge <- function(y, n, which = "sign", book = FALSE) {
  if (n < 1) stop("sample size must be at least 1.")
  if (!which %in% c("sign", "wilcoxon")) stop("which must be \"sign\" or \"wilcoxon\".")
  yv <- as.numeric(y)
  base <- stats::pnorm(yv)
  phi <- stats::dnorm(yv)
  he3 <- yv^3 - 3 * yv
  if (identical(which, "sign")) {
    coef <- 1 / (24 * n)
    corr <- coef * he3 * phi
  } else if (isTRUE(book)) {
    coef <- 7 / 20
    corr <- (7 / 20 * yv^3 - 21 / 20 * yv) * phi
  } else {
    coef <- 1 / (20 * n)
    corr <- coef * he3 * phi
  }
  list(estimate = base - corr, normal = base, correction = corr, coef = coef,
       which = which, book = isTRUE(book),
       method = "Edgeworth expansion of the smoothed sign/Wilcoxon test (Theorem 5.9)")
}

# CANONICAL TEST
# r <- Smthedge(y = 0, n = 100, which = "wilcoxon")
# stopifnot(abs(r$estimate - 0.5) < 1e-15)

#' @rdname Smthedge
#' @keywords internal
#' @export
morie_fauzi_thm5_9_edgeworth_wilcoxon <- Smthedge
