# SPDX-License-Identifier: AGPL-3.0-or-later
#' GEGLU gated activation
#'
#' Shazeer (2020), arXiv:2002.05202 (FETCHED): GEGLU(x, W, V, b, c) =
#' GELU(xW + b) * (xV + c), with GELU(z) = z Phi(z), the EXACT Gaussian
#' error linear unit of Hendrycks and Gimpel (2016), arXiv:1606.08415 --
#' the tanh expression that circulates as "GELU" is an approximation to
#' it and is not used here, because at 1e-9 the two differ.
#'
#' @param y the input x (first slot, for signature stability).
#' @param x the input; wins over y.
#' @param W,V gate and value projections.
#' @param b,c biases.
#' @param W2 optional output projection.
#' @return list: estimate, out, gate, ffn, method.
#' @keywords internal
#' @examples
#' Geglu(c(1, -1), W = diag(2), V = diag(2))$out
#' @export
Geglu <- function(y, x = NULL, W = NULL, V = NULL, b = NULL, c = NULL,
                  W2 = NULL) {
  v <- .s03vec(if (!is.null(x)) x else y)
  g <- .s03matvec(t(.s03mat(W)), v)
  u <- .s03matvec(t(.s03mat(V)), v)
  bb <- if (!is.null(b)) .s03vec(b) else numeric(length(g))
  cc <- if (!is.null(c)) .s03vec(c) else numeric(length(u))
  gate <- numeric(length(g)); out <- numeric(length(g))
  for (i in seq_along(g)) {
    gate[i] <- .s03gelu(g[i] + bb[i])
    out[i] <- gate[i] * (u[i] + cc[i])
  }
  ffn <- if (!is.null(W2)) .s03matvec(t(.s03mat(W2)), out) else numeric(0)
  list(estimate = if (length(out)) out[1] else NaN, out = out, gate = gate,
       ffn = ffn,
       method = "GEGLU gated activation with the exact GELU (Shazeer 2020)")
}
