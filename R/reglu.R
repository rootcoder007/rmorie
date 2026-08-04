# SPDX-License-Identifier: AGPL-3.0-or-later
#' ReGLU gated activation
#'
#' Shazeer (2020), arXiv:2002.05202 (FETCHED): ReGLU(x, W, V, b, c) =
#' max(0, xW + b) * (xV + c), the rectifier in place of the sigmoid of
#' Dauphin et al.'s (2017) original gated linear unit.  Because the gate
#' is exactly zero on half its domain, the count of dead units is
#' reported: it is the diagnostic that distinguishes ReGLU from its smooth
#' siblings.
#'
#' @param y the input x (first slot, for signature stability).
#' @param x the input; wins over y.
#' @param W,V gate and value projections.
#' @param b,c biases.
#' @param W2 optional output projection.
#' @return list: estimate, out, gate, ffn, n_dead, method.
#' @keywords internal
#' @examples
#' Reglu(c(1, -1), W = diag(2), V = diag(2))$n_dead
#' @export
Reglu <- function(y, x = NULL, W = NULL, V = NULL, b = NULL, c = NULL,
                  W2 = NULL) {
  v <- .s03vec(if (!is.null(x)) x else y)
  g <- .s03matvec(t(.s03mat(W)), v)
  u <- .s03matvec(t(.s03mat(V)), v)
  bb <- if (!is.null(b)) .s03vec(b) else numeric(length(g))
  cc <- if (!is.null(c)) .s03vec(c) else numeric(length(u))
  gate <- numeric(length(g)); out <- numeric(length(g))
  for (i in seq_along(g)) {
    gate[i] <- .s03relu(g[i] + bb[i])
    out[i] <- gate[i] * (u[i] + cc[i])
  }
  dead <- 0L
  for (z in gate) if (z == 0) dead <- dead + 1L
  ffn <- if (!is.null(W2)) .s03matvec(t(.s03mat(W2)), out) else numeric(0)
  list(estimate = if (length(out)) out[1] else NaN, out = out, gate = gate,
       ffn = ffn, n_dead = dead,
       method = "ReGLU gated activation (Shazeer 2020)")
}
