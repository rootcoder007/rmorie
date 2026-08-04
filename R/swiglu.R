# SPDX-License-Identifier: AGPL-3.0-or-later
#' SwiGLU gated activation
#'
#' Shazeer (2020), GLU variants improve transformer, arXiv:2002.05202
#' (FETCHED), prints the family verbatim: ReGLU = max(0, xW + b) * (xV +
#' c), GEGLU = GELU(xW + b) * (xV + c), SwiGLU = Swish_beta(xW + b) * (xV
#' + c), with Swish_beta(z) = z sigma(beta z) (Ramachandran et al. 2017)
#' and GELU(z) = z Phi(z) (Hendrycks and Gimpel 2016).  The paper's
#' FFN_SwiGLU reduces the hidden width by 2/3 to match the parameter count
#' of an ordinary FFN; that adjustment is NOT applied here, because the
#' caller supplies W and V and has already fixed the width.
#'
#' @param y the input x (first slot, for signature stability).
#' @param x the input; wins over y.
#' @param W,V gate and value projections, input units in rows.
#' @param b,c biases.
#' @param beta the Swish parameter.
#' @param W2 optional output projection.
#' @return list: estimate, out, gate, ffn, beta, method.
#' @keywords internal
#' @examples
#' Swiglu(c(1, -1), W = diag(2), V = diag(2))$out
#' @export
Swiglu <- function(y, x = NULL, W = NULL, V = NULL, b = NULL, c = NULL,
                   beta = 1, W2 = NULL) {
  v <- .s03vec(if (!is.null(x)) x else y)
  g <- .s03matvec(t(.s03mat(W)), v)
  u <- .s03matvec(t(.s03mat(V)), v)
  bb <- if (!is.null(b)) .s03vec(b) else numeric(length(g))
  cc <- if (!is.null(c)) .s03vec(c) else numeric(length(u))
  gate <- numeric(length(g)); out <- numeric(length(g))
  for (i in seq_along(g)) {
    gate[i] <- .s03swish(g[i] + bb[i], as.numeric(beta))
    out[i] <- gate[i] * (u[i] + cc[i])
  }
  ffn <- if (!is.null(W2)) .s03matvec(t(.s03mat(W2)), out) else numeric(0)
  list(estimate = if (length(out)) out[1] else NaN, out = out, gate = gate,
       ffn = ffn, beta = as.numeric(beta),
       method = "SwiGLU gated activation (Shazeer 2020)")
}
