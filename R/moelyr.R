# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparsely-gated mixture-of-experts layer
#'
#' Shazeer et al. (2017), Outrageously large neural networks: the
#' sparsely-gated mixture-of-experts layer, ICLR (arXiv:1701.06538 --
#' FETCHED).  Equation (3): y = sum_i G(x)_i E_i(x), with only the nonzero
#' gates evaluated.  Equations (4)-(6) give the noisy top-k gate, G(x) =
#' Softmax(KeepTopK(H(x), k)) with H(x)_i = (x . W_g)_i + StandardNormal()
#' . Softplus((x . W_noise)_i) and KeepTopK setting everything outside the
#' top k to -infinity.
#'
#' Determinism: StandardNormal() would make routing irreproducible, so the
#' noise is supplied by the caller and defaults to zero, which is the
#' paper's own inference-time behaviour.  Ties break to the lowest expert
#' index.
#'
#' @param y the input x (first slot, for signature stability).
#' @param x the input; wins over y.
#' @param W_g gate weights, input units in rows, experts in columns.
#' @param experts list of functions x -> vector, or a matrix of outputs.
#' @param top_k number of experts kept.
#' @param W_noise noise-scale weights of eq. (5).
#' @param noise the StandardNormal() draws; zeros by default.
#' @return list: estimate, out, gate, chosen, h, keep, method.
#' @keywords internal
#' @examples
#' Moelayer(c(1, 0), W_g = diag(2),
#'          experts = matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE), top_k = 1)$out
#' @export
Moelayer <- function(y, x = NULL, W_g = NULL, experts = NULL, top_k = 2,
                     W_noise = NULL, noise = NULL) {
  v <- .s03vec(if (!is.null(x)) x else y)
  Wg <- .s03mat(W_g)
  h <- .s03matvec(t(Wg), v)
  m <- length(h)
  if (!is.null(W_noise)) {
    wn <- .s03matvec(t(.s03mat(W_noise)), v)
    z <- if (!is.null(noise)) .s03vec(noise) else numeric(m)
    for (i in seq_len(m)) {
      sp <- log1p(exp(-abs(wn[i]))) + max(wn[i], 0)
      h[i] <- h[i] + z[i] * sp
    }
  }
  kk <- as.integer(top_k)
  if (kk > m) kk <- m
  ord <- order(-h, seq_len(m))
  chosen <- sort(ord[seq_len(kk)])
  keep <- rep(-Inf, m)
  keep[chosen] <- h[chosen]
  sub <- .s03softmax(h[chosen])
  gate <- numeric(m)
  for (j in seq_along(chosen)) gate[chosen[j]] <- sub[j]
  outs <- vector("list", m)
  for (i in seq_len(m)) {
    outs[[i]] <- if (is.function(experts[[i]])) .s03vec(experts[[i]](v)) else
      if (is.matrix(experts)) as.numeric(experts[i, ]) else .s03vec(experts[[i]])
  }
  d <- length(outs[[1]])
  out <- numeric(d)
  for (i in chosen) for (j in seq_len(d)) out[j] <- out[j] + gate[i] * outs[[i]][j]
  list(estimate = if (d) out[1] else NaN, out = out, gate = gate,
       chosen = chosen - 1L, h = h, keep = keep,
       method = "Sparsely-gated MoE layer (Shazeer et al. 2017, eqs. 3-6)")
}
