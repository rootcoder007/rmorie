# SPDX-License-Identifier: AGPL-3.0-or-later
#' One LLaMA decoder block
#'
#' Touvron et al. (2023), LLaMA: open and efficient foundation language
#' models, arXiv:2302.13971 (FETCHED), section 2.2, lists the three
#' departures from the original transformer: pre-normalisation -- "we
#' normalize the input of each transformer sub-layer, instead of
#' normalizing the output" -- with RMSNorm (Zhang and Sennrich 2019); the
#' SwiGLU activation of Shazeer (2020); and rotary positional embeddings
#' (Su et al. 2021, arXiv:2104.09864).  So the block is h = x +
#' Attn(RMSNorm(x)), y = h + SwiGLU-FFN(RMSNorm(h)), with RoPE rotating
#' each consecutive coordinate pair by m theta_j, theta_j =
#' 10000^(-2j/d).  Attention here is single-head and causal; the caller
#' supplies the projections, so nothing is invented.
#'
#' @param tokens token embeddings, one row per position.
#' @param model optional list holding the projections by name.
#' @param Wq,Wk,Wv,Wo attention projections (input units in rows).
#' @param W1,W3,W2 SwiGLU gate, value and output projections.
#' @param g1,g2 RMSNorm gains.
#' @param rope_base the RoPE base.
#' @return list: estimate, out, attn, h, method.
#' @keywords internal
#' @examples
#' X <- matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE)
#' Llamablock(X, NULL, diag(2), diag(2), diag(2), diag(2), diag(2), diag(2),
#'            diag(2))$out
#' @export
Llamablock <- function(tokens, model = NULL, Wq = NULL, Wk = NULL, Wv = NULL,
                       Wo = NULL, W1 = NULL, W3 = NULL, W2 = NULL,
                       g1 = NULL, g2 = NULL, rope_base = 10000) {
  if (!is.null(model)) {
    for (nm in c("Wq", "Wk", "Wv", "Wo", "W1", "W3", "W2")) {
      if (!is.null(model[[nm]])) assign(nm, model[[nm]])
    }
  }
  rope <- function(v, pos, base) {
    d <- length(v); out <- v
    j <- 1L
    while (j + 1L <= d) {
      th <- pos * (base^(-(j - 1) / d))
      cc <- cos(th); ss <- sin(th)
      a <- v[j]; b <- v[j + 1L]
      out[j] <- a * cc - b * ss
      out[j + 1L] <- a * ss + b * cc
      j <- j + 2L
    }
    out
  }
  X <- .s03mat(tokens)
  n <- nrow(X); d <- ncol(X)
  rms <- function(v, gain) {
    s <- 0
    for (z in v) s <- s + z * z
    r <- if (length(v)) sqrt(s / length(v)) else 0
    gg <- if (!is.null(gain)) .s03vec(gain) else rep(1, length(v))
    if (r > 0) (v / r) * gg else numeric(length(v))
  }
  Qm <- t(.s03mat(Wq)); Km <- t(.s03mat(Wk)); Vm <- t(.s03mat(Wv))
  q <- vector("list", n); kk <- vector("list", n); vv <- vector("list", n)
  for (t in seq_len(n)) {
    xn <- rms(X[t, ], g1)
    q[[t]] <- rope(.s03matvec(Qm, xn), t - 1L, rope_base)
    kk[[t]] <- rope(.s03matvec(Km, xn), t - 1L, rope_base)
    vv[[t]] <- .s03matvec(Vm, xn)
  }
  dk <- length(q[[1]])
  attn <- vector("list", n); ctx <- vector("list", n)
  for (t in seq_len(n)) {
    logits <- numeric(t)
    for (m in seq_len(t)) {
      s <- 0
      for (a in seq_len(dk)) s <- s + q[[t]][a] * kk[[m]][a]
      logits[m] <- s / sqrt(dk)
    }
    w <- .s03softmax(logits)
    attn[[t]] <- w
    cc <- numeric(length(vv[[1]]))
    for (m in seq_len(t)) for (b in seq_along(cc)) cc[b] <- cc[b] + w[m] * vv[[m]][b]
    ctx[[t]] <- cc
  }
  Om <- t(.s03mat(Wo))
  h <- matrix(0, n, d)
  for (t in seq_len(n)) {
    o <- .s03matvec(Om, ctx[[t]])
    for (j in seq_len(d)) h[t, j] <- X[t, j] + o[j]
  }
  A <- t(.s03mat(W1)); B <- t(.s03mat(W3)); C <- t(.s03mat(W2))
  out <- matrix(0, n, d)
  for (t in seq_len(n)) {
    hn <- rms(h[t, ], g2)
    gt <- .s03matvec(A, hn); ut <- .s03matvec(B, hn)
    mid <- numeric(length(gt))
    for (i in seq_along(gt)) mid[i] <- .s03swish(gt[i]) * ut[i]
    o <- .s03matvec(C, mid)
    for (j in seq_len(d)) out[t, j] <- h[t, j] + o[j]
  }
  list(estimate = if (n && d) out[n, 1] else NaN, out = out, attn = attn,
       h = h,
       method = "LLaMA block: pre-RMSNorm, RoPE attention, SwiGLU FFN (Touvron et al. 2023 sec. 2.2)")
}
