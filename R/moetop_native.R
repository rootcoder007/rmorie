# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Mixture-of-experts top-k routing + Switch auxiliary loss (Moetop).
# Bit-identical mirror of src/morie/fn/moetop.py. Tie-break on equal
# gates: descending gate, ascending index (Python sort key (-g, i)).

#' Mixture-of-experts top-k routing with auxiliary load-balance loss
#'
#' Router and combination per Lepikhin et al. (2021) GShard, ICLR 2021,
#' arXiv:2006.16668, Section 2.1/Algorithm 1: G(x) = softmax(W_g x),
#' each token dispatched to its top-k experts, output the
#' renormalised gate-weighted sum. Auxiliary loss per Fedus, Zoph and
#' Shazeer (2022), Switch Transformers, JMLR 23(120), arXiv:2101.03961,
#' Eqs 4-6: aux = alpha N sum_i f_i P_i; uniform routing gives exactly
#' alpha (the anchor).
#'
#' @param x Token batch (T x d_in).
#' @param W_g Router weights (d_in x N).
#' @param experts List of N linear expert maps (d_in x d_out each).
#' @param k Experts per token (default 2, GShard).
#' @param alpha Aux-loss coefficient (default 0.01, Switch).
#' @return List with \code{output}, \code{gates}, \code{topk_indices}
#'   (1-based), \code{topk_gates}, \code{aux_loss}, \code{f}, \code{P},
#'   \code{n_experts}, \code{k}, \code{estimate}, \code{n},
#'   \code{method}.
#' @references Lepikhin, D. et al. (2021), arXiv:2006.16668, Sec 2.1;
#'   Fedus, W., Zoph, B. and Shazeer, N. (2022), JMLR 23(120),
#'   arXiv:2101.03961, Eqs 4-6. Local sources in fetched-wave3/.
#' @export
Moetop <- function(x, W_g, experts, k = 2L, alpha = 0.01) {
  X <- as.matrix(x)
  Wg <- as.matrix(W_g)
  storage.mode(X) <- "double"
  storage.mode(Wg) <- "double"
  T_ <- nrow(X)
  din <- ncol(X)
  if (nrow(Wg) != din) stop(sprintf("Moetop: W_g rows %d != token width %d", nrow(Wg), din), call. = FALSE)
  N <- ncol(Wg)
  if (length(experts) != N) {
    stop(sprintf("Moetop: need one expert per router column (%d), got %d", N, length(experts)), call. = FALSE)
  }
  k <- as.integer(k)
  if (k < 1L || k > N) stop(sprintf("Moetop: k must lie in 1..%d, got %d", N, k), call. = FALSE)
  Es <- vector("list", N)
  dout <- NULL
  for (i in seq_len(N)) {
    Em <- as.matrix(experts[[i]])
    storage.mode(Em) <- "double"
    if (nrow(Em) != din) stop(sprintf("Moetop: expert %d rows %d != %d", i, nrow(Em), din), call. = FALSE)
    if (is.null(dout)) dout <- ncol(Em)
    else if (ncol(Em) != dout) stop("Moetop: experts disagree on output width", call. = FALSE)
    Es[[i]] <- Em
  }
  logits <- X %*% Wg
  gates <- t(apply(logits, 1L, function(row) {
    m <- max(row)
    e <- exp(row - m)
    e / sum(e)
  }))
  if (N == 1L) gates <- matrix(gates, nrow = T_)
  out <- matrix(0, T_, dout)
  top_idx <- vector("list", T_)
  top_gate <- vector("list", T_)
  argmax_count <- rep(0L, N)
  for (t in seq_len(T_)) {
    ord <- order(-gates[t, ], seq_len(N))
    sel <- ord[seq_len(k)]
    gsel <- gates[t, sel]
    gnorm <- gsel / sum(gsel)
    top_idx[[t]] <- sel
    top_gate[[t]] <- gnorm
    argmax_count[ord[1L]] <- argmax_count[ord[1L]] + 1L
    for (j in seq_len(k)) {
      yi <- X[t, , drop = FALSE] %*% Es[[sel[j]]]
      out[t, ] <- out[t, ] + gnorm[j] * as.vector(yi)
    }
  }
  f <- argmax_count / T_
  P <- colSums(gates) / T_
  aux <- as.numeric(alpha) * N * sum(f * P)
  list(output = out, gates = gates, topk_indices = top_idx,
       topk_gates = top_gate, aux_loss = aux, f = f, P = P,
       n_experts = N, k = k, estimate = out[1, 1], n = T_,
       method = "MoE top-k routing + Switch aux load-balance loss (GShard Sec 2.1; Switch Eqs 4-6)")
}
