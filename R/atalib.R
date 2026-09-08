# SPDX-License-Identifier: AGPL-3.0-or-later
#' ALiBi: attention with a linear positional bias
#'
#' Press, Smith and Lewis (2022), "Train short, test long: attention with
#' linear biases enables input length extrapolation", ICLR 2022,
#' arXiv:2108.12409, read from the fetched PDF.  Page 4 gives the modification
#' verbatim, applied after the query-key dot product,
#' softmax(q_i K' + m \[-(i-1), ..., -2, -1, 0\]), and states the slope
#' schedule: "for n heads, our set of slopes is the geometric sequence that
#' starts at 2^(-8/n) and uses that same value as its ratio", so head k
#' (1-based) gets m_k = 2^(-8k/n).
#'
#' This file carries the whole ALiBi implementation for the shelf; the sibling
#' module alibi delegates to the bias builder here rather than holding a second
#' copy.
#'
#' The bias used is -m|i - j|, symmetric in the distance.  On the causal lower
#' triangle j <= i that is identical to the paper's \[-(i-1), ..., -1, 0\] row,
#' because |i - j| = i - j there; the symmetric form simply extends it to the
#' non-causal case.  Set causal = TRUE to mask the future out entirely, which
#' reproduces the paper exactly.  No position embeddings are added anywhere:
#' that absence is the method.
#'
#' @param y ignored; accepted for interface compatibility with the shelf.
#' @param Q n_q by d queries.
#' @param K n_k by d keys.
#' @param V n_k by d_v values.
#' @param slopes the head slope m, or one per head; defaults to a single head
#'   at the paper's m = 2^-8.
#' @param causal mask keys after the query position, as in the paper.
#' @return list: output, estimate, weights, bias, slopes, n_q, n_k, d, d_v,
#'   causal, method.
#' @keywords internal
#' @examples
#' Atalib(NULL, diag(2), diag(2), diag(2))$weights
#' @export
Atalib <- function(y = NULL, Q = NULL, K = NULL, V = NULL, slopes = NULL, causal = FALSE) {
  if (is.null(Q) || is.null(K) || is.null(V)) {
    stop("alibi_position_bias: Q, K and V are all required")
  }
  Qm <- as.matrix(Q)
  Km <- as.matrix(K)
  Vm <- as.matrix(V)
  storage.mode(Qm) <- "double"
  storage.mode(Km) <- "double"
  storage.mode(Vm) <- "double"
  nq <- nrow(Qm)
  nk <- nrow(Km)
  if (nq == 0L || nk == 0L) stop("alibi_position_bias: Q and K must be non-empty")
  d <- ncol(Qm)
  if (ncol(Km) != d) stop("alibi_position_bias: Q and K must share the key dimension")
  if (nrow(Vm) != nk) stop("alibi_position_bias: V must have one row per key")
  dv <- ncol(Vm)
  sl <- if (is.null(slopes)) 2^-8 else as.numeric(slopes)
  if (length(sl) == 0L) stop("alibi_position_bias: slopes is empty")
  sc <- 1 / sqrt(d)
  outs <- vector("list", length(sl))
  W0 <- NULL
  B0 <- NULL
  for (h in seq_along(sl)) {
    B <- .atalib_bias(nq, nk, sl[h], causal)
    O <- matrix(0, nrow = nq, ncol = dv)
    Wh <- matrix(0, nrow = nq, ncol = nk)
    for (i in seq_len(nq)) {
      row <- numeric(nk)
      for (j in seq_len(nk)) {
        dot <- 0
        for (t in seq_len(d)) dot <- dot + Qm[i, t] * Km[j, t]
        row[j] <- dot * sc + B[i, j]
      }
      w <- .atalib_softmax(row)
      Wh[i, ] <- w
      for (j in seq_len(nk)) for (t in seq_len(dv)) O[i, t] <- O[i, t] + w[j] * Vm[j, t]
    }
    outs[[h]] <- O
    if (h == 1L) { W0 <- Wh
    B0 <- B }
  }
  list(output = if (length(sl) == 1L) outs[[1]] else outs, estimate = outs[[1]][1, 1],
       weights = W0, bias = B0, slopes = sl, n_q = nq, n_k = nk, d = d, d_v = dv,
       causal = isTRUE(causal),
       method = "softmax(QK'/sqrt(d) - m|i-j|) V; Press, Smith and Lewis (2022), arXiv:2108.12409")
}

#' @noRd
.atalib_slopes <- function(n_heads) {
  n <- as.integer(n_heads)
  if (n < 1L) stop("alibi_position_bias: n_heads must be at least one")
  2^(-8 * seq_len(n) / n)
}

#' @noRd
.atalib_bias <- function(n_q, n_k, slope, causal = FALSE) {
  B <- matrix(0, nrow = n_q, ncol = n_k)
  for (i in seq_len(n_q)) {
    for (j in seq_len(n_k)) {
      B[i, j] <- if (isTRUE(causal) && j > i) -Inf else -as.numeric(slope) * abs(i - j)
    }
  }
  B
}

#' @noRd
.atalib_softmax <- function(v) {
  fin <- v[is.finite(v)]
  if (length(fin) == 0L) stop("alibi_position_bias: a query row has every key masked out")
  mx <- max(fin)
  e <- ifelse(is.finite(v), exp(v - mx), 0)
  e / sum(e)
}
