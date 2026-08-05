# SPDX-License-Identifier: AGPL-3.0-or-later
#' FlashAttention: IO-aware block-tiled exact attention
#'
#' Dao, Fu, Ermon, Rudra and Re (2022), "FlashAttention: fast and
#' memory-efficient exact attention with IO-awareness", NeurIPS 2022,
#' arXiv:2205.14135, read from the fetched PDF; and Dao (2023),
#' "FlashAttention-2", arXiv:2307.08691.
#'
#' The claim in the title is the one that matters here: exact.  FlashAttention
#' is not an approximation.  It never materialises the n_q by n_k score matrix;
#' it walks the keys in blocks and keeps a running maximum m and a running
#' denominator l, rescaling the accumulated output whenever a new block raises
#' the maximum: m_new = max(m_old, rowmax(S_block)),
#' l_new = e^(m_old - m_new) l_old + rowsum(e^(S_block - m_new)), and
#' O_new = e^(m_old - m_new) O_old + e^(S_block - m_new) V_block, with O
#' divided by l once at the end.  The rescaling factor e^(m_old - m_new) is
#' what makes the recurrence exact rather than merely stable: dropping it
#' leaves earlier blocks normalised against a stale maximum, and the error is
#' small enough to look like rounding while being systematic.
#'
#' That exactness is the anchor, and a strong one: the output must equal
#' unblocked softmax attention to floating-point precision for every block
#' size, including block_size = 1 and block_size >= n_k.  A tiling bug
#' invisible at one block size shows up at another.  The IO savings are a
#' property of the memory hierarchy, not of the arithmetic, so nothing here is
#' faster than the naive version; the point is that it computes the same number.
#'
#' @param y ignored; accepted for interface compatibility with the shelf.
#' @param Q n_q by d queries.
#' @param K n_k by d keys.
#' @param V n_k by d_v values.
#' @param block_size number of keys per tile, at least one.
#' @param causal mask keys after the query position.
#' @return list: output, estimate, l, m, n_blocks, block_size, n_q, n_k, d,
#'   d_v, causal, method.
#' @keywords internal
#' @examples
#' Atfla(NULL, diag(2), diag(2), diag(2), block_size = 1)$output
#' @export
Atfla <- function(y = NULL, Q = NULL, K = NULL, V = NULL, block_size = 2, causal = FALSE) {
  if (is.null(Q) || is.null(K) || is.null(V)) {
    stop("flash_attention_block: Q, K and V are all required")
  }
  Qm <- as.matrix(Q); Km <- as.matrix(K); Vm <- as.matrix(V)
  storage.mode(Qm) <- "double"; storage.mode(Km) <- "double"; storage.mode(Vm) <- "double"
  nq <- nrow(Qm); nk <- nrow(Km)
  if (nq == 0L || nk == 0L) stop("flash_attention_block: Q and K must be non-empty")
  d <- ncol(Qm)
  if (ncol(Km) != d) stop("flash_attention_block: Q and K must share the key dimension")
  if (nrow(Vm) != nk) stop("flash_attention_block: V must have one row per key")
  dv <- ncol(Vm)
  bs <- as.integer(block_size)
  if (bs < 1L) stop("flash_attention_block: block_size must be at least one")
  sc <- 1 / sqrt(d)
  O <- matrix(0, nrow = nq, ncol = dv)
  l <- numeric(nq)
  m <- rep(-Inf, nq)
  nb <- 0L
  j0 <- 1L
  while (j0 <= nk) {
    j1 <- min(j0 + bs - 1L, nk)
    nb <- nb + 1L
    for (i in seq_len(nq)) {
      idx <- j0:j1
      row <- numeric(length(idx))
      for (a in seq_along(idx)) {
        j <- idx[a]
        if (isTRUE(causal) && j > i) { row[a] <- -Inf; next }
        dot <- 0
        for (t in seq_len(d)) dot <- dot + Qm[i, t] * Km[j, t]
        row[a] <- dot * sc
      }
      bmax <- max(row)
      if (!is.finite(bmax)) next
      mnew <- if (m[i] > bmax) m[i] else bmax
      resc <- if (is.finite(m[i])) exp(m[i] - mnew) else 0
      e <- ifelse(is.finite(row), exp(row - mnew), 0)
      l[i] <- resc * l[i] + sum(e)
      for (t in seq_len(dv)) {
        acc <- resc * O[i, t]
        for (a in seq_along(idx)) acc <- acc + e[a] * Vm[idx[a], t]
        O[i, t] <- acc
      }
      m[i] <- mnew
    }
    j0 <- j1 + 1L
  }
  for (i in seq_len(nq)) {
    if (l[i] <= 0) stop("flash_attention_block: a query row has every key masked out")
    for (t in seq_len(dv)) O[i, t] <- O[i, t] / l[i]
  }
  list(output = O, estimate = O[1, 1], l = l, m = m, n_blocks = nb, block_size = bs,
       n_q = nq, n_k = nk, d = d, d_v = dv, causal = isTRUE(causal),
       method = "online-softmax block tiling, exact; Dao et al. (2022), arXiv:2205.14135")
}
