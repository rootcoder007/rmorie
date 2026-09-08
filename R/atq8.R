# SPDX-License-Identifier: AGPL-3.0-or-later
#' INT8 quantised attention with per-row scales
#'
#' Dettmers, Lewis, Belkada and Zettlemoyer (2022), "LLM.int8(): 8-bit matrix
#' multiplication for transformers at scale", NeurIPS 2022, arXiv:2208.07339,
#' read from the fetched PDF.  The paper's first ingredient is vector-wise
#' quantisation: rather than one scale for a whole tensor, each row of the left
#' operand and each column of the right operand gets its own absmax scale, the
#' product is accumulated in int32, and dequantisation divides by the outer
#' product of the two scales.  For a row q_i and a key k_j,
#' s_q(i) = max_t |Q_it| / 127, s_k(j) = max_t |K_jt| / 127,
#' Q_int = round(Q / s_q), K_int = round(K / s_k), and
#' (Q K')_ij ~= s_q(i) s_k(j) (Q_int K_int')_ij, with the same treatment for
#' the value matmul.  Softmax itself is done in floating point: quantising the
#' probabilities is what destroys the method, since they span several orders of
#' magnitude within a row.
#'
#' Why per-row and not per-tensor: a single outlier feature forces one global
#' scale to be huge and every other entry then quantises to zero.  That is the
#' failure the paper is about, and the anchor exercises it directly -- a matrix
#' with one large row is quantised, dequantised and compared per row, and the
#' small rows must survive.  The other anchor is exactness: when every entry is
#' already an exact multiple of its row scale, rounding does nothing and the
#' output must equal float attention to machine precision.
#'
#' Rounding is pinned to half-away-from-zero via floor(x + 0.5) in both arms,
#' because R's round() and Python's are both half-to-even and neither is the
#' hardware convention; pinning it keeps the two arms bit-identical.
#'
#' @param y ignored; accepted for interface compatibility with the shelf.
#' @param Q n_q by d queries.
#' @param K n_k by d keys.
#' @param V n_k by d_v values.
#' @param scales optional list of three explicit per-row scale vectors for Q, K
#'   and V; absmax scales by default.
#' @return list: output, estimate, scores, weights, s_q, s_k, s_v,
#'   max_abs_error_vs_float, n_q, n_k, d, d_v, method.
#' @keywords internal
#' @examples
#' Atq8(NULL, diag(2), diag(2), diag(2))$max_abs_error_vs_float
#' @export
Atq8 <- function(y = NULL, Q = NULL, K = NULL, V = NULL, scales = NULL) {
  if (is.null(Q) || is.null(K) || is.null(V)) {
    stop("int8_attention: Q, K and V are all required")
  }
  Qm <- as.matrix(Q)
  Km <- as.matrix(K)
  Vm <- as.matrix(V)
  storage.mode(Qm) <- "double"
  storage.mode(Km) <- "double"
  storage.mode(Vm) <- "double"
  nq <- nrow(Qm)
  nk <- nrow(Km)
  if (nq == 0L || nk == 0L) stop("int8_attention: Q and K must be non-empty")
  d <- ncol(Qm)
  if (ncol(Km) != d) stop("int8_attention: Q and K must share the key dimension")
  if (nrow(Vm) != nk) stop("int8_attention: V must have one row per key")
  dv <- ncol(Vm)
  if (is.null(scales)) {
    sq <- .atq8_scales(Qm)
    sk <- .atq8_scales(Km)
    sv <- .atq8_scales(Vm)
  } else {
    if (length(scales) != 3L) {
      stop("int8_attention: scales must hold three vectors, for Q, K and V")
    }
    sq <- as.numeric(scales[[1]])
    sk <- as.numeric(scales[[2]])
    sv <- as.numeric(scales[[3]])
    if (length(sq) != nq || length(sk) != nk || length(sv) != nk) {
      stop("int8_attention: a scale vector has the wrong length")
    }
    if (any(!(c(sq, sk, sv) > 0))) stop("int8_attention: scales must be positive")
  }
  Qi <- .atq8_quant(Qm, sq)
  Ki <- .atq8_quant(Km, sk)
  Vi <- .atq8_quant(Vm, sv)
  sc <- 1 / sqrt(d)
  S <- matrix(0, nrow = nq, ncol = nk)
  for (i in seq_len(nq)) {
    for (j in seq_len(nk)) {
      acc <- 0
      for (t in seq_len(d)) acc <- acc + Qi[i, t] * Ki[j, t]
      S[i, j] <- acc * sq[i] * sk[j] * sc
    }
  }
  O <- matrix(0, nrow = nq, ncol = dv)
  W <- matrix(0, nrow = nq, ncol = nk)
  for (i in seq_len(nq)) {
    mx <- max(S[i, ])
    e <- exp(S[i, ] - mx)
    w <- e / sum(e)
    W[i, ] <- w
    for (j in seq_len(nk)) {
      wj <- w[j] * sv[j]
      for (t in seq_len(dv)) O[i, t] <- O[i, t] + wj * Vi[j, t]
    }
  }
  err <- 0
  for (i in seq_len(nq)) {
    row <- numeric(nk)
    for (j in seq_len(nk)) {
      acc <- 0
      for (t in seq_len(d)) acc <- acc + Qm[i, t] * Km[j, t]
      row[j] <- acc * sc
    }
    e <- exp(row - max(row))
    ww <- e / sum(e)
    for (t in seq_len(dv)) {
      ref <- 0
      for (j in seq_len(nk)) ref <- ref + ww[j] * Vm[j, t]
      dd <- abs(O[i, t] - ref)
      if (dd > err) err <- dd
    }
  }
  list(output = O, estimate = O[1, 1], scores = S, weights = W, s_q = sq, s_k = sk,
       s_v = sv, max_abs_error_vs_float = err, n_q = nq, n_k = nk, d = d, d_v = dv,
       method = "vector-wise int8 quantisation, float softmax; Dettmers et al. (2022), arXiv:2208.07339")
}

#' @noRd
.atq8_round <- function(x) if (x >= 0) floor(x + 0.5) else -floor(-x + 0.5)

#' @noRd
.atq8_scales <- function(M) {
  out <- numeric(nrow(M))
  for (i in seq_len(nrow(M))) {
    a <- 0
    for (v in M[i, ]) if (abs(v) > a) a <- abs(v)
    out[i] <- if (a > 0) a / 127 else 1
  }
  out
}

#' @noRd
.atq8_quant <- function(M, s) {
  out <- matrix(0, nrow = nrow(M), ncol = ncol(M))
  for (i in seq_len(nrow(M))) {
    for (j in seq_len(ncol(M))) {
      q <- .atq8_round(M[i, j] / s[i])
      if (q > 127) q <- 127
      if (q < -127) q <- -127
      out[i, j] <- q
    }
  }
  out
}
