# Mistral: sliding-window attention, GQA, RoPE, SwiGLU, RMSNorm.
# Sources: Jiang, A. Q. et al. (2023), "Mistral 7B", arXiv:2310.06825;
# Su, J. et al. (2024), "RoFormer", arXiv:2104.09864; Shazeer, N.
# (2020), "GLU Variants Improve Transformer", arXiv:2002.05202;
# Zhang, B. & Sennrich, R. (2019), "Root Mean Square Layer
# Normalization", arXiv:1910.07467; Ainslie, J. et al. (2023), "GQA",
# arXiv:2305.13245; Beltagy, I. et al. (2020), "Longformer",
# arXiv:2004.05150.
#
# Native implementation mirroring Python morie.fn.mistr exactly: the
# same RoPE pairing (2i, 2i+1), the same GQA sharing, the same sliding
# window with causal+window mask, the same SwiGLU gate, and the same
# RMSNorm.

.GHC_MISTR_EPS <- 1e-12

#' RMSNorm
#'
#' \code{x / sqrt(mean(x^2) + eps)}, times a gain. No mean
#' subtraction: invariant to the SCALE of its input and, unlike
#' LayerNorm, not to a shift.
#'
#' @param x Vector of length d.
#' @param weight Optional gain of length d.
#' @param eps Numerical guard.
#' @return The normalised vector.
#' @export
morie_mistr_rms_norm <- function(x, weight = NULL, eps = 1e-6) {
  d <- length(x)
  if (d == 0L) stop("mistr: empty vector")
  x <- as.numeric(x)
  ms <- sum(x * x) / d
  inv <- 1 / sqrt(ms + as.numeric(eps))
  if (is.null(weight)) return(x * inv)
  if (length(weight) != d)
    stop(paste0("mistr: gain has ", length(weight),
                " entries for ", d, " channels"))
  x * inv * as.numeric(weight)
}

#' SwiGLU gate
#'
#' \code{(Swish(x W1) * x W3) W2}.
#'
#' @param x Input vector.
#' @param W1,W2,W3 Projection matrices.
#' @return The gated projection.
#' @export
morie_mistr_swiglu <- function(x, W1, W2, W3) {
  W1 <- as.matrix(W1)
  W2 <- as.matrix(W2)
  W3 <- as.matrix(W3)
  x <- as.numeric(x)
  # the old zero crossprod term used a wrongly sized vector and made
  # every call non-conformable; the gate path is just x W1
  a <- as.numeric(x %*% W1)
  b <- as.numeric(x %*% W3)
  if (length(a) != length(b))
    stop("mistr: W1 and W3 must have the same width")
  s <- 1 / (1 + exp(-a))
  gated <- s * a * b
  as.numeric(gated %*% W2)
}

#' RoPE angles
#'
#' @param d Even dimension.
#' @param base Frequency base.
#' @return Length d/2 vector of angles.
#' @export
morie_mistr_rope_angles <- function(d, base = 10000) {
  if (d %% 2 != 0L) stop(paste0("mistr: RoPE needs an even dimension, ",
                                "got ", d))
  base ^ (-(2 * seq_len(d %/% 2) - 2) / d)
}

#' Apply RoPE to a vector
#'
#' Rotate each channel pair (2i, 2i+1) by \code{pos * theta_i}.
#'
#' @param x Vector of length d.
#' @param pos Position.
#' @param theta Optional pre-computed angles.
#' @param base Frequency base.
#' @return The rotated vector.
#' @export
morie_mistr_apply_rope <- function(x, pos, theta = NULL, base = 10000) {
  x <- as.numeric(x)
  d <- length(x)
  th <- if (is.null(theta)) morie_mistr_rope_angles(d, base)
        else as.numeric(theta)
  if (length(th) != d %/% 2)
    stop(paste0("mistr: ", length(th), " angles for ", d, " channels"))
  out <- numeric(d)
  for (i in seq_len(d %/% 2)) {
    ang <- pos * th[i]
    c <- cos(ang)
    s <- sin(ang)
    a <- x[2 * i - 1L]
    b <- x[2 * i]
    out[2 * i - 1L] <- a * c - b * s
    out[2 * i] <- a * s + b * c
  }
  out
}

#' Sliding-window attention mask
#'
#' @param L Sequence length.
#' @param window Window size.
#' @param causal Apply causal mask.
#' @return An L x L logical matrix.
#' @export
morie_mistr_sliding_window_mask <- function(L, window, causal = TRUE) {
  if (window < 1L)
    stop(paste0("mistr: window must be at least 1, got ", window))
  mask <- matrix(FALSE, L, L)
  for (i in seq_len(L)) {
    for (j in seq_len(L)) {
      ok <- (j <= i || !causal) && (i - j) < window
      mask[i, j] <- ok
    }
  }
  mask
}

#' Theoretical attention span
#'
#' @param window Window size.
#' @param n_layers Number of layers.
#' @return The span (window * n_layers).
#' @export
morie_mistr_attention_span <- function(window, n_layers) {
  as.integer(window) * as.integer(n_layers)
}

#' Grouped-query attention
#'
#' @param Q,K,V Sequence matrices.
#' @param n_heads Query heads.
#' @param n_kv_heads Key-value heads (shared).
#' @param mask Optional L x L logical matrix.
#' @param positions Positions for RoPE; NULL uses 0..L-1, FALSE
#'   disables RoPE.
#' @param base RoPE base.
#' @return An L x d matrix of per-token output.
#' @export
morie_mistr_grouped_query_attention <- function(Q, K, V, n_heads,
                                                n_kv_heads, mask = NULL,
                                                positions = NULL,
                                                base = 10000) {
  Qm <- as.matrix(Q)
  Km <- as.matrix(K)
  Vm <- as.matrix(V)
  L <- nrow(Qm)
  if (nrow(Km) != L || nrow(Vm) != L)
    stop("mistr: Q, K and V must have the same length")
  d <- ncol(Qm)
  if (n_heads < 1 || n_kv_heads < 1)
    stop("mistr: need at least one head of each kind")
  if (n_heads %% n_kv_heads != 0)
    stop(paste0("mistr: n_heads (", n_heads,
                ") must be a multiple of n_kv_heads (", n_kv_heads, ")"))
  if (d %% n_heads != 0)
    stop(paste0("mistr: dimension ", d, " is not divisible by ",
                n_heads, " heads"))
  hd <- d %/% n_heads
  dk <- ncol(Km)
  if (dk != n_kv_heads * hd)
    stop(paste0("mistr: K and V must be ", n_kv_heads * hd,
                " wide (n_kv_heads=", n_kv_heads, " times head_dim=",
                hd, "), got ", dk))
  kd <- hd
  group <- n_heads %/% n_kv_heads
  pos <- if (is.null(positions)) seq_len(L) - 1L else as.numeric(positions)
  out <- matrix(0, L, d)
  for (h in seq_len(n_heads) - 1L) {
    g <- h %/% group
    qs <- Qm[, (h * hd + 1):((h + 1) * hd), drop = FALSE]
    ks <- Km[, (g * kd + 1):((g + 1) * kd), drop = FALSE]
    vs <- Vm[, (g * kd + 1):((g + 1) * kd), drop = FALSE]
    if (!identical(positions, FALSE)) {
      for (t in seq_len(L)) {
        qs[t, ] <- morie_mistr_apply_rope(qs[t, ], pos[t], base = base)
        ks[t, ] <- morie_mistr_apply_rope(ks[t, ], pos[t], base = base)
      }
    }
    scale <- 1 / sqrt(hd)
    for (i in seq_len(L)) {
      allowed <- if (is.null(mask)) seq_len(L)
                 else which(mask[i, ])
      if (length(allowed) == 0L)
        stop(paste0("mistr: row ", i - 1L, " may attend to nothing"))
      sc <- scale * as.numeric(qs[i, , drop = FALSE] %*%
                                t(ks[allowed, , drop = FALSE]))
      mx <- max(sc)
      w <- exp(sc - mx)
      tot <- sum(w)
      out[i, (h * hd + 1):((h + 1) * hd)] <-
        (t(w) %*% vs[allowed, , drop = FALSE]) / tot
    }
  }
  out
}

#' One Mistral decoder block
#'
#' @param X L x d input matrix.
#' @param Wq,Wk,Wv,Wo Attention projections.
#' @param W1,W2,W3 SwiGLU projections.
#' @param n_heads,n_kv_heads GQA.
#' @param window Sliding window.
#' @param norm1,norm2 Optional RMSNorm gains.
#' @param base RoPE base.
#' @return A list with output, attention_mask, and bookkeeping.
#' @export
morie_mistr_mistral_block <- function(X, Wq, Wk, Wv, Wo, W1, W2, W3,
                                       n_heads, n_kv_heads, window,
                                       norm1 = NULL, norm2 = NULL,
                                       base = 10000) {
  Xm <- as.matrix(X)
  L <- nrow(Xm)
  d <- ncol(Xm)
  mask <- morie_mistr_sliding_window_mask(L, window)
  proj <- function(row, Wm) as.numeric(crossprod(row, Wm)[1, ])
  h <- t(apply(Xm, 1, function(t) morie_mistr_rms_norm(t, norm1)))
  Q <- t(apply(h, 1, proj, Wm = Wq))
  K <- t(apply(h, 1, proj, Wm = Wk))
  V <- t(apply(h, 1, proj, Wm = Wv))
  a <- morie_mistr_grouped_query_attention(Q, K, V, n_heads, n_kv_heads,
                                            mask = mask, base = base)
  a <- t(apply(a, 1, proj, Wm = Wo))
  x1 <- Xm + a
  h2 <- t(apply(x1, 1, function(t) morie_mistr_rms_norm(t, norm2)))
  f <- t(apply(h2, 1, function(t) morie_mistr_swiglu(t, W1, W2, W3)))
  out <- x1 + f
  list(estimate = out, output = out, attention_mask = mask,
       L = L, d = d, n_heads = n_heads, n_kv_heads = n_kv_heads,
       window = as.integer(window),
       kv_cache_entries = min(as.integer(window), L) * n_kv_heads,
       method = paste0("Mistral decoder block: SWA + GQA + RoPE + ",
                       "SwiGLU + RMSNorm, Jiang et al. (2023)"))
}

morie_mistr <- morie_mistr_mistral_block
