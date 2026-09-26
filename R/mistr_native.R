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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_mistr_rms_norm(V)
#' @keywords internal
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
#' @examples
#' set.seed(1)
#' x <- rnorm(4)
#' morie_mistr_swiglu(x, W1 = matrix(rnorm(32, 0, 0.3), 4, 8),
#'                    W2 = matrix(rnorm(32, 0, 0.3), 8, 4),
#'                    W3 = matrix(rnorm(32, 0, 0.3), 4, 8))
#' @keywords internal
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
#' @examples
#' morie_mistr_rope_angles(d = 8)
#' @keywords internal
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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_mistr_apply_rope(V, V)
#' @keywords internal
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
#' @examples
#' morie_mistr_sliding_window_mask(L = c(1, 2, 3, 4, 5, 6, 7, 8), window = 5L)
#' @keywords internal
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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_mistr_attention_span(V, V)
#' @keywords internal
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
#' @examples
#' set.seed(2)
#' L <- 4
#' Q <- matrix(rnorm(L * 8), L, 8)
#' K <- matrix(rnorm(L * 4), L, 4)
#' V <- matrix(rnorm(L * 4), L, 4)
#' r <- morie_mistr_grouped_query_attention(Q, K, V, n_heads = 2,
#'                                          n_kv_heads = 1)
#' str(r, max.level = 1)
#' @keywords internal
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
#' @examples
#' set.seed(3)
#' L <- 4; d <- 8
#' X <- matrix(rnorm(L * d, 0, 0.5), L, d)
#' W <- function(o) matrix(rnorm(d * o, 0, 0.3), d, o)
#' r <- morie_mistr_mistral_block(X, Wq = W(8), Wk = W(4), Wv = W(4),
#'                                Wo = matrix(rnorm(8 * d, 0, 0.3), 8, d),
#'                                W1 = W(16),
#'                                W2 = matrix(rnorm(16 * d, 0, 0.3), 16, d),
#'                                W3 = W(16),
#'                                n_heads = 2, n_kv_heads = 1, window = 3)
#' str(r, max.level = 1)
#' @keywords internal
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

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_apply_rope
#'
#' A step of the mistr_native implementation. Called by \code{mistr_grouped_query_attention}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param pos Numeric; combined arithmetically in the body.
#' @param theta Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param base Passed to \code{mistr_rope_angles}. Defaults to \code{10000}.
#' @return The value of \code{out}, as built in the body.
#' @export
mistr_apply_rope <- function(x, pos, theta = NULL, base = 10000) {
  d <- length(x)
  th <- if (is.null(theta)) mistr_rope_angles(d, base) else theta
  if (length(th) != d %/% 2L) {
    stop(sprintf("mistr: %d angles for %d channels", length(th), d))
  }
  out <- numeric(d)
  for (i in seq_len(d %/% 2L)) {
    ang <- pos * th[i]
    c_ <- cos(ang)
    s_ <- sin(ang)
    a <- x[2 * i - 1L]
    b <- x[2 * i]
    out[2 * i - 1L] <- a * c_ - b * s_
    out[2 * i]     <- a * s_ + b * c_
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_attention_span
#'
#' A step of the mistr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param window Coerced to integer by the body, with \code{as.integer}.
#' @param n_layers Coerced to integer by the body, with \code{as.integer}.
#' @return A numeric value.
#' @export
mistr_attention_span <- function(window, n_layers) {
  as.integer(window) * as.integer(n_layers)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_cheatsheet
#'
#' A step of the mistr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
mistr_cheatsheet <- function() {
  paste(paste0(
    "mistr: SWA -- token i attends to (i-W, i]; span grows to k*W",
    " over k layers because attention composes. RoPE rotates pair",
    "s (2i, 2i+1) by pos*theta_i, and <R_m q, R_n k> = <R_{m-n} q",
    ", k> EXACTLY. GQA shares one kv head across n_heads/n_kv que",
    "ry heads. SwiGLU gates; RMSNorm is scale-invariant but NOT s",
    "hift-invariant."
  ))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_grouped_query_attention
#'
#' A step of the mistr_native implementation. Called by \code{mistr_mistral_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q A matrix; passed to \code{as.matrix}.
#' @param K A matrix; passed to \code{as.matrix}.
#' @param V A matrix; passed to \code{as.matrix}.
#' @param n_heads A count; the body uses it as \code{seq_len(...)}.
#' @param n_kv_heads Numeric; combined arithmetically in the body.
#' @param mask Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @param positions Optional; may be \code{NULL}. A vector; its length is taken.
#' @param base Passed to \code{mistr_apply_rope}. Defaults to \code{10000}.
#' @return The value of \code{out}, as built in the body.
#' @export
mistr_grouped_query_attention <- function(Q, K, V, n_heads, n_kv_heads,
                                          mask = NULL, positions = NULL,
                                          base = 10000) {
  Qm <- as.matrix(Q)
  Km <- as.matrix(K)
  Vm <- as.matrix(V)
  L <- nrow(Qm)
  if (nrow(Km) != L || nrow(Vm) != L) {
    stop("mistr: Q, K and V must have the same length")
  }
  d <- ncol(Qm)
  if (n_heads < 1L || n_kv_heads < 1L) {
    stop("mistr: need at least one head of each kind")
  }
  if (n_heads %% n_kv_heads != 0L) {
    stop(sprintf("mistr: n_heads (%d) must be a multiple of n_kv_heads (%d)",
                 n_heads, n_kv_heads))
  }
  if (d %% n_heads != 0L) {
    stop(sprintf("mistr: dimension %d is not divisible by %d heads",
                 d, n_heads))
  }
  hd <- d %/% n_heads
  dk <- ncol(Km)
  if (dk != n_kv_heads * hd) {
    stop(sprintf("mistr: K and V must be %d wide (n_kv_heads=%d times head_dim=%d), got %d",
                 n_kv_heads * hd, n_kv_heads, hd, dk))
  }
  kd <- hd
  group <- n_heads %/% n_kv_heads
  pos <- if (is.null(positions)) seq_len(L) - 1L else as.numeric(positions)
  out <- matrix(0, nrow = L, ncol = d)
  apply_rope_flag <- !is.null(positions) || is.null(positions)
  # default: rotate when positions is NULL or provided, unless explicitly FALSE
  use_rope <- apply_rope_flag
  if (!is.null(positions) && length(positions) == 1L && is.logical(positions) && !positions) {
    use_rope <- FALSE
  }
  for (h in seq_len(n_heads) - 1L) {
    g <- h %/% group
    qs <- Qm[, (h * hd + 1L):((h + 1L) * hd), drop = FALSE]
    ks <- Km[, (g * kd + 1L):((g + 1L) * kd), drop = FALSE]
    vs <- Vm[, (g * kd + 1L):((g + 1L) * kd), drop = FALSE]
    if (use_rope) {
      qs <- t(sapply(seq_len(L), function(t) mistr_apply_rope(qs[t, ], pos[t], base = base)))
      ks <- t(sapply(seq_len(L), function(t) mistr_apply_rope(ks[t, ], pos[t], base = base)))
    }
    scale <- 1 / sqrt(hd)
    for (i in seq_len(L)) {
      allowed <- if (is.null(mask)) seq_len(L) else which(mask[i, ])
      if (length(allowed) == 0L) {
        stop(sprintf("mistr: row %d may attend to nothing", i))
      }
      sc <- sapply(allowed, function(j) scale * sum(qs[i, ] * ks[j, ]))
      mx <- max(sc)
      w <- exp(sc - mx)
      tot <- sum(w)
      for (c_ in seq_len(hd)) {
        out[i, h * hd + c_] <- sum(w * vs[allowed, c_]) / tot
      }
    }
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_mistral_block
#'
#' A step of the mistr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param Wq Passed to \code{proj}.
#' @param Wk Passed to \code{proj}.
#' @param Wv Passed to \code{proj}.
#' @param Wo Passed to \code{proj}.
#' @param W1 Passed to \code{mistr_swiglu}.
#' @param W2 Passed to \code{mistr_swiglu}.
#' @param W3 Passed to \code{mistr_swiglu}.
#' @param n_heads Carried through into a list the body builds.
#' @param n_kv_heads Numeric; combined arithmetically in the body.
#' @param window Coerced to integer by the body, with \code{as.integer}.
#' @param norm1 Optional; may be \code{NULL}. Passed to \code{mistr_rms_norm}.
#' @param norm2 Passed to \code{mistr_rms_norm}.
#' @param base Passed to \code{mistr_grouped_query_attention}. Defaults to \code{10000}.
#' @return A list with \code{estimate}, \code{output}, \code{attention_mask}, \code{L},
#' \code{d}, \code{n_heads}, \code{n_kv_heads}, \code{window}, \code{kv_cache_entries},
#' \code{method}.
#' @export
mistr_mistral_block <- function(X, Wq, Wk, Wv, Wo, W1, W2, W3,
                                n_heads, n_kv_heads, window,
                                norm1 = NULL, norm2 = NULL, base = 10000) {
  Xm <- as.matrix(X)
  L <- nrow(Xm)
  d <- ncol(Xm)
  mask <- mistr_sliding_window_mask(L, window)
  proj <- function(row, Wm) as.numeric(crossprod(row, Wm))
  h <- t(sapply(seq_len(L), function(t) mistr_rms_norm(Xm[t, ], norm1)))
  if (is.null(norm1)) h <- Xm
  Q <- t(sapply(seq_len(L), function(t) proj(h[t, ], Wq)))
  K <- t(sapply(seq_len(L), function(t) proj(h[t, ], Wk)))
  V <- t(sapply(seq_len(L), function(t) proj(h[t, ], Wv)))
  a <- mistr_grouped_query_attention(Q, K, V, n_heads, n_kv_heads,
                                     mask = mask, base = base)
  a <- t(sapply(seq_len(L), function(t) proj(a[t, ], Wo)))
  x1 <- Xm + a
  h2 <- t(sapply(seq_len(L), function(t) mistr_rms_norm(x1[t, ], norm2)))
  f <- t(sapply(seq_len(L), function(t) mistr_swiglu(h2[t, ], W1, W2, W3)))
  out <- x1 + f
  list(estimate = out, output = out, attention_mask = mask,
       L = L, d = d, n_heads = n_heads, n_kv_heads = n_kv_heads,
       window = as.integer(window),
       kv_cache_entries = min(as.integer(window), L) * n_kv_heads,
       method = "Mistral decoder block: SWA + GQA + RoPE + SwiGLU + RMSNorm, Jiang et al. (2023)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_rms_norm
#'
#' A step of the mistr_native implementation. Called by \code{mistr_mistral_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @param weight Optional; may be \code{NULL}. A vector; its length is taken.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.
#' @return A numeric value.
#' @export
mistr_rms_norm <- function(x, weight = NULL, eps = 1e-6) {
  d <- length(x)
  if (d == 0L) stop("mistr: empty vector")
  ms <- sum(x * x) / d
  inv <- 1 / sqrt(ms + eps)
  if (is.null(weight)) {
    return(x * inv)
  }
  if (length(weight) != d) {
    stop(sprintf("mistr: gain has %d entries for %d channels",
                 length(weight), d))
  }
  x * inv * weight
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_rope_angles
#'
#' A step of the mistr_native implementation. Called by \code{mistr_apply_rope}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d Numeric; combined arithmetically in the body.
#' @param base Numeric; combined arithmetically in the body. Defaults to \code{10000}.
#' @return A numeric value.
#' @export
mistr_rope_angles <- function(d, base = 10000) {
  if (d %% 2L != 0L) {
    stop(sprintf("mistr: RoPE needs an even dimension, got %d", d))
  }
  base^(-(2 * seq_len(d %/% 2L) - 2) / d)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_sliding_window_mask
#'
#' A step of the mistr_native implementation. Called by \code{mistr_mistral_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A count; the body uses it as \code{seq_len(...)}.
#' @param window Passed to \code{<}.
#' @param causal A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{mask}, as built in the body.
#' @export
mistr_sliding_window_mask <- function(L, window, causal = TRUE) {
  if (window < 1L) {
    stop(sprintf("mistr: window must be at least 1, got %d", window))
  }
  mask <- matrix(FALSE, nrow = L, ncol = L)
  for (i in seq_len(L)) {
    for (j in seq_len(L)) {
      ok <- (j <= i || !causal) && (i - j) < window
      mask[i, j] <- ok
    }
  }
  mask
}

# -- restored: morie-only definition kept through the rmorie sync --
#' mistr_swiglu
#'
#' A step of the mistr_native implementation. Called by \code{mistr_mistral_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{crossprod}.
#' @param W1 A matrix; passed to \code{crossprod}.
#' @param W2 A matrix; passed to \code{crossprod}.
#' @param W3 A matrix; passed to \code{crossprod}.
#' @return A vector, from \code{as.numeric}.
#' @export
mistr_swiglu <- function(x, W1, W2, W3) {
  x <- as.numeric(x)
  W1 <- as.matrix(W1)
  W3 <- as.matrix(W3)
  W2 <- as.matrix(W2)
  a <- as.numeric(crossprod(x, W1))  # length ncol(W1)
  b <- as.numeric(crossprod(x, W3))
  if (length(a) != length(b)) {
    stop("mistr: W1 and W3 must have the same width")
  }
  s <- 1 / (1 + exp(-a))
  gated <- s * a * b
  as.numeric(crossprod(gated, W2))
}
