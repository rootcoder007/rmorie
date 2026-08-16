# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Kamath Ch 2 shelf (DL shelf W3 tranche 1). Mirrors morie.fn km001-041.
#
# Kamath, Keenan, Somers and Sorenson (2024) Large Language Models: A
# Deep Dive, Springer, Ch 2. Eq 2.1-2.3 PDF-verified at printed p. 30.
# Eq 2.12/2.15 delegate to morie_alammar_sdp_attention -- one formula,
# one implementation.

#' .morie_km_softmax
#'
#' A step of the kamath_ch2_native implementation. Called by \code{morie_kamath_attention_softmax}, \code{morie_kamath_decoder_token_distribution}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.morie_km_softmax <- function(a) {
  z <- a - max(a)
  exp(z) / sum(exp(z))
}

#' .morie_km_probs
#'
#' A step of the kamath_ch2_native implementation. Called by \code{morie_kamath_clm_loss}, \code{morie_kamath_gpt_unsupervised}, \code{morie_kamath_mlm_loss} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p A vector; its length is taken.
#' @param name See Usage.
#' @return The value of \code{p}, as built in the body.
#' @export
.morie_km_probs <- function(p, name) {
  p <- as.numeric(p)
  if (length(p) == 0L) stop(sprintf("%s is empty.", name), call. = FALSE)
  if (any(p < 0 | p > 1)) {
    stop(sprintf("every entry of %s must lie in [0, 1].", name),
         call. = FALSE)
  }
  p
}

#' Kamath Ch 2 encoder-decoder scaffolding (Eq 2.1-2.6)
#' @param h_t_1,x_t Previous state and input.
#' @param f Optional cell.
#' @export
morie_kamath_encoder_state <- function(h_t_1, x_t, f = NULL) {
  h <- as.numeric(h_t_1); x <- as.numeric(x_t)
  if (is.null(f)) {
    if (length(h) != length(x)) {
      stop("the default cell needs matching shapes; pass a callable f for projected inputs.",
           call. = FALSE)
    }
    out <- tanh(h + x)
  } else {
    out <- as.numeric(f(h, x))
  }
  list(h = out, estimate = out[1], n = length(out),
       method = "Encoder recurrence h_t = f(h_t-1, x_t) (Kamath Eq 2.1)")
}

#' @rdname morie_kamath_encoder_state
#' @param h_1_h_T State matrix, one row per step.
#' @param mapping
#'   "mean", "last", "max" or a function.
#' @export
morie_kamath_context_vector <- function(h_1_h_T, mapping = "mean") {
  H <- as.matrix(h_1_h_T)
  if (nrow(H) == 0L) stop("no hidden states supplied.", call. = FALSE)
  if (is.function(mapping)) {
    c_ <- as.numeric(mapping(H)); name <- "callable"
  } else if (mapping == "mean") {
    c_ <- colMeans(H); name <- "mean"
  } else if (mapping == "last") {
    c_ <- H[nrow(H), ]; name <- "last"
  } else if (mapping == "max") {
    c_ <- apply(H, 2, max); name <- "max"
  } else {
    stop(sprintf("mapping must be mean, last, max or a function; got '%s'.",
                 mapping), call. = FALSE)
  }
  list(context = as.numeric(c_), mapping = name, estimate = c_[1],
       n = nrow(H),
       method = "Context vector c = m(h_1..h_T) (Kamath Eq 2.2)")
}

#' @rdname morie_kamath_encoder_state
#' @param h_T Final state.
#' @param all_states Optional full stack.
#' @export
morie_kamath_context_simplest <- function(h_T, all_states = NULL) {
  h <- as.numeric(h_T)
  agrees <- NULL
  if (!is.null(all_states)) {
    last <- morie_kamath_context_vector(all_states, "last")$context
    agrees <- isTRUE(all.equal(last, h))
    if (!agrees) {
      stop("h_T does not equal the last row of all_states; the simplest mapping is c = h_T and these disagree.",
           call. = FALSE)
    }
  }
  list(context = h, agrees_with_eq22 = agrees, estimate = h[1],
       n = length(h),
       method = "Simplest context c = h_T (Kamath Eq 2.3)")
}

#' @rdname morie_kamath_encoder_state
#' @param s_t_1,y_t_1,c Decoder inputs.
#' @param g Optional cell.
#' @export
morie_kamath_decoder_state <- function(s_t_1, y_t_1, c, g = NULL) {
  s <- as.numeric(s_t_1); y <- as.numeric(y_t_1); cc <- as.numeric(c)
  if (is.null(g)) {
    if (length(s) != length(y) || length(s) != length(cc)) {
      stop("the default cell needs matching shapes; pass a callable g for projected inputs.",
           call. = FALSE)
    }
    out <- tanh(s + y + cc)
  } else {
    out <- as.numeric(g(s, y, cc))
  }
  list(s = out, estimate = out[1], n = length(out),
       method = "Decoder recurrence s = g(s, y, c) (Kamath Eq 2.4)")
}

#' @rdname morie_kamath_encoder_state
#' @param W Optional vocab x 3d score projection.
#' @export
morie_kamath_decoder_token_distribution <- function(s_t_1, y_t_1, c,
                                                    W = NULL) {
  feats <- c(as.numeric(s_t_1), as.numeric(y_t_1), as.numeric(c))
  if (!is.null(W)) {
    Wm <- as.matrix(W)
    if (ncol(Wm) != length(feats)) {
      stop(sprintf("W has %d columns but the concatenated features have %d.",
                   ncol(Wm), length(feats)), call. = FALSE)
    }
    scores <- as.numeric(Wm %*% feats)
  } else {
    scores <- feats
  }
  p <- .morie_km_softmax(scores)
  list(distribution = p, predicted_token = which.max(p) - 1L,
       estimate = max(p), n = length(p),
       method = "Decoder token distribution softmax (Kamath Eq 2.5)")
}

#' @rdname morie_kamath_encoder_state
#' @param y Target token indices, 0-based.
#' @param U Optional length pin.
#' @export
morie_kamath_seq2seq_cross_entropy <- function(y, c, U = NULL) {
  idx <- as.integer(y)
  P <- as.matrix(c)
  if (nrow(P) != length(idx)) {
    stop(sprintf("need one distribution row per target token; got %d rows for %d tokens.",
                 nrow(P), length(idx)), call. = FALSE)
  }
  if (!is.null(U) && as.integer(U) != length(idx)) {
    stop(sprintf("U = %s does not match the sequence length %d.", U,
                 length(idx)), call. = FALSE)
  }
  if (any(abs(rowSums(P) - 1) > 1e-8) || any(P < 0)) {
    stop("every row of c must be a probability distribution.",
         call. = FALSE)
  }
  if (any(idx < 0L | idx >= ncol(P))) {
    stop("a target index is outside the vocabulary.", call. = FALSE)
  }
  picked <- P[cbind(seq_along(idx), idx + 1L)]
  losses <- -log(picked)
  list(estimate = sum(losses), per_step = losses,
       mean_loss = mean(losses), n = length(idx),
       method = "Seq2seq cross-entropy (Kamath Eq 2.6)")
}

#' Kamath Ch 2 attention chain (Eq 2.7-2.12, 2.15-2.16, 2.19)
#' @param q,k_i Query and key.
#' @param alpha Score family or function.
#' @export
morie_kamath_attention_score <- function(q, k_i, alpha = "scaled_dot") {
  q <- as.numeric(q); k <- as.numeric(k_i)
  if (length(q) != length(k)) {
    stop("q and k_i must have the same dimension.", call. = FALSE)
  }
  if (is.function(alpha)) {
    a <- as.numeric(alpha(q, k)); name <- "callable"
  } else if (alpha == "dot") {
    a <- sum(q * k); name <- "dot"
  } else if (alpha == "scaled_dot") {
    a <- sum(q * k) / sqrt(length(q)); name <- "scaled_dot"
  } else if (alpha == "cosine") {
    nq <- sqrt(sum(q^2)); nk <- sqrt(sum(k^2))
    if (nq == 0 || nk == 0) {
      stop("cosine score is undefined for a zero vector.", call. = FALSE)
    }
    a <- sum(q * k) / (nq * nk); name <- "cosine"
  } else {
    stop(sprintf("unknown alpha '%s'.", alpha), call. = FALSE)
  }
  list(estimate = a, alpha = name, n = length(q),
       method = "Attention score a_i = alpha(q, k_i) (Kamath Eq 2.7)")
}

#' @rdname morie_kamath_attention_score
#' @param a Score vector.
#' @export
morie_kamath_attention_softmax <- function(a) {
  a <- as.numeric(a)
  if (length(a) == 0L) stop("no scores supplied.", call. = FALSE)
  b <- .morie_km_softmax(a)
  list(weights = b, estimate = b[1], n = length(a),
       method = "Attention softmax weights (Kamath Eq 2.8)")
}

#' @rdname morie_kamath_attention_score
#' @param a_i One score that must appear in a.
#' @export
morie_kamath_softmax_element <- function(a_i, a) {
  a <- as.numeric(a)
  hit <- which(abs(a - as.numeric(a_i)) < 1e-12)
  if (length(hit) == 0L) {
    stop("a_i is not one of the scores in a; Eq 2.9 is an element of Eq 2.8's vector, not a free function.",
         call. = FALSE)
  }
  full <- morie_kamath_attention_softmax(a)$weights
  list(estimate = full[hit[1]], index = hit[1] - 1L,
       full_weights = full, n = length(a),
       method = "Softmax element (Kamath Eq 2.9)")
}

#' @rdname morie_kamath_attention_score
#' @param b Weights.
#' @param v Value matrix, one row per weight.
#' @export
morie_kamath_attention_output <- function(b, v) {
  b <- as.numeric(b)
  V <- as.matrix(v)
  if (nrow(V) != length(b)) {
    stop(sprintf("need one value row per weight; got %d rows for %d weights.",
                 nrow(V), length(b)), call. = FALSE)
  }
  o <- as.numeric(b %*% V)
  convex <- all(b >= 0) && abs(sum(b) - 1) < 1e-9
  list(output = o, is_convex_combination = convex, estimate = o[1],
       n = length(b),
       method = "Attention output sum b_i v_i (Kamath Eq 2.10)")
}

#' @rdname morie_kamath_attention_score
#' @param k Key.
#' @param d_k Optional dimension pin.
#' @export
morie_kamath_scaled_dot_score <- function(q, k, d_k = NULL) {
  q <- as.numeric(q); k <- as.numeric(k)
  if (length(q) != length(k)) {
    stop("q and k must have the same dimension.", call. = FALSE)
  }
  d <- if (is.null(d_k)) length(q) else as.integer(d_k)
  if (d != length(q)) {
    stop(sprintf("d_k = %d contradicts the vector dimension %d.", d,
                 length(q)), call. = FALSE)
  }
  list(estimate = sum(q * k) / sqrt(d), d_k = d, n = length(q),
       method = "Scaled dot score (Kamath Eq 2.11)")
}

#' @rdname morie_kamath_attention_score
#' @param Q,K,V Matrices.
#' @export
morie_kamath_scaled_dot_attention <- function(Q, K, V, d_k = NULL) {
  Q <- as.matrix(Q)
  if (!is.null(d_k) && as.integer(d_k) != ncol(Q)) {
    stop(sprintf("d_k = %s contradicts Q's width %d.", d_k, ncol(Q)),
         call. = FALSE)
  }
  out <- morie_alammar_sdp_attention(Q, K, V)
  list(output = out$output, attention = out$attention,
       estimate = out$estimate, n = out$n,
       method = "Scaled dot-product attention (Kamath Eq 2.12, shared core)")
}

#' @rdname morie_kamath_attention_score
#' @param W_Qi,W_Ki,W_Vi Per-head projections.
#' @export
morie_kamath_multihead_head_i <- function(Q, K, V, W_Qi, W_Ki, W_Vi) {
  Q <- as.matrix(Q); K <- as.matrix(K); V <- as.matrix(V)
  Wq <- as.matrix(W_Qi); Wk <- as.matrix(W_Ki); Wv <- as.matrix(W_Vi)
  for (pair in list(list("Q", Q, Wq), list("K", K, Wk),
                    list("V", V, Wv))) {
    if (ncol(pair[[2]]) != nrow(pair[[3]])) {
      stop(sprintf("%s has width %d but its projection has %d rows.",
                   pair[[1]], ncol(pair[[2]]), nrow(pair[[3]])),
           call. = FALSE)
    }
  }
  out <- morie_alammar_sdp_attention(Q %*% Wq, K %*% Wk, V %*% Wv)
  list(head = out$output, attention = out$attention,
       estimate = out$estimate, n = nrow(Q),
       method = "Single projected attention head (Kamath Eq 2.15)")
}

#' @rdname morie_kamath_attention_score
#' @param heads List of head outputs.
#' @param W_O Output projection.
#' @export
morie_kamath_multihead_concat <- function(heads, W_O) {
  hs <- lapply(heads, as.matrix)
  if (length(hs) == 0L) stop("no heads supplied.", call. = FALSE)
  rows <- nrow(hs[[1]])
  if (any(vapply(hs, nrow, integer(1)) != rows)) {
    stop("every head must have the same number of rows.", call. = FALSE)
  }
  concat <- do.call(cbind, hs)
  Wo <- as.matrix(W_O)
  if (ncol(concat) != nrow(Wo)) {
    stop(sprintf("concatenated width %d does not match W_O's %d rows.",
                 ncol(concat), nrow(Wo)), call. = FALSE)
  }
  out <- concat %*% Wo
  list(output = out, heads = length(hs), estimate = out[1, 1], n = rows,
       method = "Multi-head concat + output projection (Kamath Eq 2.16)")
}

#' @rdname morie_kamath_attention_score
#' @param M Additive mask, applied INSIDE the scaling (the book's
#'   convention; differs from Vaswani's for finite masks).
#' @export
morie_kamath_masked_attention <- function(Q, K, V, M, d_k = NULL) {
  Q <- as.matrix(Q); K <- as.matrix(K); V <- as.matrix(V)
  M <- as.matrix(M)
  if (ncol(Q) != ncol(K)) stop("Q and K must share d_k.", call. = FALSE)
  if (nrow(K) != nrow(V)) {
    stop("K and V must have the same number of rows.", call. = FALSE)
  }
  d <- if (is.null(d_k)) ncol(Q) else as.integer(d_k)
  if (d != ncol(Q)) {
    stop(sprintf("d_k = %d contradicts Q's width %d.", d, ncol(Q)),
         call. = FALSE)
  }
  scores <- (Q %*% t(K) + M) / sqrt(d)
  if (!all(dim(M) == dim(scores))) {
    stop("M's shape must match QK^T.", call. = FALSE)
  }
  z <- scores - apply(scores, 1, max)
  A <- exp(z) / rowSums(exp(z))
  out <- A %*% V
  list(output = out, attention = A, estimate = out[1, 1], n = nrow(Q),
       method = "Masked attention, mask inside the scaling (Kamath Eq 2.19)")
}

#' Positional encodings, FFN, layer norm (Eq 2.13-2.14, 2.17-2.18)
#' @param i Position.
#' @param j Frequency index.
#' @param d Model width.
#' @export
morie_kamath_positional_sin <- function(i, j, d) {
  i <- as.integer(i); j <- as.integer(j); d <- as.integer(d)
  if (d < 1L) stop("the model dimension d must be positive.",
                   call. = FALSE)
  if (i < 0L || j < 0L) {
    stop("position and index must be non-negative.", call. = FALSE)
  }
  if (2L * j >= d) {
    stop(sprintf("2j = %d must lie below d = %d; the pair (sin, cos) fills dimensions 2j and 2j+1.",
                 2L * j, d), call. = FALSE)
  }
  list(estimate = sin(i / 10000^(2 * j / d)),
       wavelength = 2 * pi * 10000^(2 * j / d), n = d,
       method = "Sinusoidal positional encoding, even dims (Kamath Eq 2.13)")
}

#' @rdname morie_kamath_positional_sin
#' @export
morie_kamath_positional_cos <- function(i, j, d) {
  i <- as.integer(i); j <- as.integer(j); d <- as.integer(d)
  if (d < 1L) stop("the model dimension d must be positive.",
                   call. = FALSE)
  if (i < 0L || j < 0L) {
    stop("position and index must be non-negative.", call. = FALSE)
  }
  if (2L * j + 1L >= d) {
    stop(sprintf("2j+1 = %d must lie below d = %d.", 2L * j + 1L, d),
         call. = FALSE)
  }
  list(estimate = cos(i / 10000^(2 * j / d)), n = d,
       method = "Sinusoidal positional encoding, odd dims (Kamath Eq 2.14)")
}

#' @rdname morie_kamath_positional_sin
#' @param z Input rows.
#' @param W_1,W_2,b_1,b_2 FFN parameters.
#' @export
morie_kamath_ffn_relu <- function(z, W_1, W_2, b_1, b_2) {
  Z <- as.matrix(z); W1 <- as.matrix(W_1); W2 <- as.matrix(W_2)
  b1 <- as.numeric(b_1); b2 <- as.numeric(b_2)
  if (ncol(Z) != nrow(W1)) stop("z's width must match W_1's rows.",
                                call. = FALSE)
  if (ncol(W1) != length(b1)) stop("b_1 must match W_1's columns.",
                                   call. = FALSE)
  if (ncol(W1) != nrow(W2)) {
    stop("W_2's rows must match W_1's columns.", call. = FALSE)
  }
  if (ncol(W2) != length(b2)) stop("b_2 must match W_2's columns.",
                                   call. = FALSE)
  hidden <- pmax(Z %*% W1 + matrix(b1, nrow(Z), length(b1),
                                   byrow = TRUE), 0)
  out <- hidden %*% W2 + matrix(b2, nrow(Z), length(b2), byrow = TRUE)
  list(output = out, hidden = hidden, estimate = out[1, 1], n = nrow(Z),
       method = "Position-wise FFN ReLU(zW1+b1)W2+b2 (Kamath Eq 2.17)")
}

#' @rdname morie_kamath_positional_sin
#' @param h_i Vector.
#' @param mu,sigma Optional pinned statistics.
#' @param g Gain.
#' @param eps Unused stabiliser kept for the signature.
#' @export
morie_kamath_layer_norm <- function(h_i, mu = NULL, sigma = NULL, g = 1,
                                    eps = 1e-5) {
  h <- as.numeric(h_i)
  if (length(h) < 2L && is.null(mu)) {
    stop("layer norm over a single element is 0/0; supply mu and sigma or a longer vector.",
         call. = FALSE)
  }
  # population sd to match np.std (n denominator), not stats::sd (n-1)
  m <- if (is.null(mu)) mean(h) else as.numeric(mu)
  s <- if (is.null(sigma)) sqrt(mean((h - m)^2)) else as.numeric(sigma)
  if (s <= 0) {
    stop("sigma must be positive; a constant vector cannot be layer-normalised.",
         call. = FALSE)
  }
  out <- as.numeric(g) * (h - m) / s
  list(output = out, normalised = (h - m) / s, mu = m, sigma = s,
       estimate = out[1], n = length(h),
       method = "Layer normalisation g(h - mu)/sigma (Kamath Eq 2.18)")
}

#' The Kamath Ch 2 pretraining loss family (Eq 2.20-2.33)
#' @param L_PTi Pretext losses.
#' @param lambda_i Optional weights.
#' @export
morie_kamath_ssl_loss <- function(L_PTi, lambda_i = NULL) {
  L <- as.numeric(L_PTi)
  if (length(L) == 0L) stop("no pretext losses supplied.", call. = FALSE)
  lam <- if (is.null(lambda_i)) rep(1, length(L)) else
    as.numeric(lambda_i)
  if (length(lam) != length(L)) {
    stop("need one lambda per pretext loss.", call. = FALSE)
  }
  if (any(lam < 0)) {
    stop("negative task weights invert a loss into a reward; refused.",
         call. = FALSE)
  }
  list(estimate = sum(lam * L), components = lam * L, lambdas = lam,
       n = length(L), method = "Composite SSL loss (Kamath Eq 2.20)")
}

#' @rdname morie_kamath_ssl_loss
#' @param x Per-position probabilities of the true token.
#' @export
morie_kamath_clm_loss <- function(x) {
  p <- .morie_km_probs(x, "x")
  losses <- -log(p)
  list(estimate = mean(losses), per_position = losses, n = length(p),
       method = "Causal language modelling (CLM) loss (Kamath Eq 2.21)")
}

#' @rdname morie_kamath_ssl_loss
#' @param M_x 0-based scored positions, no duplicates.
#' @export
morie_kamath_mlm_loss <- function(x, M_x) {
  p <- .morie_km_probs(x, "x")
  idx <- as.integer(M_x)
  if (length(idx) == 0L) {
    stop("the scored index set is empty; a loss over nothing is not 0, it is undefined.",
         call. = FALSE)
  }
  if (any(idx < 0L | idx >= length(p))) {
    stop("an index lies outside the sequence.", call. = FALSE)
  }
  if (anyDuplicated(idx)) {
    stop("the index set contains duplicates.", call. = FALSE)
  }
  losses <- -log(p[idx + 1L])
  list(estimate = mean(losses), per_position = losses,
       positions_scored = idx, n = length(p),
       method = "Masked language modelling (MLM) loss (Kamath Eq 2.22)")
}

#' @rdname morie_kamath_ssl_loss
#' @param xhat Per-token probability of ORIGINAL.
#' @param d Labels.
#' @export
morie_kamath_rtd_loss <- function(xhat, d) {
  p <- .morie_km_probs(xhat, "xhat")
  d <- as.integer(d)
  if (length(d) != length(p)) stop("need one label per token.",
                                   call. = FALSE)
  if (any(d != 0L & d != 1L)) {
    stop("labels must be 0 (replaced) or 1 (original).", call. = FALSE)
  }
  scored <- ifelse(d == 1L, p, 1 - p)
  losses <- -log(scored)
  list(estimate = mean(losses), per_token = losses,
       accuracy = mean((p >= 0.5) == (d == 1L)), n = length(p),
       method = "Replaced token detection (RTD) loss (Kamath Eq 2.23)")
}

#' @rdname morie_kamath_ssl_loss
#' @param y Target-side probabilities.
#' @param M_y Target mask.
#' @export
morie_kamath_tlm_loss <- function(x, y, M_x, M_y) {
  lx <- morie_kamath_mlm_loss(x, M_x)
  ly <- morie_kamath_mlm_loss(y, M_y)
  list(estimate = lx$estimate + ly$estimate,
       source_loss = lx$estimate, target_loss = ly$estimate,
       n = lx$n + ly$n,
       method = "TLM = source MLM + target MLM (Kamath Eq 2.27)")
}

#' @rdname morie_kamath_ssl_loss
#' @export
morie_kamath_nsp_loss <- function(x, y, d) {
  p <- as.numeric(x)
  if (p < 0 || p > 1) {
    stop("the model probability must lie in [0, 1].", call. = FALSE)
  }
  d <- as.integer(d)
  if (!(d %in% c(0L, 1L))) stop("d must be 0 or 1.", call. = FALSE)
  scored <- if (d == 1L) p else 1 - p
  loss <- if (scored > 0) -log(scored) else Inf
  list(estimate = loss, p_next = p, label = d, n = 1L,
       method = "Next sentence prediction loss (Kamath Eq 2.30)")
}

#' @rdname morie_kamath_ssl_loss
#' @param i,j 0-based inclusive span bounds.
#' @export
morie_kamath_span_loss <- function(x, xhat, i, j) {
  p <- as.numeric(x)
  if (any(p < 0 | p > 1)) {
    stop("probabilities must lie in [0, 1].", call. = FALSE)
  }
  i <- as.integer(i); j <- as.integer(j)
  if (i < 0L || j < i || j >= length(p)) {
    stop(sprintf("the span [%d, %d] must lie inside the sequence of length %d with i <= j.",
                 i, j, length(p)), call. = FALSE)
  }
  losses <- -log(p[(i + 1L):(j + 1L)])
  list(estimate = mean(losses), span_length = j - i + 1L,
       per_position = losses, n = length(p),
       method = "Span seq2seq loss (Kamath Eq 2.32)")
}

#' GPT objectives and MoE (Eq 2.34-2.41)
#' @param U Per-token probabilities.
#' @param k Context size (recorded).
#' @param Theta Unused, kept for the signature.
#' @export
morie_kamath_gpt_unsupervised <- function(U, k = NULL, Theta = NULL) {
  p <- .morie_km_probs(U, "U")
  if (!is.null(k) && as.integer(k) < 1L) {
    stop("the context size k must be positive.", call. = FALSE)
  }
  logs <- log(p)
  list(estimate = sum(logs), cross_entropy = -mean(logs),
       context_size = if (is.null(k)) NULL else as.integer(k),
       n = length(p),
       method = "GPT unsupervised objective L1 (Kamath Eq 2.34)")
}

#' @rdname morie_kamath_gpt_unsupervised
#' @param L_1,L_2,lam Objective values and weight.
#' @export
morie_kamath_gpt_combined <- function(L_1, L_2, lam = 0.5) {
  l <- as.numeric(lam)
  if (l < 0) {
    stop("lambda must be non-negative; a negative weight turns the auxiliary objective into a penalty on likelihood.",
         call. = FALSE)
  }
  list(estimate = as.numeric(L_2) + l * as.numeric(L_1),
       L1 = as.numeric(L_1), L2 = as.numeric(L_2), lambda = l, n = 2L,
       method = "Combined objective L2 + lambda L1 (Kamath Eq 2.37)")
}

#' @rdname morie_kamath_gpt_unsupervised
#' @param x Input.
#' @param G Gate weights or function.
#' @param E_i Expert functions or precomputed outputs.
#' @export
morie_kamath_moe_output <- function(x, G, E_i) {
  x <- as.numeric(x)
  g <- as.numeric(if (is.function(G)) G(x) else G)
  if (length(g) != length(E_i)) {
    stop(sprintf("the gate produced %d weights for %d experts.",
                 length(g), length(E_i)), call. = FALSE)
  }
  if (any(g < 0)) stop("gate weights must be non-negative.",
                       call. = FALSE)
  total <- NULL
  ran <- 0L
  for (i in seq_along(g)) {
    if (g[i] == 0) next
    out_i <- as.numeric(if (is.function(E_i[[i]])) E_i[[i]](x) else
      E_i[[i]])
    ran <- ran + 1L
    total <- if (is.null(total)) g[i] * out_i else total + g[i] * out_i
  }
  if (is.null(total)) {
    stop("every gate weight is 0; the mixture selects no expert.",
         call. = FALSE)
  }
  list(output = total, gate_weights = g, experts_evaluated = ran,
       estimate = total[1], n = length(E_i),
       method = "Mixture-of-experts combination (Kamath Eq 2.39)")
}

#' @rdname morie_kamath_gpt_unsupervised
#' @param W_g Gate projection, d x n.
#' @param k Experts kept.
#' @export
morie_kamath_moe_topk_gating <- function(x, W_g, k = 2) {
  x <- as.numeric(x)
  W <- as.matrix(W_g)
  if (nrow(W) != length(x)) {
    stop(sprintf("W_g has %d rows but x has %d dimensions.", nrow(W),
                 length(x)), call. = FALSE)
  }
  k <- as.integer(k)
  n <- ncol(W)
  if (k < 1L || k > n) stop(sprintf("k must lie in [1, %d].", n),
                            call. = FALSE)
  scores <- as.numeric(x %*% W)
  keep <- order(scores, decreasing = TRUE)[seq_len(k)]
  masked <- rep(-Inf, n)
  masked[keep] <- scores[keep]
  z <- masked - max(masked)
  e <- exp(z)
  e[is.nan(e)] <- 0
  w <- e / sum(e)
  list(weights = w, selected_experts = sort(keep) - 1L,
       n_active = sum(w > 0), estimate = max(w), n = n,
       method = "Top-k expert gating (Kamath Eq 2.40)")
}

#' @rdname morie_kamath_gpt_unsupervised
#' @param expert_weights Optional list of (W1, W3, W2) per expert;
#'   NULL isolates the gate with identity experts.
#' @export
morie_kamath_mixtral_moe <- function(x, W_g, expert_weights = NULL) {
  x <- as.numeric(x)
  gate <- morie_kamath_moe_topk_gating(x, W_g, k = 2)
  n <- length(gate$weights)
  if (is.null(expert_weights)) {
    experts <- rep(list(function(xv) xv), n)
  } else {
    if (length(expert_weights) != n) {
      stop(sprintf("need one (W1, W3, W2) triple per expert; got %d for %d.",
                   length(expert_weights), n), call. = FALSE)
    }
    experts <- lapply(expert_weights, function(ws) {
      W1 <- as.matrix(ws[[1]]); W3 <- as.matrix(ws[[2]])
      W2 <- as.matrix(ws[[3]])
      if (!all(dim(W1) == dim(W3))) {
        stop("W1 and W3 must share a shape.", call. = FALSE)
      }
      if (ncol(W1) != nrow(W2)) {
        stop("W2's rows must match W1's columns.", call. = FALSE)
      }
      function(xv) {
        a <- as.numeric(xv %*% W1)
        swish <- a / (1 + exp(-a))
        as.numeric((swish * as.numeric(xv %*% W3)) %*% W2)
      }
    })
  }
  combined <- morie_kamath_moe_output(x, gate$weights, experts)
  list(output = combined$output, gate = gate,
       experts_evaluated = combined$experts_evaluated,
       estimate = combined$estimate, n = n,
       method = "Mixtral top-2 SwiGLU MoE (Kamath Eq 2.41)")
}
