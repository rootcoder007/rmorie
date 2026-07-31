# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Alammar/Grootendorst + Vaswani shelf. Mirrors the 55 morie.fn
# modules al* and att* (DL shelf W2).
#
# Alammar J, Grootendorst M (2024) Hands-On Large Language Models,
# O'Reilly; Vaswani et al. (2017); per-function papers cited inline.
# Caller-supplied models arrive as R functions, mirroring Python's
# callables; the algorithm around them is computed natively here.

.morie_al_softmax_rows <- function(z) {
  z <- z - apply(z, 1, max)
  e <- exp(z)
  e / rowSums(e)
}

.morie_al_cos <- function(a, b) {
  na <- sqrt(sum(a^2)); nb <- sqrt(sum(b^2))
  if (na == 0 || nb == 0) {
    stop("a zero vector has no direction; cosine similarity with it is undefined.",
         call. = FALSE)
  }
  sum(a * b) / (na * nb)
}

.morie_al_lcg <- function(state) {
  (1664525 * state + 1013904223) %% 2^32
}

#' Scaled dot-product attention (Vaswani et al. 2017)
#'
#' softmax(Q K^T / sqrt(d_k)) V; `mask` is additive (0 keep, -Inf
#' drop) or logical (TRUE keep). Rows of the attention matrix sum to
#' 1 by construction, and the parity tests assert it.
#'
#' @param Q,K,V Matrices.
#' @param mask Optional matrix.
#' @return List with `output`, `attention`.
#' @export
morie_alammar_sdp_attention <- function(Q, K, V, mask = NULL) {
  Q <- as.matrix(Q); K <- as.matrix(K); V <- as.matrix(V)
  if (ncol(Q) != ncol(K)) {
    stop(sprintf("Q and K must share d_k; got %d and %d.", ncol(Q),
                 ncol(K)), call. = FALSE)
  }
  if (nrow(K) != nrow(V)) {
    stop("K and V must have the same number of rows.", call. = FALSE)
  }
  scores <- Q %*% t(K) / sqrt(ncol(Q))
  if (!is.null(mask)) {
    m <- as.matrix(mask)
    if (!all(dim(m) == dim(scores))) {
      stop("mask shape does not match scores.", call. = FALSE)
    }
    if (is.logical(m)) {
      scores[!m] <- -Inf
    } else {
      scores <- scores + m
    }
  }
  A <- .morie_al_softmax_rows(scores)
  list(output = A %*% V, attention = A, d_k = ncol(Q), n = nrow(Q),
       estimate = (A %*% V)[1, 1],
       method = "Scaled dot-product attention (Vaswani et al. 2017)")
}

#' Multi-head attention (Vaswani et al. 2017)
#'
#' Concat(head_1..head_h) W_O; Wq, Wk, Wv are LISTS of per-head
#' projections -- one per head, refused otherwise, since broadcasting
#' one matrix would compute h copies of the same head.
#'
#' @param Q,K,V Matrices.
#' @param Wq,Wk,Wv Lists of matrices.
#' @param Wo Output projection.
#' @param heads Head count.
#' @export
morie_alammar_multi_head_attention <- function(Q, K, V, Wq, Wk, Wv, Wo,
                                               heads) {
  heads <- as.integer(heads)
  for (nm in c("Wq", "Wk", "Wv")) {
    W <- get(nm)
    if (length(W) != heads) {
      stop(sprintf("%s has %d projection matrices but heads = %d; one per head, or the heads are copies.",
                   nm, length(W), heads), call. = FALSE)
    }
  }
  Q <- as.matrix(Q); K <- as.matrix(K); V <- as.matrix(V)
  outs <- vector("list", heads)
  for (i in seq_len(heads)) {
    h <- morie_alammar_sdp_attention(Q %*% as.matrix(Wq[[i]]),
                                     K %*% as.matrix(Wk[[i]]),
                                     V %*% as.matrix(Wv[[i]]))
    outs[[i]] <- h$output
  }
  concat <- do.call(cbind, outs)
  Wo <- as.matrix(Wo)
  if (ncol(concat) != nrow(Wo)) {
    stop(sprintf("concatenated heads have width %d but Wo has %d rows.",
                 ncol(concat), nrow(Wo)), call. = FALSE)
  }
  out <- concat %*% Wo
  list(output = out, heads = heads, estimate = out[1, 1], n = nrow(Q),
       method = "Multi-head attention (Vaswani et al. 2017)")
}

#' Grouped-query and multi-query attention (Ainslie et al. 2023;
#' Shazeer 2019)
#'
#' head_i = Attn(Q_i, `K_{g(i)}, V_{g(i)}`) with g(i) = i mod G. G = 1
#' recovers MQA; G = H full multi-head. H must divide by G.
#'
#' @param Q_heads List of per-head query matrices.
#' @param K_groups,V_groups Lists of shared K and V.
#' @param n_query_heads,n_kv_groups Counts.
#' @export
morie_alammar_grouped_query_attention <- function(Q_heads, K_groups,
                                                  V_groups, n_query_heads,
                                                  n_kv_groups) {
  H <- as.integer(n_query_heads); G <- as.integer(n_kv_groups)
  if (H < 1L || G < 1L) stop("head and group counts must be positive.",
                             call. = FALSE)
  if (H %% G != 0L) {
    stop(sprintf("n_query_heads = %d must be divisible by n_kv_groups = %d.",
                 H, G), call. = FALSE)
  }
  if (length(Q_heads) != H) {
    stop(sprintf("expected %d query heads; got %d.", H, length(Q_heads)),
         call. = FALSE)
  }
  if (length(K_groups) != G || length(V_groups) != G) {
    stop(sprintf("expected %d K and V groups.", G), call. = FALSE)
  }
  outs <- vector("list", H)
  assignment <- integer(H)
  for (i in seq_len(H)) {
    g <- (i - 1L) %% G + 1L
    assignment[i] <- g - 1L
    outs[[i]] <- morie_alammar_sdp_attention(Q_heads[[i]], K_groups[[g]],
                                             V_groups[[g]])$output
  }
  concat <- do.call(cbind, outs)
  list(output = concat, group_assignment = assignment,
       kv_cache_ratio = G / H, estimate = concat[1, 1], n = H,
       method = "Grouped-query attention (Ainslie et al. 2023)")
}

#' @rdname morie_alammar_grouped_query_attention
#' @param K_shared,V_shared Single shared K and V matrices.
#' @export
morie_alammar_multi_query_attention <- function(Q_heads, K_shared,
                                                V_shared, n_query_heads) {
  H <- as.integer(n_query_heads)
  if (H < 1L) stop("n_query_heads must be positive.", call. = FALSE)
  if (length(Q_heads) != H) {
    stop(sprintf("expected %d query heads; got %d.", H, length(Q_heads)),
         call. = FALSE)
  }
  outs <- lapply(Q_heads, function(q) {
    morie_alammar_sdp_attention(q, K_shared, V_shared)$output
  })
  concat <- do.call(cbind, outs)
  list(output = concat, kv_cache_ratio = 1 / H, estimate = concat[1, 1],
       n = H, method = "Multi-query attention (Shazeer 2019)")
}

#' Sliding-window causal attention (Beltagy et al. 2020)
#' @param Q,K,V One sequence's matrices.
#' @param window_size W.
#' @export
morie_alammar_sliding_window_attention <- function(Q, K, V, window_size) {
  Q <- as.matrix(Q); K <- as.matrix(K)
  W <- as.integer(window_size)
  if (W < 1L) stop("window_size must be positive.", call. = FALSE)
  n <- nrow(Q)
  if (n != nrow(K)) {
    stop("sliding-window attention is defined over one sequence.",
         call. = FALSE)
  }
  mask <- matrix(-Inf, n, n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - W + 1L)
    mask[i, lo:i] <- 0
  }
  out <- morie_alammar_sdp_attention(Q, K, V, mask = mask)
  list(output = out$output, attention = out$attention, window = W,
       estimate = out$estimate, n = n,
       method = "Sliding-window causal attention (Beltagy et al. 2020)")
}

#' KV-cache append plus one attention row (Alammar Ch 3)
#' @param K_cache,V_cache Existing caches or NULL.
#' @param k_new,v_new,q_new Single-row matrices.
#' @export
morie_alammar_kv_cache_lookup <- function(K_cache, V_cache, k_new, v_new,
                                          q_new) {
  k_new <- matrix(as.numeric(k_new), nrow = 1)
  v_new <- matrix(as.numeric(v_new), nrow = 1)
  q_new <- matrix(as.numeric(q_new), nrow = 1)
  if (is.null(K_cache) || length(K_cache) == 0L) {
    K <- k_new; V <- v_new
  } else {
    K <- rbind(as.matrix(K_cache), k_new)
    V <- rbind(as.matrix(V_cache), v_new)
  }
  if (nrow(K) != nrow(V)) {
    stop("K and V caches must stay the same length.", call. = FALSE)
  }
  out <- morie_alammar_sdp_attention(q_new, K, V)
  list(output = as.numeric(out$output[1, ]),
       attention = as.numeric(out$attention[1, ]),
       K_cache = K, V_cache = V, cache_length = nrow(K),
       estimate = out$estimate, n = nrow(K),
       method = "KV-cache single-step attention (Alammar Ch 3)")
}

#' Classification and NER heads (Alammar Ch 4)
#' @param h_cls Hidden vector.
#' @param W_cls Weight matrix.
#' @param b Bias.
#' @export
morie_alammar_classification_head <- function(h_cls, W_cls, b) {
  h <- as.numeric(h_cls); W <- as.matrix(W_cls); b <- as.numeric(b)
  if (ncol(W) != length(h)) {
    stop(sprintf("W has %d columns but h_cls has %d entries.", ncol(W),
                 length(h)), call. = FALSE)
  }
  if (nrow(W) != length(b)) {
    stop("W rows must match b.", call. = FALSE)
  }
  logits <- as.numeric(W %*% h + b)
  z <- logits - max(logits)
  p <- exp(z) / sum(exp(z))
  list(logits = logits, probabilities = p,
       predicted_class = which.max(logits) - 1L,
       estimate = logits[1], n = length(b),
       method = "Linear classification head + softmax (Alammar Ch 4)")
}

#' @rdname morie_alammar_classification_head
#' @param h_tokens Token hidden states, one row each.
#' @param W,tags Weight matrix and 0-based tag targets (or NULL).
#' @export
morie_alammar_ner_token_head <- function(h_tokens, W, b, tags = NULL) {
  H <- as.matrix(h_tokens); W <- as.matrix(W); b <- as.numeric(b)
  if (ncol(W) != ncol(H)) {
    stop("W columns must match hidden dimensions.", call. = FALSE)
  }
  logits <- H %*% t(W) + matrix(b, nrow(H), length(b), byrow = TRUE)
  P <- .morie_al_softmax_rows(logits)
  pred <- apply(logits, 1, which.max) - 1L
  loss <- NULL
  if (!is.null(tags)) {
    t <- as.integer(tags)
    if (length(t) != nrow(H)) stop("one tag per token.", call. = FALSE)
    if (any(t < 0L | t >= nrow(W))) {
      stop("tag index out of range.", call. = FALSE)
    }
    loss <- mean(-log(P[cbind(seq_along(t), t + 1L)]))
  }
  list(probabilities = P, predicted_tags = as.integer(pred),
       cross_entropy = loss,
       estimate = if (!is.null(loss)) loss else as.numeric(pred[1]),
       n = nrow(H), method = "Per-token NER head + CE (Alammar Ch 4)")
}

#' Token lookup, masked pooling, contextual extraction (Alammar Ch 2/8)
#' @param ids 0-based token ids.
#' @param E_tok V x d embedding table.
#' @export
morie_alammar_token_embedding_lookup <- function(ids, E_tok) {
  E <- as.matrix(E_tok)
  ids <- as.integer(ids)
  if (any(ids < 0L | ids >= nrow(E))) {
    stop(sprintf("token id %d is outside the vocabulary of %d.",
                 ids[which(ids < 0L | ids >= nrow(E))[1]], nrow(E)),
         call. = FALSE)
  }
  out <- E[ids + 1L, , drop = FALSE]
  list(embeddings = out, estimate = out[1, 1], vocab_size = nrow(E),
       dim = ncol(E), n = length(ids),
       method = "Token embedding lookup (Alammar Ch 2)")
}

#' @rdname morie_alammar_token_embedding_lookup
#' @param token_embeddings Token vectors, one row each.
#' @param attention_mask 0/1 vector; padding must not dilute the mean.
#' @export
morie_alammar_document_embedding_pool <- function(token_embeddings,
                                                  attention_mask = NULL) {
  H <- as.matrix(token_embeddings)
  m <- if (is.null(attention_mask)) rep(1, nrow(H)) else
    as.numeric(attention_mask)
  if (length(m) != nrow(H)) {
    stop("mask length must match the token count.", call. = FALSE)
  }
  if (sum(m) == 0) {
    stop("the mask excludes every token; an all-padding document has no embedding.",
         call. = FALSE)
  }
  d <- colSums(H * m) / sum(m)
  list(embedding = as.numeric(d), tokens_pooled = sum(m),
       estimate = d[1], n = nrow(H),
       method = "Masked mean pooling (Alammar Ch 8)")
}

#' @rdname morie_alammar_token_embedding_lookup
#' @param layer_outputs A (layers x seq x dim) array.
#' @param layer_idx,position 0-based indices, negatives from the end.
#' @export
morie_alammar_contextualized_embedding <- function(layer_outputs,
                                                   layer_idx, position) {
  L <- layer_outputs
  if (length(dim(L)) != 3L) {
    stop("layer_outputs must be (n_layers, seq_len, dim).", call. = FALSE)
  }
  nl <- dim(L)[1]; sq <- dim(L)[2]
  li <- as.integer(layer_idx); pos <- as.integer(position)
  if (li < -nl || li >= nl) {
    stop(sprintf("layer %d out of range for %d layers.", li, nl),
         call. = FALSE)
  }
  if (pos < -sq || pos >= sq) {
    stop(sprintf("position %d out of range for length %d.", pos, sq),
         call. = FALSE)
  }
  if (li < 0L) li <- nl + li
  if (pos < 0L) pos <- sq + pos
  v <- L[li + 1L, pos + 1L, ]
  varies <- sq > 1L && !isTRUE(all.equal(
    L[li + 1L, , , drop = FALSE],
    array(rep(L[li + 1L, 1L, ], each = sq), dim = c(1L, sq, dim(L)[3]))))
  list(embedding = as.numeric(v), context_varies = varies,
       estimate = v[1], layer = layer_idx, position = position, n = sq,
       method = "Contextualised embedding extraction (Alammar Ch 2)")
}

#' ViT patch embedding (Dosovitskiy et al. 2021)
#' @param image Numeric matrix; must tile exactly.
#' @param patch_size P.
#' @param E (P^2 x d) projection.
#' @param cls_token,E_pos Optional class token and positional table.
#' @export
morie_alammar_vit_patch_embedding <- function(image, patch_size, E,
                                              cls_token = NULL,
                                              E_pos = NULL) {
  img <- as.matrix(image)
  P <- as.integer(patch_size)
  E <- as.matrix(E)
  if (P < 1L) stop("patch_size must be positive.", call. = FALSE)
  H <- nrow(img); W <- ncol(img)
  if (H %% P != 0L || W %% P != 0L) {
    stop(sprintf("a %d x %d image does not tile into %d x %d patches; a remainder row would silently shift every later patch.",
                 H, W, P, P), call. = FALSE)
  }
  if (nrow(E) != P * P) {
    stop(sprintf("E must have %d rows to accept a flattened patch.",
                 P * P), call. = FALSE)
  }
  patches <- list()
  for (i in seq(1L, H, by = P)) {
    for (j in seq(1L, W, by = P)) {
      # row-major flatten to match Python's ravel()
      patches[[length(patches) + 1L]] <-
        as.numeric(t(img[i:(i + P - 1L), j:(j + P - 1L)]))
    }
  }
  Z <- do.call(rbind, patches) %*% E
  if (!is.null(cls_token)) {
    ct <- as.numeric(cls_token)
    if (length(ct) != ncol(E)) {
      stop("cls token width must match E columns.", call. = FALSE)
    }
    Z <- rbind(ct, Z)
  }
  if (!is.null(E_pos)) {
    Ep <- as.matrix(E_pos)
    if (!all(dim(Ep) == dim(Z))) {
      stop("E_pos shape does not match the sequence.", call. = FALSE)
    }
    Z <- Z + Ep
  }
  rownames(Z) <- NULL
  list(sequence = Z, n_patches = length(patches), estimate = Z[1, 1],
       n = nrow(Z),
       method = "ViT patch embedding (Dosovitskiy et al. 2021)")
}

# ------------------------------------------------------------------
# Losses
# ------------------------------------------------------------------

#' Contrastive and ranking losses of the SBERT/CLIP family
#' @param a,b Row-matched embedding matrices.
#' @param y_true Cosine
#'   targets in \\[-1, 1\\].
#' @export
morie_alammar_cosine_similarity_loss <- function(a, b, y_true) {
  A <- as.matrix(a); B <- as.matrix(b); y <- as.numeric(y_true)
  if (!all(dim(A) == dim(B)) || nrow(A) != length(y)) {
    stop("need matched pairs and one target per pair.", call. = FALSE)
  }
  if (any(abs(y) > 1)) {
    stop("targets are cosine values and must lie in [-1, 1].",
         call. = FALSE)
  }
  sims <- vapply(seq_len(nrow(A)), function(i) {
    .morie_al_cos(A[i, ], B[i, ])
  }, numeric(1))
  losses <- (sims - y)^2
  list(estimate = mean(losses), losses = losses, similarities = sims,
       n = length(y),
       method = "Cosine similarity loss (Reimers and Gurevych 2019)")
}

#' @rdname morie_alammar_cosine_similarity_loss
#' @param anchor,positive,negative Row-matched matrices.
#' @param margin m.
#' @export
morie_alammar_sbert_triplet_loss <- function(anchor, positive, negative,
                                             margin = 1.0) {
  A <- as.matrix(anchor); P <- as.matrix(positive); N <- as.matrix(negative)
  m <- as.numeric(margin)
  if (!all(dim(A) == dim(P)) || !all(dim(A) == dim(N))) {
    stop("anchor, positive and negative must align.", call. = FALSE)
  }
  if (m < 0) stop("margin must be non-negative.", call. = FALSE)
  dp <- sqrt(rowSums((A - P)^2))
  dn <- sqrt(rowSums((A - N)^2))
  losses <- pmax(0, dp - dn + m)
  list(estimate = mean(losses), losses = losses, active = losses > 0,
       d_positive = dp, d_negative = dn, n = nrow(A),
       method = "Triplet loss (Schroff et al. 2015)")
}

#' @rdname morie_alammar_cosine_similarity_loss
#' @param negatives Matrix of negative embeddings.
#' @param tau Temperature.
#' @export
morie_alammar_infonce_loss <- function(anchor, positive, negatives,
                                       tau = 0.07) {
  t <- as.numeric(tau)
  if (t <= 0) stop("the temperature must be positive.", call. = FALSE)
  a <- as.numeric(anchor); p <- as.numeric(positive)
  N <- as.matrix(negatives)
  sp <- .morie_al_cos(a, p) / t
  sn <- vapply(seq_len(nrow(N)), function(i) {
    .morie_al_cos(a, N[i, ]) / t
  }, numeric(1))
  zs <- c(sp, sn)
  m <- max(zs)
  logZ <- m + log(sum(exp(zs - m)))
  list(estimate = logZ - sp, positive_similarity = sp * t,
       negative_similarities = sn * t, n = nrow(N) + 1L,
       method = "InfoNCE (van den Oord et al. 2018)")
}

.morie_al_inbatch_ce <- function(S) {
  Z <- S - apply(S, 1, max)
  logp <- Z - log(rowSums(exp(Z)))
  -diag(logp)
}

#' @rdname morie_alammar_cosine_similarity_loss
#' @param anchors,positives Row-matched batches; every other positive
#'   is an in-batch negative.
#' @export
morie_alammar_multiple_negatives_ranking <- function(anchors, positives,
                                                     tau = 0.05) {
  t <- as.numeric(tau)
  if (t <= 0) stop("the temperature must be positive.", call. = FALSE)
  A <- as.matrix(anchors); P <- as.matrix(positives)
  if (!all(dim(A) == dim(P))) {
    stop("anchors and positives must align.", call. = FALSE)
  }
  B <- nrow(A)
  if (B < 2L) {
    stop("in-batch negatives need a batch of at least 2; with one pair there are no negatives and the loss is trivially 0.",
         call. = FALSE)
  }
  S <- outer(seq_len(B), seq_len(B),
             Vectorize(function(i, j) .morie_al_cos(A[i, ], P[j, ]) / t))
  losses <- .morie_al_inbatch_ce(S)
  list(estimate = mean(losses), losses = losses,
       similarity_matrix = S * t, n = B,
       method = "Multiple negatives ranking (Henderson et al. 2017)")
}

#' @rdname morie_alammar_cosine_similarity_loss
#' @param embeddings_dropout1,embeddings_dropout2 Two dropout passes of
#'   the same sentences.
#' @export
morie_alammar_simcse_dropout_aug <- function(embeddings_dropout1,
                                             embeddings_dropout2,
                                             tau = 0.05) {
  t <- as.numeric(tau)
  if (t <= 0) stop("the temperature must be positive.", call. = FALSE)
  H1 <- as.matrix(embeddings_dropout1)
  H2 <- as.matrix(embeddings_dropout2)
  if (!all(dim(H1) == dim(H2))) {
    stop("the two dropout passes must align.", call. = FALSE)
  }
  B <- nrow(H1)
  if (B < 2L) stop("need a batch of at least 2 for in-batch negatives.",
                   call. = FALSE)
  S <- outer(seq_len(B), seq_len(B),
             Vectorize(function(i, j) .morie_al_cos(H1[i, ], H2[j, ]) / t))
  losses <- .morie_al_inbatch_ce(S)
  list(estimate = mean(losses), losses = losses, n = B,
       method = "SimCSE with dropout augmentation (Gao et al. 2021)")
}

#' @rdname morie_alammar_cosine_similarity_loss
#' @param I_emb,T_emb Image and text towers, row-matched.
#' @export
morie_alammar_openclip_contrastive <- function(I_emb, T_emb, tau = 0.07) {
  t <- as.numeric(tau)
  if (t <= 0) stop("the temperature must be positive.", call. = FALSE)
  I <- as.matrix(I_emb); Tm <- as.matrix(T_emb)
  if (!all(dim(I) == dim(Tm))) {
    stop("image and text batches must align.", call. = FALSE)
  }
  if (nrow(I) < 2L) stop("need a batch of at least 2.", call. = FALSE)
  ni <- sqrt(rowSums(I^2)); nt <- sqrt(rowSums(Tm^2))
  if (any(ni == 0) || any(nt == 0)) {
    stop("zero embedding vectors have no direction.", call. = FALSE)
  }
  I <- I / ni; Tm <- Tm / nt
  S <- I %*% t(Tm) / t
  li <- .morie_al_inbatch_ce(S)
  lt <- .morie_al_inbatch_ce(t(S))
  list(estimate = (mean(li) + mean(lt)) / 2,
       image_to_text_loss = mean(li), text_to_image_loss = mean(lt),
       similarity_matrix = S * t, n = nrow(I),
       method = "CLIP symmetric contrastive loss (Radford et al. 2021)")
}

#' Skip-gram negative sampling and the Bradley-Terry reward loss
#' @param center_vec,context_vec,negative_vecs Word vectors.
#' @export
morie_alammar_negative_sampling_skipgram <- function(center_vec,
                                                     context_vec,
                                                     negative_vecs) {
  c <- as.numeric(center_vec); w <- as.numeric(context_vec)
  N <- as.matrix(negative_vecs)
  if (length(c) != length(w) || ncol(N) != length(c)) {
    stop("all vectors must share one dimension.", call. = FALSE)
  }
  logsig <- function(z) -log1p(exp(-abs(z))) + pmin(z, 0)
  pos <- logsig(sum(c * w))
  negs <- vapply(seq_len(nrow(N)), function(i) {
    logsig(-sum(c * N[i, ]))
  }, numeric(1))
  list(estimate = -(pos + sum(negs)), positive_logsig = pos,
       negative_logsigs = negs, k = nrow(N), n = length(c),
       method = "Skip-gram negative sampling (Mikolov et al. 2013)")
}

#' @rdname morie_alammar_negative_sampling_skipgram
#' @param scores_w,scores_l Winner and loser reward scores.
#' @export
morie_alammar_reward_model_bt <- function(scores_w, scores_l) {
  rw <- as.numeric(scores_w); rl <- as.numeric(scores_l)
  if (length(rw) != length(rl)) {
    stop("need one loser score per winner score.", call. = FALSE)
  }
  diff <- rw - rl
  losses <- log1p(exp(-abs(diff))) + pmax(-diff, 0)   # log(1 + e^-d)
  list(estimate = mean(losses), losses = losses,
       pair_accuracy = mean(diff > 0), n = length(diff),
       method = "Bradley-Terry reward loss (Ouyang et al. 2022)")
}

# ------------------------------------------------------------------
# Metrics and decoding
# ------------------------------------------------------------------

#' Retrieval metrics: MRR, recall@k, NDCG@k, MTEB aggregation
#' @param rankings List of ranked id vectors.
#' @param relevant_indices List of relevant-id vectors.
#' @export
morie_alammar_mean_reciprocal_rank <- function(rankings,
                                               relevant_indices) {
  if (length(rankings) != length(relevant_indices)) {
    stop("need one relevant set per ranking.", call. = FALSE)
  }
  if (length(rankings) == 0L) stop("no queries supplied.", call. = FALSE)
  rrs <- numeric(length(rankings))
  missed <- 0L
  for (q in seq_along(rankings)) {
    hit <- match(TRUE, rankings[[q]] %in% relevant_indices[[q]])
    if (is.na(hit)) {
      missed <- missed + 1L
    } else {
      rrs[q] <- 1 / hit
    }
  }
  list(estimate = mean(rrs), reciprocal_ranks = rrs,
       queries_missed = missed, n = length(rrs),
       method = "Mean reciprocal rank (Alammar Ch 8)")
}

#' @rdname morie_alammar_mean_reciprocal_rank
#' @param retrieved Ranked ids.
#' @param relevant Relevant ids.
#' @param k k.
#' @export
morie_alammar_recall_at_k <- function(retrieved, relevant, k) {
  k <- as.integer(k)
  if (k < 1L) stop("k must be positive.", call. = FALSE)
  rel <- unique(relevant)
  if (length(rel) == 0L) {
    stop("the relevant set is empty; recall is 0/0 and reporting 1 or 0 there would be a choice, not a measurement.",
         call. = FALSE)
  }
  top <- utils::head(retrieved, k)
  hit <- sum(rel %in% top)
  list(estimate = hit / length(rel), hits = hit, n_relevant = length(rel),
       k = k, n = length(top), method = "Recall@k (Alammar Ch 8)")
}

#' @rdname morie_alammar_mean_reciprocal_rank
#' @param relevances Graded relevances in ranked order.
#' @export
morie_alammar_ndcg_at_k <- function(relevances, k) {
  r <- as.numeric(relevances)
  k <- as.integer(k)
  if (k < 1L) stop("k must be positive.", call. = FALSE)
  if (any(r < 0)) stop("graded relevances must be non-negative.",
                       call. = FALSE)
  dcg <- function(v) {
    v <- utils::head(v, k)
    sum((2^v - 1) / log2(seq_along(v) + 1))
  }
  got <- dcg(r)
  ideal <- dcg(sort(r, decreasing = TRUE))
  if (ideal == 0) {
    stop("every relevance is 0, so IDCG is 0 and NDCG is undefined; declaring the ranking perfect there would be a lie.",
         call. = FALSE)
  }
  list(estimate = got / ideal, dcg = got, idcg = ideal, k = k,
       n = length(r), method = "NDCG@k (Jarvelin and Kekalainen 2002)")
}

#' @rdname morie_alammar_mean_reciprocal_rank
#' @param task_scores Named list/vector.
#' @param category_map Named map
#'   task -> category.
#' @export
morie_alammar_mteb_benchmark_score <- function(task_scores, category_map) {
  ts <- unlist(task_scores)
  if (length(ts) == 0L) stop("no task scores supplied.", call. = FALSE)
  missing <- setdiff(names(ts), names(category_map))
  if (length(missing)) {
    stop(sprintf("tasks %s have no category.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  cats <- unlist(category_map)[names(ts)]
  cat_means <- tapply(ts, cats, mean)
  overall <- mean(cat_means)
  flat <- mean(ts)
  list(estimate = overall, category_means = as.list(cat_means),
       flat_task_mean = flat,
       weighting_matters = abs(overall - flat) > 1e-12, n = length(ts),
       method = "MTEB mean-of-category-means (Muennighoff et al. 2023)")
}

#' Greedy and sampled decoding (Alammar Ch 6)
#' @param logits Matrix, one row per step.
#' @export
morie_alammar_greedy_decoding <- function(logits) {
  Z <- as.matrix(logits)
  toks <- integer(nrow(Z)); ties <- logical(nrow(Z))
  for (i in seq_len(nrow(Z))) {
    m <- max(Z[i, ])
    winners <- which(Z[i, ] == m)
    toks[i] <- winners[1] - 1L
    ties[i] <- length(winners) > 1L
  }
  list(tokens = toks, had_ties = ties, estimate = as.numeric(toks[1]),
       n = length(toks), method = "Greedy decoding argmax (Alammar Ch 6)")
}

#' @rdname morie_alammar_greedy_decoding
#' @param seed LCG seed; the Python mirror draws the same tokens.
#' @export
morie_alammar_sampling_decoding <- function(logits, seed = 0) {
  Z <- as.matrix(logits)
  s <- as.numeric(seed) %% 2^32
  toks <- integer(nrow(Z)); us <- numeric(nrow(Z))
  for (i in seq_len(nrow(Z))) {
    z <- Z[i, ] - max(Z[i, ])
    p <- exp(z) / sum(exp(z))
    s <- .morie_al_lcg(s)
    u <- (s + 0.5) / 2^32
    us[i] <- u
    toks[i] <- sum(cumsum(p) <= u)   # smallest v with cum > u, 0-based
  }
  list(tokens = toks, uniforms = us, estimate = as.numeric(toks[1]),
       n = length(toks),
       method = "Ancestral sampling via shared LCG (Alammar Ch 6)")
}

# ------------------------------------------------------------------
# Text and RAG utilities
# ------------------------------------------------------------------

#' BoW, c-TF-IDF, BIO tagging, vocabulary overlap (Alammar Ch 1-5)
#' @param tokens,vocab Character vectors.
#' @export
morie_alammar_bag_of_words <- function(tokens, vocab) {
  toks <- as.character(tokens); voc <- as.character(vocab)
  if (anyDuplicated(voc)) stop("the vocabulary contains duplicates.",
                               call. = FALSE)
  if (length(voc) == 0L) stop("the vocabulary is empty.", call. = FALSE)
  bow <- vapply(voc, function(v) sum(toks == v), integer(1))
  oov <- sum(!(toks %in% voc))
  list(bow_vector = unname(bow), oov_count = oov,
       estimate = as.numeric(bow[1]), vocab_size = length(voc),
       n = length(toks),
       method = "Bag-of-words counts over a fixed vocabulary (Alammar Ch 1)")
}

#' @rdname morie_alammar_bag_of_words
#' @param term_counts_by_class Classes x terms count matrix.
#' @param corpus_freq,A Optional column sums and mean class size.
#' @export
morie_alammar_c_tfidf <- function(term_counts_by_class, corpus_freq = NULL,
                                  A = NULL) {
  M <- as.matrix(term_counts_by_class)
  if (any(M < 0)) stop("counts must be non-negative.", call. = FALSE)
  f_t <- if (is.null(corpus_freq)) colSums(M) else as.numeric(corpus_freq)
  if (length(f_t) != ncol(M)) {
    stop("corpus_freq must have one entry per term.", call. = FALSE)
  }
  if (any(f_t <= 0)) {
    stop("a term with zero corpus frequency cannot be weighted; drop it before calling.",
         call. = FALSE)
  }
  a <- if (is.null(A)) mean(rowSums(M)) else as.numeric(A)
  W <- M * matrix(log1p(a / f_t), nrow(M), ncol(M), byrow = TRUE)
  list(weights = W, A = a, corpus_freq = f_t,
       top_term_per_class = apply(W, 1, which.max) - 1L,
       estimate = W[1, 1], n = nrow(M),
       method = "c-TF-IDF (Grootendorst 2022, Eq 3)")
}

#' @rdname morie_alammar_bag_of_words
#' @param entity_spans List of c(start, end, type), 0-based, end
#'   exclusive, matching the Python mirror.
#' @param scheme BIO or BIOES.
#' @export
morie_alammar_bio_tagging <- function(tokens, entity_spans,
                                      scheme = "BIO") {
  toks <- as.character(tokens)
  n <- length(toks)
  scheme <- toupper(as.character(scheme))
  if (!scheme %in% c("BIO", "BIOES")) {
    stop(sprintf("scheme must be BIO or BIOES; got '%s'.", scheme),
         call. = FALSE)
  }
  tags <- rep("O", n)
  claimed <- rep(FALSE, n)
  for (span in entity_spans) {
    s <- as.integer(span[[1]]); e <- as.integer(span[[2]])
    typ <- as.character(span[[3]])
    if (s < 0L || e <= s || e > n) {
      stop(sprintf("span (%d, %d) is out of range for %d tokens (end exclusive).",
                   s, e, n), call. = FALSE)
    }
    if (any(claimed[(s + 1L):e])) {
      stop(sprintf("span (%d, %d) overlaps an earlier span; BIO cannot represent overlapping entities.",
                   s, e), call. = FALSE)
    }
    claimed[(s + 1L):e] <- TRUE
    if (scheme == "BIO") {
      tags[s + 1L] <- paste0("B-", typ)
      if (e - s > 1L) tags[(s + 2L):e] <- paste0("I-", typ)
    } else {
      if (e - s == 1L) {
        tags[s + 1L] <- paste0("S-", typ)
      } else {
        tags[s + 1L] <- paste0("B-", typ)
        if (e - s > 2L) tags[(s + 2L):(e - 1L)] <- paste0("I-", typ)
        tags[e] <- paste0("E-", typ)
      }
    }
  }
  list(tags = tags, n_entities = length(entity_spans),
       estimate = sum(tags != "O"), n = n,
       method = sprintf("%s span tagging (Alammar Ch 4)", scheme))
}

#' @rdname morie_alammar_bag_of_words
#' @param vocab_a,vocab_b Token vocabularies.
#' @export
morie_alammar_tokenizer_vocab_overlap <- function(vocab_a, vocab_b) {
  A <- unique(as.character(vocab_a)); B <- unique(as.character(vocab_b))
  if (length(A) == 0L && length(B) == 0L) {
    stop("both vocabularies are empty; 0/0.", call. = FALSE)
  }
  inter <- length(intersect(A, B)); un <- length(union(A, B))
  list(estimate = inter / un, intersection = inter, union = un,
       only_a = length(setdiff(A, B)), only_b = length(setdiff(B, A)),
       n = un, method = "Jaccard vocabulary overlap (Alammar Ch 2)")
}

#' Recursive chunking, buffer memory, templates (Alammar Ch 6-12)
#' @param text Input string.
#' @param separators Tier list.
#' @param target_size,overlap Sizes.
#' @export
morie_alammar_recursive_chunking <- function(text, separators = NULL,
                                             target_size = 200,
                                             overlap = 0) {
  s <- as.character(text)
  seps <- if (is.null(separators)) c("\n\n", "\n", ". ", " ") else
    as.character(separators)
  size <- as.integer(target_size)
  ov <- as.integer(overlap)
  if (size < 1L) stop("target_size must be positive.", call. = FALSE)
  if (ov < 0L || ov >= size) {
    stop("overlap must be non-negative and below target_size.",
         call. = FALSE)
  }
  split_rec <- function(piece, tier) {
    if (nchar(piece) <= size) return(piece)
    if (tier > length(seps)) {
      starts <- seq(1L, nchar(piece), by = size)
      return(vapply(starts, function(i) {
        substr(piece, i, min(i + size - 1L, nchar(piece)))
      }, character(1)))
    }
    parts <- strsplit(piece, seps[tier], fixed = TRUE)[[1]]
    if (length(parts) == 1L) return(split_rec(piece, tier + 1L))
    unlist(lapply(parts, split_rec, tier = tier + 1L))
  }
  chunks <- split_rec(s, 1L)
  chunks <- chunks[nzchar(chunks)]
  if (ov > 0L && length(chunks) > 1L) {
    for (i in seq(length(chunks), 2L)) {
      prev <- chunks[i - 1L]
      tail_ <- substr(prev, max(1L, nchar(prev) - ov + 1L), nchar(prev))
      chunks[i] <- paste0(tail_, chunks[i])
    }
  }
  list(chunks = chunks, n_chunks = length(chunks),
       max_chunk_length = if (length(chunks)) max(nchar(chunks)) else 0L,
       overlap = ov, estimate = length(chunks), n = nchar(s),
       method = "Recursive character chunking (Alammar Ch 8)")
}

#' @rdname morie_alammar_recursive_chunking
#' @param conversation List of c(user, assistant) turns.
#' @param N Window.
#' @export
morie_alammar_conversation_buffer_memory <- function(conversation, N) {
  n <- as.integer(N)
  if (n < 1L) stop("N must be positive.", call. = FALSE)
  turns <- lapply(conversation, function(t) c(as.character(t[[1]]),
                                              as.character(t[[2]])))
  kept <- utils::tail(turns, n)
  list(memory = kept, turns_forgotten = max(0L, length(turns) - n),
       estimate = length(kept), n = length(turns),
       method = "Conversation buffer memory (Alammar Ch 7)")
}

#' @rdname morie_alammar_recursive_chunking
#' @param turns List of c(role, content).
#' @param template_tokens Named
#'   list role -> c(open, close).
#' @export
morie_alammar_chat_template <- function(turns, template_tokens = NULL) {
  tt <- if (is.null(template_tokens)) {
    list(system = c("<|system|>\n", "\n"), user = c("<|user|>\n", "\n"),
         assistant = c("<|assistant|>\n", "\n"))
  } else template_tokens
  parts <- character(0)
  for (turn in turns) {
    role <- as.character(turn[[1]])
    if (is.null(tt[[role]])) {
      stop(sprintf("role '%s' has no template tokens; rendering it unmarked would hide the turn from the model.",
                   role), call. = FALSE)
    }
    oc <- tt[[role]]
    parts <- c(parts, paste0(oc[1], turn[[2]], oc[2]))
  }
  prompt <- paste(parts, collapse = "")
  list(prompt = prompt, n_turns = length(turns),
       estimate = nchar(prompt), n = length(turns),
       method = "Chat template rendering (Alammar Ch 6)")
}

#' @rdname morie_alammar_recursive_chunking
#' @param prompts,chosen,rejected Aligned character vectors.
#' @export
morie_alammar_chosen_rejected_template <- function(prompts, chosen,
                                                   rejected) {
  P <- as.character(prompts); C <- as.character(chosen)
  R <- as.character(rejected)
  if (length(P) != length(C) || length(P) != length(R)) {
    stop("prompts, chosen and rejected must align.", call. = FALSE)
  }
  if (length(P) == 0L) stop("no records supplied.", call. = FALSE)
  for (i in seq_along(P)) {
    if (C[i] == R[i]) {
      stop(sprintf("record %d has identical chosen and rejected; it encodes no preference.",
                   i - 1L), call. = FALSE)
    }
  }
  recs <- lapply(seq_along(P), function(i) {
    list(prompt = P[i], chosen = C[i], rejected = R[i])
  })
  list(records = recs, estimate = length(recs), n = length(recs),
       method = "Preference pair records (Alammar Ch 12)")
}

#' @rdname morie_alammar_recursive_chunking
#' @param records List of lists with instruction/input/output.
#' @param template Format string with `{instruction}` and `{input}`.
#' @export
morie_alammar_instruction_data_template <- function(records,
                                                    template = NULL) {
  tmpl <- if (is.null(template)) {
    "### Instruction:\n{instruction}\n### Input:\n{input}\n### Response:\n"
  } else template
  if (length(records) == 0L) stop("no records supplied.", call. = FALSE)
  texts <- character(0); spans <- list()
  for (i in seq_along(records)) {
    rec <- records[[i]]
    for (key in c("instruction", "output")) {
      if (is.null(rec[[key]])) {
        stop(sprintf("record %d is missing '%s'.", i - 1L, key),
             call. = FALSE)
      }
    }
    head_ <- gsub("{instruction}", rec$instruction, tmpl, fixed = TRUE)
    head_ <- gsub("{input}", if (is.null(rec$input)) "" else rec$input,
                  head_, fixed = TRUE)
    out <- as.character(rec$output)
    texts <- c(texts, paste0(head_, out))
    spans[[i]] <- c(nchar(head_), nchar(head_) + nchar(out))
  }
  list(texts = texts, output_spans = spans, estimate = length(texts),
       n = length(texts),
       method = "Instruction template with output loss mask (Alammar Ch 11)")
}

#' WordPiece tokenisation pipeline (Alammar Ch 2)
#' @param text Input.
#' @param vocab Vocabulary incl. UNK and specials.
#' @param unk_token,lowercase,specials Pipeline settings.
#' @export
morie_alammar_tokenization_pipeline <- function(text, vocab,
                                                unk_token = "[UNK]",
                                                lowercase = TRUE,
                                                specials = c("[CLS]",
                                                             "[SEP]")) {
  voc <- as.character(vocab)
  if (!(unk_token %in% voc)) {
    stop(sprintf("the vocabulary must contain '%s'.", unk_token),
         call. = FALSE)
  }
  s <- as.character(text)
  if (isTRUE(lowercase)) s <- tolower(s)
  words <- strsplit(trimws(s), "\\s+")[[1]]
  toks <- character(0)
  for (w in words) {
    i <- 1L
    pieces <- character(0)
    ok <- TRUE
    while (i <= nchar(w)) {
      j <- nchar(w)
      found <- NULL
      while (j >= i) {
        cand <- if (i == 1L) substr(w, i, j) else
          paste0("##", substr(w, i, j))
        if (cand %in% voc) { found <- cand; break }
        j <- j - 1L
      }
      if (is.null(found)) { ok <- FALSE; break }
      pieces <- c(pieces, found)
      i <- j + 1L
    }
    toks <- c(toks, if (ok) pieces else unk_token)
  }
  if (!is.null(specials) && length(specials)) {
    missing <- specials[!(specials %in% voc)]
    if (length(missing)) {
      stop(sprintf("special tokens %s are not in the vocabulary.",
                   paste(missing, collapse = ", ")), call. = FALSE)
    }
    toks <- c(specials[1], toks, specials[2])
  }
  list(tokens = toks, n_unk = sum(toks == unk_token),
       estimate = length(toks), n = length(words),
       method = "WordPiece tokenisation pipeline (Alammar Ch 2)")
}

# ------------------------------------------------------------------
# Clustering, topics, projection
# ------------------------------------------------------------------

#' Density clustering, HDBSCAN style (Campello et al. 2013)
#'
#' Core distance to the min_samples-th neighbour, mutual reachability,
#' single-linkage MST, and a flat cut chosen to MAXIMISE the number of
#' clusters of size >= min_cluster_size (a single largest-gap cut
#' fails on blobs plus one far outlier). Noise is -1. The Prim
#' implementation's visited mask is load-bearing -- overwriting the
#' Inf of in-tree nodes re-admits them, the bug the Python mirror
#' caught on planted blobs.
#'
#' @param X Point matrix.
#' @param min_cluster_size,min_samples Sizes.
#' @export
morie_alammar_hdbscan_cluster <- function(X, min_cluster_size = 3,
                                          min_samples = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  mcs <- as.integer(min_cluster_size)
  ms <- if (is.null(min_samples)) mcs else as.integer(min_samples)
  if (mcs < 2L) stop("min_cluster_size must be at least 2.", call. = FALSE)
  if (ms < 1L || ms >= n) {
    stop(sprintf("min_samples must lie in [1, %d]; got %d.", n - 1L, ms),
         call. = FALSE)
  }
  D <- as.matrix(stats::dist(X))
  core <- apply(D, 1, function(r) sort(r)[ms + 1L])
  MR <- pmax(outer(core, rep(1, n)), outer(rep(1, n), core), D)
  diag(MR) <- 0
  visited <- rep(FALSE, n); visited[1] <- TRUE
  edges <- matrix(0, n - 1L, 3L)
  dist_ <- MR[1, ]; src <- rep(1L, n); dist_[1] <- Inf
  for (step in seq_len(n - 1L)) {
    j <- which.min(dist_)
    edges[step, ] <- c(dist_[j], src[j], j)
    visited[j] <- TRUE
    upd <- (MR[j, ] < dist_) & !visited
    src[upd] <- j
    dist_[upd] <- MR[j, upd]
    dist_[visited] <- Inf
  }
  edges <- edges[order(edges[, 1]), , drop = FALSE]
  components <- function(threshold) {
    parent <- seq_len(n)
    find <- function(a) {
      while (parent[a] != a) {
        parent[a] <<- parent[parent[a]]
        a <- parent[a]
      }
      a
    }
    for (e in seq_len(nrow(edges))) {
      if (edges[e, 1] < threshold) {
        ra <- find(edges[e, 2]); rb <- find(edges[e, 3])
        if (ra != rb) parent[ra] <- rb
      }
    }
    split(seq_len(n), vapply(seq_len(n), find, numeric(1)))
  }
  cands <- c(sort(unique(edges[, 1])), Inf)
  best <- -1L; threshold <- Inf
  for (cand in cands) {
    comps <- components(cand)
    score <- sum(vapply(comps, length, integer(1)) >= mcs)
    if (score > best) { best <- score; threshold <- cand }
  }
  labels <- rep(-1L, n)
  lab <- 0L
  comps <- components(threshold)
  comps <- comps[order(vapply(comps, min, numeric(1)))]
  for (members in comps) {
    if (length(members) >= mcs) {
      labels[members] <- lab
      lab <- lab + 1L
    }
  }
  list(labels = labels, n_clusters = lab, n_noise = sum(labels == -1L),
       core_distances = as.numeric(core), cut_threshold = threshold,
       estimate = as.numeric(lab), n = n,
       method = "Mutual-reachability single linkage with min cluster size (Campello et al. 2013, cluster-count-maximising cut)")
}

#' UMAP objective minimised by descent (McInnes et al. 2018,
#' simplified: exact k-NN, bisected sigma, full-batch descent)
#' @param X Points.
#' @param n_neighbors,min_dist,d_out,n_steps,learning_rate,seed Settings.
#' @export
morie_alammar_umap_projection <- function(X, n_neighbors = 5,
                                          min_dist = 0.1, d_out = 2,
                                          n_steps = 200,
                                          learning_rate = 0.05, seed = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  k <- as.integer(n_neighbors)
  if (k < 2L || k >= n) {
    stop(sprintf("n_neighbors must lie in [2, %d].", n - 1L),
         call. = FALSE)
  }
  dd <- as.integer(d_out)
  if (dd < 1L) stop("d_out must be positive.", call. = FALSE)
  D <- as.matrix(stats::dist(X))
  W <- matrix(0, n, n)
  log2k <- log2(k)
  for (i in seq_len(n)) {
    ord <- order(D[i, ])
    nb <- ord[2:(k + 1L)]
    dists <- D[i, nb]
    rho <- dists[1]
    lo <- 1e-8; hi <- 1e4
    for (it in seq_len(64L)) {
      sig <- (lo + hi) / 2
      s <- sum(exp(-pmax(dists - rho, 0) / sig))
      if (s > log2k) hi <- sig else lo <- sig
    }
    sig <- (lo + hi) / 2
    W[i, nb] <- exp(-pmax(dists - rho, 0) / sig)
  }
  P <- W + t(W) - W * t(W)
  a <- if (min_dist > 0) 1 / (min_dist^2 + 1e-12) else 100
  s <- as.numeric(seed) %% 2^32
  u <- numeric(n * dd)
  for (i in seq_len(n * dd)) {
    s <- .morie_al_lcg(s)
    u[i] <- (s + 0.5) / 2^32
  }
  # fill row-major to match Python's reshape(n, dd)
  Z <- matrix(u, n, dd, byrow = TRUE)
  Z <- (Z - 0.5) * 10
  objective <- function(Z) {
    dz2 <- as.matrix(stats::dist(Z))^2
    Q <- 1 / (1 + a * dz2)
    diag(Q) <- 1
    eps <- 1e-12
    ce <- -(P * log(Q + eps) + (1 - P) * log(1 - Q + eps))
    diag(ce) <- 0
    sum(ce)
  }
  obj0 <- objective(Z)
  lr <- as.numeric(learning_rate)
  for (step in seq_len(as.integer(n_steps))) {
    dz2 <- as.matrix(stats::dist(Z))^2
    Q <- 1 / (1 + a * dz2)
    eps <- 1e-12
    coeff <- P * a * Q - (1 - P) * a * Q * Q / (1 - Q + eps)
    diag(coeff) <- 0
    grad <- matrix(0, n, dd)
    for (d in seq_len(dd)) {
      diffd <- outer(Z[, d], Z[, d], "-")
      grad[, d] <- 2 * rowSums(coeff * diffd)
    }
    Z <- Z - lr * grad
  }
  obj1 <- objective(Z)
  list(embedding = Z, objective_initial = obj0, objective_final = obj1,
       objective_decreased = obj1 < obj0, estimate = obj1, n = n,
       method = "UMAP fuzzy cross-entropy, full-batch descent (McInnes et al. 2018, simplified)")
}

#' LDA by collapsed Gibbs on the shared LCG (Griffiths and Steyvers
#' 2004, Eq 5)
#' @param documents List of token vectors.
#' @param n_topics K.
#' @param alpha,beta,n_iter,seed Settings.
#' @export
morie_alammar_lda_topic_distribution <- function(documents, n_topics,
                                                 alpha = 0.1, beta = 0.01,
                                                 n_iter = 200, seed = 1) {
  docs <- lapply(documents, as.character)
  K <- as.integer(n_topics)
  if (K < 2L) stop("n_topics must be at least 2.", call. = FALSE)
  if (length(docs) == 0L || any(vapply(docs, length, integer(1)) == 0L)) {
    stop("every document must contain at least one token.", call. = FALSE)
  }
  a <- as.numeric(alpha); b <- as.numeric(beta)
  if (a <= 0 || b <= 0) stop("alpha and beta must be positive.",
                             call. = FALSE)
  vocab <- sort(unique(unlist(docs)))
  V <- length(vocab)
  s <- as.numeric(seed) %% 2^32
  unif <- function() {
    s <<- .morie_al_lcg(s)
    (s + 0.5) / 2^32
  }
  n_dk <- matrix(0, length(docs), K)
  n_kw <- matrix(0, K, V)
  n_k <- numeric(K)
  z <- list()
  for (di in seq_along(docs)) {
    zs <- integer(length(docs[[di]]))
    for (wi in seq_along(docs[[di]])) {
      k <- as.integer(unif() * K) + 1L
      k <- min(k, K)
      zs[wi] <- k
      v <- match(docs[[di]][wi], vocab)
      n_dk[di, k] <- n_dk[di, k] + 1
      n_kw[k, v] <- n_kw[k, v] + 1
      n_k[k] <- n_k[k] + 1
    }
    z[[di]] <- zs
  }
  for (it in seq_len(as.integer(n_iter))) {
    for (di in seq_along(docs)) {
      for (wi in seq_along(docs[[di]])) {
        k <- z[[di]][wi]
        v <- match(docs[[di]][wi], vocab)
        n_dk[di, k] <- n_dk[di, k] - 1
        n_kw[k, v] <- n_kw[k, v] - 1
        n_k[k] <- n_k[k] - 1
        p <- (n_dk[di, ] + a) * (n_kw[, v] + b) / (n_k + V * b)
        p <- p / sum(p)
        u <- unif()
        k <- sum(cumsum(p) < u) + 1L
        k <- min(k, K)
        z[[di]][wi] <- k
        n_dk[di, k] <- n_dk[di, k] + 1
        n_kw[k, v] <- n_kw[k, v] + 1
        n_k[k] <- n_k[k] + 1
      }
    }
  }
  theta <- (n_dk + a) / (rowSums(n_dk) + K * a)
  phi <- (n_kw + b) / (rowSums(n_kw) + V * b)
  list(theta = theta, phi = phi, vocabulary = vocab,
       estimate = theta[1, 1], n = length(docs),
       method = "LDA collapsed Gibbs (Griffiths and Steyvers 2004)")
}

#' Softmax head on frozen embeddings; SetFit pair generation
#' @param embeddings,labels Data (labels 0-based, every class present).
#' @param n_steps,learning_rate,l2 Training settings.
#' @export
morie_alammar_embedding_classifier <- function(embeddings, labels,
                                               n_steps = 500,
                                               learning_rate = 0.5,
                                               l2 = 1e-4) {
  X <- as.matrix(embeddings)
  y <- as.integer(labels)
  if (nrow(X) != length(y)) stop("need one label per embedding.",
                                 call. = FALSE)
  classes <- sort(unique(y))
  if (!identical(classes, seq.int(0L, length(classes) - 1L))) {
    stop("labels must be 0..K-1 with every class present.", call. = FALSE)
  }
  K <- length(classes)
  if (K < 2L) stop("need at least 2 classes.", call. = FALSE)
  n <- nrow(X); d <- ncol(X)
  W <- matrix(0, K, d); b <- numeric(K)
  lr <- as.numeric(learning_rate)
  for (step in seq_len(as.integer(n_steps))) {
    Zl <- X %*% t(W) + matrix(b, n, K, byrow = TRUE)
    P <- .morie_al_softmax_rows(Zl)
    G <- P
    G[cbind(seq_len(n), y + 1L)] <- G[cbind(seq_len(n), y + 1L)] - 1
    W <- W - lr * (t(G) %*% X / n + as.numeric(l2) * W)
    b <- b - lr * colMeans(G)
  }
  Zl <- X %*% t(W) + matrix(b, n, K, byrow = TRUE)
  P <- .morie_al_softmax_rows(Zl)
  pred <- apply(P, 1, which.max) - 1L
  list(weights = W, bias = b, train_accuracy = mean(pred == y),
       cross_entropy = mean(-log(P[cbind(seq_len(n), y + 1L)] + 1e-12)),
       predictions = as.integer(pred), estimate = mean(pred == y), n = n,
       method = "Softmax head on frozen embeddings (Alammar Ch 4)")
}

#' @rdname morie_alammar_embedding_classifier
#' @export
morie_alammar_setfit_twostep <- function(embeddings, labels) {
  X <- as.matrix(embeddings)
  y <- as.integer(labels)
  if (nrow(X) != length(y)) stop("need one label per embedding.",
                                 call. = FALSE)
  n <- length(y)
  pos <- list(); neg <- list()
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      pair <- c(i - 1L, j - 1L)    # 0-based, matching Python
      if (y[i] == y[j]) pos[[length(pos) + 1L]] <- pair
      else neg[[length(neg) + 1L]] <- pair
    }
  }
  if (length(pos) == 0L || length(neg) == 0L) {
    stop("contrastive pairs need at least two classes with at least two members each.",
         call. = FALSE)
  }
  head_ <- morie_alammar_embedding_classifier(X, y)
  list(positive_pairs = pos, negative_pairs = neg,
       n_positive = length(pos), n_negative = length(neg),
       head_train_accuracy = head_$train_accuracy,
       head_predictions = head_$predictions,
       estimate = head_$train_accuracy, n = n,
       method = "SetFit pair generation + head (Tunstall et al. 2022)")
}

#' Greedy NSW approximate nearest neighbour (Malkov and Yashunin 2020)
#' @param query_vec Query.
#' @param index List with points, neighbors
#'   (0-based lists), entry (0-based).
#' @param ef_search Beam budget.
#' @export
morie_alammar_ann_search <- function(query_vec, index, ef_search = 8) {
  q <- as.numeric(query_vec)
  P <- as.matrix(index$points)
  nbrs <- index$neighbors
  entry <- as.integer(if (is.null(index$entry)) 0L else index$entry)
  ef <- as.integer(ef_search)
  if (ef < 1L) stop("ef_search must be positive.", call. = FALSE)
  if (length(nbrs) != nrow(P)) {
    stop("need one neighbour list per point.", call. = FALSE)
  }
  if (entry < 0L || entry >= nrow(P)) {
    stop("entry point out of range.", call. = FALSE)
  }
  if (ncol(P) != length(q)) {
    stop("query dimension does not match the index.", call. = FALSE)
  }
  d <- function(i) sqrt(sum((P[i + 1L, ] - q)^2))
  cur <- entry
  hops <- cur
  repeat {
    improved <- FALSE
    for (j in nbrs[[cur + 1L]]) {
      j <- as.integer(j)
      if (d(j) < d(cur)) {
        cur <- j
        hops <- c(hops, cur)
        improved <- TRUE
      }
    }
    if (!improved) break
  }
  cand <- cur
  frontier <- cur
  while (length(frontier) && length(cand) < ef) {
    nxt <- integer(0)
    for (c in frontier) {
      for (j in nbrs[[c + 1L]]) {
        j <- as.integer(j)
        if (!(j %in% cand)) {
          cand <- c(cand, j)
          nxt <- c(nxt, j)
        }
      }
    }
    frontier <- nxt
  }
  best <- cand[which.min(vapply(cand, d, numeric(1)))]
  true_best <- which.min(sqrt(rowSums((P - matrix(q, nrow(P), length(q),
                                                  byrow = TRUE))^2))) - 1L
  list(nearest = best, distance = d(best), greedy_path = hops,
       candidates_examined = length(cand), exact_nearest = true_best,
       found_exact = best == true_best, estimate = as.numeric(best),
       n = nrow(P),
       method = "Greedy NSW descent + beam (Malkov and Yashunin 2020)")
}

# ------------------------------------------------------------------
# Orchestration with caller-supplied models (R functions)
# ------------------------------------------------------------------

#' Zero-shot, T5 and judge orchestration around caller functions
#' @param text Input.
#' @param candidate_labels Labels.
#' @param nli_model function(premise, hypothesis) -> score.
#' @param hypothesis_template Format string with one %s.
#' @export
morie_alammar_zero_shot_classification <- function(text, candidate_labels,
                                                   nli_model,
                                                   hypothesis_template =
                                                     "This example is about %s.") {
  labels <- as.character(candidate_labels)
  if (length(labels) == 0L) stop("no candidate labels supplied.",
                                 call. = FALSE)
  if (anyDuplicated(labels)) stop("candidate labels contain duplicates.",
                                  call. = FALSE)
  if (!is.function(nli_model)) {
    stop("nli_model must be a function (premise, hypothesis) -> score.",
         call. = FALSE)
  }
  scores <- vapply(labels, function(l) {
    as.numeric(nli_model(as.character(text), sprintf(hypothesis_template, l)))
  }, numeric(1))
  z <- scores - max(scores)
  p <- exp(z) / sum(exp(z))
  list(probabilities = as.list(stats::setNames(p, labels)),
       predicted_label = labels[which.max(p)],
       entailment_scores = unname(scores), estimate = max(p),
       n = length(labels),
       method = "Zero-shot NLI classification (Yin et al. 2019)")
}

#' @rdname morie_alammar_zero_shot_classification
#' @param input_text Input.
#' @param label_tokens Closed label set.
#' @param model function(input, label) -> log-probability.
#' @param prefix Optional task prefix.
#' @export
morie_alammar_t5_classify <- function(input_text, label_tokens, model,
                                      prefix = "") {
  labels <- as.character(label_tokens)
  if (length(labels) == 0L) stop("no label tokens supplied.",
                                 call. = FALSE)
  if (!is.function(model)) {
    stop("model must be a function (input, label) -> log-probability.",
         call. = FALSE)
  }
  lp <- vapply(labels, function(l) {
    as.numeric(model(paste0(prefix, input_text), l))
  }, numeric(1))
  z <- lp - max(lp)
  p <- exp(z) / sum(exp(z))
  list(predicted_label = labels[which.max(p)],
       probabilities = as.list(stats::setNames(p, labels)),
       log_scores = unname(lp), estimate = max(p), n = length(labels),
       method = "T5 text-to-text classification (Raffel et al. 2020)")
}

#' @rdname morie_alammar_zero_shot_classification
#' @param responses Character vector.
#' @param rubric Judge rubric.
#' @param judge_model function(rubric, response, sample_index) -> score.
#' @param n_samples Judge samples per response.
#' @export
morie_alammar_llm_as_judge <- function(responses, rubric, judge_model,
                                       n_samples = 1) {
  if (!is.function(judge_model)) {
    stop("judge_model must be a function (rubric, response, sample_index) -> score.",
         call. = FALSE)
  }
  R <- as.character(responses)
  if (length(R) == 0L) stop("no responses supplied.", call. = FALSE)
  k <- as.integer(n_samples)
  if (k < 1L) stop("n_samples must be positive.", call. = FALSE)
  scores <- t(vapply(R, function(r) {
    vapply(seq_len(k) - 1L, function(s) {
      as.numeric(judge_model(rubric, r, s))
    }, numeric(1))
  }, numeric(k)))
  if (k == 1L) scores <- matrix(scores, ncol = 1L)
  means <- rowMeans(scores)
  sds <- if (k > 1L) apply(scores, 1, stats::sd) else numeric(length(R))
  list(scores = unname(means), judge_sd = unname(sds),
       best_response = which.max(means) - 1L, estimate = max(means),
       n = length(R),
       method = "LLM-as-judge with self-disagreement reported (Zheng et al. 2023)")
}

#' @rdname morie_alammar_zero_shot_classification
#' @param response Text under test.
#' @param criteria Character vector.
#' @param verifier_model function(response, criterion) -> "PASS"|"FAIL".
#' @export
morie_alammar_output_verification <- function(response, criteria,
                                              verifier_model) {
  if (!is.function(verifier_model)) {
    stop("verifier_model must be a function (response, criterion) -> 'PASS' | 'FAIL'.",
         call. = FALSE)
  }
  crits <- as.character(criteria)
  if (length(crits) == 0L) {
    stop("no criteria supplied; an empty gate passes everything and verifies nothing.",
         call. = FALSE)
  }
  verdicts <- character(length(crits))
  for (i in seq_along(crits)) {
    v <- verifier_model(as.character(response), crits[i])
    if (!(v %in% c("PASS", "FAIL"))) {
      stop(sprintf("verifier returned '%s' for '%s'; only 'PASS' or 'FAIL' count as answers.",
                   v, crits[i]), call. = FALSE)
    }
    verdicts[i] <- v
  }
  names(verdicts) <- crits
  passed <- all(verdicts == "PASS")
  list(passed = passed, verdicts = as.list(verdicts),
       failed_criteria = crits[verdicts == "FAIL"],
       estimate = as.numeric(passed), n = length(crits),
       method = "Criterion-gated output verification (Alammar Ch 7)")
}

#' Prompt chains, multi-query retrieval, the ReAct loop
#' @param x Original input.
#' @param prompts List of
#'   function(previous_output, original_input) -> prompt.
#' @param model function(prompt) -> text.
#' @export
morie_alammar_chain_prompting <- function(x, prompts, model) {
  if (!is.function(model)) stop("model must be a function prompt -> text.",
                                call. = FALSE)
  if (length(prompts) == 0L) stop("no prompts supplied.", call. = FALSE)
  y <- NULL
  steps <- list()
  for (i in seq_along(prompts)) {
    P <- prompts[[i]]
    if (!is.function(P)) {
      stop(sprintf("prompt %d is not a function.", i - 1L), call. = FALSE)
    }
    prompt <- P(y, x)
    y <- as.character(model(prompt))
    steps[[i]] <- list(prompt = as.character(prompt), output = y)
  }
  list(final_output = y, steps = steps, estimate = length(steps),
       n = length(steps), method = "Prompt chaining (Alammar Ch 7)")
}

#' @rdname morie_alammar_chain_prompting
#' @param query Original query.
#' @param K Rephrasings.
#' @param retriever function(query) -> ranked ids.
#' @param rephraser function(query, i) -> alternative query.
#' @export
morie_alammar_multi_query_retrieval <- function(query, K, retriever,
                                                rephraser) {
  if (!is.function(retriever) || !is.function(rephraser)) {
    stop("retriever and rephraser must be functions.", call. = FALSE)
  }
  k <- as.integer(K)
  if (k < 0L) stop("K must be non-negative.", call. = FALSE)
  queries <- c(as.character(query),
               vapply(seq_len(k) - 1L, function(i) {
                 as.character(rephraser(as.character(query), i))
               }, character(1)))
  seen <- c()
  added <- integer(length(queries))
  for (qi in seq_along(queries)) {
    hits <- retriever(queries[qi])
    new <- hits[!(hits %in% seen)]
    seen <- c(seen, new)
    added[qi] <- length(new)
  }
  list(documents = seen, queries = queries, added_per_query = added,
       estimate = length(seen), n = length(queries),
       method = "Multi-query retrieval union (Alammar Ch 8)")
}

#' @rdname morie_alammar_chain_prompting
#' @param tools Named list of functions.
#' @param max_steps Budget.
#' @export
morie_alammar_react_agent_loop <- function(query, tools, model,
                                           max_steps = 5) {
  if (!is.function(model)) {
    stop("model must be a function context -> step list.", call. = FALSE)
  }
  steps <- as.integer(max_steps)
  if (steps < 1L) stop("max_steps must be positive.", call. = FALSE)
  ctx <- list(list(query = as.character(query)))
  trace <- list()
  for (i in seq_len(steps)) {
    step <- model(ctx)
    if (!is.null(step$final)) {
      trace[[length(trace) + 1L]] <- list(thought = step$thought,
                                          final = step$final)
      return(list(answer = step$final, trace = trace,
                  steps_used = length(trace), exhausted = FALSE,
                  estimate = length(trace), n = length(trace),
                  method = "ReAct loop (Yao et al. 2023)"))
    }
    action <- step$action
    obs <- if (!is.null(action) && !is.null(tools[[action]])) {
      as.character(tools[[action]](step$action_input))
    } else {
      sprintf("ERROR: unknown action '%s'; available: %s", action,
              paste(sort(names(tools)), collapse = ", "))
    }
    rec <- list(thought = step$thought, action = action,
                observation = obs)
    trace[[length(trace) + 1L]] <- rec
    ctx[[length(ctx) + 1L]] <- rec
  }
  list(answer = NULL, trace = trace, steps_used = length(trace),
       exhausted = TRUE, estimate = length(trace), n = length(trace),
       method = "ReAct loop (Yao et al. 2023)")
}

#' Captioning projection, unfreezing, continued pretraining,
#' augmentation, TSDAE
#' @param image Opaque image object passed to the encoder.
#' @param visual_encoder function(image) -> feature vector.
#' @param projector Matrix (d_llm x d_vis) or function.
#' @param llm function(projected, prompt) -> caption.
#' @param prompt Text prompt.
#' @export
morie_alammar_image_captioning <- function(image, visual_encoder,
                                           projector, llm,
                                           prompt = "Describe the image.") {
  if (!is.function(visual_encoder) || !is.function(llm)) {
    stop("visual_encoder and llm must be functions.", call. = FALSE)
  }
  feats <- as.numeric(visual_encoder(image))
  z <- if (is.function(projector)) {
    as.numeric(projector(feats))
  } else {
    W <- as.matrix(projector)
    if (ncol(W) != length(feats)) {
      stop(sprintf("projector has %d columns but the encoder produced %d features.",
                   ncol(W), length(feats)), call. = FALSE)
    }
    as.numeric(W %*% feats)
  }
  caption <- as.character(llm(z, as.character(prompt)))
  list(caption = caption, projected = z, feature_dim = length(feats),
       projected_dim = length(z), estimate = nchar(caption),
       n = length(z),
       method = "Visual projection into the LM (Alammar Ch 9)")
}

#' @rdname morie_alammar_image_captioning
#' @param n_layers,n_stages Schedule sizes.
#' @export
morie_alammar_layer_freezing <- function(n_layers, n_stages = NULL) {
  L <- as.integer(n_layers)
  if (L < 1L) stop("n_layers must be positive.", call. = FALSE)
  S <- if (is.null(n_stages)) L else as.integer(n_stages)
  if (S < 1L || S > L) {
    stop(sprintf("n_stages must lie in [1, %d].", L), call. = FALSE)
  }
  masks <- lapply(seq_len(S), function(s) {
    thaw <- round(s * L / S)
    seq_len(L) - 1L >= L - thaw
  })
  list(masks = masks, n_stages = S,
       trainable_per_stage = vapply(masks, sum, integer(1)),
       estimate = as.numeric(S), n = L,
       method = "Gradual unfreezing (Howard and Ruder 2018)")
}

#' @rdname morie_alammar_image_captioning
#' @param domain_corpus Documents.
#' @param mlm_loss_fn function(corpus,
#'   step) -> loss.
#' @param n_mlm_steps Steps.
#' @param task_loss_fn
#'   Optional function() -> loss.
#' @export
morie_alammar_continued_pretraining <- function(domain_corpus,
                                                mlm_loss_fn, n_mlm_steps,
                                                task_loss_fn = NULL) {
  if (!is.function(mlm_loss_fn)) {
    stop("mlm_loss_fn must be a function (corpus, step) -> loss.",
         call. = FALSE)
  }
  steps <- as.integer(n_mlm_steps)
  if (steps < 1L) stop("n_mlm_steps must be positive.", call. = FALSE)
  if (length(domain_corpus) == 0L) {
    stop("the domain corpus is empty.", call. = FALSE)
  }
  curve <- vapply(seq_len(steps) - 1L, function(s) {
    as.numeric(mlm_loss_fn(domain_corpus, s))
  }, numeric(1))
  list(mlm_loss_curve = curve,
       mlm_improved = if (steps > 1L) curve[steps] < curve[1] else NA,
       task_loss = if (is.function(task_loss_fn))
         as.numeric(task_loss_fn()) else NULL,
       estimate = curve[steps], n = steps,
       method = "Continued domain pretraining (Gururangan et al. 2020)")
}

#' @rdname morie_alammar_image_captioning
#' @param unlabeled_pairs List of c(a, b).
#' @param cross_encoder
#'   function(a, b) -> score.
#' @param gold_pairs,gold_labels Optional.
#' @export
morie_alammar_augmented_sbert <- function(unlabeled_pairs, cross_encoder,
                                          gold_pairs = NULL,
                                          gold_labels = NULL) {
  if (!is.function(cross_encoder)) {
    stop("cross_encoder must be a function (text_a, text_b) -> score.",
         call. = FALSE)
  }
  if (length(unlabeled_pairs) == 0L) {
    stop("no unlabeled pairs supplied.", call. = FALSE)
  }
  silver <- vapply(unlabeled_pairs, function(p) {
    as.numeric(cross_encoder(as.character(p[[1]]), as.character(p[[2]])))
  }, numeric(1))
  agreement <- NULL
  n_gold <- 0L
  if (!is.null(gold_pairs) && !is.null(gold_labels)) {
    gl <- as.numeric(gold_labels)
    if (length(gold_pairs) != length(gl)) {
      stop("gold pairs and labels must align.", call. = FALSE)
    }
    n_gold <- length(gl)
    pred <- vapply(gold_pairs, function(p) {
      as.numeric(cross_encoder(as.character(p[[1]]),
                               as.character(p[[2]])))
    }, numeric(1))
    if (n_gold >= 2L && stats::sd(gl) > 0 && stats::sd(pred) > 0) {
      agreement <- stats::cor(pred, gl)
    }
  }
  list(n_silver = length(silver), n_gold = n_gold,
       silver_labels = silver,
       cross_encoder_gold_agreement = agreement,
       estimate = mean(silver), n = length(silver) + n_gold,
       method = "Augmented SBERT silver labelling (Thakur et al. 2021)")
}

#' @rdname morie_alammar_image_captioning
#' @param tokens Token vector.
#' @param delete_ratio Deletion rate.
#' @param seed LCG seed.
#' @param reconstruction_logprob Optional
#'   per-ORIGINAL-token log-probs.
#' @export
morie_alammar_tsdae_objective <- function(tokens, delete_ratio = 0.6,
                                          seed = 1,
                                          reconstruction_logprob = NULL) {
  toks <- as.character(tokens)
  if (length(toks) == 0L) stop("no tokens supplied.", call. = FALSE)
  r <- as.numeric(delete_ratio)
  if (r <= 0 || r >= 1) stop("delete_ratio must lie in (0, 1).",
                             call. = FALSE)
  s <- as.numeric(seed) %% 2^32
  keep_mask <- logical(length(toks))
  for (i in seq_along(toks)) {
    s <- .morie_al_lcg(s)
    keep_mask[i] <- (s + 0.5) / 2^32 >= r
  }
  kept <- toks[keep_mask]
  deleted <- toks[!keep_mask]
  if (length(kept) == 0L) {
    kept <- toks[1]
    deleted <- toks[-1]
  }
  loss <- NULL
  if (!is.null(reconstruction_logprob)) {
    lps <- as.numeric(reconstruction_logprob)
    if (length(lps) != length(toks)) {
      stop("need one reconstruction log-prob per ORIGINAL token; the decoder must rebuild the uncorrupted sentence.",
           call. = FALSE)
    }
    loss <- -sum(lps)
  }
  list(corrupted = kept, deleted = deleted,
       actual_delete_ratio = length(deleted) / length(toks),
       loss = loss,
       estimate = if (!is.null(loss)) loss else length(deleted),
       n = length(toks),
       method = "TSDAE deletion corruption + NLL (Wang et al. 2021)")
}

#' BERTopic pipeline: reduce, cluster, c-TF-IDF (Grootendorst 2022)
#' @param documents List of token vectors.
#' @param embeddings One row
#'   per document.
#' @param min_cluster_size Cluster floor.
#' @export
morie_alammar_bertopic_pipeline <- function(documents, embeddings,
                                            min_cluster_size = 2) {
  docs <- lapply(documents, as.character)
  E <- as.matrix(embeddings)
  if (nrow(E) != length(docs)) {
    stop("need one embedding per document.", call. = FALSE)
  }
  if (length(docs) < 4L) {
    stop("need at least 4 documents to cluster.", call. = FALSE)
  }
  C <- sweep(E, 2, colMeans(E))
  sv <- svd(C)
  Z <- C %*% sv$v[, seq_len(min(2L, ncol(C))), drop = FALSE]
  cl <- morie_alammar_hdbscan_cluster(Z,
                                      min_cluster_size = min_cluster_size,
                                      min_samples = 1)
  labels <- cl$labels
  vocab <- sort(unique(unlist(docs)))
  clusters <- sort(unique(labels[labels >= 0L]))
  if (length(clusters) == 0L) {
    stop("every document came out as noise; loosen min_cluster_size.",
         call. = FALSE)
  }
  M <- matrix(0, length(clusters), length(vocab))
  for (i in seq_along(docs)) {
    if (labels[i] >= 0L) {
      row <- match(labels[i], clusters)
      for (w in docs[[i]]) {
        v <- match(w, vocab)
        M[row, v] <- M[row, v] + 1
      }
    }
  }
  ct <- morie_alammar_c_tfidf(M)
  W <- ct$weights
  top <- stats::setNames(
    vapply(seq_len(nrow(W)), function(i) vocab[which.max(W[i, ])],
           character(1)),
    as.character(clusters))
  list(labels = labels, reduced = Z, topic_top_word = as.list(top),
       n_topics = length(clusters), vocabulary = vocab,
       estimate = length(clusters), n = length(docs),
       method = "BERTopic: reduce, cluster, c-TF-IDF (Grootendorst 2022)")
}
