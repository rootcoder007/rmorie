# morie.fn -- function file (rootcoder007/morie)
# SSE-PT: a personalised Transformer, regularised by shared
# embeddings.
#
# A self-attentive sequential recommender models what a user did, in
# order, and ignores *who* the user is. That is a deliberate
# simplification with a cost: two users with the same recent items get
# the same recommendation, and everything the model knows about their
# differing long-run taste is thrown away.
#
# **Personalisation is a concatenation, not an extra tower.** The user
# embedding is concatenated to **every item embedding** in the sequence
# before attention, so the user modulates every position rather than
# being added once at the end. ``personalise`` does exactly that, and
# the anchor checks that two different users with an *identical* history
# receive different outputs -- which fails immediately if the user
# embedding is dropped.
#
# **But personalisation is also where the parameters explode.** One
# embedding per user, with each user contributing only a handful of
# sequences, is a recipe for memorisation.
#
# **Stochastic Shared Embeddings is the regulariser that pays for it.**
# During training, an embedding is replaced at random -- with probability
# :math:`p` -- by *another* embedding from the same table. Not zeroed,
# which is dropout; **exchanged**, which forces the representations of
# different users (or items) to remain mutually compatible rather than
# each memorising its own rows. At :math:`p = 0` it is the identity,
# which the anchor asserts exactly, and the realised replacement rate
# must track :math:`p`.
#
# References
# ----------
# Wu, L., Li, S., Hsieh, C.-J. & Sharpnack, J. (2020) "SSE-PT:
# Sequential Recommendation Via Personalized Transformer", *Proceedings
# of the 14th ACM Conference on Recommender Systems (RecSys '20)*,
# 328-337, doi:10.1145/3383313.3412258; earlier circulated as "Temporal
# Collaborative Ranking Via Personalized Transformer",
# arXiv:1908.05435. The observation that existing self-attentive
# sequential models are not personalised and that different users' rating
# patterns are treated alike; the personalised Transformer concatenating
# a user embedding with each item embedding in the input sequence; and
# the use of Stochastic Shared Embeddings regularisation, replacing an
# embedding by another from the same table with a given probability
# during training, to make the large per-user embedding table trainable.
#
# Wu, L., Li, S., Hsieh, C.-J. & Sharpnack, J. (2019) "Stochastic
# Shared Embeddings: Data-driven Regularization of Embedding Layers",
# *NeurIPS 2019*, arXiv:1905.10630. The SSE regulariser itself.
#
# Kang, W.-C. & McAuley, J. (2018) "Self-Attentive Sequential
# Recommendation", *ICDM 2018*, 197-206, arXiv:1808.09781. SASRec, the
# unpersonalised model being extended; implemented in :mod:`sasRec`.

.sse4r_personalise <- function(item_embeddings, user_embedding) {
  I <- as.matrix(item_embeddings)
  storage.mode(I) <- "double"
  u <- as.numeric(user_embedding)
  if (nrow(I) == 0L) {
    stop("sse4r: the sequence is empty")
  }
  sequence <- lapply(seq_len(nrow(I)), function(i) c(I[i, ], u))
  list(
    sequence = sequence,
    item_dim = ncol(I),
    user_dim = length(u),
    width = ncol(I) + length(u),
    length = nrow(I),
    note = "every position carries the user, so two users with the same history diverge"
  )
}

.sse4r_sse_replace <- function(indices, table_size, p = 0.0, seed = 0) {
  idx <- as.integer(indices)
  n <- as.integer(table_size)
  pr <- as.numeric(p)

  if (n < 1L) {
    stop("sse4r: the embedding table is empty")
  }
  if (pr < 0.0 || pr > 1.0) {
    stop(sprintf("sse4r: p must lie in [0,1], got %g", p))
  }
  if (any(idx < 0L | idx >= n)) {
    stop("sse4r: an index is outside the table")
  }

  if (pr == 0.0) {
    return(list(
      indices = as.list(idx),
      replaced = list(),
      p = 0.0,
      rate = 0.0,
      note = "p = 0 is exactly the identity"
    ))
  }

  .ghc_rng(seed)
  n_idx <- length(idx)
  decisions <- .ghc_unif(1.0, n_idx)
  pass <- decisions < pr
  n_pass <- sum(pass)

  out <- idx
  rep_list <- list()

  if (n_pass > 0L) {
    repls <- .ghc_unif(as.numeric(n), n_pass)
    j_vals <- as.integer(repls) %% n
    pass_idx <- which(pass)
    for (k in seq_along(pass_idx)) {
      i_pos <- pass_idx[k]
      i_0based <- i_pos - 1L
      v <- idx[i_pos]
      j <- j_vals[k]
      out[i_pos] <- j
      if (j != v) {
        rep_list[[length(rep_list) + 1L]] <- list(i_0based, v, j)
      }
    }
  }

  list(
    indices = as.list(out),
    replaced = rep_list,
    p = pr,
    rate = sum(out != idx) / as.numeric(n_idx),
    note = "the replacement is drawn from the SAME table"
  )
}

.sse4r_expected_replacement <- function(p, table_size) {
  pr <- as.numeric(p)
  n <- as.integer(table_size)
  if (n < 1L) {
    stop("sse4r: the table is empty")
  }
  list(
    expected_rate = pr * (1.0 - 1.0 / n),
    p = pr,
    table_size = n,
    note = "self-replacement is invisible, so the observed rate is below p"
  )
}

.sse4r_parameter_count <- function(n_users, n_items, user_dim, item_dim) {
  nu <- as.integer(n_users)
  ni <- as.integer(n_items)
  du <- as.integer(user_dim)
  di <- as.integer(item_dim)
  if (min(nu, ni, du, di) < 1L) {
    stop("sse4r: every count must be positive")
  }
  up <- nu * du
  ip <- ni * di
  list(
    user_params = up,
    item_params = ip,
    total = up + ip,
    user_share = up / as.numeric(up + ip),
    note = "one row per user with few sequences each -- which is why SSE is needed rather than optional"
  )
}

morie_sse4r <- function(sequence, user_embedding, item_table,
                       attend = NULL, top_k = 3) {
  pers <- .sse4r_personalise(sequence, user_embedding)
  seq_list <- pers$sequence
  u <- as.numeric(user_embedding)
  T_mat <- as.matrix(item_table)
  storage.mode(T_mat) <- "double"

  if (is.null(attend)) {
    d <- length(seq_list[[1L]])
    di <- d - length(u)
    qy <- seq_list[[length(seq_list)]]
    m <- min(length(u), di)

    n_seq <- length(seq_list)
    seq_mat <- matrix(0, nrow = n_seq, ncol = d)
    for (i in seq_len(n_seq)) {
      seq_mat[i, ] <- seq_list[[i]]
    }

    item_scores <- as.numeric(seq_mat[, seq_len(di), drop = FALSE] %*% qy[seq_len(di)])
    user_scores <- as.numeric(seq_mat[, seq_len(m), drop = FALSE] %*% u[seq_len(m)])
    sc <- (item_scores + user_scores) / sqrt(d)

    m_max <- max(sc)
    e <- exp(sc - m_max)
    w <- e / sum(e)

    ctx <- as.numeric(crossprod(w, seq_mat))
  } else {
    ctx <- as.numeric(attend(seq_list))
    d <- length(ctx)
    di <- d - length(u)
  }

  n_items <- nrow(T_mat)
  scores <- numeric(n_items)
  for (j in seq_len(n_items)) {
    row <- T_mat[j, ]
    if (length(row) != di) {
      stop(sprintf("sse4r: the item table is %d-wide but the item part of the context is %d",
                   length(row), di))
    }
    scores[j] <- sum(ctx[seq_len(di)] * row) +
      sum(ctx[(di + 1L):length(ctx)] * u)
  }

  order_idx <- order(-scores)
  kk <- min(as.integer(top_k), length(order_idx))

  list(
    estimate = order_idx[seq_len(kk)],
    top_k = order_idx[seq_len(kk)],
    scores = scores,
    context = ctx,
    method = "personalised Transformer recommendation; Wu, Li, Hsieh & Sharpnack (2020)",
    note = "the user term is present at every position, so the same history gives different users different answers"
  )
}

.sse4r_cheatsheet <- function() {
  paste("sse4r: a self-attentive sequential recommender models WHAT "
        "was clicked and ignores WHO clicked, so two users with the "
        "same recent items get the same answer. Fix it by "
        "CONCATENATING a user embedding to EVERY item in the "
        "sequence -- appended once at the end, attention could "
        "ignore it. That adds one row per user, each with few "
        "sequences, so it memorises; SSE regularises by REPLACING "
        "an embedding with another from the SAME table with "
        "probability p. Not zeroing (that is dropout) -- exchanging, "
        "which keeps different rows mutually compatible. The "
        "observed rate is p(1-1/n), not p.")
}
