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

#' .sse4r_personalise
#'
#' A step of the sse4r_native implementation. Called by \code{morie_sse4r}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param item_embeddings A matrix; passed to \code{as.matrix}.
#' @param user_embedding See Usage.
#' @return A list with \code{sequence}, \code{item_dim}, \code{user_dim}, \code{width}, \code{length}, \code{note}.
#' @export
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

#' .sse4r_sse_replace
#'
#' A step of the sse4r_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param indices See Usage.
#' @param table_size See Usage.
#' @param p Defaults to \code{0}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return A list with \code{indices}, \code{replaced}, \code{p}, \code{rate}, \code{note}.
#' @export
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

#' .sse4r_expected_replacement
#'
#' A step of the sse4r_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @param table_size See Usage.
#' @return A list with \code{expected_rate}, \code{p}, \code{table_size}, \code{note}.
#' @export
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

#' .sse4r_parameter_count
#'
#' A step of the sse4r_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_users See Usage.
#' @param n_items See Usage.
#' @param user_dim See Usage.
#' @param item_dim See Usage.
#' @return A list with \code{user_params}, \code{item_params}, \code{total}, \code{user_share}, \code{note}.
#' @export
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

#' morie_sse4r
#'
#' A step of the sse4r_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sequence Passed to \code{.sse4r_personalise}.
#' @param user_embedding Passed to \code{.sse4r_personalise}.
#' @param item_table A matrix; passed to \code{as.matrix}.
#' @param attend Defaults to \code{NULL}.
#' @param top_k Defaults to \code{3}.
#' @return A list with \code{estimate}, \code{top_k}, \code{scores}, \code{context}, \code{method}, \code{note}.
#' @export
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
    m_int <- min(length(u), di)

    L <- length(seq_list)
    sc <- numeric(L)
    for (t in seq_len(L)) {
      st <- seq_list[[t]]
      item_part <- sum(qy[seq_len(di)] * st[seq_len(di)])
      user_part <- sum(u[seq_len(m_int)] * st[seq_len(m_int)])
      sc[t] <- (item_part + user_part) / sqrt(d)
    }

    m_max <- max(sc)
    e <- exp(sc - m_max)
    z <- sum(e)
    w <- e / z

    ctx <- numeric(d)
    for (a in seq_len(d)) {
      ctx[a] <- sum(w * vapply(seq_list, function(s) s[a], numeric(1)))
    }
  } else {
    ctx <- as.numeric(attend(seq_list))
  }

  di_final <- length(ctx) - length(u)

  if (ncol(T_mat) != di_final) {
    stop(sprintf("sse4r: the item table is %d-wide but the item part of the context is %d",
                 ncol(T_mat), di_final))
  }

  n_items <- nrow(T_mat)
  u_len <- length(u)
  scores <- numeric(n_items)
  item_idx <- seq_len(di_final)
  user_idx <- di_final + seq_len(u_len)

  for (j in seq_len(n_items)) {
    row <- T_mat[j, ]
    scores[j] <- sum(ctx[item_idx] * row) + sum(ctx[user_idx] * u)
  }

  order_idx <- order(-scores)
  kk <- min(as.integer(top_k), length(order_idx))
  top_k_idx <- if (kk > 0L) order_idx[seq_len(kk)] else integer(0L)

  list(
    estimate = top_k_idx,
    top_k = top_k_idx,
    scores = scores,
    context = ctx,
    method = "personalised Transformer recommendation; Wu, Li, Hsieh & Sharpnack (2020)",
    note = "the user term is present at every position, so the same history gives different users different answers"
  )
}

#' sse4r_cheatsheet
#'
#' A step of the sse4r_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
sse4r_cheatsheet <- function() {
  paste0("sse4r: a self-attentive sequential recommender models WHAT ",
         "was clicked and ignores WHO clicked, so two users with the ",
         "same recent items get the same answer. Fix it by ",
         "CONCATENATING a user embedding to EVERY item in the ",
         "sequence -- appended once at the end, attention could ",
         "ignore it. That adds one row per user, each with few ",
         "sequences, so it memorises; SSE regularises by REPLACING ",
         "an embedding with another from the SAME table with ",
         "probability p. Not zeroing (that is dropout) -- exchanging, ",
         "which keeps different rows mutually compatible. The ",
         "observed rate is p(1-1/n), not p.")
}

# compact alias per ledger/NAMING.md
ssept <- morie_sse4r

# public names resolved by fn/_lazy_map.json
ssepta_seq <- morie_sse4r
sseptaseq <- morie_sse4r
