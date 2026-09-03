# morie.fn -- function file (rootcoder007/morie)
# Two-tower retrieval, and the sampling bias you must remove.
#
# Retrieval over a corpus of millions of items cannot afford a softmax
# over the corpus, so the standard trick is to treat the **other items
# in the batch** as negatives. That is efficient and it is biased:
# in-batch negatives are drawn from the *training distribution*, so a
# popular item appears as a negative constantly and is pushed down
# regardless of relevance.
#
# **The correction is one subtraction.** With :math:`p_j` the
# probability that item :math:`j` appears in a batch, use the corrected
# logit
#
# .. math:: s^c(x, y_j) = s(x, y_j) - \log p_j,
#
# which is the standard logQ correction for sampled softmax, applied to
# in-batch sampling. ``corrected_logits`` implements it, and the anchor
# constructs a case where the *uncorrected* ranking puts a popular
# irrelevant item above a rare relevant one and the correction restores
# the right order -- so the term is shown to matter rather than
# described.
#
# **The item frequency is estimated in a stream, not counted.** The
# paper's estimator tracks, per item, the number of steps since it was
# last seen, and updates a running average :math:`B` of that gap; the
# sampling probability is :math:`1/B`. No global count, no second pass,
# and it adapts as the distribution drifts. ``streaming_frequency``
# implements exactly that, and the anchor checks it converges to
# :math:`1/\Delta` for an item that appears every :math:`\Delta` steps.
#
# **Normalisation and temperature are not cosmetic.** Embeddings are
# L2-normalised and the inner product divided by a temperature, which
# sharpens the softmax; without normalisation the model can win the
# contrastive objective by inflating norms rather than by learning
# directions.
#
# References
# ----------
# Yi, X., Yang, J., Hong, L., Cheng, D. Z., Heldt, L., Kumthekar, A.,
# Zhao, Z., Wei, L. & Chi, E. (2019) "Sampling-Bias-Corrected Neural
# Modeling for Large Corpus Item Recommendations", *Proceedings of the
# 13th ACM Conference on Recommender Systems (RecSys '19)*, 269-277,
# doi:10.1145/3298689.3346996. The two-tower architecture for
# large-corpus item retrieval; batch softmax with in-batch negatives and
# the resulting sampling bias toward popular items; the logQ correction
# subtracting log p_j from the logit; the streaming frequency estimation
# algorithm tracking the number of steps between successive hits of an
# item and estimating the sampling probability as its reciprocal; and
# normalisation of the embeddings with a temperature in the softmax.
#
# Bengio, Y. & Senecal, J.-S. (2008) "Adaptive Importance Sampling to
# Accelerate Training of a Neural Probabilistic Language Model", *IEEE
# Transactions on Neural Networks* 19(4), 713-722,
# doi:10.1109/TNN.2007.912312. The sampled-softmax correction being
# applied.
#
# Covington, P., Adams, J. & Sargin, E. (2016) "Deep Neural Networks
# for YouTube Recommendations", *RecSys 2016*, 191-198,
# doi:10.1145/2959100.2959190. The retrieval setting.

.twoT_EPS <- 1e-12

#' .twoT_as_vec
#'
#' A step of the twoT_native implementation. Called by
#' \code{morie_twoT_batch_softmax_loss}, \code{morie_twoT_corrected_logits},
#' \code{morie_twoT_retrieve} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Optional; may be \code{NULL}. A matrix; the body checks with \code{is.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .twoT_as_vec(v = x)
#' res
.twoT_as_vec <- function(v) {
  if (is.null(v)) return(numeric(0))
  if (is.matrix(v)) as.numeric(v)
  else as.numeric(unlist(v))
}

#' .twoT_as_mat
#'
#' A step of the twoT_native implementation. Called by
#' \code{morie_twoT_batch_softmax_loss}, \code{morie_twoT_retrieve},
#' \code{morie_twoT_tower_embedding}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .twoT_as_mat(m = X)
#' res
.twoT_as_mat <- function(m) {
  if (is.null(m)) return(matrix(numeric(0), nrow = 0L, ncol = 0L))
  if (is.list(m)) {
    if (length(m) == 0L) return(matrix(numeric(0), nrow = 0L, ncol = 0L))
    do.call(rbind, lapply(m, .twoT_as_vec))
  } else {
    as.matrix(m)
  }
}

#' morie_twoT_tower_embedding
#'
#' A step of the twoT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param features Passed to \code{.twoT_as_vec}.
#' @param W Passed to \code{.twoT_as_mat}.
#' @param b Optional; may be \code{NULL}. Passed to \code{.twoT_as_vec}.
#' @param normalise A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{embedding}, \code{norm}, \code{normalised}.
#' @export
morie_twoT_tower_embedding <- function(features, W, b = NULL, normalise = TRUE) {
  x <- .twoT_as_vec(features)
  Wm <- .twoT_as_mat(W)
  if (ncol(Wm) != length(x)) {
    stop(sprintf("twoT: the tower expects %d features but got %d",
                 ncol(Wm), length(x)))
  }
  nr <- nrow(Wm)
  bb <- if (is.null(b)) rep(0.0, nr) else .twoT_as_vec(b)
  if (length(bb) != nr) {
    stop(sprintf("twoT: bias has %d entries but tower has %d rows",
                 length(bb), nr))
  }
  z <- as.numeric(Wm %*% x) + bb
  if (!isTRUE(normalise)) {
    return(list(embedding = z, normalised = FALSE))
  }
  n <- sqrt(sum(z * z))
  if (n <= .twoT_EPS) {
    stop("twoT: the tower produced a zero embedding")
  }
  list(embedding = z / n, norm = n, normalised = TRUE)
}

#' morie_twoT_corrected_logits
#'
#' A step of the twoT_native implementation. Called by
#' \code{morie_twoT_batch_softmax_loss}, \code{morie_twoT_retrieve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param scores Passed to \code{.twoT_as_vec}.
#' @param probabilities Passed to \code{.twoT_as_vec}.
#' @param temperature Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{corrected}, \code{raw}, \code{shift}, \code{note}.
#' @export
morie_twoT_corrected_logits <- function(scores, probabilities,
                                        temperature = 1.0) {
  s <- .twoT_as_vec(scores)
  p <- .twoT_as_vec(probabilities)
  if (length(s) != length(p)) {
    stop(sprintf("twoT: %d scores but %d probabilities",
                 length(s), length(p)))
  }
  if (any(p <= 0.0 | p > 1.0)) {
    stop("twoT: the sampling probabilities must lie in (0,1]")
  }
  tt <- as.numeric(temperature)
  if (tt <= 0.0) {
    stop("twoT: the temperature must be positive")
  }
  raw <- s / tt
  cor <- raw - log(p)
  list(corrected = cor, raw = raw, shift = -log(p),
       note = paste("a frequent item gets the LARGEST positive shift,",
                    "undoing its over-representation"))
}

#' .twoT_get_items
#'
#' A step of the twoT_native implementation. Called by \code{morie_twoT_streaming_frequency}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hits Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param t Numeric; combined arithmetically in the body.
#' @return A vector, from \code{integer}.
#' @export
.twoT_get_items <- function(hits, t) {
  if (is.null(hits)) return(integer(0))
  if (!is.null(hits[[t + 1L]])) return(hits[[t + 1L]])
  k <- as.character(t)
  if (!is.null(hits[[k]])) return(hits[[k]])
  integer(0)
}

#' morie_twoT_streaming_frequency
#'
#' A step of the twoT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hits Passed to \code{.twoT_get_items}.
#' @param n_steps A count; the body uses it as \code{seq_len(...)}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.
#' @param init Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{B}, \code{probability}, \code{n_items}, \code{note}.
#' @export
morie_twoT_streaming_frequency <- function(hits, n_steps, alpha = 0.05,
                                          init = NULL) {
  a <- as.numeric(alpha)
  if (a <= 0.0 || a > 1.0) {
    stop("twoT: the step size must lie in (0,1]")
  }
  n_steps <- as.integer(n_steps)
  last <- list()
  B <- list()
  for (t in seq_len(n_steps) - 1L) {
    items <- .twoT_get_items(hits, t)
    for (j in items) {
      jk <- as.character(j)
      if (!is.null(last[[jk]])) {
        gap <- t - last[[jk]]
        B[[jk]] <- (1.0 - a) * B[[jk]] + a * gap
      } else {
        B[[jk]] <- if (is.null(init)) 1.0 else as.numeric(init)
      }
      last[[jk]] <- t
    }
  }
  prob <- list()
  for (jk in names(B)) {
    v <- B[[jk]]
    prob[[jk]] <- 1.0 / max(v, .twoT_EPS)
  }
  list(B = B, probability = prob, n_items = length(B),
       note = paste("B is the average number of steps between hits,",
                    "so 1/B is the sampling probability"))
}

#' morie_twoT_batch_softmax_loss
#'
#' A step of the twoT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query_embeddings Passed to \code{.twoT_as_mat}.
#' @param item_embeddings Passed to \code{.twoT_as_mat}.
#' @param probabilities Optional; may be \code{NULL}. Passed to \code{.twoT_as_vec}.
#' @param temperature Coerced to numeric by the body, with \code{as.numeric}. Defaults to
#' \code{0.05}.
#' @return A list with \code{loss}, \code{per_example}, \code{corrected}.
#' @export
morie_twoT_batch_softmax_loss <- function(query_embeddings, item_embeddings,
                                          probabilities = NULL,
                                          temperature = 0.05) {
  Q <- .twoT_as_mat(query_embeddings)
  I <- .twoT_as_mat(item_embeddings)
  n <- nrow(Q)
  if (nrow(I) != n) {
    stop(sprintf("twoT: %d queries but %d items", n, nrow(I)))
  }
  if (n < 2L) {
    stop("twoT: in-batch negatives need at least 2 examples")
  }
  S <- Q %*% t(I)
  tt <- as.numeric(temperature)
  tot <- 0.0
  per <- numeric(n)
  for (i in seq_len(n)) {
    s <- S[i, ]
    if (is.null(probabilities)) {
      lg <- s / tt
    } else {
      p <- .twoT_as_vec(probabilities)
      lg <- morie_twoT_corrected_logits(s, p, tt)$corrected
    }
    m <- max(lg)
    z <- sum(exp(lg - m))
    li <- -(lg[i] - m - log(z))
    per[i] <- li
    tot <- tot + li
  }
  list(loss = tot / n, per_example = per,
       corrected = !is.null(probabilities))
}

#' morie_twoT_retrieve
#'
#' A step of the twoT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query_embedding Passed to \code{.twoT_as_vec}.
#' @param item_embeddings Passed to \code{.twoT_as_mat}.
#' @param probabilities Optional; may be \code{NULL}. Passed to \code{.twoT_as_vec}.
#' @param top_k Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.
#' @param temperature Passed to \code{morie_twoT_corrected_logits}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{top_k}, \code{uncorrected_top_k},
#' \code{scores}, \code{corrected_scores}, \code{changed}, \code{method}, \code{note}.
#' @export
morie_twoT_retrieve <- function(query_embedding, item_embeddings,
                                probabilities = NULL, top_k = 5,
                                temperature = 1.0) {
  q <- .twoT_as_vec(query_embedding)
  I <- .twoT_as_mat(item_embeddings)
  s <- as.numeric(I %*% q)
  raw_order <- order(-s)
  if (is.null(probabilities)) {
    corder <- raw_order
    cor <- s
  } else {
    p <- .twoT_as_vec(probabilities)
    cor <- morie_twoT_corrected_logits(s, p, temperature)$corrected
    corder <- order(-cor)
  }
  kk <- min(as.integer(top_k), length(corder))
  idx <- seq_len(kk)
  list(estimate = corder[idx], top_k = corder[idx],
       uncorrected_top_k = raw_order[idx],
       scores = s, corrected_scores = cor,
       changed = !identical(corder[idx], raw_order[idx]),
       method = paste("sampling-bias-corrected two-tower retrieval;",
                      "Yi et al. (2019)"),
       note = paste("without the correction, popularity is mistaken",
                    "for irrelevance"))
}

#' morie_twoT_cheatsheet
#'
#' A step of the twoT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_twoT_cheatsheet <- function() {
  paste("twoT: a softmax over millions of items is impossible, so ",
        "use IN-BATCH negatives -- which are drawn from the ",
        "TRAINING distribution, so a popular item is pushed down ",
        "for being popular. Correct it with one subtraction: ",
        "s^c = s - log p_j. Estimate p_j in a STREAM from the ",
        "average gap between an item's hits, p = 1/B -- no global ",
        "count, no second pass, and it tracks drift. L2-normalise ",
        "the towers and divide by a temperature, or the model wins ",
        "the objective by inflating norms rather than learning ",
        "directions.")
}

# compact alias per ledger/NAMING.md
morie_twoT <- morie_twoT_retrieve

# public names resolved by fn/_lazy_map.json
twotower <- morie_twoT_retrieve
two_tower <- morie_twoT_retrieve
