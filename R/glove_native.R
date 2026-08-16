# morie.fn -- function file (rootcoder007/morie)
# GloVe -- global vectors for word representation.
#
# Pennington, Socher & Manning (2014). The model is a weighted least
# squares regression on the logarithm of the word-word co-occurrence
# counts. Their eq. (8):
#
#   J = sum_{i,j=1}^{V} f(X_{ij}) (w_i^T tilde_w_j + b_i + tilde_b_j - log X_{ij})^2
#
# with the weighting function of eq. (9),
#
#   f(x) = (x / x_max)^alpha   if x < x_max
#          1                   otherwise
#
# and their stated defaults x_max = 100 and alpha = 3/4 ("we fix to
# x_max = 100 for all our experiments"; "alpha = 3/4 gives a modest
# improvement over a linear version with alpha = 1").
#
# Three details that are easy to get wrong and are in the paper:
#
# Zero counts are skipped, not weighted to zero.  f(0) = 0 is
# property 1, but log 0 is undefined, so the sum runs over nonzero
# X_{ij} only.  Evaluating the term first and multiplying by zero
# afterwards produces a NaN that then poisons every gradient.
#
# Two sets of vectors, and the sum is what you use.  The paper keeps
# w and tilde_w, notes they are equivalent up to initialisation, and
# recommends w + tilde_w as the final representation -- which also
# acts as a variance reduction.  The default here is that sum, with
# the separate matrices returned as well.
#
# The context window is harmonic.  Sec. 4.2: a token d positions away
# contributes 1/d to the count, "so that very distant word pairs are
# expected to contain less relevant information".  A flat window is
# a different model and gives different vectors.
#
# Training is AdaGrad, as in the paper, because the per-parameter step
# size matters here: word frequencies span orders of magnitude and a
# single global learning rate either crawls on rare words or diverges
# on frequent ones.
#
# References
# ----------
# Pennington, J., Socher, R. & Manning, C. D. (2014) "GloVe: Global
# Vectors for Word Representation", Proceedings of EMNLP 2014,
# 1532-1543, doi:10.3115/v1/D14-1162.  Equations (8) and (9), Sec. 4.2
# for the harmonic weighting and the AdaGrad training.
#
# Duchi, J., Hazan, E. & Singer, Y. (2011) "Adaptive subgradient methods
# for online learning and stochastic optimization", JMLR 12, 2121-2159
# -- the AdaGrad the paper uses.


# Private helper: turn a corpus into a list of token character vectors.
.glove_as_docs <- function(corpus) {
  if (is.null(corpus)) {
    stop("glove: corpus must not be None")
  }
  docs <- list()
  for (item in corpus) {
    if (is.character(item) && length(item) == 1L) {
      # A single string: split on whitespace.
      parts <- strsplit(item, "\\s+")[[1L]]
      parts <- parts[nzchar(parts)]
      docs[[length(docs) + 1L]] <- parts
    } else {
      # A vector or list of tokens.
      docs[[length(docs) + 1L]] <- as.character(item)
    }
  }
  if (length(docs) == 0L) {
    stop("glove: the corpus is empty")
  }
  docs
}


# Eq. (9): the GloVe weighting function. f(0) = 0, non-decreasing,
# capped at 1.
#' Eq. (9): the GloVe weighting function. f(0) = 0, non-decreasing,
#'
#' capped at 1.
#'
#' @param x See Usage.
#' @param x_max Defaults to \code{100}.
#' @param alpha Defaults to \code{0.75}.
#' @return A numeric value.
#' @export
glove_weight <- function(x, x_max = 100.0, alpha = 0.75) {
  x <- as.numeric(x)
  x_max <- as.numeric(x_max)
  alpha <- as.numeric(alpha)
  if (x <= 0.0) {
    return(0.0)
  }
  if (x >= x_max) {
    return(1.0)
  }
  (x / x_max) ^ alpha
}


# Word-word co-occurrence counts over a symmetric context window.
# Sec. 4.2: a token d positions away contributes 1/d to the count
# when harmonic = TRUE.
#' Word-word co-occurrence counts over a symmetric context window
#'
#' Sec. 4.2: a token d positions away contributes 1/d to the count when
#' harmonic = TRUE.
#'
#' @param corpus See Usage.
#' @param window Defaults to \code{10}.
#' @param harmonic Defaults to \code{TRUE}.
#' @param min_count Defaults to \code{1}.
#' @return A list with \code{X}, \code{vocab}, \code{index}.
#' @export
cooccurrence <- function(corpus, window = 10, harmonic = TRUE, min_count = 1) {
  docs <- .glove_as_docs(corpus)
  all_tokens <- unlist(docs, use.names = FALSE)
  if (length(all_tokens) == 0L) {
    stop("glove: the corpus has no tokens")
  }
  token_counts <- table(all_tokens)
  vocab <- sort(names(token_counts)[token_counts >= as.integer(min_count)])
  # 1-based index: token -> row position in W and Wt.
  index_vec <- setNames(seq_along(vocab), vocab)
  w <- as.integer(window)
  if (w < 1L) {
    stop(sprintf("cooccurrence: window must be at least 1, got %r", window))
  }
  # Collect (i, j, increment) triples.
  triple_list <- list()
  for (doc in docs) {
    if (length(doc) == 0L) next
    ids <- as.integer(index_vec[doc])
    ids <- ids[!is.na(ids)]
    n_ids <- length(ids)
    for (pos in seq_len(n_ids)) {
      lo <- max(1L, pos - w)
      # Guard: 1:0 in R is c(1, 0), so the loop must be skipped when empty.
      if (lo >= pos) next
      i_ <- ids[pos]
      for (other in lo:(pos - 1L)) {
        j_ <- ids[other]
        d_ <- pos - other
        inc <- if (isTRUE(harmonic)) 1.0 / d_ else 1.0
        triple_list[[length(triple_list) + 1L]] <- c(i_, j_, inc)
        triple_list[[length(triple_list) + 1L]] <- c(j_, i_, inc)
      }
    }
  }
  if (length(triple_list) == 0L) {
    Xdf <- data.frame(i = integer(0), j = integer(0), count = numeric(0))
  } else {
    M <- do.call(rbind, triple_list)
    df <- data.frame(i = M[, 1L], j = M[, 2L], inc = M[, 3L])
    Xdf <- aggregate(inc ~ i + j, data = df, FUN = sum)
    colnames(Xdf) <- c("i", "j", "count")
    # Sort by (i, j) to match Python's sorted(X.items()).
    Xdf <- Xdf[order(Xdf$i, Xdf$j), , drop = FALSE]
    rownames(Xdf) <- NULL
  }
  list(X = Xdf, vocab = as.character(vocab), index = index_vec)
}


# Eq. (8), evaluated over the nonzero entries only.
#' Eq. (8), evaluated over the nonzero entries only
#'
#' Part of the glove_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param W See Usage.
#' @param Wt See Usage.
#' @param b See Usage.
#' @param bt See Usage.
#' @param x_max Defaults to \code{100}.
#' @param alpha Defaults to \code{0.75}.
#' @return The value of \code{total}, as built in the body.
#' @export
glove_loss <- function(X, W, Wt, b, bt, x_max = 100.0, alpha = 0.75) {
  total <- 0.0
  n <- nrow(X)
  if (n == 0L) return(0.0)
  for (k in seq_len(n)) {
    i <- X$i[k]
    j <- X$j[k]
    x <- X$count[k]
    if (x <= 0.0) next
    pred <- sum(W[i, ] * Wt[j, ]) + b[i] + bt[j]
    diff <- pred - log(x)
    fw <- glove_weight(x, x_max, alpha)
    total <- total + fw * diff * diff
  }
  total
}


# Fit GloVe vectors.
#' Fit GloVe vectors
#'
#' Part of the glove_native implementation; see the file header for the
#' source it follows.
#'
#' @param corpus See Usage.
#' @param dim Defaults to \code{50}.
#' @param window Defaults to \code{10}.
#' @param epochs Defaults to \code{25}.
#' @param lr Defaults to \code{0.05}.
#' @param x_max Defaults to \code{100}.
#' @param alpha Defaults to \code{0.75}.
#' @param harmonic Defaults to \code{TRUE}.
#' @param min_count Defaults to \code{1}.
#' @param seed Defaults to \code{0}.
#' @param combine Defaults to \code{"sum"}.
#' @return A list with \code{estimate}, \code{vectors}, \code{vocab}, \code{index}, \code{W}, \code{W_tilde}, \code{b}, \code{b_tilde}, \code{cooccurrence}, \code{loss_history}, \code{running_loss}, \code{final_loss}, \code{n_vocab}, \code{n_pairs}, \code{dim}, \code{window}, \code{harmonic}, \code{x_max}, \code{alpha}, \code{combine}, \code{method}.
#' @export
morie_glove <- function(corpus, dim = 50, window = 10, epochs = 25, lr = 0.05,
                        x_max = 100.0, alpha = 0.75, harmonic = TRUE,
                        min_count = 1, seed = 0, combine = "sum") {
  if (!combine %in% c("sum", "w", "wtilde", "concat")) {
    stop(sprintf("glove: combine must be 'sum', 'w', 'wtilde' or 'concat', got %r",
                 combine))
  }
  d <- as.integer(dim)
  if (d < 1L) {
    stop(sprintf("glove: dim must be at least 1, got %r", dim))
  }
  cooc <- cooccurrence(corpus, window = window, harmonic = harmonic,
                       min_count = min_count)
  X <- cooc$X
  vocab <- cooc$vocab
  index_vec <- cooc$index
  V <- length(vocab)
  if (V < 2L) {
    stop(sprintf(paste0("glove: the corpus has %d word(s) above min_count=%r; ",
                        "GloVe factorises a co-occurrence matrix and needs ",
                        "at least two"), V, min_count))
  }
  if (nrow(X) == 0L) {
    stop("glove: no co-occurrences within the window, so eq. (8) has no terms")
  }

  # Initialise parameters with the same RNG sequence as the Python
  # np.random.default_rng(seed): W row by row, then Wt row by row,
  # then b, then bt.
  rng <- .ghc_rng(as.integer(seed))
  scale <- 0.5 / d
  uW <- .ghc_unif(rng, V * d)
  uWt <- .ghc_unif(rng, V * d)
  ub <- .ghc_unif(rng, V)
  ubt <- .ghc_unif(rng, V)
  W <- matrix((uW - 0.5) * scale, nrow = V, ncol = d, byrow = TRUE)
  Wt <- matrix((uWt - 0.5) * scale, nrow = V, ncol = d, byrow = TRUE)
  b <- (ub - 0.5) * scale
  bt <- (ubt - 0.5) * scale

  # AdaGrad accumulators, initialised to 1 as in the reference code.
  gW <- matrix(1.0, nrow = V, ncol = d)
  gWt <- matrix(1.0, nrow = V, ncol = d)
  gb <- rep(1.0, V)
  gbt <- rep(1.0, V)

  # Sort entries by (i, j) to match Python's sorted(X.items()).
  entries <- X[order(X$i, X$j), , drop = FALSE]

  history <- numeric(0)   # eq. (8) at the end of each epoch
  running <- numeric(0)   # the SGD running total
  eta <- as.numeric(lr)

  for (epoch in seq_len(as.integer(epochs))) {
    total <- 0.0
    n <- nrow(entries)
    for (k in seq_len(n)) {
      i <- entries$i[k]
      j <- entries$j[k]
      x <- entries$count[k]
      if (x <= 0.0) next
      wi <- W[i, ]
      wj <- Wt[j, ]
      pred <- sum(wi * wj) + b[i] + bt[j]
      diff <- pred - log(x)
      fw <- glove_weight(x, x_max, alpha)
      total <- total + fw * diff * diff
      g <- 2.0 * fw * diff
      gi <- g * wj
      gj <- g * wi
      W[i, ]  <- W[i, ]  - eta * gi / sqrt(gW[i, ])
      Wt[j, ] <- Wt[j, ] - eta * gj / sqrt(gWt[j, ])
      gW[i, ]  <- gW[i, ]  + gi * gi
      gWt[j, ] <- gWt[j, ] + gj * gj
      b[i]  <- b[i]  - eta * g / sqrt(gb[i])
      bt[j] <- bt[j] - eta * g / sqrt(gbt[j])
      gb[i]  <- gb[i]  + g * g
      gbt[j] <- gbt[j] + g * g
    }
    running <- c(running, total)
    # eq. (8) evaluated at the parameters this epoch ended with,
    # rather than the running sum accumulated while they moved.
    history <- c(history, glove_loss(X, W, Wt, b, bt, x_max, alpha))
  }

  if (combine == "sum") {
    vecs_mat <- W + Wt
  } else if (combine == "w") {
    vecs_mat <- W
  } else if (combine == "wtilde") {
    vecs_mat <- Wt
  } else {
    vecs_mat <- cbind(W, Wt)
  }

  # Mirror the Python list-of-lists shape for estimate / vectors.
  vecs_list <- lapply(seq_len(V), function(i) as.numeric(vecs_mat[i, ]))

  list(
    estimate = vecs_list,
    vectors = vecs_list,
    vocab = vocab,
    index = index_vec,
    W = W,
    W_tilde = Wt,
    b = b,
    b_tilde = bt,
    cooccurrence = X,
    loss_history = history,
    running_loss = running,
    final_loss = if (length(history) > 0L) history[length(history)] else NaN,
    n_vocab = V,
    n_pairs = nrow(entries),
    dim = d,
    window = as.integer(window),
    harmonic = as.logical(harmonic),
    x_max = as.numeric(x_max),
    alpha = as.numeric(alpha),
    combine = combine,
    method = paste0("GloVe weighted least squares on log co-occurrence, ",
                    "Pennington, Socher & Manning (2014) eqs. (8)-(9)")
  )
}


.glove_cheatsheet <- function() {
  paste0("glove: J = sum f(X_ij)(w_i.wt_j + b_i + bt_j - log X_ij)^2 ",
         "with f(x) = (x/xmax)^alpha capped at 1, xmax=100, ",
         "alpha=3/4 (Pennington-Socher-Manning 2014 eqs.8-9). ",
         "Harmonic 1/d context window; AdaGrad; final vector w + wt.")
}
