# SPDX-License-Identifier: AGPL-3.0-or-later
# morie.fn.dssm -- R parity for the Python "DSSM: word hashing, then
# a semantic space trained on clicks" module.
#
# Latent semantic models for web search had two problems: they were
# trained on **objective functions unrelated to retrieval** (word
# co-occurrence, reconstruction), and the vocabulary of a real query
# stream is far too large for a neural network's input layer.
#
# **Word hashing solves the vocabulary problem with letter n-grams.** A
# word is bracketed (``#good#``) and represented by the multiset of its
# letter trigrams. A 500K-word vocabulary collapses to about 30K
# trigrams -- and the collapse is *not* lossy in the way a hash table is:
# two different words share a representation only if they share every
# trigram, so ``collision_rate`` measures the real cost, which the paper
# finds negligible. The side benefit is that an out-of-vocabulary or
# misspelled word still has a representation, because its trigrams exist
# even if the word never appeared in training.
#
# **The objective is clickthrough, which is the actual retrieval
# signal.** Query and document are projected into a common semantic
# space, scored by **cosine similarity**, and the model maximises the
# conditional likelihood of the **clicked** document under a softmax
# over the clicked one plus randomly sampled unclicked ones. Training on
# what users clicked is the point of departure from the earlier models.
#
# **The smoothing factor is not decoration.** The cosine similarity is
# scaled by :math:`\gamma` inside the softmax; it sets how sharply the
# posterior concentrates, and at :math:`\gamma \to 0` every document is
# equally likely no matter what the model learned.
#
# References
# ----------
# Huang, P.-S., He, X., Gao, J., Deng, L., Acero, A. & Heck, L. (2013)
# "Learning Deep Structured Semantic Models for Web Search using
# Clickthrough Data", *Proceedings of the 22nd ACM International
# Conference on Information and Knowledge Management (CIKM '13)*,
# 2333-2338, doi:10.1145/2505515.2505665. Sec. 3: the deep structured
# semantic model projecting queries and documents into a common
# low-dimensional semantic space where relevance is computed by cosine
# similarity; WORD HASHING based on letter n-grams, which reduces the
# dimensionality of the bag-of-words term vectors (a 500K vocabulary to
# roughly 30K letter trigrams) with a very low collision rate and gives
# representations to out-of-vocabulary and misspelled words; the
# training objective maximising the conditional likelihood of the
# CLICKED documents given the query under a softmax over the clicked
# document and randomly sampled unclicked documents, with a smoothing
# factor gamma in the softmax; and the criticism of earlier latent
# semantic models as trained on objective functions loosely related to
# the retrieval task.
#
# Deerwester, S., Dumais, S. T., Furnas, G. W., Landauer, T. K. &
# Harshman, R. (1990) "Indexing by latent semantic analysis", *Journal
# of the American Society for Information Science* 41(6), 391-407. The
# unsupervised alternative being displaced.

.DSSM_EPS <- 1e-12


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Vectorise a "vec" coercion: an R numeric/integer, a list of numbers,
# or anything that as.numeric() can flatten. Mirrors k.vec() on the
# Python side which is forgiving about list/array inputs.
.dssm_to_vec <- function(x) {
  if (is.null(x)) return(numeric(0L))
  if (is.numeric(x)) return(as.numeric(x))
  as.numeric(unlist(x, use.names = FALSE))
}


# ---------------------------------------------------------------------------
# letter_ngrams
# ---------------------------------------------------------------------------
#' Letter n-grams of a bracketed word
#'
#' Bracketing matters: it distinguishes a prefix from the same letters
#' inside a word. With \code{boundary = "#"} and \code{n = 3} the word
#' "good" becomes the trigrams of \code{"#good#"}.
#'
#' @param word character(1). The input word; coerced via \code{as.character}
#'   and lower-cased.
#' @param n integer(1). Length of the n-grams. Default \code{3}.
#' @param boundary character(1). Bracket character pasted on both sides
#'   before sliding the window. Default \code{"#"}.
#'
#' @return A character vector of length \code{max(1, nchar(w) - n + 1)}.
#'   When the bracketed word is shorter than \code{n}, the entire
#'   bracketed string is returned as a single "n-gram".
#' @export
morie_dssm_letter_ngrams <- function(word, n = 3L, boundary = "#") {
  m <- as.integer(n)
  if (length(m) != 1L || is.na(m))
    stop("dssm: n must be a single integer", call. = FALSE)
  if (m < 1L)
    stop("dssm: n must be at least 1", call. = FALSE)
  b <- as.character(boundary)
  if (length(b) != 1L || is.na(b))
    stop("dssm: boundary must be a single character", call. = FALSE)
  w <- paste0(b, tolower(as.character(word)), b)
  if (nchar(w) < m)
    return(w)
  starts <- seq_len(nchar(w) - m + 1L)
  vapply(starts, function(i) substr(w, i, i + m - 1L),
         character(1L), USE.NAMES = FALSE)
}


# ---------------------------------------------------------------------------
# word_hash
# ---------------------------------------------------------------------------
#' Word hashing: replace the bag-of-words with a bag of letter n-grams
#'
#' The input layer's width becomes the number of n-grams, not the
#' size of the vocabulary -- and an unseen word still has a vector.
#'
#' @param words character vector. The token stream.
#' @param n integer(1). Letter n-gram size. Default \code{3}.
#' @param vocabulary character vector or \code{NULL}. If \code{NULL}
#'   (default) the n-grams present in \code{words} are themselves the
#'   vocabulary; otherwise only n-grams that appear in this list count
#'   toward the output dimension, and any others contribute to
#'   \code{unseen_ngrams}.
#'
#' @return A named list with \code{vector} (numeric, the bag-of-ngrams
#'   counts in the canonical order), \code{dimension}, \code{ngrams}
#'   (raw counts keyed by n-gram string), \code{unseen_ngrams} (number
#'   of n-grams that were not in the supplied vocabulary), and
#'   \code{note}.
#' @export
morie_dssm_word_hash <- function(words, n = 3L, vocabulary = NULL) {
  W <- as.character(words)
  if (length(W) == 0L)
    stop("dssm: words is empty", call. = FALSE)
  grams <- list()
  for (w in W) {
    gs <- morie_dssm_letter_ngrams(w, n)
    for (g in gs) {
      hit <- match(g, names(grams), nomatch = 0L)
      if (hit == 0L) {
        grams[[g]] <- 1
        names(grams)[length(grams)] <- g
      } else {
        grams[[hit]] <- grams[[hit]] + 1L
      }
    }
  }
  keys <- if (is.null(vocabulary)) sort(names(grams)) else sort(as.character(vocabulary))
  idx <- setNames(seq_along(keys), keys)
  vec <- rep(0.0, length(keys))
  unseen <- 0L
  for (g in names(grams)) {
    j <- idx[[g]]
    if (!is.na(j)) {
      vec[j] <- vec[j] + grams[[g]]
    } else {
      unseen <- unseen + 1L
    }
  }
  list(
    vector        = vec,
    dimension     = length(keys),
    ngrams        = grams,
    unseen_ngrams = unseen,
    note          = paste("an out-of-vocabulary or misspelled word still",
                          "has trigrams, so it still has a representation")
  )
}


# ---------------------------------------------------------------------------
# collision_rate
# ---------------------------------------------------------------------------
#' Trigram-collision rate of a vocabulary
#'
#' Two words collide only if they share EVERY n-gram. Not a hash
#' function's arbitrary collision -- the cost is real but measurable,
#' and the paper finds it negligible.
#'
#' @param vocabulary character vector. The vocabulary to inspect.
#' @param n integer(1). Letter n-gram size. Default \code{3}.
#'
#' @return A named list with \code{vocabulary} (number of distinct
#'   words), \code{ngram_dimension} (the size of the n-gram space
#'   actually used), \code{reduction} (the ratio), \code{collisions}
#'   (the number of words that share their full trigram set with at
#'   least one other), \code{collision_rate} (collisions / |V|),
#'   \code{colliding_groups} (a list of the colliding word groups in
#'   sorted order), and \code{note}.
#' @export
morie_dssm_collision_rate <- function(vocabulary, n = 3L) {
  V <- as.character(vocabulary)
  if (length(V) == 0L)
    stop("dssm: the vocabulary is empty", call. = FALSE)
  # sort the trigrams inside each bucket key so order is irrelevant
  keys <- vapply(V, function(w) {
    gs <- sort(morie_dssm_letter_ngrams(w, n))
    paste(gs, collapse = "\r")
  }, character(1L))
  bucket_of <- split(V, keys)
  collided_groups <- bucket_of[vapply(bucket_of, length, integer(1L)) > 1L]
  n_col <- sum(vapply(collided_groups, length, integer(1L)))
  grams <- unique(unlist(lapply(V, morie_dssm_letter_ngrams, n = n),
                         use.names = FALSE))
  list(
    vocabulary        = length(V),
    ngram_dimension   = length(grams),
    reduction         = length(V) / as.numeric(length(grams)),
    collisions        = n_col,
    collision_rate    = n_col / as.numeric(length(V)),
    colliding_groups  = lapply(collided_groups, sort),
    note              = paste("the input layer shrinks from |V| to",
                              "|n-grams|")
  )
}


# ---------------------------------------------------------------------------
# cosine_similarity
# ---------------------------------------------------------------------------
#' Cosine similarity in the common semantic space
#'
#' @param query_vector numeric. A query embedding (any vector-like).
#' @param doc_vector numeric. A document embedding of the same width.
#'
#' @return A single numeric in \code{[-1, 1]}. Raises an error if the
#'   widths differ or either vector is the zero vector.
#' @export
morie_dssm_cosine_similarity <- function(query_vector, doc_vector) {
  q <- .dssm_to_vec(query_vector)
  d <- .dssm_to_vec(doc_vector)
  if (length(q) != length(d))
    stop("dssm: the query and document vectors differ in width",
         call. = FALSE)
  nq <- sqrt(sum(q * q))
  nd <- sqrt(sum(d * d))
  if (nq <= .DSSM_EPS || nd <= .DSSM_EPS)
    stop("dssm: a zero vector has no direction, so cosine similarity is undefined",
         call. = FALSE)
  sum(q * d) / (nq * nd)
}


# ---------------------------------------------------------------------------
# click_posterior
# ---------------------------------------------------------------------------
#' Clickthrough-trained softmax posterior
#'
#' Softmax over the clicked document and a sample of unclicked ones.
#' \eqn{\gamma} sets how sharply the posterior concentrates; at
#' \eqn{\gamma \to 0} every document is equally likely whatever the
#' model learned.
#'
#' @param query_vector numeric. A query embedding.
#' @param clicked_vector numeric. The embedding of the clicked document.
#' @param unclicked_vectors list or matrix of numeric embeddings of the
#'   sampled non-clicks. Each element must have the same width as
#'   \code{query_vector}.
#' @param gamma numeric(1). Smoothing factor. Must be strictly positive.
#'   Default \code{10.0}.
#'
#' @return A named list whose names match the Python
#'   \code{RichResult} payload: \code{estimate}, \code{posterior_clicked},
#'   \code{posterior}, \code{similarities}, \code{gamma}, \code{loss},
#'   \code{n_negatives}, \code{method}, \code{note}.
#' @export
morie_dssm_click_posterior <- function(query_vector, clicked_vector,
                                       unclicked_vectors,
                                       gamma = 10.0) {
  g <- as.numeric(gamma)
  if (length(g) != 1L || is.na(g) || g <= 0.0)
    stop("dssm: the smoothing factor must be positive", call. = FALSE)
  unclicked <- if (is.null(unclicked_vectors)) list() else unclicked_vectors
  sims <- c(
    morie_dssm_cosine_similarity(query_vector, clicked_vector),
    vapply(unclicked,
           function(d) morie_dssm_cosine_similarity(query_vector, d),
           numeric(1L))
  )
  sc <- g * sims
  m <- max(sc)
  e <- exp(sc - m)
  z <- sum(e)
  p <- e / z
  list(
    estimate           = p[1L],
    posterior_clicked  = p[1L],
    posterior          = p,
    similarities       = sims,
    gamma              = g,
    loss               = -log(max(p[1L], .DSSM_EPS)),
    n_negatives        = length(unclicked),
    method             = paste("clickthrough-trained semantic model;",
                               "Huang et al. (2013)"),
    note               = paste("trained on what users CLICKED, not on",
                               "word co-occurrence")
  )
}

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams

#' @rdname morie_dssm_letter_ngrams
#' @export
morie_dssm <- morie_dssm_letter_ngrams
