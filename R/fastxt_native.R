# FastText -- word vectors enriched with subword information.
#
# Sources: Bojanowski, P., Grave, E., Joulin, A. & Mikolov, T. (2017)
# "Enriching word vectors with subword information", Transactions of
# the Association for Computational Linguistics 5, 135-146,
# doi:10.1162/tacl_a_00051; arXiv:1607.04606. Sec. 3.2 for the subword
# model and the *where* example, Sec. 3.1 for the objective.
# Mikolov, T., Sutskever, I., Chen, K., Corrado, G. & Dean, J. (2013)
# "Distributed representations of words and phrases and their
# compositionality", Advances in Neural Information Processing
# Systems 26, 3111-3119 -- the skipgram with negative sampling this
# extends.
#
# Native implementation mirroring Python morie.fn.fastxt exactly: the
# same subword decomposition with boundary symbols and whole-word
# special, the same sum-of-ngrams word vector, the same skipgram with
# negative sampling and the same unigram^(3/4) sampling distribution,
# and the same RichResult-style payload as a named list. Randomness
# is drawn from .ghc_rng / .ghc_unif so both arms produce the same
# stream.

#' subwords
#'
#' Part of the fastxt_native implementation; see the file header for the
#' source it follows.
#'
#' @param word See Usage.
#' @param n_min Defaults to \code{3}.
#' @param n_max Defaults to \code{6}.
#' @param boundary Defaults to \code{TRUE}.
#' @param whole_word Defaults to \code{TRUE}.
#' @return The value of \code{grams}, as built in the body.
#' @export
subwords <- function(word, n_min = 3, n_max = 6, boundary = TRUE,
                     whole_word = TRUE) {
  lo <- as.integer(n_min)
  hi <- as.integer(n_max)
  if (lo < 1L)
    stop("subwords: n_min must be at least 1, got ", n_min)
  if (hi < lo)
    stop("subwords: n_max (", n_max, ") is below n_min (", n_min, ")")
  w <- as.character(word)
  padded <- if (boundary) paste0("<", w, ">") else w
  grams <- character(0)
  seen <- character(0)
  for (n in lo:hi) {
    if (nchar(padded) < n) next
    chars <- strsplit(padded, "")[[1]]
    for (i in seq_len(nchar(padded) - n + 1L)) {
      g <- substr(padded, i, i + n - 1L)
      if (!(g %in% seen)) {
        seen <- c(seen, g)
        grams <- c(grams, g)
      }
    }
  }
  if (whole_word) {
    special <- if (boundary) paste0("<", w, ">") else w
    if (!(special %in% seen))
      grams <- c(grams, special)
  }
  grams
}

#' word_vector
#'
#' Part of the fastxt_native implementation; see the file header for the
#' source it follows.
#'
#' @param word See Usage.
#' @param Z See Usage.
#' @param gram_index See Usage.
#' @param n_min Defaults to \code{3}.
#' @param n_max Defaults to \code{6}.
#' @param boundary Defaults to \code{TRUE}.
#' @param whole_word Defaults to \code{TRUE}.
#' @param hash_buckets Defaults to \code{NULL}.
#' @return A list with \code{v}, \code{hit}.
#' @export
word_vector <- function(word, Z, gram_index, n_min = 3, n_max = 6,
                        boundary = TRUE, whole_word = TRUE,
                        hash_buckets = NULL) {
  Z <- as.matrix(Z)
  dim_n <- ncol(Z)
  v <- rep(0, dim_n)
  hit <- 0L
  for (g in subwords(word, n_min, n_max, boundary, whole_word)) {
    idx <- .gram_slot(g, gram_index, hash_buckets)
    if (is.null(idx)) next
    hit <- hit + 1L
    v <- v + Z[idx + 1L, ]
  }
  list(v = v, hit = hit)
}

.gram_slot <- function(g, gram_index, hash_buckets) {
  if (!is.null(hash_buckets))
    return(.fnv1a(g) %% as.integer(hash_buckets))
  gi <- gram_index[[g]]
  if (is.null(gi)) NULL else gi
}

.fnv1a <- function(s) {
  s <- charToRaw(as.character(s))
  h <- 2166136261
  for (i in seq_along(s)) {
    h <- bitwXor(h, as.integer(s[i]))
    h <- bitwAnd(h * 16777619, 4294967295)
  }
  as.integer(h)
}

.as_docs <- function(corpus) {
  if (is.null(corpus))
    stop("fasttext: corpus must not be None")
  docs <- list()
  for (item in corpus) {
    if (is.character(item) && length(item) == 1L) {
      docs[[length(docs) + 1L]] <- strsplit(item, "\\s+")[[1]]
    } else {
      docs[[length(docs) + 1L]] <- as.character(item)
    }
  }
  if (length(docs) == 0L)
    stop("fasttext: the corpus is empty")
  docs
}

#' fasttext
#'
#' Part of the fastxt_native implementation; see the file header for the
#' source it follows.
#'
#' @param corpus See Usage.
#' @param dim Defaults to \code{50}.
#' @param n_min Defaults to \code{3}.
#' @param n_max Defaults to \code{6}.
#' @param window Defaults to \code{5}.
#' @param epochs Defaults to \code{5}.
#' @param lr Defaults to \code{0.05}.
#' @param negative Defaults to \code{5}.
#' @param min_count Defaults to \code{1}.
#' @param boundary Defaults to \code{TRUE}.
#' @param whole_word Defaults to \code{TRUE}.
#' @param hash_buckets Defaults to \code{NULL}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{vectors}, \code{vocab}, \code{index}, \code{ngrams}, \code{ngram_index}, \code{Z}, \code{context}, \code{loss_history}, \code{final_loss}, \code{oov}, \code{n_vocab}, \code{n_ngrams}, \code{dim}, \code{n_min}, \code{n_max}, \code{hash_buckets}, \code{method}.
#' @export
fasttext <- function(corpus, dim = 50, n_min = 3, n_max = 6,
                     window = 5, epochs = 5, lr = 0.05, negative = 5,
                     min_count = 1, boundary = TRUE,
                     whole_word = TRUE, hash_buckets = NULL,
                     seed = 0) {
  docs <- .as_docs(corpus)
  d <- as.integer(dim)
  if (d < 1L)
    stop("fasttext: dim must be at least 1, got ", dim)
  counts <- list()
  for (doc in docs) {
    for (t in doc) {
      if (is.null(counts[[t]])) counts[[t]] <- 0L
      counts[[t]] <- counts[[t]] + 1L
    }
  }
  vocab <- sort(names(which(vapply(counts, function(c) c >= min_count,
                                    logical(1L)))))
  if (length(vocab) < 2L)
    stop("fasttext: ", length(vocab),
         " word(s) above min_count=", min_count,
         "; skipgram needs a context to predict")
  windex <- setNames(seq_along(vocab) - 1L, vocab)

  grams <- character(0)
  gram_index <- list()
  for (wd in vocab) {
    for (g in subwords(wd, n_min, n_max, boundary, whole_word)) {
      if (is.null(gram_index[[g]])) {
        gram_index[[g]] <- length(grams)
        grams <- c(grams, g)
      }
    }
  }
  n_slots <- if (is.null(hash_buckets)) length(grams)
              else as.integer(hash_buckets)

  rng <- .ghc_rng(as.numeric(seed))
  sc <- 0.5 / d
  Z <- matrix(0, nrow = n_slots, ncol = d)
  u_init <- .ghc_unif(rng, n_slots * d)
  for (i in seq_len(n_slots))
    Z[i, ] <- (u_init[((i - 1L) * d + 1L):(i * d)] - 0.5) * sc
  Vc <- matrix(0, nrow = length(vocab), ncol = d)

  freqs <- vapply(vocab, function(t) counts[[t]] ^ 0.75, numeric(1L))
  tot <- sum(freqs)
  cum <- cumsum(freqs / tot)

  draw_negative <- function(rng) {
    u <- .ghc_unif(rng, 1L)
    lo <- 1L
    hi <- length(cum)
    while (lo < hi) {
      mid <- (lo + hi) %/% 2L
      if (u > cum[mid]) lo <- mid + 1L else hi <- mid
    }
    lo - 1L
  }

  eta <- as.numeric(lr)
  losses <- numeric(0)
  for (ep in seq_len(as.integer(epochs))) {
    total <- 0
    n_upd <- 0L
    for (doc in docs) {
      ids <- as.integer(vapply(doc, function(t) {
        w <- windex[[t]]
        if (is.null(w)) -1L else w
      }, integer(1L)))
      ids <- ids[ids >= 0L]
      for (pos in seq_along(ids)) {
        wd <- vocab[ids[pos] + 1L]
        slots <- vapply(subwords(wd, n_min, n_max, boundary,
                                 whole_word), function(g)
                                 .gram_slot(g, gram_index, hash_buckets),
                          integer(1L))
        slots <- slots[!is.na(slots)]
        if (length(slots) == 0L) next
        u <- colSums(Z[slots + 1L, , drop = FALSE])
        lo_i <- max(1L, pos - as.integer(window))
        hi_i <- min(length(ids), pos + as.integer(window))
        for (other in lo_i:hi_i) {
          if (other == pos) next
          targets <- list(c(ids[other], 1.0))
          for (kk in seq_len(as.integer(negative))) {
            targets[[length(targets) + 1L]] <-
              c(draw_negative(rng), 0.0)
          }
          grad_u <- rep(0, d)
          for (tg in targets) {
            ci <- tg[1] + 1L
            label <- tg[2]
            dot <- sum(u * Vc[ci, ])
            d_clamped <- max(-30, min(30, dot))
            p <- 1 / (1 + exp(-d_clamped))
            g <- p - label
            total <- total -
              (if (label > 0.5) log(p + 1e-12)
               else log(1 - p + 1e-12))
            n_upd <- n_upd + 1L
            grad_u <- grad_u + g * Vc[ci, ]
            Vc[ci, ] <- Vc[ci, ] - eta * g * u
          }
          for (s in slots) {
            Z[s + 1L, ] <- Z[s + 1L, ] - eta * grad_u
          }
        }
      }
    }
    losses <- c(losses, if (n_upd > 0L) total / n_upd else NaN)
  }

  vecs <- lapply(vocab, function(wd)
    word_vector(wd, Z, gram_index, n_min, n_max, boundary,
                whole_word, hash_buckets)$v)

  oov <- function(word) {
    word_vector(word, Z, gram_index, n_min, n_max, boundary,
                whole_word, hash_buckets)$v
  }

  list(estimate = vecs, vectors = vecs, vocab = vocab, index = windex,
       ngrams = grams, ngram_index = gram_index, Z = Z, context = Vc,
       loss_history = losses,
       final_loss = if (length(losses)) losses[length(losses)] else NaN,
       oov = oov,
       n_vocab = length(vocab), n_ngrams = length(grams), dim = d,
       n_min = as.integer(n_min), n_max = as.integer(n_max),
       hash_buckets = hash_buckets,
       method = paste("fastText subword skipgram with negative",
                      "sampling, Bojanowski, Grave, Joulin &",
                      "Mikolov (2017) Sec. 3.2",
                      sep = " "))
}

.fastxt_cheatsheet <- function() {
  paste("fastxt: word = bag of character n-grams with < >",
        "boundaries plus the whole word; s(w,c) = sum_g z_g . v_c",
        "(Bojanowski et al. 2017 Sec.3.2). where/n=3 -> <wh whe her",
        "ere re> + <where>. n in 3..6. Gives OOV words a vector.",
        sep = " ")
}

#' morie_fastxt
#'
#' Part of the fastxt_native implementation; see the file header for the
#' source it follows.
#'
#' @param corpus See Usage.
#' @param dim Defaults to \code{50}.
#' @param n_min Defaults to \code{3}.
#' @param n_max Defaults to \code{6}.
#' @param window Defaults to \code{5}.
#' @param epochs Defaults to \code{5}.
#' @param lr Defaults to \code{0.05}.
#' @param negative Defaults to \code{5}.
#' @param min_count Defaults to \code{1}.
#' @param boundary Defaults to \code{TRUE}.
#' @param whole_word Defaults to \code{TRUE}.
#' @param hash_buckets Defaults to \code{NULL}.
#' @param seed Defaults to \code{0}.
#' @return The value of \code{fasttext}.
#' @export
morie_fastxt <- function(corpus, dim = 50, n_min = 3, n_max = 6,
                         window = 5, epochs = 5, lr = 0.05,
                         negative = 5, min_count = 1, boundary = TRUE,
                         whole_word = TRUE, hash_buckets = NULL,
                         seed = 0) {
  fasttext(corpus, dim, n_min, n_max, window, epochs, lr, negative,
           min_count, boundary, whole_word, hash_buckets, seed)
}
