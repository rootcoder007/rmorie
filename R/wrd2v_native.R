# morie.fn -- function file (rootcoder007/morie)
# word2vec: the CBOW and continuous skip-gram architectures.
#
# Mikolov, T., Chen, K., Corrado, G., & Dean, J. (2013) "Efficient
# Estimation of Word Representations in Vector Space", arXiv:1301.3781
# -- the CBOW and skip-gram architectures.
#
# Mikolov, T., Sutskever, I., Chen, K., Corrado, G., & Dean, J. (2013)
# "Distributed Representations of Words and Phrases and their
# Compositionality", NeurIPS, arXiv:1310.4546 -- negative sampling and
# subsampling of frequent words.
#
# Both architectures are log-linear: the non-linear hidden layer is
# removed, leaving a projection into D dimensions and a softmax output.
#
# CBOW (section 3.1) predicts the current word from its context; all
# context words project to the same place -- their vectors are averaged
# -- and word order does not influence the projection. Training
# complexity Q = N D + D log2 V (eq. 4).
#
# Skip-gram (section 3.2) inverts it: each current word predicts words
# within a range before and after. The range is not fixed: for each
# training word a number R in <1; C> is drawn and R words from history
# and R from the future are used, so a word at distance d is used with
# probability (C - d + 1)/C, a triangular weighting achieved by
# sampling. Q = C (D + D log2 V) (eq. 5).
#
# Negative sampling (loss="neg", 2013b sec 2.2, eq. 4) replaces each
# log P(w_O | w_I) with log sigma(o_wO . v_wI) + sum_i E_{wi~Pn}
# [log sigma(-o_wi . v_wI)], with Pn(w) = U(w)^{3/4} / Z.
#
# Subsampling (2013b sec 2.3, eq. 5) discards each occurrence of w_i
# with probability 1 - sqrt(t / f(w_i)).
#
# analogy performs the offset query vector("King") - vector("Man") +
# vector("Woman") ~ vector("Queen"), excluding the three question words.

.wrd2v_ARCH <- c("skip-gram", "cbow")

.wrd2v_rng <- function(seed) {
  # float-safe 32-bit LCG returning uniforms in [0, 1)
  e <- new.env(parent=emptyenv())
  e$s <- as.numeric(seed) %% 2147483648
  if (e$s == 0) {
    e$s <- 1
  }
  e$random <- function() {
    hi <- e$s %/% 65536
    lo <- e$s %% 65536
    e$s <- ((((1103515245 * hi) %% 2147483648) * 65536) %% 2147483648 +
            1103515245 * lo + 12345) %% 2147483648
    e$s / 2147483648
  }
  e
}

.wrd2v_softmax <- function(v) {
  m <- max(v)
  ex <- exp(v - m)
  ex / sum(ex)
}

.wrd2v_sigmoid <- function(z) {
  if (z >= 0.0) {
    1.0 / (1.0 + exp(-z))
  } else {
    e <- exp(z)
    e / (1.0 + e)
  }
}

.wrd2v_cos <- function(a, b) {
  na <- sqrt(sum(a * a))
  nb <- sqrt(sum(b * b))
  if (na == 0.0 || nb == 0.0) {
    return(0.0)
  }
  sum(a * b) / (na * nb)
}

morie_wrd2v_training_complexity <- function(architecture, D, V, N=NULL,
                                            C=NULL, hierarchical=TRUE) {
  # The paper's Q for one training example (eqs. 4-5).
  if (!(architecture %in% .wrd2v_ARCH)) {
    stop(sprintf("wrd2v: architecture must be one of %s, got %s",
                 paste(.wrd2v_ARCH, collapse=", "), architecture))
  }
  out <- if (hierarchical) log2(V) else as.numeric(V)
  if (architecture == "cbow") {
    if (is.null(N)) {
      stop("wrd2v: CBOW complexity needs N")
    }
    return(as.numeric(N) * D + D * out)
  }
  if (is.null(C)) {
    stop("wrd2v: skip-gram complexity needs C")
  }
  as.numeric(C) * (D + D * out)
}

morie_wrd2v_noise_distribution <- function(counts, power=0.75) {
  # Pn(w) = U(w)^{3/4} / Z of Mikolov et al. (2013b) 2.2.
  ws <- sort(names(counts))
  raw <- vapply(ws, function(w) as.numeric(counts[[w]]) ^ as.numeric(power),
                numeric(1))
  z <- sum(raw)
  if (z <= 0.0) {
    stop("wrd2v: noise distribution has no mass")
  }
  stats::setNames(as.list(raw / z), ws)
}

morie_wrd2v_subsample_probability <- function(counts, t=1e-5) {
  # Discard probability 1 - sqrt(t / f(w)) (2013b eq. 5). Clamped at 0.
  total <- sum(unlist(counts))
  if (total <= 0.0) {
    stop("wrd2v: empty counts")
  }
  t <- as.numeric(t)
  if (t <= 0.0) {
    stop("wrd2v: t must be > 0")
  }
  out <- list()
  for (w in names(counts)) {
    f <- counts[[w]] / total
    out[[w]] <- max(0.0, 1.0 - sqrt(t / f))
  }
  out
}

.wrd2v_scores <- function(O, h) {
  as.numeric(O %*% h)
}

.wrd2v_sg_step <- function(st, c, j, size, V, lr) {
  # Skip-gram: input is the centre word, target a context word.
  h <- st$W[c, ]
  p <- .wrd2v_softmax(.wrd2v_scores(st$O, h))
  loss <- -log(max(p[j], 1e-300))
  e <- p
  e[j] <- e[j] - 1.0
  gh <- as.numeric(crossprod(st$O, e))
  st$O <- st$O - lr * outer(e, h)
  st$W[c, ] <- st$W[c, ] - lr * gh
  loss
}

.wrd2v_neg_step <- function(st, c, j, size, lr, k, draw_noise) {
  # Mikolov et al. (2013b) eq. 4, one positive and k noise draws.
  h <- st$W[c, ]
  targets <- c(j, vapply(seq_len(k), function(z) draw_noise(), integer(1)))
  labels <- c(1.0, rep(0.0, k))
  gh <- numeric(size)
  loss <- 0.0
  for (m in seq_along(targets)) {
    it <- targets[m]
    label <- labels[m]
    orow <- st$O[it, ]
    z <- sum(orow * h)
    p <- .wrd2v_sigmoid(z)
    loss <- loss - log(max(if (label == 1.0) p else 1.0 - p, 1e-300))
    ee <- p - label
    gh <- gh + ee * orow
    st$O[it, ] <- orow - lr * ee * h
  }
  st$W[c, ] <- st$W[c, ] - lr * gh
  loss
}

.wrd2v_cbow_step <- function(st, ctx, c, size, V, lr) {
  # CBOW: the projection is the MEAN of the context vectors.
  n <- length(ctx)
  h <- colSums(st$W[ctx, , drop=FALSE]) / n
  p <- .wrd2v_softmax(.wrd2v_scores(st$O, h))
  loss <- -log(max(p[c], 1e-300))
  e <- p
  e[c] <- e[c] - 1.0
  gh <- as.numeric(crossprod(st$O, e))
  st$O <- st$O - lr * outer(e, h)
  # the averaging shares the gradient equally across the context
  st$W[ctx, ] <- st$W[ctx, , drop=FALSE] -
    matrix(lr * gh / n, n, size, byrow=TRUE)
  loss
}

morie_wrd2v_wrd2v <- function(corpus, size=16, window=5,
                              architecture="skip-gram", lr=0.05, epochs=20,
                              min_count=1, dynamic_window=TRUE,
                              loss="softmax", negative=5, noise_power=0.75,
                              subsample=NULL, seed=0) {
  # Train word vectors with CBOW or continuous skip-gram.
  if (!(architecture %in% .wrd2v_ARCH)) {
    stop(sprintf("wrd2v: architecture must be one of %s, got %s",
                 paste(.wrd2v_ARCH, collapse=", "), architecture))
  }
  if (!(loss %in% c("softmax", "neg"))) {
    stop(sprintf("wrd2v: loss must be 'softmax' or 'neg', got %s", loss))
  }
  if (loss == "neg" && architecture != "skip-gram") {
    stop(paste0("wrd2v: negative sampling is defined in Mikolov et al. ",
                "(2013b) eq. 4 as a replacement for the terms of the ",
                "SKIP-GRAM objective; use architecture='skip-gram' or ",
                "loss='softmax'"))
  }
  negative <- as.integer(negative)
  if (loss == "neg" && negative < 1L) {
    stop("wrd2v: negative must be >= 1")
  }
  size <- as.integer(size)
  window <- as.integer(window)
  if (size < 1L) {
    stop("wrd2v: size must be >= 1")
  }
  if (window < 1L) {
    stop("wrd2v: window must be >= 1")
  }
  sents <- lapply(corpus, as.character)
  if (length(sents) == 0L || !any(vapply(sents, length, integer(1)) > 0L)) {
    stop("wrd2v: corpus must contain at least one non-empty sentence")
  }
  counts <- list()
  for (s in sents) {
    for (w in s) {
      counts[[w]] <- (if (is.null(counts[[w]])) 0L else counts[[w]]) + 1L
    }
  }
  vocab <- sort(names(counts)[unlist(counts) >= as.integer(min_count)])
  if (length(vocab) == 0L) {
    stop(sprintf("wrd2v: min_count = %s discarded every word",
                 as.character(min_count)))
  }
  idx <- stats::setNames(seq_along(vocab), vocab)
  V <- length(vocab)
  invocab <- vocab
  sents <- lapply(sents, function(s) s[s %in% invocab])
  if (!is.null(subsample)) {
    keep_drop <- morie_wrd2v_subsample_probability(
      stats::setNames(counts[vocab], vocab), subsample)
    srng <- .wrd2v_rng(seed + 7)
    sents <- lapply(sents, function(s) {
      if (length(s) == 0L) return(s)
      s[vapply(s, function(w) srng$random() >= keep_drop[[w]], logical(1))]
    })
  }
  rng <- .wrd2v_rng(seed)
  noise <- morie_wrd2v_noise_distribution(
    stats::setNames(counts[vocab], vocab), noise_power)
  cum <- cumsum(vapply(vocab, function(w) noise[[w]], numeric(1)))
  draw_noise <- function() {
    u <- rng$random() * cum[length(cum)]
    lo <- 1L
    hi <- length(cum)
    while (lo < hi) {
      mid <- (lo + hi) %/% 2L
      if (cum[mid] < u) {
        lo <- mid + 1L
      } else {
        hi <- mid
      }
    }
    lo
  }
  scale <- 0.5 / size
  st <- new.env(parent=emptyenv())
  Wvals <- vapply(seq_len(V * size),
                  function(z) (rng$random() * 2.0 - 1.0) * scale, numeric(1))
  st$W <- matrix(Wvals, V, size, byrow=TRUE)
  st$O <- matrix(0.0, V, size)
  curve <- numeric(0)
  for (ep in seq_len(max(1L, as.integer(epochs)))) {
    total <- 0.0
    n_ex <- 0L
    for (s in sents) {
      L <- length(s)
      if (L == 0L) {
        next
      }
      for (t in seq_len(L)) {
        R <- if (dynamic_window) {
          1L + as.integer(rng$random() * window)
        } else {
          window
        }
        lo <- max(1L, t - R)
        hi <- min(L, t + R)
        ctxpos <- setdiff(lo:hi, t)
        if (length(ctxpos) == 0L) {
          next
        }
        ctx <- as.integer(idx[s[ctxpos]])
        c <- as.integer(idx[[s[t]]])
        if (architecture == "cbow") {
          total <- total + .wrd2v_cbow_step(st, ctx, c, size, V, lr)
          n_ex <- n_ex + 1L
        } else if (loss == "neg") {
          for (j in ctx) {
            total <- total + .wrd2v_neg_step(st, c, j, size, lr, negative,
                                             draw_noise)
            n_ex <- n_ex + 1L
          }
        } else {
          for (j in ctx) {
            total <- total + .wrd2v_sg_step(st, c, j, size, V, lr)
            n_ex <- n_ex + 1L
          }
        }
      }
    }
    curve <- c(curve, if (n_ex > 0L) total / n_ex else 0.0)
  }
  vectors <- stats::setNames(
    lapply(seq_len(V), function(i) st$W[i, ]), vocab)
  outv <- stats::setNames(
    lapply(seq_len(V), function(i) st$O[i, ]), vocab)
  similarity <- function(a, b) .wrd2v_cos(vectors[[a]], vectors[[b]])
  most_similar <- function(word, topn=5) {
    if (is.null(vectors[[word]])) {
      stop(sprintf("wrd2v: %s is not in the vocabulary", word))
    }
    others <- setdiff(vocab, word)
    sims <- vapply(others,
                   function(w) .wrd2v_cos(vectors[[word]], vectors[[w]]),
                   numeric(1))
    ord <- order(-sims)
    lapply(ord[seq_len(min(topn, length(ord)))],
           function(i) list(others[i], sims[i]))
  }
  list(
    estimate=vectors, vectors=vectors, output_vectors=outv,
    vocab=stats::setNames(counts[vocab], vocab), loss_curve=curve,
    final_loss=if (length(curve) > 0L) curve[length(curve)] else NaN,
    similarity=similarity, most_similar=most_similar,
    size=size, window=window, architecture=architecture, loss=loss,
    negative=if (loss == "neg") negative else 0L, noise=noise,
    method=paste0("word2vec (Mikolov et al. 2013a secs 3.1-3.2",
                  if (loss == "neg") "; 2013b eq. 4 negative sampling)"
                  else ")"))
}

morie_wrd2v_analogy <- function(vectors, a, b, c, topn=1) {
  # Section 4's offset query: b - a + c. The three question words are
  # excluded from the answer.
  for (w in c(a, b, c)) {
    if (is.null(vectors[[w]])) {
      stop(sprintf("analogy: %s is not in the vocabulary", w))
    }
  }
  target <- vectors[[b]] - vectors[[a]] + vectors[[c]]
  words <- setdiff(names(vectors), c(a, b, c))
  sims <- vapply(words, function(w) .wrd2v_cos(target, vectors[[w]]),
                 numeric(1))
  ord <- order(-sims)
  lapply(ord[seq_len(min(topn, length(ord)))],
         function(i) list(words[i], sims[i]))
}

morie_wrd2v_cheatsheet <- function() {
  paste0(
    "wrd2v: log-linear word vectors (Mikolov 2013a). CBOW ",
    "predicts the centre word from AVERAGED context vectors ",
    "(sec 3.1, Q = N*D + D*log2 V); skip-gram predicts context ",
    "from the centre word (sec 3.2, Q = C*(D + D*log2 V)) with ",
    "a DYNAMIC window R ~ Unif{1..C}, so distance d is used ",
    "with probability (C-d+1)/C. loss='neg' is negative ",
    "sampling from the FOLLOW-UP paper (2013b eq. 4) with ",
    "Pn(w) = U(w)^0.75/Z, plus eq. 5 subsampling. analogy() ",
    "is the b - a + c offset query of 2013a sec 4."
  )
}

# compact alias per ledger/NAMING.md
morie_wrd2v_word2vec <- morie_wrd2v_wrd2v

#' @export
morie_wrd2v <- morie_wrd2v_wrd2v
