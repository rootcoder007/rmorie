```r
# word2vec: the CBOW and continuous skip-gram architectures.
# 
# Mikolov, T., Chen, K., Corrado, G., & Dean, J. (2013) "Efficient
# Estimation of Word Representations in Vector Space",
# arXiv:1301.3781 -- the CBOW and skip-gram architectures.
# 
# Mikolov, T., Sutskever, I., Chen, K., Corrado, G., & Dean, J. (2013)
# "Distributed Representations of Words and Phrases and their
# Compositionality", *NeurIPS*, arXiv:1310.4546 -- negative sampling and
# subsampling of frequent words, which are extensions of skip-gram
# published separately and are marked as such below.

.wrd2v_ARCH <- c("skip-gram", "cbow")

.wrd2v_softmax <- function(v) {
  m <- max(v)
  e <- exp(v - m)
  s <- sum(e)
  e / s
}

training_complexity <- function(architecture, D, V, N = NULL, C = NULL,
                                hierarchical = TRUE) {
  if (!(architecture %in% .wrd2v_ARCH)) {
    stop(sprintf("wrd2v: architecture must be one of %s, got %s",
                 paste(.wrd2v_ARCH, collapse = ", "), architecture))
  }
  out <- if (hierarchical) log(V, 2) else as.numeric(V)
  if (architecture == "cbow") {
    if (is.null(N)) stop("wrd2v: CBOW complexity needs N")
    return(as.numeric(N) * D + D * out)
  }
  if (is.null(C)) stop("wrd2v: skip-gram complexity needs C")
  as.numeric(C) * (D + D * out)
}

noise_distribution <- function(counts, power = 0.75) {
  ws <- sort(names(counts))
  raw <- as.numeric(counts[ws]) ^ as.numeric(power)
  z <- sum(raw)
  if (z <= 0) stop("wrd2v: noise distribution has no mass")
  setNames(raw / z, ws)
}

subsample_probability <- function(counts, t = 1e-5) {
  total <- sum(as.numeric(counts))
  if (total <= 0) stop("wrd2v: empty counts")
  t <- as.numeric(t)
  if (t <= 0) stop("wrd2v: t must be > 0")
  f <- as.numeric(counts) / total
  p <- pmax(0, 1 - sqrt(t / f))
  setNames(p, names(counts))
}

.wrd2v_cos <- function(a, b) {
  na <- sqrt(sum(a * a))
  nb <- sqrt(sum(b * b))
  if (na == 0 || nb == 0) return(0)
  sum(a * b) / (na * nb)
}

.wrd2v_sigmoid <- function(z) {
  if (z >= 0) {
    return(1 / (1 + exp(-z)))
  }
  e <- exp(z)
  e / (1 + e)
}

morie_wrd2v <- function(corpus, size = 16, window = 5, architecture = "skip-gram",
                        lr = 0.05, epochs = 20, min_count = 1,
                        dynamic_window = TRUE, loss = "softmax",
                        negative = 5, noise_power = 0.75, subsample = NULL,
                        seed = 0) {
  if (!(architecture %in% .wrd2v_ARCH)) {
    stop(sprintf("wrd2v: architecture must be one of %s, got %s",
                 paste(.wrd2v_ARCH, collapse = ", "), architecture))
  }
  if (!(loss %in% c("softmax", "neg"))) {
    stop(sprintf("wrd2v: loss must be 'softmax' or 'neg', got %s", loss))
  }
  if (loss == "neg" && architecture != "skip-gram") {
    stop("wrd2v: negative sampling is defined in Mikolov et al. (2013b) eq. 4 as a replacement for the terms of the SKIP-GRAM objective; use architecture='skip-gram' or loss='softmax'")
  }
  negative <- as.integer(negative)
  if (loss == "neg" && negative < 1) stop("wrd2v: negative must be >= 1")
  size <- as.integer(size)
  window <- as.integer(window)
  if (size < 1) stop("wrd2v: size must be >= 1")
  if (window < 1) stop("wrd2v: window must be >= 1")
  
  sents <- as.list(corpus)
  if (length(sents) == 0 || !any(sapply(sents, length) > 0)) {
    stop("wrd2v: corpus must contain at least one non-empty sentence")
  }
  
  counts <- list()
  for (s in sents) {
    for (w in s) {
      counts[[w]] <- if (is.null(counts[[w]])) 1L else counts[[w]] + 1L
    }
  }
  
  min_count <- as.integer(min_count)
  vocab <- sort(names(counts)[sapply(counts, function(c) c >= min_count)])
  if (length(vocab) == 0) {
    stop(sprintf("wrd2v: min_count = %d discarded every word", min_count))
  }
  
  idx <- setNames(seq_along(vocab), vocab)
  V <- length(vocab)
  
  sents <- lapply(sents, function(s) s[s %in% vocab])
  
  if (!is.null(subsample)) {
    keep_drop <- subsample_probability(counts[vocab], subsample)
    srng <- .ghc_rng(as.integer(seed + 7))
    new_sents <- list()
    for (s in sents) {
      n <- length(s)
      if (n == 0) {
        new_sents[[length(new_sents) + 1]] <- character(0)
        next
      }
      r <- .ghc_unif(srng, n)
      drops <- keep_drop[s]
      new_sents[[length(new_sents) + 1]] <- s[r >= drops]
    }
    sents <- new_sents
  }
  
  rng <- .ghc_rng(as.integer(seed))
  
  noise <- noise_distribution(counts[vocab], noise_power)
  cum <- numeric(V)
  acc <- 0
  for (i in seq_len(V)) {
    acc <- acc + noise[[vocab[i]]]
    cum[i] <- acc
  }
  
  draw_noise <- function() {
    u <- .ghc_unif(rng, 1)[1] * cum[V]
    lo <- 1
    hi <- V
    while (lo < hi) {
      mid <- (lo + hi) %/% 2
      if (cum[mid] < u) {
        lo <- mid + 1
      } else {
        hi <- mid
      }
    }
    lo
  }
  
  scale <- 0.5 / size
  W <- matrix(0, nrow = V, ncol = size)
  for (i in seq_len(V)) {
    W[i, ] <- (.ghc_unif(rng, size) * 2 - 1) * scale
  }
  O <- matrix(0, nrow = V, ncol = size)
  
  curve <- numeric(0)
  for (ep in seq_len(max(1, as.integer(epochs)))) {
    total <- 0
    n_ex <- 0
    for (s in sents) {
      L <- length(s)
      if (L == 0) next
      for (t in seq_len(L)) {
        R <- if (dynamic_window) 1 + as.integer(.ghc_unif(rng, 1)[1] * window) else window
        t0 <- t - 1L
        lo0 <- max(0L, t0 - R)
        hi0 <- min(L, t0 + R + 1L)
        ctx_idx <- setdiff((lo0 + 1L):hi0, t)
        if (length(ctx_idx) == 0) next
        
        c <- idx[[s[t]]]
        ctx_word_idx <- idx[s[ctx_idx]]
        
        if (architecture == "cbow") {
          n_ctx <- length(ctx_word_idx)
          h <- colSums(W[ctx_word_idx, , drop = FALSE]) / n_ctx
          scores <- as.numeric(O %*% h)
          p <- .wrd2v_softmax(scores)
          loss <- -log(max(p[c], 1e-300))
          
          e <- p
          e[c] <- e[c] - 1.0
          
          gh <- as.numeric(t(e) %*% O)
          
          O <- O - lr * outer(e, h)
          
          W[ctx_word_idx, ] <- W[ctx_word_idx, ] - lr * gh / n_ctx
          
          total <- total + loss
          n_ex <- n_ex + 1
        } else if (loss == "neg") {
          for (j in ctx_word_idx) {
            h <- W[c, ]
            gh <- numeric(size)
            loss <- 0
            
            z <- sum(O[j, ] * h)
            p <- .wrd2v_sigmoid(z)
            loss <- loss - log(max(p, 1e-300))
            e_grad <- p - 1.0
            gh <- gh + e_grad * O[j, ]
            O[j, ] <- O[j, ] - lr * e_grad * h
            
            for (kk in seq_len(negative)) {
              n_idx <- draw_noise()
              z <- sum(O[n_idx, ] * h)
              p <- .wrd2v_sigmoid(z)
              loss <- loss - log(max(1 - p, 1e-300))
              e_grad <- p
              gh <- gh + e_grad * O[n_idx, ]
              O[n_idx, ] <- O[n_idx, ] - lr * e_grad * h
            }
            
            W[c, ] <- W[c, ] - lr * gh
            
            total <- total + loss
            n_ex <- n_ex + 1
          }
        } else {
          for (j in ctx_word_idx) {
            h <- W[c, ]
            scores <- as.numeric(O %*% h)
            p <- .wrd2v_softmax(scores)
            loss <- -log(max(p[j], 1e-300))
            
            e <- p
            e[j] <- e[j] - 1.0
            
            gh <- as.numeric(t(e) %*% O)
            O <- O - lr * outer(e, h)
            W[c, ] <- W[c, ] - lr * gh
            
            total <- total + loss
            n_ex <- n_ex + 1
          }
        }
      }
    }
    curve[ep] <- if (n_ex > 0) total / n_ex else 0
  }
  
  vectors <- list()
  for (i in seq_len(V)) {
    vectors[[vocab[i]]] <- W[i, ]
  }
  outv <- list()
  for (i in seq_len(V)) {
    outv[[vocab[i]]] <- O[i, ]
  }
  
  similarity <- function(a, b) {
    .wrd2v_cos(vectors[[a]], vectors[[b]])
  }
  
  most_similar <- function(word, topn = 5) {
    if (is.null(vectors[[word]])) {
      stop(sprintf("wrd2v: '%s' is not in the vocabulary", word))
    }
    ws <- character(0)
    sims <- numeric(0)
    for (w in names(vectors)) {
      if (w != word) {
        ws <- c(ws, w)
        sims <- c(sims, .wrd2v_cos(vectors[[word]], vectors[[w]]))
      }
    }
    if (length(sims) == 0) {
      return(data.frame(word = character(0), similarity = numeric(0),
                        stringsAsFactors = FALSE))
    }
    ord <- order(-sims)
    ws
