# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Burkov Hundred-Page Language Models shelf. Mirrors the morie.fn
# modules b101-b111, b201-b203 and bk* (29 Python modules; the pure
# notational ones fold into their operational sibling here).
#
# Burkov A (2025) The Hundred-Page Language Models Book, True Positive
# Inc. Eq numbers PDF-verified (Eq 1.1 p.20, Eq 1.2 p.22, ...).

#' Linear model f(x) = wx + b (Burkov Eq 1.1)
#' @param x Feature values.
#' @param w Weight.
#' @param b Bias.
#' @return List with `predictions`.
#' @export
#' @examples
#' morie_burkov_linear_function(x = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7,
#' 8), b = 5L)
morie_burkov_linear_function <- function(x, w, b) {
  x <- as.numeric(x)
  list(
    predictions = w * x + b, estimate = (w * x + b)[1], w = w, b = b,
    n = length(x), method = "Linear model f(x) = wx + b (Burkov Eq 1.1)"
  )
}

#' Squared error (Burkov Eq 1.2)
#' @param y_hat,y Predictions and targets.
#' @return List with `errors`.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_burkov_squared_error(V, V)
morie_burkov_squared_error <- function(y_hat, y) {
  y_hat <- as.numeric(y_hat)
  y <- as.numeric(y)
  if (length(y_hat) != length(y)) {
    stop("y_hat and y must have the same length.", call. = FALSE)
  }
  err <- (y_hat - y)^2
  list(
    errors = err, estimate = err[1], n = length(y),
    method = "Squared error (Burkov Eq 1.2)"
  )
}

#' MSE cost of the linear model (Burkov Eq 1.3)
#' @param w,b Parameters.
#' @param x,y Data.
#' @param N Optional size check.
#' @return List with `cost`, `residuals`.
#' @export
#' @examples
#' morie_burkov_mse_cost(w = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, x = c(1, 2, 3, 4, 5, 6,
#' 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_burkov_mse_cost <- function(w, b, x, y, N = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) {
    stop("x and y must have the same length.", call. = FALSE)
  }
  if (!is.null(N) && as.integer(N) != length(x)) {
    stop(sprintf(paste(
      "N = %s does not match the dataset size %d; the N",
      "in Eq 1.3 is the dataset size, not a free",
      "parameter."
    ), N, length(x)), call. = FALSE)
  }
  resid <- w * x + b - y
  list(
    cost = mean(resid^2), estimate = mean(resid^2), residuals = resid,
    n = length(x), method = "MSE cost J(w, b) (Burkov Eq 1.3)"
  )
}

#' Vector-form linear model w.x + b (Burkov Eq 1.4)
#' @param w,x Vectors.
#' @param b Bias.
#' @export
morie_burkov_linear_vector <- function(w, x, b) {
  w <- as.numeric(w)
  x <- as.numeric(x)
  if (length(w) != length(x)) {
    stop("w and x must have the same length.", call. = FALSE)
  }
  list(
    estimate = sum(w * x) + b, dot = sum(w * x), b = b, n = length(x),
    method = "Linear model y = w.x + b (Burkov Eq 1.4)"
  )
}

#' Cosine similarity (Burkov Eq 1.5)
#' @param x,y Vectors; neither may be zero.
#' @export
morie_burkov_cosine_similarity <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) {
    stop("x and y must have the same length.", call. = FALSE)
  }
  nx <- sqrt(sum(x^2))
  ny <- sqrt(sum(y^2))
  if (nx == 0 || ny == 0) {
    stop("a zero vector has no direction; cosine similarity with it is undefined.",
      call. = FALSE
    )
  }
  cc <- max(-1, min(1, sum(x * y) / (nx * ny)))
  list(
    estimate = cc, angle_radians = acos(cc), n = length(x),
    method = "Cosine similarity (Burkov Eq 1.5)"
  )
}

#' .morie_burkov_phi
#'
#' A step of the burkov_lm_native implementation. Called by
#' \code{morie_burkov_layer1_output}, \code{morie_burkov_layer2_output}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param name A function; the body checks with \code{is.function}.
#' @return The value of \code{switch}.
#' @export
.morie_burkov_phi <- function(name) {
  if (is.function(name)) {
    return(name)
  }
  switch(name,
    identity = function(z) z,
    relu = function(z) pmax(z, 0),
    tanh = tanh,
    sigmoid = function(z) 1 / (1 + exp(-z)),
    stop(sprintf(
      "unknown activation '%s'; pass a function or one of identity, relu, sigmoid, tanh.",
      name
    ), call. = FALSE)
  )
}

#' First hidden layer phi(W1 x + b1) (Burkov Eq 1.6)
#' @param W_1 Weight matrix.
#' @param x Input.
#' @param b_1 Bias.
#' @param phi Activation name or function.
#' @export
morie_burkov_layer1_output <- function(W_1, x, b_1, phi = "relu") {
  W <- as.matrix(W_1)
  x <- as.numeric(x)
  b <- as.numeric(b_1)
  if (ncol(W) != length(x)) {
    stop(sprintf(
      "W_1 has %d columns but x has %d entries.", ncol(W),
      length(x)
    ), call. = FALSE)
  }
  if (nrow(W) != length(b)) {
    stop(sprintf(
      "W_1 has %d rows but b_1 has %d entries.", nrow(W),
      length(b)
    ), call. = FALSE)
  }
  pre <- as.numeric(W %*% x + b)
  out <- .morie_burkov_phi(phi)(pre)
  list(
    output = out, preactivation = pre, estimate = out[1],
    n = length(out), method = "Layer 1 output phi(W1 x + b1) (Burkov Eq 1.6)"
  )
}

#' Second-layer scalar output (Burkov Eq 1.7)
#' @param W_2 Weight row.
#' @param y_1 Layer-1 output.
#' @param b_2_1 Bias.
#' @param phi Activation.
#' @export
morie_burkov_layer2_output <- function(W_2, y_1, b_2_1, phi = "identity") {
  w <- as.numeric(W_2)
  y1 <- as.numeric(y_1)
  if (length(w) != length(y1)) {
    stop(sprintf(
      "W_2 has %d weights but y_1 has %d entries.", length(w),
      length(y1)
    ), call. = FALSE)
  }
  pre <- sum(w * y1) + b_2_1
  list(
    estimate = .morie_burkov_phi(phi)(pre), preactivation = pre,
    n = length(y1),
    method = "Layer 2 output phi(W2 y1 + b21) (Burkov Eq 1.7)"
  )
}

#' Logistic regression sigma(w.x + b) (Burkov Eq 1.8)
#' @param w,x Vectors.
#' @param b Bias.
#' @export
morie_burkov_logistic <- function(w, x, b) {
  w <- as.numeric(w)
  x <- as.numeric(x)
  if (length(w) != length(x)) {
    stop("w and x must have the same length.", call. = FALSE)
  }
  z <- sum(w * x) + b
  p <- 1 / (1 + exp(-z))
  list(
    estimate = p, logit = z, predicted_class = as.integer(p >= 0.5),
    n = length(x),
    method = "Logistic regression sigma(w.x + b) (Burkov Eq 1.8)"
  )
}

#' Binary cross-entropy for one example (Burkov Eq 1.9)
#' @param y_hat Predicted probabilities in \\[0, 1\\].
#' @param y Targets, 0 or 1.
#' @export
morie_burkov_binary_cross_entropy <- function(y_hat, y) {
  yh <- as.numeric(y_hat)
  y <- as.numeric(y)
  if (length(yh) != length(y)) {
    stop("y_hat and y must have the same length.", call. = FALSE)
  }
  if (any(yh < 0 | yh > 1)) {
    stop("predicted probabilities must lie in [0, 1].", call. = FALSE)
  }
  if (any(y != 0 & y != 1)) {
    stop("targets must be 0 or 1 for Eq 1.9.", call. = FALSE)
  }
  loss <- -(y * log(yh) + (1 - y) * log(1 - yh))
  loss[is.nan(loss)] <- 0 # 0 * log 0 limit
  list(
    losses = loss, estimate = loss[1], mean_loss = mean(loss),
    n = length(y), method = "Binary cross-entropy (Burkov Eq 1.9)"
  )
}

#' Closed-form BCE gradients for logistic regression (Burkov Eq 1.11)
#' @param y_hat,y Predictions and targets.
#' @param x Design matrix, one
#'   row per example.
#' @param N Optional size check.
#' @param j Optional
#'   0-based coordinate for `estimate` (matching the Python mirror).
#' @export
morie_burkov_bce_gradients <- function(y_hat, y, x, N = NULL, j = NULL) {
  yh <- as.numeric(y_hat)
  y <- as.numeric(y)
  X <- as.matrix(x)
  if (nrow(X) != length(yh)) X <- t(X)
  if (nrow(X) != length(yh) || length(y) != length(yh)) {
    stop("need one row of x per example.", call. = FALSE)
  }
  if (!is.null(N) && as.integer(N) != length(y)) {
    stop(sprintf(
      "N = %s does not match the dataset size %d.", N,
      length(y)
    ), call. = FALSE)
  }
  resid <- yh - y
  gw <- colMeans(X * resid)
  gb <- mean(resid)
  est <- if (!is.null(j)) gw[as.integer(j) + 1L] else gw[1]
  list(
    grad_w = as.numeric(gw), grad_b = gb, estimate = as.numeric(est),
    n = length(y),
    method = "BCE gradients for logistic regression (Burkov Eq 1.11)"
  )
}

#' Categorical cross-entropy with a one-hot target (Burkov Eq 2.1)
#' @param y_hat Probability distribution.
#' @param c Correct class,
#'   0-based to match the Python mirror.
#' @export
morie_burkov_categorical_cross_entropy <- function(y_hat, c) {
  p <- as.numeric(y_hat)
  c <- as.integer(c)
  if (c < 0L || c >= length(p)) {
    stop(sprintf(
      "class %d is out of range for %d classes.", c,
      length(p)
    ), call. = FALSE)
  }
  if (any(p < 0) || abs(sum(p) - 1) > 1e-8) {
    stop(sprintf(
      paste(
        "y_hat must be a probability distribution",
        "(non-negative, summing to 1); it sums to %.6g."
      ),
      sum(p)
    ), call. = FALSE)
  }
  loss <- if (p[c + 1L] > 0) -log(p[c + 1L]) else Inf
  list(
    estimate = loss, p_correct = p[c + 1L], n_classes = length(p),
    n = length(p),
    method = "Categorical cross-entropy -log p_c (Burkov Eq 2.1)"
  )
}

#' Autoregressive next-token probability, bigram MLE (Burkov Eq 2.2/2.3)
#'
#' Eq 2.2 DEFINES a language model rather than giving an estimator, so
#' the operational content is the simplest one the book builds on:
#' count how often the last token of s is followed by t_next within s.
#' Eq 2.3's notational equivalence folds in as `notations_agree`.
#'
#' @param t_next Candidate token.
#' @param s Token sequence.
#' @export
morie_burkov_next_token <- function(t_next, s) {
  seq_ <- as.character(s)
  if (length(seq_) < 2L) {
    stop("need at least 2 tokens to form one bigram.", call. = FALSE)
  }
  ctx <- seq_[length(seq_)]
  idx <- which(seq_[-length(seq_)] == ctx)
  if (length(idx) == 0L) {
    stop(sprintf(
      paste(
        "the context token '%s' never has a successor in",
        "s, so the MLE conditional is undefined (0/0)."
      ),
      ctx
    ), call. = FALSE)
  }
  follow <- seq_[idx + 1L]
  p <- mean(follow == as.character(t_next))
  dist <- table(follow) / length(follow)
  list(
    estimate = p, context = ctx,
    distribution = as.list(setNames(as.numeric(dist), names(dist))),
    notations_agree = TRUE, n = length(seq_),
    method = "Autoregressive next-token probability, bigram MLE (Burkov Eq 2.2)"
  )
}

#' N-gram MLE probability (Burkov Ch 2)
#' @param counts_ngram,counts_prefix Counts.
#' @export
morie_burkov_ngram_mle <- function(counts_ngram, counts_prefix) {
  c <- as.numeric(counts_ngram)
  p <- as.numeric(counts_prefix)
  if (c < 0 || p < 0) stop("counts must be non-negative.", call. = FALSE)
  if (p == 0) {
    stop(paste(
      "the prefix was never observed, so the MLE conditional is",
      "undefined (0/0); use smoothing or backoff."
    ), call. = FALSE)
  }
  if (c > p) {
    stop(sprintf(paste(
      "count(ngram) = %g exceeds count(prefix) = %g,",
      "which is impossible: every ngram occurrence",
      "contains its prefix."
    ), c, p), call. = FALSE)
  }
  list(
    estimate = c / p, count_ngram = c, count_prefix = p, n = as.integer(p),
    method = "N-gram MLE count/prefix (Burkov Ch 2)"
  )
}

#' Laplace add-1 smoothing (Burkov Ch 2)
#' @param counts_ngram,counts_prefix Counts.
#' @param V Vocabulary size.
#' @export
morie_burkov_laplace <- function(counts_ngram, counts_prefix, V) {
  c <- as.numeric(counts_ngram)
  p <- as.numeric(counts_prefix)
  v <- as.integer(V)
  if (c < 0 || p < 0) stop("counts must be non-negative.", call. = FALSE)
  if (v < 1L) stop("vocabulary size must be positive.", call. = FALSE)
  if (c > p) stop("count(ngram) cannot exceed count(prefix).", call. = FALSE)
  list(
    estimate = (c + 1) / (p + v), count_ngram = c, count_prefix = p,
    vocab_size = v, n = as.integer(p),
    method = "Laplace add-1 smoothing (Burkov Ch 2)"
  )
}

#' Add-k smoothing (Burkov Ch 2)
#' @param counts_ngram,counts_prefix Counts.
#' @param V Vocabulary size.
#' @param k Pseudo-count, positive.
#' @export
morie_burkov_add_k <- function(counts_ngram, counts_prefix, V, k = 0.5) {
  c <- as.numeric(counts_ngram)
  p <- as.numeric(counts_prefix)
  v <- as.integer(V)
  k <- as.numeric(k)
  if (c < 0 || p < 0) stop("counts must be non-negative.", call. = FALSE)
  if (v < 1L) stop("vocabulary size must be positive.", call. = FALSE)
  if (k <= 0) {
    stop(
      sprintf(paste(
        "k must be positive; got %g. k = 0 is the",
        "unsmoothed MLE and has its own function."
      ), k),
      call. = FALSE
    )
  }
  if (c > p) stop("count(ngram) cannot exceed count(prefix).", call. = FALSE)
  list(
    estimate = (c + k) / (p + k * v), count_ngram = c, count_prefix = p,
    vocab_size = v, k = k, n = as.integer(p),
    method = "Add-k smoothing (Burkov Ch 2)"
  )
}

#' Linear interpolation of n-gram orders (Burkov Ch 2)
#' @param probs_by_order Probabilities.
#' @param lambdas Weights, sum 1.
#' @export
morie_burkov_interpolation <- function(probs_by_order, lambdas) {
  ps <- as.numeric(probs_by_order)
  ls <- as.numeric(lambdas)
  if (length(ps) != length(ls)) {
    stop("need one lambda per order.", call. = FALSE)
  }
  if (any(ls < 0) || abs(sum(ls) - 1) > 1e-9) {
    stop(sprintf(
      "lambdas must be non-negative and sum to 1; they sum to %.6g.",
      sum(ls)
    ), call. = FALSE)
  }
  if (any(ps < 0 | ps > 1)) {
    stop("probabilities must lie in [0, 1].", call. = FALSE)
  }
  list(
    estimate = sum(ls * ps), probs = ps, lambdas = ls, n = length(ps),
    method = "Linear interpolation of n-gram orders (Burkov Ch 2)"
  )
}

#' N-gram backoff with per-level discount (Burkov Ch 2)
#' @param counts_by_order List of `c(count_ngram, count_prefix)` pairs,
#'   highest order first.
#' @param alpha Per-level discount in (0, 1\\].
#' @export
morie_burkov_backoff <- function(counts_by_order, alpha = 0.4) {
  a <- as.numeric(alpha)
  if (a <= 0 || a > 1) {
    stop(sprintf("alpha must lie in (0, 1]; got %g.", a), call. = FALSE)
  }
  if (length(counts_by_order) == 0L) {
    stop("no orders supplied.", call. = FALSE)
  }
  discount <- 1
  for (level in seq_along(counts_by_order)) {
    pair <- as.numeric(counts_by_order[[level]])
    c <- pair[1]
    p <- pair[2]
    if (c < 0 || p < 0) stop("counts must be non-negative.", call. = FALSE)
    if (c > p) stop("count(ngram) cannot exceed count(prefix).", call. = FALSE)
    if (c > 0) {
      return(list(
        estimate = discount * c / p, order_used = level - 1L,
        backed_off = level - 1L, discount = discount,
        n = as.integer(p),
        method = "N-gram backoff (Burkov Ch 2)"
      ))
    }
    discount <- discount * a
  }
  stop(paste(
    "every order has count 0, including the lowest; backoff has",
    "nowhere left to go. Supply a unigram floor with a positive",
    "count."
  ), call. = FALSE)
}

#' Kneser-Ney smoothing (Burkov Ch 2)
#' @param counts_ngram,counts_prefix Counts.
#' @param continuation_counts `c(n_types_after_prefix,
#'   continuation_count_of_word, total_bigram_types)`.
#' @param d Absolute discount in (0, 1).
#' @export
morie_burkov_kneser_ney <- function(counts_ngram, counts_prefix,
                                    continuation_counts, d = 0.75) {
  c <- as.numeric(counts_ngram)
  p <- as.numeric(counts_prefix)
  dd <- as.numeric(d)
  if (c < 0 || p <= 0) {
    stop("need non-negative count and positive prefix.", call. = FALSE)
  }
  if (c > p) stop("count(ngram) cannot exceed count(prefix).", call. = FALSE)
  if (dd <= 0 || dd >= 1) {
    stop(sprintf("the discount d must lie in (0, 1); got %g.", dd),
      call. = FALSE
    )
  }
  cc <- as.numeric(continuation_counts)
  n_after <- cc[1]
  cont_w <- cc[2]
  total_types <- cc[3]
  if (total_types <= 0 || cont_w < 0 || n_after < 0) {
    stop("continuation counts must be non-negative with positive total bigram types.",
      call. = FALSE
    )
  }
  if (cont_w > total_types) {
    stop("a word cannot appear in more contexts than there are bigram types.",
      call. = FALSE
    )
  }
  lam <- dd * n_after / p
  p_cont <- cont_w / total_types
  list(
    estimate = max(c - dd, 0) / p + lam * p_cont,
    discounted_mle = max(c - dd, 0) / p, lambda = lam,
    p_continuation = p_cont, n = as.integer(p),
    method = "Kneser-Ney smoothing (Burkov Ch 2)"
  )
}

#' Bits per character from cross-entropy (Burkov Ch 2)
#' @param ce_loss Cross-entropy in nats per token.
#' @param n_tokens,n_characters Counts.
#' @export
morie_burkov_bits_per_character <- function(ce_loss, n_tokens,
                                            n_characters) {
  l <- as.numeric(ce_loss)
  nt <- as.integer(n_tokens)
  nc <- as.integer(n_characters)
  if (l < 0) stop("cross-entropy cannot be negative.", call. = FALSE)
  if (nt < 1L || nc < 1L) {
    stop("token and character counts must be positive.", call. = FALSE)
  }
  list(
    estimate = (l * nt) / (log(2) * nc), bits_per_token = l / log(2),
    chars_per_token = nc / nt, n = nt,
    method = "Bits per character (Burkov Ch 2)"
  )
}

#' Dot product, L2 norm and unit vector (Burkov Ch 1)
#' @param a,b Vectors.
#' @export
morie_burkov_dot_product <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (length(a) != length(b)) {
    stop("vectors must have the same length.", call. = FALSE)
  }
  list(
    estimate = sum(a * b), n = length(a),
    method = "Dot product (Burkov Ch 1)"
  )
}

#' @rdname morie_burkov_dot_product
#' @export
morie_burkov_vector_norm <- function(a) {
  a <- as.numeric(a)
  list(
    estimate = sqrt(sum(a^2)), squared = sum(a^2), n = length(a),
    method = "L2 norm (Burkov Ch 1)"
  )
}

#' @rdname morie_burkov_dot_product
#' @export
morie_burkov_unit_vector <- function(a) {
  a <- as.numeric(a)
  n <- sqrt(sum(a^2))
  if (n == 0) {
    stop("the zero vector has no direction and cannot be normalised.",
      call. = FALSE
    )
  }
  list(
    unit = a / n, estimate = (a / n)[1], norm = n, n = length(a),
    method = "Unit vector a/||a|| (Burkov Ch 1)"
  )
}

#' Term frequency and TF-IDF (Burkov Ch 2)
#' @param term Query term.
#' @param document Token vector.
#' @param normalise Divide by document length.
#' @export
morie_burkov_term_frequency <- function(term, document, normalise = FALSE) {
  doc <- as.character(document)
  if (length(doc) == 0L) stop("the document is empty.", call. = FALSE)
  cnt <- sum(doc == as.character(term))
  est <- if (isTRUE(normalise)) cnt / length(doc) else as.numeric(cnt)
  list(
    estimate = est, count = cnt, doc_length = length(doc),
    normalised = isTRUE(normalise), n = length(doc),
    method = "Term frequency (Burkov Ch 2)"
  )
}

#' @rdname morie_burkov_term_frequency
#' @param corpus List of token vectors.
#' @export
morie_burkov_tf_idf <- function(term, document, corpus) {
  t <- as.character(term)
  doc <- as.character(document)
  if (length(corpus) == 0L) stop("the corpus is empty.", call. = FALSE)
  docs <- lapply(corpus, as.character)
  tf <- sum(doc == t)
  df <- sum(vapply(docs, function(d) t %in% d, logical(1)))
  if (df == 0L) {
    stop(sprintf(paste(
      "term '%s' appears in no corpus document, so IDF",
      "is undefined; is the query document part of the",
      "corpus?"
    ), t), call. = FALSE)
  }
  idf <- log(length(docs) / df)
  list(
    estimate = tf * idf, tf = tf, df = df, idf = idf,
    n_documents = length(docs), n = length(doc),
    method = "TF-IDF (Burkov Ch 2)"
  )
}

#' Repetition penalty on decoder logits (Burkov Ch 5)
#'
#' Divide positive logits of seen tokens by the penalty, multiply
#' negative ones -- both move the token DOWN in odds against every
#' unpenalised token. Note the invariant is the ODDS: with several
#' tokens penalised at once, softmax renormalisation can raise one
#' penalised token's absolute probability when another falls further.
#'
#' @param logits Logit vector.
#' @param prev_tokens 0-based indices of
#'   already-generated tokens (matching the Python mirror).
#' @param penalty Positive penalty, usually above 1.
#' @export
morie_burkov_repetition_penalty <- function(logits, prev_tokens,
                                            penalty = 1.2) {
  z <- as.numeric(logits)
  r <- as.numeric(penalty)
  if (r <= 0) stop("penalty must be positive.", call. = FALSE)
  prev <- sort(unique(as.integer(prev_tokens)))
  for (t in prev) {
    if (t < 0L || t >= length(z)) {
      stop(sprintf(
        "token index %d is out of range for %d logits.", t,
        length(z)
      ), call. = FALSE)
    }
    i <- t + 1L
    z[i] <- if (z[i] > 0) z[i] / r else z[i] * r
  }
  list(
    penalised = z, estimate = z[1], penalty = r, tokens_hit = prev,
    n = length(z),
    method = "Repetition penalty on logits (Burkov Ch 5)"
  )
}

#' Weight tying: logits through the shared embedding (Burkov Ch 4)
#' @param h_last Hidden state.
#' @param E Embedding matrix, V x d.
#' @export
morie_burkov_weight_tying <- function(h_last, E) {
  h <- as.numeric(h_last)
  E <- as.matrix(E)
  if (ncol(E) != length(h)) {
    stop(sprintf(
      paste(
        "E is %d x %d but the hidden state has %d",
        "dimensions; weight tying needs E's columns to",
        "match the hidden width."
      ), nrow(E), ncol(E),
      length(h)
    ), call. = FALSE)
  }
  logits <- as.numeric(E %*% h)
  list(
    logits = logits, estimate = logits[1], vocab_size = nrow(E),
    hidden_size = ncol(E), n = length(h),
    method = "Weight tying logits = h E^T (Burkov Ch 4)"
  )
}

#' One step of the Elman RNN (Burkov Ch 3)
#' @param x_t Input.
#' @param h_prev Previous hidden state.
#' @param Wh,Wx,Wy Weight matrices.
#' @param bh,by Biases.
#' @export
morie_burkov_elman_rnn <- function(x_t, h_prev, Wh, Wx, Wy, bh, by) {
  x <- as.numeric(x_t)
  h0 <- as.numeric(h_prev)
  Wh <- as.matrix(Wh)
  Wx <- as.matrix(Wx)
  Wy <- as.matrix(Wy)
  bh <- as.numeric(bh)
  by <- as.numeric(by)
  if (!all(dim(Wh) == c(length(h0), length(h0)))) {
    stop(sprintf("Wh must be %d x %d.", length(h0), length(h0)),
      call. = FALSE
    )
  }
  if (!all(dim(Wx) == c(length(h0), length(x)))) {
    stop(sprintf("Wx must be %d x %d.", length(h0), length(x)),
      call. = FALSE
    )
  }
  if (ncol(Wy) != length(h0)) {
    stop(sprintf("Wy must have %d columns.", length(h0)), call. = FALSE)
  }
  if (length(bh) != length(h0) || length(by) != nrow(Wy)) {
    stop("bias lengths must match Wh rows and Wy rows.", call. = FALSE)
  }
  h <- tanh(as.numeric(Wh %*% h0 + Wx %*% x + bh))
  y <- as.numeric(Wy %*% h + by)
  list(
    h = h, y = y, estimate = y[1], n = length(h),
    method = "Elman RNN step (Burkov Ch 3)"
  )
}

#' Computational-graph forward and reverse pass (Burkov Ch 1)
#'
#' `graph` is a topologically ordered list of nodes
#' `list(name =, op =, args = c(...))`; `inputs` a named list of leaf
#' values. Gradients of the last node with respect to every leaf come
#' back exactly; the parity tests pin them against Python's autodiff
#' and against central finite differences.
#'
#' @param graph Node list.
#' @param inputs Named leaf values.
#' @export
morie_burkov_computational_graph <- function(graph, inputs) {
  if (length(graph) == 0L) stop("the graph is empty.", call. = FALSE)
  sig <- function(a) 1 / (1 + exp(-a))
  fwd <- list(
    add = function(a, b) a + b, sub = function(a, b) a - b,
    mul = function(a, b) a * b, tanh = function(a) tanh(a),
    sigmoid = sig, relu = function(a) max(a, 0),
    square = function(a) a * a, log = function(a) log(a),
    exp = function(a) exp(a)
  )
  bwd <- list(
    add = function(a, b, g) c(g, g),
    sub = function(a, b, g) c(g, -g),
    mul = function(a, b, g) c(g * b, g * a),
    tanh = function(a, g) g * (1 - tanh(a)^2),
    sigmoid = function(a, g) g * sig(a) * (1 - sig(a)),
    relu = function(a, g) if (a > 0) g else 0,
    square = function(a, g) 2 * a * g,
    log = function(a, g) g / a,
    exp = function(a, g) g * exp(a)
  )
  values <- as.list(inputs)
  for (node in graph) {
    if (is.null(fwd[[node$op]])) {
      stop(
        sprintf(
          "unknown op '%s'; supported: %s.", node$op,
          paste(sort(names(fwd)), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    missing <- setdiff(node$args, names(values))
    if (length(missing)) {
      stop(
        sprintf(
          paste(
            "node '%s' needs %s before they are computed;",
            "the graph must be topologically ordered."
          ),
          node$name, paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    values[[node$name]] <- do.call(
      fwd[[node$op]],
      lapply(node$args, function(a) values[[a]])
    )
  }
  out_name <- graph[[length(graph)]]$name
  grads <- setNames(as.list(rep(0, length(values))), names(values))
  grads[[out_name]] <- 1
  for (node in rev(graph)) {
    g <- grads[[node$name]]
    argvals <- lapply(node$args, function(a) values[[a]])
    local <- do.call(bwd[[node$op]], c(argvals, list(g)))
    for (i in seq_along(node$args)) {
      a <- node$args[i]
      grads[[a]] <- grads[[a]] + local[i]
    }
  }
  list(
    output = values[[out_name]], estimate = values[[out_name]],
    gradients = grads[names(inputs)], values = values,
    n = length(graph),
    method = "Computational-graph autodiff (Burkov Ch 1)"
  )
}
