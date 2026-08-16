# morie.fn -- function file (rootcoder007/morie)
# Hierarchical attention for document classification.
# 
# A document has structure -- words make sentences, sentences make
# documents -- and the model mirrors it: a word-level encoder with
# attention produces a sentence vector, and a sentence-level encoder with
# attention produces the document vector. Two levels of encoding, two
# levels of attention.
# 
# References
# ----------
# Yang, Z., Yang, D., Dyer, C., He, X., Smola, A. & Hovy, E. (2016)
# "Hierarchical Attention Networks for Document Classification",
# NAACL-HLT 2016, 1480-1489, doi:10.18653/v1/N16-1174.
# 
# Bahdanau, D., Cho, K. & Bengio, Y. (2015) "Neural Machine Translation
# by Jointly Learning to Align and Translate", ICLR 2015,
# arXiv:1409.0473.
# 
# Sukhbaatar, S., Szlam, A., Weston, J. & Fergus, R. (2015)
# "End-To-End Memory Networks", NIPS 2015, 2440-2448, arXiv:1503.08895.

.pratt_EPS <- 1e-12

#' .pratt_mat
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param H See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.pratt_mat <- function(H) {
  if (is.list(H) && !is.matrix(H)) {
    if (length(H) == 0L) {
      return(matrix(numeric(0), nrow = 0L, ncol = 0L))
    }
    rows <- lapply(H, function(r) as.numeric(r))
    do.call(rbind, rows)
  } else {
    as.matrix(H)
  }
}

#' .pratt_vec
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.pratt_vec <- function(v) {
  as.numeric(v)
}

#' .pratt_attention
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param H See Usage.
#' @param W See Usage.
#' @param b See Usage.
#' @param u_context See Usage.
#' @return A numeric value.
#' @export
.pratt_attention <- function(H, W, b, u_context) {
  rows <- .pratt_mat(H)
  if (nrow(rows) == 0L) {
    stop("pratt: nothing to attend over")
  }
  uc <- .pratt_vec(u_context)
  W <- as.matrix(W)
  b <- .pratt_vec(b)
  U <- tanh(rows %*% t(W) +
            matrix(b, nrow = nrow(rows), ncol = length(b), byrow = TRUE))
  if (ncol(U) != length(uc)) {
    stop(sprintf("pratt: the context vector is %d-dimensional but the projection is %d",
                 length(uc), ncol(U)))
  }
  sc <- as.numeric(U %*% uc)
  m <- max(sc)
  e <- exp(sc - m)
  z <- sum(e)
  e / z
}

#' .pratt_sentence_vector
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param H_words See Usage.
#' @param W See Usage.
#' @param b See Usage.
#' @param u_w See Usage.
#' @return A list with \code{vector}, \code{alpha}.
#' @export
.pratt_sentence_vector <- function(H_words, W, b, u_w) {
  a <- .pratt_attention(H_words, W, b, u_w)
  rows <- .pratt_mat(H_words)
  list(
    vector = as.numeric(a %*% rows),
    alpha = a
  )
}

#' .pratt_document_vector
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param H_sentences See Usage.
#' @param W See Usage.
#' @param b See Usage.
#' @param u_s See Usage.
#' @return A list with \code{vector}, \code{alpha}.
#' @export
.pratt_document_vector <- function(H_sentences, W, b, u_s) {
  a <- .pratt_attention(H_sentences, W, b, u_s)
  rows <- .pratt_mat(H_sentences)
  list(
    vector = as.numeric(a %*% rows),
    alpha = a
  )
}

#' morie_pratt
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param word_states See Usage.
#' @param Ww See Usage.
#' @param bw See Usage.
#' @param u_w See Usage.
#' @param Ws See Usage.
#' @param bs See Usage.
#' @param u_s See Usage.
#' @param Wc See Usage.
#' @param bc See Usage.
#' @return A list with \code{estimate}, \code{probabilities}, \code{document_vector}, \code{sentence_attention}, \code{word_attention}, \code{n_sentences}, \code{method}, \code{note}.
#' @export
morie_pratt <- function(word_states, Ww, bw, u_w, Ws, bs, u_s, Wc, bc) {
  S <- list()
  wa <- list()
  for (H in word_states) {
    r <- .pratt_sentence_vector(H, Ww, bw, u_w)
    S[[length(S) + 1L]] <- r$vector
    wa[[length(wa) + 1L]] <- r$alpha
  }
  dv <- .pratt_document_vector(S, Ws, bs, u_s)
  Wc <- as.matrix(Wc)
  bc <- .pratt_vec(bc)
  z <- as.numeric(Wc %*% dv$vector) + bc
  m <- max(z)
  e <- exp(z - m)
  tot <- sum(e)
  probs <- e / tot
  list(
    estimate = probs,
    probabilities = probs,
    document_vector = dv$vector,
    sentence_attention = dv$alpha,
    word_attention = wa,
    n_sentences = length(S),
    method = "hierarchical attention network; Yang et al. (2016)",
    note = paste("both context vectors are randomly initialised and",
                 "learned -- the model discovers what 'informative' means")
  )
}

hierarchicalattention <- morie_pratt
pretrained_attention <- morie_pratt

morie_pratt_attention <- .pratt_attention
morie_pratt_sentence_vector <- .pratt_sentence_vector
morie_pratt_document_vector <- .pratt_document_vector
morie_pratt_classify <- morie_pratt

#' morie_pratt_attention_entropy
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @param alpha See Usage.
#' @return A list with \code{entropy}, \code{max_entropy}, \code{concentration}.
#' @export
morie_pratt_attention_entropy <- function(alpha) {
  a <- .pratt_vec(alpha)
  s <- sum(a)
  if (s <= .pratt_EPS) {
    stop("pratt: the attention weights have no mass")
  }
  a <- a / s
  h <- -sum(a * log(pmax(a, .pratt_EPS)))
  list(
    entropy = h,
    max_entropy = log(length(a)),
    concentration = if (length(a) > 1L) 1.0 - h / log(length(a)) else 1.0
  )
}

#' morie_pratt_cheatsheet
#'
#' Part of the pratt_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_pratt_cheatsheet <- function() {
  paste("pratt: mirror the document's own structure -- words to sentences",
        "to document -- with attention at BOTH levels, because which word",
        "matters within a sentence and which sentence matters within a",
        "document are different judgements. At each level: u = tanh(W h + b),",
        "then softmax of u'u_context, then a weighted sum. The CONTEXT VECTOR",
        "is a learned fixed query ('what is the informative word'), randomly",
        "initialised, not supplied. The alphas are a distribution over",
        "positions, so they read directly as the explanation.")
}
