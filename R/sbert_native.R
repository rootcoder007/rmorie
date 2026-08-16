# Sentence-BERT: sentence embeddings that can actually be compared.
# Sources: Reimers, N. & Gurevych, I. (2019) "Sentence-BERT: Sentence
# Embeddings using Siamese BERT-Networks", *Proceedings of the 2019
# Conference on Empirical Methods in Natural Language Processing and
# the 9th International Joint Conference on Natural Language
# Processing (EMNLP-IJCNLP)*, 3980-3990, doi:10.18653/v1/D19-1410,
# arXiv:1908.10084. Sec. 2 (BERT's sentence-pair regression setup
# with [SEP] and its cost; the absence of independent sentence
# embeddings; the averaging and [CLS] workarounds and the observation
# that they were unevaluated) and the architecture figures giving the
# softmax objective over (u, v, |u-v|) and the cosine-similarity
# objective. Conneau, A., Kiela, D., Schwenk, H., Barrault, L. &
# Bordes, A. (2017) "Supervised Learning of Universal Sentence
# Representations from Natural Language Inference Data", *EMNLP
# 2017*, 670-680, arXiv:1705.02364. InferSent: the siamese BiLSTM
# with max pooling trained on NLI that this follows. Devlin, J.,
# Chang, M.-W., Lee, K. & Toutanova, K. (2019) "BERT: Pre-training of
# Deep Bidirectional Transformers for Language Understanding",
# *NAACL-HLT 2019*, 4171-4186, arXiv:1810.04805.

.SBERT_EPS <- 1e-12
.SBERT_POOLING <- c("mean", "cls", "max")

#' .sbert_mat
#'
#' A step of the sbert_native implementation. Called by \code{pool}, \code{rank_by_similarity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; the body checks with \code{is.matrix}.
#' @return Nothing; this branch always raises.
#' @export
.sbert_mat <- function(x) {
  if (is.list(x) && !is.matrix(x)) return(do.call(rbind, x))
  if (is.matrix(x)) { storage.mode(x) <- "double"; return(x) }
  if (is.vector(x)) return(matrix(as.numeric(x), nrow = 1))
  stop("sbert: expected a matrix or list of rows")
}

#' .sbert_vec
#'
#' A step of the sbert_native implementation. Called by \code{classification_features}, \code{cosine_similarity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A list; the body checks with \code{is.list}.
#' @return A vector, from \code{as.numeric}.
#' @export
.sbert_vec <- function(x) {
  if (is.list(x)) return(as.numeric(unlist(x)))
  as.numeric(x)
}

#' pool
#'
#' A step of the sbert_native implementation. Called by \code{morie_geron_lenet5}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param token_vectors Passed to \code{.sbert_mat}.
#' @param mode One of \code{"cls"}, \code{"max"}. Defaults to \code{"mean"}.
#' @param mask The body requires: sbert: the mask excludes every token.
#' @return The value of \code{out}, as built in the body.
#' @export
pool <- function(token_vectors, mode = "mean", mask = NULL) {
  if (!(mode %in% .SBERT_POOLING))
    stop(sprintf("sbert: pooling must be one of %s, got '%s'",
                 paste(.SBERT_POOLING, collapse = ", "), mode))
  T <- .sbert_mat(token_vectors)
  if (nrow(T) == 0) stop("sbert: no token vectors given")
  d <- ncol(T)
  m <- if (is.null(mask)) rep(TRUE, nrow(T)) else as.logical(unlist(mask))
  if (length(m) != nrow(T))
    stop(sprintf("sbert: %d mask entries for %d tokens", length(m), nrow(T)))
  keep <- which(m)
  if (length(keep) == 0) stop("sbert: the mask excludes every token")
  if (mode == "cls") return(as.numeric(T[keep[1], ]))
  if (mode == "max") {
    out <- numeric(d)
    for (j in 1:d) out[j] <- max(T[keep, j])
    return(out)
  }
  out <- numeric(d)
  for (j in 1:d) out[j] <- mean(T[keep, j])
  out
}

#' cosine_similarity
#'
#' A step of the sbert_native implementation. Called by \code{rank_by_similarity}, \code{sts_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u Passed to \code{.sbert_vec}.
#' @param v Passed to \code{.sbert_vec}.
#' @return A numeric value.
#' @export
cosine_similarity <- function(u, v) {
  a <- .sbert_vec(u); b <- .sbert_vec(v)
  if (length(a) != length(b))
    stop(sprintf("sbert: vectors differ in length (%d, %d)", length(a), length(b)))
  na <- sqrt(sum(a * a)); nb <- sqrt(sum(b * b))
  if (na <= .SBERT_EPS || nb <= .SBERT_EPS)
    stop("sbert: cosine similarity is undefined for a zero vector")
  sum(a * b) / (na * nb)
}

#' classification_features
#'
#' A step of the sbert_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u Passed to \code{.sbert_vec}.
#' @param v Passed to \code{.sbert_vec}.
#' @return A list with \code{features}, \code{u}, \code{v}, \code{abs_diff}, \code{dim}, \code{note}.
#' @export
classification_features <- function(u, v) {
  a <- .sbert_vec(u); b <- .sbert_vec(v)
  if (length(a) != length(b))
    stop(sprintf("sbert: vectors differ in length (%d, %d)", length(a), length(b)))
  diff <- abs(a - b)
  list(features = c(a, b, diff), u = a, v = b, abs_diff = diff,
       dim = 3 * length(a),
       note = "|u - v| is the term neither u nor v supplies")
}

#' pair_cost
#'
#' A step of the sbert_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param mode One of \code{"bi-encoder"}, \code{"cross-encoder"}. Defaults to \code{"cross-encoder"}.
#' @return A list with \code{forward_passes}, \code{cross_encoder}, \code{bi_encoder}, \code{speedup}, \code{n}, \code{note}.
#' @export
pair_cost <- function(n, mode = "cross-encoder") {
  N <- as.integer(n)
  if (N < 2L) stop("sbert: need at least 2 sentences")
  if (!(mode %in% c("cross-encoder", "bi-encoder")))
    stop(sprintf("sbert: mode must be cross-encoder or bi-encoder, got '%s'", mode))
  cross <- N * (N - 1L) / 2L
  list(forward_passes = if (mode == "cross-encoder") cross else N,
       cross_encoder = cross, bi_encoder = N,
       speedup = cross / N, n = N,
       note = "the bi-encoder also does O(n^2) dot products, but those are arithmetic, not network passes")
}

#' rank_by_similarity
#'
#' A step of the sbert_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query See Usage.
#' @param corpus_embeddings Passed to \code{.sbert_mat}.
#' @param top_k Numeric; passed to \code{min}. Defaults to \code{5}.
#' @return A list with \code{ranking}, \code{n_corpus}, \code{forward_passes}, \code{note}.
#' @export
rank_by_similarity <- function(query, corpus_embeddings, top_k = 5) {
  E <- .sbert_mat(corpus_embeddings)
  if (nrow(E) == 0) stop("sbert: the corpus is empty")
  scores <- numeric(nrow(E))
  for (i in seq_len(nrow(E))) scores[i] <- cosine_similarity(query, E[i, ])
  ord <- order(-scores)
  keep <- ord[1:min(top_k, length(ord))]
  rk <- cbind(keep, scores[keep])
  colnames(rk) <- c("index", "score")
  list(ranking = rk, n_corpus = nrow(E), forward_passes = 0,
       note = "no network passes at query time -- the corpus was embedded once")
}

#' sts_score
#'
#' A step of the sbert_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pairs A vector; its length is taken and its elements indexed.
#' @param embed Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{estimate}, \code{scores}, \code{embed_calls}, \code{n_pairs}, \code{cross_encoder_calls}, \code{method}.
#' @export
sts_score <- function(pairs, embed) {
  cache <- new.env(hash = TRUE)
  out <- numeric(length(pairs))
  calls <- 0L
  for (i in seq_along(pairs)) {
    pr <- pairs[[i]]
    a <- pr[1]; b <- pr[2]
    if (!exists(a, envir = cache, inherits = FALSE)) {
      assign(a, as.numeric(embed(a)), envir = cache); calls <- calls + 1L
    }
    if (!exists(b, envir = cache, inherits = FALSE)) {
      assign(b, as.numeric(embed(b)), envir = cache); calls <- calls + 1L
    }
    out[i] <- cosine_similarity(get(a, envir = cache), get(b, envir = cache))
  }
  list(estimate = out, scores = out, embed_calls = calls,
       n_pairs = length(pairs), cross_encoder_calls = length(pairs),
       method = "siamese bi-encoder scored by cosine; Reimers & Gurevych (2019)")
}

#' .sbert_cheatsheet
#'
#' A step of the sbert_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.sbert_cheatsheet <- function() {
  paste("sbert: BERT scores a PAIR, so comparing n sentences needs ",
        "C(n,2) forward passes -- 10k sentences is ~50M. A SIAMESE ",
        "network embeds each sentence ONCE with shared weights, so it ",
        "is n passes plus dot products. Classification objective: ",
        "softmax over (u, v, |u-v|) -- the difference term is what ",
        "locates the disagreement. Regression objective: cosine ",
        "directly, and only that one trains the cosine geometry. ",
        "Pooling (mean/CLS/max) is a real choice, all three ablated.",
        sep = "")
}

sentencebert <- sts_score
sbert <- sts_score

morie_sbert <- sts_score
