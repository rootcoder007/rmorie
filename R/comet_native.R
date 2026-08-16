# COMET: MT evaluation trained against human judgement.
# Sources: Rei, R., Stewart, C., Farinha, A. C. & Lavie, A. (2020)
# "COMET: A Neural Framework for MT Evaluation", *Proceedings of the
# 2020 Conference on Empirical Methods in Natural Language Processing
# (EMNLP 2020)*, 2685-2702, doi:10.18653/v1/2020.emnlp-main.213,
# arXiv:2009.09025, for the source-aware architecture, the estimator
# and triplet-margin heads, the (u, v, |u-v|) feature construction
# and segment-level Kendall's tau evaluation. Papineni, K., Roukos,
# S., Ward, T. & Zhu, W.-J. (2002) "BLEU", *ACL 2002*, 311-318,
# doi:10.3115/1073083.1073135, the surface metric being replaced.
# Reimers, N. & Gurevych, I. (2019) "Sentence-BERT", *EMNLP-IJCNLP
# 2019*, 3980-3990, doi:10.18653/v1/D19-1410, the (u, v, |u-v|)
# feature construction.

# Native implementation mirroring Python morie.fn.comet exactly: the
# same (u, v, |u-v|) pooling against both source and reference, the
# same estimator head, the same triplet margin against both anchors,
# the same segment-level Kendall's tau, and the same reference-free
# variant that works only because the source is in the input.

.comet_EPS <- 1e-12

#' .comet_vec
#'
#' A step of the comet_native implementation. Called by \code{.comet_dist}, \code{kendall_tau}, \code{pooled_features} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
.comet_vec <- function(x) as.numeric(x)

#' pooled_features
#'
#' A step of the comet_native implementation. Called by \code{estimator_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hyp Passed to \code{.comet_vec}.
#' @param src Passed to \code{.comet_vec}.
#' @param ref Passed to \code{.comet_vec}.
#' @return A list with \code{features}, \code{dim}, \code{hyp_ref_diff}, \code{hyp_src_diff}, \code{note}.
#' @export
pooled_features <- function(hyp, src, ref) {
  h <- .comet_vec(hyp); s <- .comet_vec(src); r <- .comet_vec(ref)
  if (!(length(h) == length(s) && length(s) == length(r)))
    stop(sprintf("comet: the three embeddings differ in length (%d, %d, %d)",
                 length(h), length(s), length(r)))
  d <- length(h)
  hs <- h * s; hr <- h * r
  ds <- abs(h - s); dr <- abs(h - r)
  list(features = c(h, r, hr, dr, hs, ds), dim = 6L * d,
       hyp_ref_diff = dr, hyp_src_diff = ds,
       note = paste("the SOURCE enters too, which is what separates a",
                    "mistranslation from a differently-worded correct",
                    "translation"))
}

#' estimator_score
#'
#' A step of the comet_native implementation. Called by \code{morie_comet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hyp See Usage.
#' @param src See Usage.
#' @param ref See Usage.
#' @param W A matrix; passed to \code{nrow}.
#' @param b Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{estimate}, \code{score}, \code{method}, \code{note}.
#' @export
estimator_score <- function(hyp, src, ref, W, b = NULL) {
  W <- as.matrix(W); storage.mode(W) <- "double"
  f <- pooled_features(hyp, src, ref)$features
  if (ncol(W) != length(f))
    stop(sprintf("comet: the head expects %d features but got %d",
                 ncol(W), length(f)))
  bb <- if (is.null(b)) rep(0, nrow(W)) else as.numeric(b)
  z <- as.numeric(bb + W %*% f)
  list(estimate = if (length(z) == 1L) z[1L] else z,
       score = if (length(z) == 1L) z[1L] else z,
       method = "COMET estimator; Rei, Stewart, Farinha & Lavie (2020)",
       note = "trained against HUMAN judgements, not n-gram overlap")
}

#' .comet_dist
#'
#' A step of the comet_native implementation. Called by \code{triplet_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{.comet_vec}.
#' @param b Passed to \code{.comet_vec}.
#' @return A numeric value.
#' @export
.comet_dist <- function(a, b) {
  x <- .comet_vec(a); y <- .comet_vec(b)
  if (length(x) != length(y))
    stop("comet: embeddings differ in length")
  sqrt(sum((x - y)^2))
}

#' triplet_loss
#'
#' A step of the comet_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param better Passed to \code{.comet_dist}.
#' @param worse Passed to \code{.comet_dist}.
#' @param src Passed to \code{.comet_dist}.
#' @param ref Passed to \code{.comet_dist}.
#' @param margin Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{loss}, \code{source_term}, \code{reference_term}, \code{satisfied}, \code{note}.
#' @export
triplet_loss <- function(better, worse, src, ref, margin = 1.0) {
  m <- as.numeric(margin)
  if (m <= 0) stop("comet: the margin must be positive")
  ls <- max(0, .comet_dist(better, src) - .comet_dist(worse, src) + m)
  lr <- max(0, .comet_dist(better, ref) - .comet_dist(worse, ref) + m)
  list(loss = ls + lr, source_term = ls, reference_term = lr,
       satisfied = (ls + lr) == 0,
       note = paste("zero loss means the better hypothesis is already",
                    "closer to BOTH anchors by the margin"))
}

#' kendall_tau
#'
#' A step of the comet_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param scores Passed to \code{.comet_vec}.
#' @param human Passed to \code{.comet_vec}.
#' @return A list with \code{tau}, \code{concordant}, \code{discordant}, \code{n_segments}.
#' @export
kendall_tau <- function(scores, human) {
  a <- .comet_vec(scores); b <- .comet_vec(human)
  if (length(a) != length(b))
    stop(sprintf("comet: %d scores but %d human judgements",
                 length(a), length(b)))
  n <- length(a)
  if (n < 2L) stop("comet: at least 2 segments are needed")
  conc <- 0; disc <- 0
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      da <- a[i] - a[j]; db <- b[i] - b[j]
      if (da == 0 || db == 0) next
      if ((da > 0) == (db > 0)) conc <- conc + 1L
      else disc <- disc + 1L
    }
  }
  tot <- conc + disc
  list(tau = if (tot) (conc - disc) / tot else 0,
       concordant = conc, discordant = disc, n_segments = n)
}

#' reference_free
#'
#' A step of the comet_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hyp Passed to \code{.comet_vec}.
#' @param src Passed to \code{.comet_vec}.
#' @param W A matrix; passed to \code{nrow}.
#' @param b Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{score}, \code{reference_used}, \code{note}.
#' @export
reference_free <- function(hyp, src, W, b = NULL) {
  h <- .comet_vec(hyp); s <- .comet_vec(src)
  if (length(h) != length(s))
    stop("comet: the embeddings differ in length")
  d <- length(h)
  f <- c(h, s, h * s, abs(h - s))
  W <- as.matrix(W); storage.mode(W) <- "double"
  if (ncol(W) != length(f))
    stop(sprintf("comet: the reference-free head expects %d features but got %d",
                 ncol(W), length(f)))
  bb <- if (is.null(b)) rep(0, nrow(W)) else as.numeric(b)
  z <- as.numeric(bb + W %*% f)
  list(score = if (length(z) == 1L) z[1L] else z,
       reference_used = FALSE,
       note = "only possible because the source was always part of the model")
}

cometmetric <- estimator_score
comet <- estimator_score

#' morie_comet
#'
#' A step of the comet_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hyp See Usage.
#' @param src See Usage.
#' @param ref See Usage.
#' @param W See Usage.
#' @param b Defaults to \code{NULL}.
#' @return The value of \code{estimator_score}.
#' @export
morie_comet <- function(hyp, src, ref, W, b = NULL) {
  estimator_score(hyp, src, ref, W, b = b)
}

#' .comet_cheatsheet
#'
#' A step of the comet_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.comet_cheatsheet <- function() {
  paste("comet: replace n-gram overlap with a LEARNED metric",
        "trained on human judgements, embedding hypothesis,",
        "reference AND SOURCE. The source is the structural",
        "difference: without it you cannot separate a",
        "MISTRANSLATION from a correct translation worded",
        "differently from the reference -- and it is what makes",
        "a reference-free variant possible. Two heads: an",
        "ESTIMATOR regressing absolute scores, and a RANKING",
        "model with a triplet margin, because relative judgements",
        "are far cheaper to collect. Report SEGMENT-level Kendall",
        "tau.")
}
