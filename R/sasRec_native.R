# SASRec: self-attentive sequential recommendation.
# Sources: Kang, W.-C. & McAuley, J. (2018) "Self-Attentive
# Sequential Recommendation", *Proceedings of the 2018 IEEE
# International Conference on Data Mining (ICDM 2018)*, 197-206,
# doi:10.1109/ICDM.2018.00035, arXiv:1808.09781. The abstract: Markov
# chains assume the next action is predictable from the last few,
# while RNNs allow longer-term semantics; MC-based methods perform
# best in extremely sparse datasets where parsimony is critical, RNNs
# in denser datasets where complexity is affordable; SASRec balances
# these by capturing long-term semantics like an RNN while making
# predictions from relatively few actions like an MC, identifying at
# each step which items are relevant; outperforming MC/CNN/RNN
# baselines on both sparse and dense datasets; being an order of
# magnitude more efficient; and attention-weight visualisations
# showing adaptive handling of datasets of various density. Vaswani,
# A., Shazeer, N., Parmar, N., Uszkoreit, J., Jones, L., Gomez, A. N.,
# Kaiser, L. & Polosukhin, I. (2017) "Attention Is All You Need",
# *NIPS 2017*, 5998-6008, arXiv:1706.03762. Hidasi, B., Karatzoglou,
# A., Baltrunas, L. & Tikk, D. (2016) "Session-based Recommendations
# with Recurrent Neural Networks", *ICLR 2016*, arXiv:1511.06939.
# The RNN baseline; implemented in gru4r.

.SASREC_EPS <- 1e-12

#' causal_mask
#'
#' A step of the sasRec_native implementation. Called by \code{self_attention}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{mask}, as built in the body.
#' @export
causal_mask <- function(n) {
  m <- as.integer(n)
  if (m < 1L) stop("sasRec: the sequence must be non-empty")
  mask <- matrix(0, nrow = m, ncol = m)
  for (i in 1:m) for (j in 1:m) mask[i, j] <- if (j <= i) 1.0 else 0.0
  mask
}

#' self_attention
#'
#' A step of the sasRec_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param E See Usage.
#' @param WQ See Usage.
#' @param WK See Usage.
#' @param WV See Usage.
#' @param mask Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{output}, \code{weights}, \code{note}.
#' @export
self_attention <- function(E, WQ, WK, WV, mask = NULL) {
  X <- E
  if (is.list(X) && !is.matrix(X)) X <- do.call(rbind, X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
  M <- if (is.null(mask)) causal_mask(n) else mask
  WQm <- WQ
  if (is.list(WQm) && !is.matrix(WQm)) WQm <- do.call(rbind, WQm)
  WKm <- WK
  if (is.list(WKm) && !is.matrix(WKm)) WKm <- do.call(rbind, WKm)
  WVm <- WV
  if (is.list(WVm) && !is.matrix(WVm)) WVm <- do.call(rbind, WVm)
  storage.mode(WQm) <- "double"
  storage.mode(WKm) <- "double"
  storage.mode(WVm) <- "double"
  dk <- nrow(WQm)
  Q <- X %*% t(WQm)
  K <- X %*% t(WKm)
  V <- X %*% t(WVm)
  sc <- Q %*% t(K) / sqrt(dk)
  sc[M == 0] <- -1e30
  mx <- apply(sc, 1, max)
  e <- exp(sc - mx)
  e[M == 0] <- 0
  z <- rowSums(e)
  z[z == 0] <- 1
  w <- e / z
  out <- w %*% V
  list(output = out, weights = w,
       note = "the mask is a correctness condition, not an optimisation")
}

#' attention_span
#'
#' A step of the sasRec_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param weights See Usage.
#' @param position Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return A list with \code{mean_lookback}, \code{mass_on_last}, \code{effective_order},
#' \code{note}.
#' @export
attention_span <- function(weights, position = NULL) {
  W <- weights
  if (is.list(W) && !is.matrix(W)) W <- do.call(rbind, W)
  storage.mode(W) <- "double"
  i <- if (is.null(position)) nrow(W) else as.integer(position) + 1L
  row <- W[i, ]
  tot <- sum(row[1:i])
  if (tot <= .SASREC_EPS) stop("sasRec: the attention row has no mass")
  idx <- 1:i
  span <- sum((i - idx) * row[1:i]) / tot
  list(mean_lookback = span, mass_on_last = row[i] / tot,
       effective_order = span + 1.0,
       note = "a short span IS Markov behaviour; a long one is RNN behaviour, chosen per sequence")
}

#' predict_next
#'
#' A step of the sasRec_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state Coerced to numeric by the body, with \code{as.numeric}.
#' @param item_embeddings See Usage.
#' @param top_k Numeric; passed to \code{min}. Defaults to \code{5}.
#' @param exclude Passed to \code{unlist}. Defaults to \code{numeric(0)}.
#' @return A list with \code{estimate}, \code{ranking}, \code{n_scored}, \code{method}.
#' @export
predict_next <- function(state, item_embeddings, top_k = 5, exclude = numeric(0)) {
  s <- as.numeric(state)
  E <- item_embeddings
  if (is.list(E) && !is.matrix(E)) E <- do.call(rbind, E)
  storage.mode(E) <- "double"
  ex <- as.integer(unlist(exclude))
  sc <- numeric(nrow(E))
  for (i in seq_len(nrow(E))) sc[i] <- sum(s * E[i, ])
  if (length(ex) > 0) sc[ex + 1L] <- NA
  ord <- order(-sc, na.last = TRUE)
  keep <- ord[!is.na(sc[ord])][1:min(top_k, length(ord))]
  rk <- cbind(keep, sc[keep])
  colnames(rk) <- c("index", "score")
  list(estimate = rk, ranking = rk, n_scored = sum(!is.na(sc)),
       method = "self-attentive sequential recommendation; Kang & McAuley (2018)")
}

#' complexity
#'
#' A step of the sasRec_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param d Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{attention_ops}, \code{rnn_ops},
#' \code{attention_sequential_steps}, \code{rnn_sequential_steps}, \code{note}.
#' @export
complexity <- function(n, d) {
  nn <- as.integer(n)
  dd <- as.integer(d)
  if (nn < 1 || dd < 1) stop("sasRec: n and d must be positive")
  list(attention_ops = nn * nn * dd, rnn_ops = nn * dd * dd,
       attention_sequential_steps = 1, rnn_sequential_steps = nn,
       note = "the parallelism, not the operation count, is where the order-of-magnitude speed-up comes from")
}

#' .sasRec_cheatsheet
#'
#' A step of the sasRec_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .sasRec_cheatsheet()
#' res
.sasRec_cheatsheet <- function() {
  paste("sasRec: Markov chains win where data are SPARSE (parsimony ",
        "is critical), RNNs where they are DENSE (complexity is ",
        "affordable) -- and the choice is normally made once for a ",
        "whole dataset. Self-attention picks per sequence: it can ",
        "reach far back like an RNN while predicting from FEW actions ",
        "like an MC, and the attention weights show it adapting to ",
        "density. Causal masking is a CORRECTNESS condition -- ",
        "attending forward leaks the target. O(n^2 d) but fully ",
        "parallel against an RNN's inherently sequential O(n d^2).",
        sep = "")
}

selfattentivesequential <- self_attention
sasrec <- self_attention

morie_sasRec <- self_attention
