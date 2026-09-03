# morie.fn -- function file (rootcoder007/morie)
# Informer's ProbSparse attention -- duplicate ledger entry.
#
# The wave-3 ledger carries this method twice, as infmer and as
# informer, both citing Zhou et al. (2021) and both describing the
# same sparse-attention forecaster. They are one paper and one method.
#
# Rather than maintain two copies that could drift apart, this module
# re-exports :mod:`informer`. Everything -- the query sparsity
# measurement, the top-:math:`u` selection with :math:`u = c\ln L_Q`,
# Lemma 1's max-mean approximation, and the complexity accounting --
# lives there and is documented there.
#
# References
# ----------
# Zhou, H., Zhang, S., Peng, J., Zhang, S., Li, J., Xiong, H. & Zhang,
# W. (2021) "Informer: Beyond Efficient Transformer for Long Sequence
# Time-Series Forecasting", Proceedings of the AAAI Conference on
# Artificial Intelligence 35(12), 11106-11115, arXiv:2012.07436.
#
# See Also
# --------
# :mod:`morie.fn.informer` -- the implementation.

# Private helpers (prefixed .infmer_ to share one R/ namespace safely)

#' .infmer_kl_from_uniform
#'
#' A step of the infmer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; its length is taken.
#' @return The value of \code{kl}, as built in the body.
#' @export
#' @examples
#' res <- .infmer_kl_from_uniform(p = 0.5)
#' res
.infmer_kl_from_uniform <- function(p) {
  n <- length(p)
  if (n == 0L) return(0)
  p <- pmax(p, 1e-12)
  p_sum <- sum(p)
  if (p_sum > 0) p <- p / p_sum
  uniform <- 1 / n
  kl <- sum(p * (log(p) - log(uniform)))
  return(kl)
}

#' .infmer_sparsity_measure
#'
#' A step of the infmer_native implementation. Called by \code{.infmer_select_queries},
#' \code{morie_infmer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param q A matrix; passed to \code{ncol}.
#' @param k A matrix; passed to \code{ncol}.
#' @return The value of \code{m_scores}, as built in the body.
#' @export
#' @examples
#' res <- .infmer_sparsity_measure(q = 0.5, k = 3L)
#' res
.infmer_sparsity_measure <- function(q, k) {
  if (is.null(dim(q))) q <- matrix(q, nrow = 1L)
  if (is.null(dim(k))) k <- matrix(k, nrow = 1L)
  d <- ncol(q)
  if (ncol(k) != d) stop("q and k must share the feature dimension")
  scores <- q %*% t(k) / sqrt(d)
  # Lemma 1 max-mean approximation: M(q_i,K) = max_j - mean_j
  m_scores <- apply(scores, 1L, max) - rowMeans(scores)
  return(m_scores)
}

#' .infmer_select_queries
#'
#' A step of the infmer_native implementation. Called by \code{morie_infmer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param q A matrix; passed to \code{nrow}.
#' @param k Passed to \code{.infmer_sparsity_measure}.
#' @param c Numeric; combined arithmetically in the body. Defaults to \code{5}.
#' @return A vector, from \code{sort}.
#' @export
#' @examples
#' res <- .infmer_select_queries(q = 0.5, k = 3L)
#' res
.infmer_select_queries <- function(q, k, c = 5) {
  if (is.null(dim(q))) q <- matrix(q, nrow = 1L)
  L_Q <- nrow(q)
  if (L_Q == 0L) return(integer(0))
  u <- as.integer(max(1L, floor(c * log(L_Q))))
  u <- min(u, L_Q)
  m <- .infmer_sparsity_measure(q, k)
  top_idx <- order(m, decreasing = TRUE)[seq_len(u)]
  return(sort(top_idx))
}

#' .infmer_full_attention
#'
#' A step of the infmer_native implementation. Called by \code{morie_infmer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param q A matrix; passed to \code{ncol}.
#' @param k A matrix; passed to \code{dim}.
#' @param v A matrix; passed to \code{dim}.
#' @return A list with \code{output}, \code{attention}, \code{scores}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .infmer_full_attention(q = 0.5, k = 3L, v = x)
#' res
.infmer_full_attention <- function(q, k, v) {
  if (is.null(dim(q))) q <- matrix(q, nrow = 1L)
  if (is.null(dim(k))) k <- matrix(k, nrow = 1L)
  if (is.null(dim(v))) v <- matrix(v, nrow = 1L)
  d <- ncol(q)
  scores <- q %*% t(k) / sqrt(d)
  row_max <- apply(scores, 1L, max)
  exp_scores <- exp(scores - row_max)
  attn <- exp_scores / rowSums(exp_scores)
  output <- attn %*% v
  return(list(output = output, attention = attn, scores = scores))
}

#' .infmer_complexity
#'
#' A step of the infmer_native implementation. Called by \code{morie_infmer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L_Q Coerced to numeric by the body, with \code{as.numeric}.
#' @param L_K Coerced to numeric by the body, with \code{as.numeric}.
#' @param c Numeric; combined arithmetically in the body. Defaults to \code{5}.
#' @return A list with \code{full_attention_flops}, \code{probsparse_flops}, \code{ratio}, \code{u}.
#' @export
.infmer_complexity <- function(L_Q, L_K, c = 5) {
  LQn <- as.numeric(L_Q)
  LKn <- as.numeric(L_K)
  full_ops <- LQn * LKn
  u <- c * log(LQn)
  # selection cost: O(L_Q ln L_Q); attention cost: O(u * L_K)
  sparse_ops <- LQn * log(LKn) + u * LKn
  list(
    full_attention_flops = full_ops,
    probsparse_flops = sparse_ops,
    ratio = sparse_ops / full_ops,
    u = u
  )
}

# Main entry point: morie_infmer
# Informer's ProbSparse self-attention (Zhou et al. 2021).
#   q: numeric matrix [L_Q, d]   (queries)
#   k: numeric matrix [L_K, d]   (keys)
#   v: numeric matrix [L_K, d_v] (values)
#   c: numeric sparsity constant  (default 5)
# Returns named list:
#   output           : [L_Q, d_v] attended values
#   selected_queries : 1-based integer vector of top-u query indices
#   n_selected       : integer length of that vector
#   sparsity_scores  : M(q_i, K) for every query
#   complexity       : full vs probsparse flop accounting
#' Main entry point: morie_infmer
#'
#' Informer\'s ProbSparse self-attention (Zhou et al. 2021). q: numeric
#' matrix \[L_Q, d\] (queries) k: numeric matrix \[L_K, d\] (keys) v:
#' numeric matrix \[L_K, d_v\] (values) c: numeric sparsity constant
#' (default 5) Returns named list: output : \[L_Q, d_v\] attended values
#' selected_queries : 1-based integer vector of top-u query indices
#' n_selected : integer length of that vector sparsity_scores : M(q_i,
#' K) for every query complexity : full vs probsparse flop accounting
#'
#' @param q A matrix; indexed by row and column.
#' @param k A matrix; passed to \code{nrow}.
#' @param v A matrix; passed to \code{ncol}.
#' @param c Passed to \code{.infmer_complexity}. Defaults to \code{5}.
#' @return A list with \code{output}, \code{selected_queries}, \code{n_selected},
#' \code{sparsity_scores}, \code{complexity}.
#' @export
morie_infmer <- function(q, k, v, c = 5) {
  if (is.null(dim(q))) q <- matrix(q, nrow = 1L)
  if (is.null(dim(k))) k <- matrix(k, nrow = 1L)
  if (is.null(dim(v))) v <- matrix(v, nrow = 1L)

  L_Q <- nrow(q)
  L_K <- nrow(k)
  d_v <- ncol(v)

  if (L_Q == 0L) {
    return(list(
      output = matrix(0, nrow = 0L, ncol = d_v),
      selected_queries = integer(0),
      n_selected = 0L,
      sparsity_scores = numeric(0),
      complexity = .infmer_complexity(L_Q, L_K, c)
    ))
  }

  m_scores <- .infmer_sparsity_measure(q, k)
  selected_idx <- .infmer_select_queries(q, k, c)
  n_selected <- length(selected_idx)

  output <- matrix(0, nrow = L_Q, ncol = d_v)

  if (n_selected > 0L) {
    q_sel <- q[selected_idx, , drop = FALSE]
    res_sel <- .infmer_full_attention(q_sel, k, v)
    output[selected_idx, ] <- res_sel$output
  }

  if (n_selected < L_Q) {
    unselected_idx <- setdiff(seq_len(L_Q), selected_idx)
    if (length(unselected_idx) > 0L) {
      v_mean <- colMeans(v)
      output[unselected_idx, ] <- matrix(v_mean,
                                         nrow = length(unselected_idx),
                                         ncol = d_v,
                                         byrow = TRUE)
    }
  }

  list(
    output = output,
    selected_queries = selected_idx,
    n_selected = n_selected,
    sparsity_scores = m_scores,
    complexity = .infmer_complexity(L_Q, L_K, c)
  )
}

# Cheatsheet accessor
#' Cheatsheet accessor
#'
#' A step of the infmer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_infmer_cheatsheet <- function() {
  paste("infmer: the same ledger method as `informer` -- one paper,",
        "one implementation, re-exported so the two entries cannot drift.",
        "Informer's ProbSparse attention: O(L log L) complexity for",
        "long sequence time-series forecasting (Zhou et al. 2021).")
}
