# morie.fn -- function file (rootcoder007/morie)
# Informer: ProbSparse attention for long-sequence forecasting.
#
# Self-attention costs O(L^2) in both time and memory, which is what
# stops a Transformer from reading a long history. Informer's observation
# is that most of that computation is wasted, and it says precisely why.
#
# A query whose attention is uniform contributes nothing. Write the
# attention of query i over the keys as p(k_j | q_i). If that distribution
# is close to uniform q(k_j | q_i) = 1/L_K, then the output for that
# query is essentially a plain average of the values -- redundant with
# the residual connection already carrying the input forward. Only
# queries whose attention is far from uniform do any work.
#
# So measure the distance from uniform. The Kullback-Leibler divergence
# between the two, with the constant dropped, gives the sparsity
# measurement M(q_i, K) = ln sum_j exp(q_i k_j^T / sqrt(d))
#                       - (1/L_K) sum_j q_i k_j^T / sqrt(d),
# a log-sum-exp minus a mean.
#
# ProbSparse keeps only the top-u queries. With u = c ln L_Q for a
# sampling factor c, each query-key lookup needs O(ln L_Q) dot
# products and the layer memory stays O(L_K ln L_Q).
#
# The measurement itself would cost what it saves. Lemma 1 supplies
# a max-mean approximation that bounds M and, evaluated on a sample
# of keys rather than all of them, ranks the queries well enough to
# select the same top set.
#
# Queries outside the top-u are not computed; they take the mean of
# the values, which is what their near-uniform attention would have
# produced anyway. Setting u = L_Q recovers full attention exactly.
#
# References
# ----------
# Zhou, H., Zhang, S., Peng, J., Zhang, S., Li, J., Xiong, H. & Zhang,
# W. (2021) "Informer: Beyond Efficient Transformer for Long Sequence
# Time-Series Forecasting", Proceedings of the AAAI Conference on
# Artificial Intelligence 35(12), 11106-11115, arXiv:2012.07436.
# Vaswani, A. et al. (2017) "Attention is all you need", Advances in
# Neural Information Processing Systems 30, arXiv:1706.03762.

.informer_EPS <- 1e-12
.informer_MEASURES <- c("exact", "maxmean")

#' .informer_to_rows
#'
#' A step of the informer_native implementation. Called by
#' \code{morie_informer_full_attention}, \code{morie_informer_probsparse_attention},
#' \code{morie_informer_select_queries} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return The value of \code{lapply}.
#' @export
# The k.* helper names the port carried never existed in either arm;
# these are their concrete forms.
.informer_vec <- function(x) as.numeric(x)

.informer_mat <- function(x) .informer_to_rows(x)

.informer_softmax <- function(z) {
  m <- max(z)
  e <- exp(z - m)
  e / sum(e)
}

.informer_logsumexp <- function(z) {
  m <- max(z)
  m + log(sum(exp(z - m)))
}

.informer_to_rows <- function(x) {
  # pass a list of rows straight through: select_queries hands the
  # sampled key subset back in as a list, and as.matrix() on a list
  # cannot coerce
  if (is.list(x)) return(lapply(x, as.numeric))
  M <- as.matrix(x)
  lapply(seq_len(nrow(M)), function(i) as.numeric(M[i, ]))
}

#' .informer_logits
#'
#' A step of the informer_native implementation. Called by
#' \code{morie_informer_full_attention}, \code{morie_informer_probsparse_attention},
#' \code{morie_informer_sparsity_measure}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param q Numeric; combined arithmetically in the body.
#' @param K Iterated over elementwise, with \code{sapply}.
#' @param scale Numeric; combined arithmetically in the body.
#' @return A vector, from \code{sapply}.
#' @export
#' @examples
#' res <- .informer_logits(q = 0.5, K = 3L, scale = TRUE)
#' res
.informer_logits <- function(q, K, scale) {
  sapply(K, function(kj) scale * sum(q * kj))
}

#' morie_informer_sparsity_measure
#'
#' A step of the informer_native implementation. Called by
#' \code{morie_informer_kl_from_uniform}, \code{morie_informer_select_queries}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param q Passed to \code{k.vec}.
#' @param K Passed to \code{.informer_to_rows}.
#' @param measure Compared against \code{"maxmean"}. Defaults to \code{"exact"}.
#' @param scale Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A numeric value.
#' @export
morie_informer_sparsity_measure <- function(q, K, measure = "exact", scale = NULL) {
  if (!(measure %in% .informer_MEASURES)) {
    stop(sprintf("informer: measure must be exact or maxmean, got '%s'", measure))
  }
  Km <- .informer_to_rows(K)
  qv <- as.numeric(.informer_vec(q))
  if (length(Km) == 0L) {
    stop("informer: the key set is empty")
  }
  d <- length(qv)
  if (length(Km[[1]]) != d) {
    stop(sprintf("informer: query has %d dimensions but keys have %d",
                 d, length(Km[[1]])))
  }
  sc <- if (is.null(scale)) (1.0 / sqrt(d)) else as.numeric(scale)
  z <- .informer_logits(qv, Km, sc)
  mean_z <- sum(z) / length(z)
  if (measure == "maxmean") {
    return(max(z) - mean_z)
  }
  return(.informer_logsumexp(z) - mean_z)
}

#' morie_informer_kl_from_uniform
#'
#' A step of the informer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param q Passed to \code{morie_informer_sparsity_measure}.
#' @param K Passed to \code{morie_informer_sparsity_measure}.
#' @param scale Passed to \code{morie_informer_sparsity_measure}.
#' @return A numeric value.
#' @export
morie_informer_kl_from_uniform <- function(q, K, scale = NULL) {
  Km <- .informer_mat(K)
  return(morie_informer_sparsity_measure(q, K, measure = "exact",
                                         scale = scale) - log(length(Km)))
}

#' morie_informer_select_queries
#'
#' A step of the informer_native implementation. Called by
#' \code{morie_informer_probsparse_attention}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q Passed to \code{.informer_to_rows}.
#' @param K Passed to \code{.informer_to_rows}.
#' @param factor Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.
#' @param measure Passed to \code{morie_informer_sparsity_measure}. Defaults to \code{"maxmean"}.
#' @param n_sample Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return A list with \code{top}, \code{u}, \code{scores}, \code{L_Q}, \code{L_K},
#' \code{n_sample}, \code{measure}.
#' @export
morie_informer_select_queries <- function(Q, K, factor = 5, measure = "maxmean",
                                          n_sample = NULL, seed = 0) {
  Qm <- .informer_to_rows(Q)
  Km <- .informer_to_rows(K)
  LQ <- length(Qm)
  LK <- length(Km)
  if (LQ == 0L || LK == 0L) {
    stop("informer: queries and keys must be non-empty")
  }
  u <- max(1L, min(LQ, as.integer(as.numeric(factor) * log(max(LQ, 2L)))))
  if (!is.null(n_sample) && as.integer(n_sample) < LK) {
    rng <- .ghc_rng(seed)
    unif_vals <- .ghc_unif(rng, LK)
    idx <- order(unif_vals)[seq_len(as.integer(n_sample))]
    Ks <- Km[idx]
  } else {
    Ks <- Km
  }
  scores <- sapply(seq_len(LQ), function(i) {
    morie_informer_sparsity_measure(Qm[[i]], Ks, measure = measure)
  })
  sorted_idx <- order(scores, decreasing = TRUE)
  top_u <- sorted_idx[seq_len(u)]
  list(
    top = sort(top_u),
    u = u,
    scores = scores,
    L_Q = LQ,
    L_K = LK,
    n_sample = if (is.null(n_sample) || n_sample == 0) LK else as.integer(n_sample),
    measure = measure
  )
}

#' morie_informer_full_attention
#'
#' A step of the informer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q Passed to \code{.informer_to_rows}.
#' @param K Passed to \code{.informer_to_rows}.
#' @param V Passed to \code{.informer_to_rows}.
#' @param scale Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_informer_full_attention <- function(Q, K, V, scale = NULL) {
  Qm <- .informer_to_rows(Q)
  Km <- .informer_to_rows(K)
  Vm <- .informer_to_rows(V)
  if (length(Km) != length(Vm)) {
    stop(sprintf("informer: keys and values must match in length (%d, %d)",
                 length(Km), length(Vm)))
  }
  d <- length(Qm[[1]])
  sc <- if (is.null(scale)) (1.0 / sqrt(d)) else as.numeric(scale)
  out <- vector("list", length(Qm))
  for (qi in seq_along(Qm)) {
    q <- Qm[[qi]]
    w <- .informer_softmax(.informer_logits(q, Km, sc))
    out[[qi]] <- sapply(seq_along(Vm[[1]]), function(a) {
      sum(w * sapply(seq_along(Vm), function(j) Vm[[j]][a]))
    })
  }
  return(out)
}

#' morie_informer_probsparse_attention
#'
#' A step of the informer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q Passed to \code{.informer_to_rows}.
#' @param K Passed to \code{.informer_to_rows}.
#' @param V Passed to \code{.informer_to_rows}.
#' @param factor Passed to \code{morie_informer_select_queries}. Defaults to \code{5}.
#' @param measure Passed to \code{morie_informer_select_queries}. Defaults to \code{"maxmean"}.
#' @param n_sample Passed to \code{morie_informer_select_queries}.
#' @param seed Passed to \code{morie_informer_select_queries}. Defaults to \code{0}.
#' @param scale Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{estimate}, \code{output}, \code{selected}, \code{u},
#' \code{L_Q}, \code{L_K}, \code{measure}, \code{complexity}, \code{method}, \code{note}.
#' @export
morie_informer_probsparse_attention <- function(Q, K, V, factor = 5,
                                                measure = "maxmean",
                                                n_sample = NULL, seed = 0,
                                                scale = NULL) {
  Qm <- .informer_to_rows(Q)
  Km <- .informer_to_rows(K)
  Vm <- .informer_to_rows(V)
  if (length(Km) != length(Vm)) {
    stop(sprintf("informer: keys and values must match in length (%d, %d)",
                 length(Km), length(Vm)))
  }
  sel <- morie_informer_select_queries(Qm, Km, factor = factor,
                                       measure = measure,
                                       n_sample = n_sample, seed = seed)
  d <- length(Qm[[1]])
  sc <- if (is.null(scale)) (1.0 / sqrt(d)) else as.numeric(scale)
  dv <- length(Vm[[1]])
  vbar <- sapply(seq_len(dv), function(a) {
    sum(sapply(seq_along(Vm), function(j) Vm[[j]][a])) / length(Vm)
  })
  out <- lapply(seq_along(Qm), function(i) as.numeric(vbar))
  for (i in sel$top) {
    w <- .informer_softmax(.informer_logits(Qm[[i]], Km, sc))
    out[[i]] <- sapply(seq_len(dv), function(a) {
      sum(w * sapply(seq_along(Vm), function(j) Vm[[j]][a]))
    })
  }
  list(
    estimate = out,
    output = out,
    selected = sel$top,
    u = sel$u,
    L_Q = sel$L_Q,
    L_K = sel$L_K,
    measure = measure,
    complexity = morie_informer_complexity(sel$L_Q, sel$L_K, factor),
    method = "ProbSparse self-attention, Zhou et al. (2021) eq. (3)",
    note = "unselected queries take the mean of V, which is what their near-uniform attention would give"
  )
}

#' morie_informer_complexity
#'
#' A step of the informer_native implementation. Called by
#' \code{morie_informer_probsparse_attention}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param L_Q Coerced to integer by the body, with \code{as.integer}.
#' @param L_K Coerced to integer by the body, with \code{as.integer}.
#' @param factor Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.
#' @return A list with \code{full}, \code{probsparse}, \code{u}, \code{ratio},
#' \code{memory_full}, \code{memory_probsparse}.
#' @export
morie_informer_complexity <- function(L_Q, L_K, factor = 5) {
  lq <- as.integer(L_Q)
  lk <- as.integer(L_K)
  u <- max(1L, min(lq, as.integer(as.numeric(factor) * log(max(lq, 2L)))))
  list(
    full = lq * lk,
    probsparse = u * lk,
    u = u,
    ratio = (lq * lk) / max(u * lk, 1L),
    memory_full = lq * lk,
    memory_probsparse = lk * max(1L, as.integer(log(max(lq, 2L))))
  )
}

#' morie_informer_cheatsheet
#'
#' A step of the informer_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_informer_cheatsheet <- function() {
  paste0(
    "informer: ProbSparse. A query whose attention is UNIFORM jus",
    "t averages V and is redundant with the residual. M(q,K) = lo",
    "gsumexp(z) - mean(z) measures the distance from uniform; it ",
    "is MINIMISED at ln L_K, attained exactly when the logits are",
    " equal, so M - ln L_K is the KL and that is what is zero the",
    "re. Keep only the top u = c ln L_Q queries: O(L ln L) time, ",
    "O(L_K ln L_Q) memory. Computing M exactly would cost the O(L",
    "^2) being saved, so Lemma 1's max-mean bound on sampled keys",
    " is used instead. u = L_Q recovers full attention exactly."
  )
}

# Compact alias per ledger/NAMING.md -- infmer and informer are the
# same ledger entry duplicated; both names resolve here
morie_informer_informerattention <- morie_informer_probsparse_attention

# Public names resolved by fn/_lazy_map.json
morie_informer_informer_long_horizon <- morie_informer_probsparse_attention

# Main entry point
morie_informer <- morie_informer_probsparse_attention
