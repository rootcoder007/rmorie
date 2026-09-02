# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of vit2lf -- log-scaled attention: log-length logits and scaled
# cosine similarity. Mirrors src/morie/fn/vit2lf.py operation for
# operation, on the shared numerics in R/aaa_helpers_w3num.R.
#
# Ordinary attention divides the query-key inner product by the square
# root of the head dimension and softmaxes the result. That constant is
# chosen so the logits have roughly unit variance at initialisation, and
# it does not depend on how many keys there are. Two separate lines of
# work in 2022 showed that this is the wrong invariance, from opposite
# directions, and both fixes are here.
#
# The FIRST is about sequence length. With a fixed logit scale the
# softmax over n keys spreads its mass over n positions, so the weight
# any one position can hold decays like 1/n and the model becomes less
# confident the longer the input gets -- which is a theorem, not an
# accident, and it is why a transformer trained on short strings
# misclassifies long ones. Chiang and Cholak's fix is one factor:
#
#     Att(q, K, V) = V' softmax( (log n / sqrt(d)) K q )
#
# Multiplying the logits by log n keeps the attention distribution's
# sharpness roughly constant as n grows. This is the "log-scaled" route,
# and it is what the module is named for.
#
# The SECOND is about amplitude. In large vision models the learnt
# attention maps of some blocks come to be dominated by a few pixel
# pairs, because the inner product grows with the norms of the
# activations and those norms grow with depth. Swin Transformer V2
# replaces the inner product with a cosine, which is normalised by
# construction:
#
#     Sim(q_i, k_j) = cos(q_i, k_j) / tau + B_ij
#
# with tau learnable, not shared across heads or layers, and held above
# 0.01 -- the floor matters, because tau appears in a denominator and a
# gradient step that takes it to zero produces infinities rather than a
# sharp distribution. The same paper's log-spaced coordinates,
#
#     dx_hat = sign(dx) log(1 + |dx|)
#
# are the other half of the idea: they compress the relative-position
# range so a bias learnt on an eight-by-eight window extrapolates to a
# sixteen-by-sixteen one. On that window the raw range [-7, 7] becomes
# [-2.079, 2.079], which is a number the paper states and this module
# checks against.
#
# A note on provenance. The ledger row for this module cites
# "Yu et al (2022)", which does not resolve to any paper stating this
# method; the two that do state it are cited below and the
# implementation follows them. Nothing here is taken from the ledger's
# own description.
#
# References
#   Chiang, D. and Cholak, P. (2022) "Overcoming a theoretical
#     limitation of self-attention." Proceedings of the 60th Annual
#     Meeting of the Association for Computational Linguistics
#     (Volume 1: Long Papers), 7654-7664, Dublin. arXiv:2202.12172.
#     Their equation (2) and section 5.3.
#   Liu, Z., Hu, H., Lin, Y., Yao, Z., Xie, Z., Wei, Y., Ning, J., Cao,
#     Y., Zhang, Z., Dong, L., Wei, F. and Guo, B. (2022) "Swin
#     Transformer V2: scaling up capacity and resolution." Proceedings
#     of the IEEE/CVF Conference on Computer Vision and Pattern
#     Recognition (CVPR), 12009-12019. arXiv:2111.09883. Their
#     equations (2) and (4).
#   Vaswani, A. et al. (2017) "Attention is all you need." Advances in
#     Neural Information Processing Systems 30.
#   Hahn, M. (2020) "Theoretical limitations of self-attention in neural
#     sequence models." Transactions of the ACL 8, 156-171.

.VIT2LF_MODES <- c("dot", "logn", "cosine", "logn_cosine")

# Swin V2 holds the cosine temperature above this. It sits in a
# denominator, so a gradient step that reaches zero gives infinities
# rather than a sharp distribution.
.VIT2LF_TAU_FLOOR <- 0.01

#' .vit2lf_norm
#'
#' A step of the vit2lf_native implementation. Called by \code{morie_vit2lf_logits}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{.w3_dot}.
#' @return A numeric value.
#' @export
.vit2lf_norm <- function(v) sqrt(.w3_dot(v, v))

#' The pre-softmax attention scores, one row per query
#'
#' "dot" is the inner product over the square root of the head
#' dimension; "logn" is the same multiplied by log n, with n defaulting
#' to the number of KEYS, which is the number of terms the softmax is
#' spreading its mass over; "cosine" is the cosine of the angle over tau,
#' with no square root of d because a cosine is already normalised, which
#' is the whole point of using one; "logn_cosine" is the cosine route
#' with the log-length factor as well. An additive bias is applied after
#' the scaling in every route, as both papers write it -- a bias scaled
#' along with the logits would change meaning with the sequence length,
#' which is exactly what a position bias must not do.
#'
#' @param q Query matrix, one row per query position.
#' @param k Key matrix, one row per key position.
#' @param mode A member of the mode list.
#' @param tau The cosine temperature.
#' @param bias An additive position bias matrix, or NULL.
#' @param n The length used for the log scaling, or NULL for the number
#'   of keys.
#' @param tau_floor The lower bound on tau.
#' @return A list with the logit matrix and the scale actually applied.
#' @export
morie_vit2lf_logits <- function(q, k, mode = "dot", tau = 1, bias = NULL,
                                n = NULL,
                                tau_floor = .VIT2LF_TAU_FLOOR) {
  if (!(mode %in% .VIT2LF_MODES))
    stop("mode must be one of ", paste(.VIT2LF_MODES, collapse = ", "))
  q <- as.matrix(q)
  k <- as.matrix(k)
  storage.mode(q) <- "double"
  storage.mode(k) <- "double"
  nq <- nrow(q)
  nk <- nrow(k)
  d <- ncol(q)
  if (ncol(k) != d)
    stop("queries and keys must share one head dimension")
  nn <- as.numeric(if (is.null(n)) nk else n)
  if (nn <= 0)
    stop("the length used for the log scaling must be positive")
  if (mode %in% c("cosine", "logn_cosine")) {
    t <- as.numeric(tau)
    if (t <= 0) stop("the cosine temperature must be positive")
    if (t < tau_floor) t <- tau_floor
    base <- 1 / t
  } else {
    base <- 1 / sqrt(as.numeric(d))
  }
  scale <- if (mode %in% c("logn", "logn_cosine")) base * log(nn) else base

  cosmode <- mode %in% c("cosine", "logn_cosine")
  qn <- if (cosmode) vapply(seq_len(nq), function(i) .vit2lf_norm(q[i, ]),
                            numeric(1)) else NULL
  kn <- if (cosmode) vapply(seq_len(nk), function(j) .vit2lf_norm(k[j, ]),
                            numeric(1)) else NULL
  out <- matrix(0, nq, nk)
  for (i in seq_len(nq)) for (j in seq_len(nk)) {
    s <- .w3_dot(q[i, ], k[j, ])
    if (cosmode) {
      den <- qn[i] * kn[j]
      # A zero vector has no direction, so it has no cosine with
      # anything. Reporting zero is the only answer that does not
      # invent one.
      s <- if (den <= 0) 0 else s / den
    }
    s <- s * scale
    if (!is.null(bias)) s <- s + as.numeric(bias[i, j])
    out[i, j] <- s
  }
  list(logits = out, scale = scale)
}

#' Row-wise softmax, max-shifted, with a compensated denominator
#'
#' Masked entries are removed before the shift rather than pushed to a
#' large negative number and exponentiated, so a fully masked row is an
#' error instead of a silently uniform one.
#'
#' @param logits The logit matrix.
#' @param mask A logical matrix, TRUE where a key is visible, or NULL.
#' @return The attention weight matrix.
#' @export
morie_vit2lf_softmax <- function(logits, mask = NULL) {
  nq <- nrow(logits)
  nk <- ncol(logits)
  out <- matrix(0, nq, nk)
  for (i in seq_len(nq)) {
    live <- if (is.null(mask)) seq_len(nk) else which(mask[i, ])
    if (!length(live)) stop("row ", i - 1L, " is masked out entirely")
    mx <- logits[i, live[1]]
    for (j in live) if (logits[i, j] > mx) mx <- logits[i, j]
    ex <- numeric(nk)
    for (j in live) ex[j] <- exp(logits[i, j] - mx)
    tot <- .w3_csum(ex[live])
    out[i, ] <- ex / tot
  }
  out
}

#' Shannon entropy of each attention row, in nats
#'
#' This is the quantity the log-length scaling exists to hold roughly
#' constant, so it is worth reporting rather than leaving to be
#' recomputed.
#'
#' @param w The attention weight matrix.
#' @return One entropy per query row.
#' @export
morie_vit2lf_entropy <- function(w) {
  vapply(seq_len(nrow(w)), function(i) {
    p <- w[i, ][w[i, ] > 0]
    if (!length(p)) 0 else .w3_csum(-p * log(p))
  }, numeric(1))
}

#' Swin V2 log-spaced relative coordinates
#'
#' sign(d) log(1 + |d|), which is odd, zero at zero, and compresses the
#' range so a bias learnt at one window size extrapolates to another. On
#' an eight-by-eight window the raw range \[-7, 7\] becomes
#' \[-2.079, 2.079\], the figure the paper quotes.
#'
#' @param dx Horizontal offset.
#' @param dy Vertical offset.
#' @return The two log-spaced offsets.
#' @export
morie_vit2lf_log_coords <- function(dx, dy) {
  f <- function(v) {
    v <- as.numeric(v)
    s <- if (v == 0) 0 else if (v > 0) 1 else -1
    s * log1p(abs(v))
  }
  c(f(dx), f(dy))
}

#' Look a relative position up in a bias table
#'
#' With log spacing the offsets are transformed first and the table is
#' read at the nearest tabulated log-spaced offset, which is what the
#' meta-network approximates continuously.
#'
#' @param coords A two-column matrix of positions.
#' @param table A (2 window - 1) square bias table.
#' @param window The window size.
#' @param log_spaced Whether to transform the offsets first.
#' @return The bias matrix.
#' @export
morie_vit2lf_relative_bias <- function(coords, table, window,
                                       log_spaced = TRUE) {
  coords <- as.matrix(coords)
  n <- nrow(coords)
  window <- as.integer(window)
  span <- 2L * window - 1L
  if (nrow(table) != span || ncol(table) != span)
    stop("the table must be (2 window - 1) square")
  grid <- vapply(seq_len(span), function(t)
    morie_vit2lf_log_coords(t - 1L - (window - 1L), 0)[1], numeric(1))
  out <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    dx <- coords[i, 1] - coords[j, 1]
    dy <- coords[i, 2] - coords[j, 2]
    if (log_spaced) {
      lc <- morie_vit2lf_log_coords(dx, dy)
      a <- order(abs(grid - lc[1]), seq_len(span))[1]
      b <- order(abs(grid - lc[2]), seq_len(span))[1]
    } else {
      a <- as.integer(dx) + window
      b <- as.integer(dy) + window
      if (a < 1L || a > span || b < 1L || b > span)
        stop("a relative offset falls outside the table")
    }
    out[i, j] <- as.numeric(table[a, b])
  }
  out
}

#' Attention with the logits scaled by log n, by a cosine, or by both
#'
#' @param q Queries, one row per query position.
#' @param k Keys, one row per key position, sharing the head dimension.
#' @param v Values, one row per key position.
#' @param mode A member of the mode list.
#' @param tau The cosine temperature, held at or above the floor.
#' @param bias An additive position bias, applied after the scaling, or
#'   NULL.
#' @param mask A logical matrix, TRUE where a key is visible to a query,
#'   or NULL.
#' @param n The length used for the log scaling, or NULL for the number
#'   of keys.
#' @param tau_floor The lower bound on tau.
#' @return A list with the attention weights, the context vectors, the
#'   logits, the scale actually applied, the per-row entropy and the
#'   largest weight -- the last two being how you see the scaling
#'   working.
#' @export
morie_vit2lf <- function(q, k, v, mode = "logn", tau = 1, bias = NULL,
                         mask = NULL, n = NULL,
                         tau_floor = .VIT2LF_TAU_FLOOR) {
  qq <- as.matrix(q)
  kk <- as.matrix(k)
  vv <- as.matrix(v)
  storage.mode(qq) <- "double"
  storage.mode(kk) <- "double"
  storage.mode(vv) <- "double"
  if (!nrow(qq) || !nrow(kk) || !nrow(vv))
    stop("queries, keys and values must be non-empty")
  if (nrow(vv) != nrow(kk)) stop("there must be one value per key")
  lg <- morie_vit2lf_logits(qq, kk, mode, tau, bias, n, tau_floor)
  w <- morie_vit2lf_softmax(lg$logits, mask)
  dv <- ncol(vv)
  ctx <- matrix(0, nrow(qq), dv)
  for (i in seq_len(nrow(qq))) for (t in seq_len(dv))
    ctx[i, t] <- .w3_csum(w[i, ] * vv[, t])
  ent <- morie_vit2lf_entropy(w)
  mx <- vapply(seq_len(nrow(w)), function(i) max(w[i, ]), numeric(1))
  list(weights = w, context = ctx, logits = lg$logits, scale = lg$scale,
       entropy = ent, max_weight = mx,
       mean_entropy = .w3_csum(ent) / length(ent),
       estimate = .w3_csum(mx) / length(mx),
       se = max(ent) - min(ent), n_query = nrow(qq), n_key = nrow(kk),
       d = ncol(qq), d_value = dv,
       tau = if (mode %in% c("cosine", "logn_cosine"))
         max(as.numeric(tau), tau_floor) else NaN,
       mode = mode, method = "log-scaled attention")
}

#' One-line summary of the vit2lf module
#'
#' @return A character scalar.
#' @export
morie_vit2lf_cheatsheet <- function()
  paste0("vit2lf: log-scaled attention. modes ",
         paste(.VIT2LF_MODES, collapse = ", "),
         "; log-length logits (Chiang-Cholak) and scaled cosine ",
         "similarity (Swin V2)")
