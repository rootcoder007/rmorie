# Crossformer: cross-time and cross-dimension dependency, separately.
# Sources: Zhang, Y. & Yan, J. (2023) "Crossformer: Transformer
# Utilizing Cross-Dimension Dependency for Multivariate Time Series
# Forecasting", ICLR 2023, Sec. 3.1 and eq. (1)-(2) (Dimension-
# Segment-Wise embedding), Sec. 3.2 and eq. (3) (Two-Stage Attention
# with a shared MSA in the cross-time stage and a router of c << D
# vectors in the cross-dimension stage), and Sec. 3.3 (hierarchical
# encoder-decoder); Vaswani, A. et al. (2017) "Attention is all you
# need", NeurIPS 30, arXiv:1706.03762, for the multi-head
# self-attention; Dosovitskiy, A. et al. (2021) "An Image is Worth
# 16x16 Words", ICLR, arXiv:2010.11929, for the patching idea DSW
# adapts.
#
# Native implementation mirroring Python morie.fn.crsfmr exactly: the
# same DSW segmentation, the same scaled dot-product attention with
# rows of the weight matrix summing to one, the same cross-time stage
# with shared weights across dimensions, the same router with gather
# then broadcast, and the same complexity counts.

#' Softmax
#'
#' Numerically stable softmax over a numeric vector.
#'
#' @param x Numeric vector.
#' @return Numeric vector, same length as \code{x}.
#' @keywords internal
#' @noRd
.crsfmr_softmax <- function(x) {
  m <- max(x)
  ex <- exp(x - m)
  ex / sum(ex)
}

#' Scaled dot-product attention
#'
#' Mirrors \code{attention}: \code{out = softmax(Q K^T / sqrt(d_k)) V}
#' with the rows of the weight matrix summing to one.
#'
#' @param Q Numeric matrix (rows are queries).
#' @param K Numeric matrix (rows are keys).
#' @param V Numeric matrix (rows are values).
#' @return A list with \code{out} and \code{weights}.
#' @export
morie_crsfmr_attention <- function(Q, K, V) {
  Qm <- as.matrix(Q); Km <- as.matrix(K); Vm <- as.matrix(V)
  if (nrow(Km) != nrow(Vm))
    stop(sprintf("crsfmr: keys and values must have the same length (%d, %d)",
                 nrow(Km), nrow(Vm)))
  dk <- ncol(Qm)
  if (ncol(Km) != dk)
    stop(sprintf("crsfmr: queries and keys must share a dimension (%d, %d)",
                 dk, ncol(Km)))
  scale <- 1 / sqrt(dk)
  out <- matrix(0, nrow(Qm), ncol(Vm))
  W <- matrix(0, nrow(Qm), nrow(Km))
  for (i in seq_len(nrow(Qm))) {
    logits <- numeric(nrow(Km))
    for (j in seq_len(nrow(Km)))
      logits[j] <- scale * sum(Qm[i, ] * Km[j, ])
    w <- .crsfmr_softmax(logits)
    W[i, ] <- w
    for (a in seq_len(ncol(Vm)))
      out[i, a] <- sum(w * Vm[, a])
  }
  list(out = out, weights = W)
}

#' Dimension-Segment-Wise embedding
#'
#' Eq. (1)-(2): segment each dimension, then embed each segment. The
#' output is a 3D array indexed as \code{H[[i, d]]} for the i-th
#' segment of dimension d.
#'
#' @param X Numeric matrix, T rows by D columns.
#' @param seg_len Integer, segment length.
#' @param E Optional embedding matrix of shape d_model by seg_len.
#' @param pos Optional positional embedding array.
#' @return A list with \code{H}, \code{n_seg}, \code{D}, \code{d_model},
#'   \code{seg_len} and \code{shape}.
#' @references Zhang, Y. & Yan, J. (2023). Crossformer. ICLR 2023.
#' @export
morie_crsfmr_dsw_embed <- function(X, seg_len, E = NULL, pos = NULL) {
  Xm <- as.matrix(X)
  T <- nrow(Xm)
  if (T == 0L) stop("crsfmr: the input series is empty")
  D <- ncol(Xm)
  L <- as.integer(seg_len)
  if (L < 1L)
    stop(sprintf("crsfmr: seg_len must be at least 1, got %d", L))
  if (T %% L != 0L)
    stop(sprintf("crsfmr: T = %d is not divisible by seg_len = %d; pad the series first (the paper pads to a proper length)",
                 T, L))
  n_seg <- T %/% L
  if (is.null(E)) {
    Em <- diag(L)
    dm <- L
  } else {
    Em <- as.matrix(E)
    if (ncol(Em) != L)
      stop(sprintf("crsfmr: E must have seg_len = %d columns, got %d",
                   L, ncol(Em)))
    dm <- nrow(Em)
  }
  H <- vector("list", n_seg)
  for (i in seq_len(n_seg)) {
    row <- vector("list", D)
    for (d in seq_len(D)) {
      seg <- Xm[((i - 1L) * L + 1L):(i * L), d]
      vec <- as.numeric(Em %*% seg)
      if (!is.null(pos)) {
        p <- pos[[i]][[d]]
        if (is.list(p) || length(p) == dm) vec <- vec + as.numeric(p)
      }
      row[[d]] <- vec
    }
    H[[i]] <- row
  }
  list(H = H, n_seg = n_seg, D = D, d_model = dm, seg_len = L,
       shape = c(n_seg, D, dm),
       note = paste("each vector is ONE dimension's segment; the",
                    "dimension axis survives embedding"))
}

#' Cross-time stage (equation 3)
#'
#' Multi-head self-attention within each dimension, weights shared.
#'
#' @param Z A length-L list of length-D lists of d_model vectors.
#' @return A 3D array indexed as \code{out[\[i\]][\[d\]]}.
#' @export
morie_crsfmr_cross_time_stage <- function(Z) {
  L <- length(Z)
  if (L == 0L) stop("crsfmr: the input array is empty")
  D <- length(Z[[1L]])
  out <- vector("list", L)
  for (i in seq_len(L)) out[[i]] <- vector("list", D)
  for (d in seq_len(D)) {
    seq <- t(sapply(Z, function(zi) zi[[d]]))
    a <- morie_crsfmr_attention(seq, seq, seq)$out
    for (i in seq_len(L))
      out[[i]][[d]] <- as.numeric(seq[i, ]) + as.numeric(a[i, ])
  }
  out
}

#' Cross-dimension stage through a router
#'
#' The gather-then-broadcast construction of Zhang & Yan (2023) Sec.
#' 3.2: a small set of \code{c} vectors first gathers from all
#' dimensions, then broadcasts back, at cost \code{O(cD)} rather than
#' the \code{O(D^2)} of direct all-pairs attention.
#'
#' @param Z A length-L list of length-D lists of d_model vectors.
#' @param router Optional list of router vectors; defaults to the
#'   first \code{c} dimensions' vectors at each time step.
#' @param n_router Integer, number of router vectors.
#' @return A 3D array indexed as \code{out[\[i\]][\[d\]]}.
#' @export
morie_crsfmr_cross_dimension_stage <- function(Z, router = NULL,
                                              n_router = NULL) {
  L <- length(Z)
  if (L == 0L) stop("crsfmr: the input array is empty")
  D <- length(Z[[1L]])
  c <- if (is.null(n_router)) max(1L, min(D, 3L)) else as.integer(n_router)
  if (c < 1L) stop("crsfmr: n_router must be at least 1")
  out <- vector("list", L)
  for (i in seq_len(L)) {
    Zi <- Z[[i]]
    if (is.null(router)) {
      B <- lapply(seq_len(c) - 1L,
                  function(q) as.numeric(Zi[[(q %% D) + 1L]]))
    } else {
      B <- lapply(seq_len(c), function(q) as.numeric(router[[q]]))
    }
    if (length(B) != c)
      stop(sprintf("crsfmr: the router array has %d rows but n_router is %d",
                   length(B), c))
    Bm <- do.call(rbind, B)
    Zim <- do.call(rbind, Zi)
    gathered <- morie_crsfmr_attention(Bm, Zim, Zim)$out
    back <- morie_crsfmr_attention(Zim, gathered, gathered)$out
    row <- vector("list", D)
    for (d in seq_len(D))
      row[[d]] <- as.numeric(Zi[[d]]) + as.numeric(back[d, ])
    out[[i]] <- row
  }
  out
}

#' Full Two-Stage Attention layer
#'
#' Cross-time then cross-dimension.
#'
#' @param Z A length-L list of length-D lists of d_model vectors.
#' @param n_router Integer, number of router vectors.
#' @param router Optional list of router vectors.
#' @return A list with \code{output}, \code{cross_time} and the
#'   operation counts.
#' @export
morie_crsfmr_two_stage_attention <- function(Z, n_router = NULL,
                                             router = NULL) {
  zt <- morie_crsfmr_cross_time_stage(Z)
  zd <- morie_crsfmr_cross_dimension_stage(zt, router = router,
                                            n_router = n_router)
  D <- length(Z[[1L]])
  c <- if (is.null(n_router)) max(1L, min(D, 3L)) else as.integer(n_router)
  list(estimate = zd, output = zd, cross_time = zt,
       L = length(Z), D = D, n_router = c,
       complexity = morie_crsfmr_complexity(length(Z), D, c),
       method = "Two-Stage Attention, Zhang & Yan (2023) Sec. 3.2")
}

#' Merge adjacent segments (one step up the hierarchy)
#'
#' The encoder's upper layer sees half as many segments, each
#' covering twice the span.
#'
#' @param Z A length-L list of length-D lists of d_model vectors.
#' @param factor Integer merge factor.
#' @return A length-(L/factor) list.
#' @export
morie_crsfmr_segment_merge <- function(Z, factor = 2L) {
  f <- as.integer(factor)
  if (f < 2L)
    stop("crsfmr: the merge factor must be at least 2")
  L <- length(Z)
  if (L %% f != 0L)
    stop(sprintf("crsfmr: %d segments do not divide by a merge factor of %d",
                 L, f))
  D <- length(Z[[1L]])
  out <- vector("list", L %/% f)
  for (i in seq_len(L %/% f)) {
    row <- vector("list", D)
    for (d in seq_len(D)) {
      acc <- numeric(length(Z[[1L]][[d]]))
      for (q in seq_len(f)) acc <- acc + as.numeric(Z[[(i - 1L) * f + q]][[d]]) / f
      row[[d]] <- acc
    }
    out[[i]] <- row
  }
  out
}

#' Operation counts for the two stages
#'
#' Cross-time is \code{O(D L^2)}. Cross-dimension is \code{O(c D L)}
#' through the router against \code{O(D^2 L)} all-pairs. Flattening
#' the whole array would be \code{O(D^2 L^2)}.
#'
#' @param L Integer, number of segments.
#' @param D Integer, number of dimensions.
#' @param c Integer, number of router vectors.
#' @return A list of named integer counts.
#' @export
morie_crsfmr_complexity <- function(L, D, c) {
  Lv <- as.integer(L); Dv <- as.integer(D); cv <- as.integer(c)
  list(cross_time = Dv * Lv * Lv,
       cross_dimension_router = cv * Dv * Lv,
       cross_dimension_full = Dv * Dv * Lv,
       flattened_2d = Dv * Dv * Lv * Lv,
       router_saving = (Dv * Dv * Lv) / max(cv * Dv * Lv, 1L))
}

# house entry point: the package exports one morie_<module>
morie_crsfmr <- morie_crsfmr_attention
