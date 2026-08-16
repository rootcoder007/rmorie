# PatchTST: subseries patches and channel independence.
# Sources: Nie, Y., Nguyen, N. H., Sinthong, P. & Kalagnanam, J.
# (2023) "A Time Series is Worth 64 Words: Long-term Forecasting
# with Transformers", ICLR 2023, arXiv:2211.14730. Subseries-level
# patches as input tokens, channel independence with shared embedding
# and Transformer weights; the three-fold benefit (local semantics,
# quadratically reduced attention, longer history). Zeng, A., Chen,
# M., Zhang, L. & Xu, Q. (2023) "Are Transformers Effective for Time
# Series Forecasting?", AAAI 2023, arXiv:2205.13504, the linear
# baseline that outperformed prior Transformer variants. Vaswani, A.
# et al. (2017) "Attention is all you need", NeurIPS 2017,
# arXiv:1706.03762.

# Base R only, faithful translation of patchT_python_reference.py.

.PATCHT_EPS <- 1e-12

#' .patcht_vec
#'
#' A step of the patchT_native implementation. Called by \code{instance_norm}, \code{patchify}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.patcht_vec <- function(v) as.numeric(unlist(v))

#' .patcht_mat
#'
#' A step of the patchT_native implementation. Called by \code{channel_independent_tokens}, \code{channel_mixed_tokens}, \code{patchtst_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; the body checks with \code{is.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
.patcht_mat <- function(M) {
  if (is.matrix(M)) {
    storage.mode(M) <- "double"
    M
  } else {
    do.call(rbind, lapply(M, function(r) as.numeric(r)))
  }
}

#' patchify
#'
#' A step of the patchT_native implementation. Called by \code{channel_independent_tokens}, \code{channel_mixed_tokens}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.patcht_vec}.
#' @param patch_len Coerced to integer by the body, with \code{as.integer}.
#' @param stride Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{patches}, \code{n_patches}, \code{patch_len}, \code{stride}, \code{L}, \code{covers}.
#' @export
patchify <- function(x, patch_len, stride = NULL) {
  v <- .patcht_vec(x)
  L <- length(v)
  P <- as.integer(patch_len)
  S <- if (is.null(stride)) P else as.integer(stride)
  if (P < 1L)
    stop("patchT: patch_len must be at least 1")
  if (S < 1L)
    stop("patchT: the stride must be at least 1")
  if (L < P)
    stop("patchT: the series has ", L, " points but the patch length is ",
         P)
  n <- (L - P) %/% S + 1L
  patches <- vector("list", n)
  for (i in seq_len(n) - 1L) {
    start <- i * S + 1L
    patches[[i + 1L]] <- v[start:(start + P - 1L)]
  }
  list(
    patches = patches,
    n_patches = n,
    patch_len = P,
    stride = S,
    L = L,
    covers = min(L, (n - 1L) * S + P)
  )
}

#' channel_independent_tokens
#'
#' A step of the patchT_native implementation. Called by \code{patchtst_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.patcht_mat}.
#' @param patch_len Coerced to integer by the body, with \code{as.integer}.
#' @param stride Defaults to \code{NULL}.
#' @return A list with \code{tokens}, \code{D}, \code{n_patches}, \code{patch_len}, \code{n_tokens_total}, \code{design}, \code{note}.
#' @export
channel_independent_tokens <- function(X, patch_len, stride = NULL) {
  Xm <- .patcht_mat(X)
  if (nrow(Xm) == 0L)
    stop("patchT: the input series is empty")
  D <- ncol(Xm)
  out <- vector("list", D)
  for (d in seq_len(D)) {
    col <- Xm[, d]
    out[[d]] <- patchify(col, patch_len, stride)$patches
  }
  n <- length(out[[1L]])
  list(
    tokens = out,
    D = D,
    n_patches = n,
    patch_len = as.integer(patch_len),
    n_tokens_total = D * n,
    design = "channel-independent",
    note = "each token holds ONE channel's subseries; the embedding and Transformer weights are shared across channels"
  )
}

#' channel_mixed_tokens
#'
#' A step of the patchT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.patcht_mat}.
#' @param patch_len See Usage.
#' @param stride Defaults to \code{NULL}.
#' @return A list with \code{tokens}, \code{n_patches}, \code{n_tokens_total}, \code{design}, \code{note}.
#' @export
channel_mixed_tokens <- function(X, patch_len, stride = NULL) {
  Xm <- .patcht_mat(X)
  if (nrow(Xm) == 0L)
    stop("patchT: the input series is empty")
  mixed <- rowSums(Xm)
  p <- patchify(mixed, patch_len, stride)
  list(
    tokens = p$patches,
    n_patches = p$n_patches,
    n_tokens_total = p$n_patches,
    design = "channel-mixing",
    note = "channels are blended before attention, so the channel identity is gone"
  )
}

#' instance_norm
#'
#' A step of the patchT_native implementation. Called by \code{patchtst_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.patcht_vec}.
#' @return A list with \code{normalised}, \code{mean}, \code{sd}, \code{degenerate}.
#' @export
instance_norm <- function(x) {
  v <- .patcht_vec(x)
  if (length(v) < 2L)
    stop("patchT: need at least 2 points to normalise")
  m <- sum(v) / length(v)
  acc <- 0.0
  for (q in v) acc <- acc + (q - m)^2
  sd <- sqrt(acc / (length(v) - 1L))
  if (sd <= .PATCHT_EPS)
    return(list(normalised = rep(0.0, length(v)), mean = m, sd = 0.0,
                degenerate = TRUE))
  list(
    normalised = as.numeric((v - m) / sd),
    mean = m,
    sd = sd,
    degenerate = FALSE
  )
}

#' attention_cost
#'
#' A step of the patchT_native implementation. Called by \code{patchtst_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L Coerced to integer by the body, with \code{as.integer}.
#' @param patch_len Coerced to integer by the body, with \code{as.integer}.
#' @param stride Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param D Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.
#' @param channel_independent A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{n_patches}, \code{pointwise}, \code{patched}, \code{reduction}, \code{stride}, \code{patch_len}, \code{note}.
#' @export
attention_cost <- function(L, patch_len, stride = NULL, D = 1,
                            channel_independent = TRUE) {
  P <- as.integer(patch_len)
  S <- if (is.null(stride)) P else as.integer(stride)
  Lv <- as.integer(L)
  if (Lv < P)
    stop("patchT: the look-back is shorter than the patch")
  n <- (Lv - P) %/% S + 1L
  per_channel <- n * n
  dmult <- if (isTRUE(channel_independent)) as.integer(D) else 1L
  list(
    n_patches = n,
    pointwise = Lv * Lv * dmult,
    patched = per_channel * dmult,
    reduction = (Lv * Lv) / max(per_channel, 1),
    stride = S,
    patch_len = P,
    note = "the reduction is about S^2 for the same look-back, which is what lets the model attend a LONGER history at equal cost"
  )
}

#' patchtst_encode
#'
#' A step of the patchT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.patcht_mat}.
#' @param patch_len See Usage.
#' @param stride Defaults to \code{NULL}.
#' @param normalise A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{tokens}, \code{D}, \code{n_patches}, \code{n_tokens_total}, \code{norm_stats}, \code{normalised}, \code{cost}, \code{method}.
#' @export
patchtst_encode <- function(X, patch_len, stride = NULL,
                            normalise = TRUE) {
  Xm <- .patcht_mat(X)
  if (nrow(Xm) == 0L)
    stop("patchT: the input series is empty")
  D <- ncol(Xm)
  L <- nrow(Xm)
  stats <- vector("list", D)
  cols <- matrix(0.0, L, D)
  for (d in seq_len(D)) {
    col <- Xm[, d]
    if (isTRUE(normalise)) {
      nz <- instance_norm(col)
      stats[[d]] <- list(mean = nz$mean, sd = nz$sd)
      col <- nz$normalised
    } else {
      stats[[d]] <- list(mean = 0.0, sd = 1.0)
    }
    cols[, d] <- col
  }
  tok <- channel_independent_tokens(cols, patch_len, stride)
  list(
    estimate = tok$tokens,
    tokens = tok$tokens,
    D = D,
    n_patches = tok$n_patches,
    n_tokens_total = tok$n_tokens_total,
    norm_stats = stats,
    normalised = isTRUE(normalise),
    cost = attention_cost(L, patch_len, stride, D),
    method = "PatchTST front end; Nie, Nguyen, Sinthong & Kalagnanam (2023)"
  )
}

#' .patchT_cheatsheet
#'
#' A step of the patchT_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.patchT_cheatsheet <- function() {
  paste("patchT: PatchTST. A single time step is not a word, so ",
        "tokenise SUBSERIES: patches of length P, stride S, giving ",
        "N = (L-P)/S + 1 tokens instead of L -- attention shrinks by ",
        "about S^2, which buys a longer look-back at the same cost. ",
        "CHANNEL-INDEPENDENT: one token stream per channel with ",
        "SHARED weights, so the model is equivariant to permuting ",
        "channels. Channel-mixing blends them at the first ",
        "projection and is permutation-INVARIANT instead.", sep = "")
}

# compact alias per ledger/NAMING.md
patchtst <- patchtst_encode

# public names resolved by fn/_lazy_map.json
patch_tst <- patchtst_encode

morie_patchT <- patchtst_encode
