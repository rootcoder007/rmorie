# Sources:
#   Ansari, A. F. et al. (2024) "Chronos: Learning the Language of Time
#   Series", Transactions on Machine Learning Research (10/2024),
#   arXiv:2403.07815.
#   Salinas, D., Flunkert, V., Gasthaus, J. & Januschowski, T. (2020)
#   "DeepAR: Probabilistic forecasting with autoregressive recurrent
#   networks", International Journal of Forecasting 36(3), 1181-1191.
#   Raffel, C., Shazeer, N., Roberts, A., Lee, K., Narang, S., Matena,
#   M., Zhou, Y., Li, W. & Liu, P. J. (2020) "Exploring the Limits of
#   Transfer Learning with a Unified Text-to-Text Transformer", JMLR
#   21(140), 1-67.

.chronos_eps <- 1e-12
.chronos_PAD <- -1L
.chronos_EOS <- -2L

#' .chronos_vec
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.chronos_vec <- function(x) as.numeric(x)

#' mean_scale
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param context Defaults to \code{NULL}.
#' @return A list with \code{scaled}, \code{scale}, \code{degenerate}, \code{context}, \code{preserves_zero}.
#' @export
mean_scale <- function(x, context = NULL) {
  v <- .chronos_vec(x)
  if (length(v) == 0L) stop("chronos: the series is empty")
  C <- if (is.null(context)) length(v) else as.integer(context)
  if (C < 1L || C > length(v))
    stop(sprintf("chronos: the context length must lie in 1..%d, got %d",
                 length(v), C))
  s <- sum(abs(v[seq_len(C)])) / C
  if (s <= .chronos_eps) {
    return(list(
      scaled = rep(0.0, length(v)),
      scale = 0.0,
      degenerate = TRUE,
      note = "the context is all zeros, so no scale is defined"
    ))
  }
  list(
    scaled = v / s,
    scale = s,
    degenerate = FALSE,
    context = C,
    preserves_zero = TRUE
  )
}

#' uniform_bins
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param lo Defaults to \code{-15}.
#' @param hi Defaults to \code{15}.
#' @param n_bins Defaults to \code{4096L}.
#' @return A list with \code{centers}, \code{edges}, \code{n_bins}, \code{scheme}, \code{range}.
#' @export
uniform_bins <- function(lo = -15.0, hi = 15.0, n_bins = 4096L) {
  B <- as.integer(n_bins)
  if (B < 2L) stop(sprintf("chronos: need at least 2 bins, got %d", B))
  if (as.numeric(hi) <= as.numeric(lo)) stop("chronos: hi must exceed lo")
  centers <- as.numeric(lo) + (as.numeric(hi) - as.numeric(lo)) * seq_len(B) / (B - 1L) * 0
  centers <- as.numeric(lo) + (as.numeric(hi) - as.numeric(lo)) * (seq_len(B) - 1L) / (B - 1L)
  edges <- 0.5 * (centers[seq_len(B - 1L)] + centers[seq.int(2L, B)])
  list(centers = centers, edges = edges, n_bins = B, scheme = "uniform",
       range = c(centers[1L], centers[B]))
}

#' quantile_bins
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param samples See Usage.
#' @param n_bins Defaults to \code{4096L}.
#' @return A list with \code{centers}, \code{edges}, \code{n_bins}, \code{scheme}, \code{range}, \code{caveat}.
#' @export
quantile_bins <- function(samples, n_bins = 4096L) {
  v <- sort(.chronos_vec(samples))
  B <- as.integer(n_bins)
  if (length(v) < B)
    stop(sprintf("chronos: %d samples cannot define %d quantile bins",
                 length(v), B))
  centers <- v[pmin(length(v), floor((seq_len(B) - 0.5) * length(v) / B)) + 1L]
  centers <- sort(unique(centers))
  if (length(centers) < 2L)
    stop("chronos: the samples are too concentrated to form bins")
  edges <- 0.5 * (centers[seq_len(length(centers) - 1L)] +
                    centers[seq.int(2L, length(centers))])
  list(centers = centers, edges = edges, n_bins = length(centers),
       scheme = "quantile",
       range = c(centers[1L], centers[length(centers)]),
       caveat = "fitted to the TRAINING distribution; an unseen dataset may fall where there are no bins")
}

#' quantize
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param bins See Usage.
#' @return A list with \code{tokens}, \code{n_clipped}, \code{clipped_fraction}, \code{in_range}, \code{note}.
#' @export
quantize <- function(x, bins) {
  v <- .chronos_vec(x)
  c <- bins$centers
  e <- bins$edges
  out <- integer(length(v))
  clipped <- 0L
  for (kk in seq_along(v)) {
    q <- v[kk]
    if (q < c[1L] || q > c[length(c)]) clipped <- clipped + 1L
    j <- 0L
    if (length(e) > 0L) {
      while (j < length(e) && q >= e[j + 1L]) j <- j + 1L
    } else {
      j <- 0L
    }
    out[kk] <- j
  }
  list(
    tokens = out,
    n_clipped = clipped,
    clipped_fraction = clipped / as.numeric(length(v)),
    in_range = clipped == 0L,
    note = paste0("predictions are confined to [c_1, c_B]; a strong ",
                  "trend leaves that interval and cannot be represented")
  )
}

#' dequantize
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param tokens See Usage.
#' @param bins See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
dequantize <- function(tokens, bins) {
  c <- bins$centers
  out <- numeric(0)
  for (t in tokens) {
    j <- as.integer(t)
    if (j == .chronos_PAD || j == .chronos_EOS) next
    if (!(j >= 0L && j < length(c)))
      stop(sprintf("chronos: token %d is outside the vocabulary of %d bins", j, length(c)))
    out <- c(out, c[j + 1L])
  }
  out
}

#' tokenize
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param bins See Usage.
#' @param context Defaults to \code{NULL}.
#' @param add_eos Defaults to \code{TRUE}.
#' @param pad_to Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{tokens}, \code{scale}, \code{n_clipped}, \code{clipped_fraction}, \code{vocab_size}, \code{method}, \code{ignores}.
#' @export
tokenize <- function(x, bins, context = NULL, add_eos = TRUE, pad_to = NULL) {
  sc <- mean_scale(x, context = context)
  qz <- quantize(sc$scaled, bins)
  toks <- as.integer(qz$tokens)
  if (isTRUE(add_eos)) toks <- c(toks, .chronos_EOS)
  if (!is.null(pad_to) && length(toks) < as.integer(pad_to))
    toks <- c(rep(.chronos_PAD, as.integer(pad_to) - length(toks)), toks)
  list(
    estimate = toks, tokens = toks, scale = sc$scale,
    n_clipped = qz$n_clipped, clipped_fraction = qz$clipped_fraction,
    vocab_size = bins$n_bins + 2L,
    method = "Chronos tokenisation: mean scaling then uniform quantisation; Ansari et al. (2024) Sec. 3.1",
    ignores = "time and frequency features, deliberately"
  )
}

#' detokenize
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param tokens See Usage.
#' @param bins See Usage.
#' @param scale See Usage.
#' @return A numeric value.
#' @export
detokenize <- function(tokens, bins, scale) {
  q <- dequantize(tokens, bins)
  q * as.numeric(scale)
}

#' forecast_summary
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param token_probs See Usage.
#' @param bins See Usage.
#' @param quantiles Defaults to \code{c(0.1, 0.5, 0.9)}.
#' @return A list with \code{mean}, \code{quantiles}, \code{mode}, \code{note}.
#' @export
forecast_summary <- function(token_probs, bins, quantiles = c(0.1, 0.5, 0.9)) {
  p <- .chronos_vec(token_probs)
  c <- bins$centers
  if (length(p) != length(c))
    stop(sprintf("chronos: %d probabilities for %d bins", length(p), length(c)))
  tot <- sum(p)
  if (tot <= .chronos_eps)
    stop("chronos: the predicted distribution has no mass")
  p <- p / tot
  mean <- sum(p * c)
  out <- list()
  for (qq in quantiles) {
    acc <- 0.0
    pick <- c[length(c)]
    for (i in seq_along(c)) {
      acc <- acc + p[i]
      if (acc >= as.numeric(qq)) { pick <- c[i]; break }
    }
    out[[as.character(qq)]] <- pick
  }
  mode_idx <- which.max(p)
  list(
    mean = mean,
    quantiles = out,
    mode = c[mode_idx],
    note = "cross-entropy training does not know bins are ordered; the model must learn that neighbouring bins are similar"
  )
}

#' morie_chronos
#'
#' Part of the chronos_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param bins See Usage.
#' @param context Defaults to \code{NULL}.
#' @param add_eos Defaults to \code{TRUE}.
#' @param pad_to Defaults to \code{NULL}.
#' @return The value of \code{tokenize}.
#' @export
morie_chronos <- function(x, bins, context = NULL, add_eos = TRUE,
                          pad_to = NULL) {
  tokenize(x, bins, context, add_eos, pad_to)
}
