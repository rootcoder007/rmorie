# SPDX-License-Identifier: AGPL-3.0-or-later
#' Separate the few large key channels from the rest
#'
#' The distortion bound is proportional to the embedding norm, and in
#' deeper layers a handful of fixed coordinates carry most of it.
#' Splitting those off and quantizing them separately shrinks the norm
#' the main quantizer must cope with. The channels are fixed across
#' tokens, so the split is identified once during the prompt phase.
#'
#' Formula: flag channel j when \code{|a_j|} exceeds the
#' \code{outlier_threshold} quantile of the channel magnitudes.
#'
#' @param channels Per-channel activation magnitudes, or a matrix whose
#'   column maxima are used.
#' @param outlier_threshold Quantile above which a channel is an outlier.
#' @return List with \code{outlier_idx}, \code{inlier_idx}, \code{cut},
#'   \code{estimate}, \code{d}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   section 4.1 (outliers).
#' @export
Tqoutl <- function(channels, outlier_threshold = 0.99) {
  Cm <- as.matrix(channels)
  mag <- if (ncol(Cm) == 1L) abs(as.numeric(Cm)) else apply(abs(Cm), 2, max)
  cut <- .s4_quantile7(mag, as.numeric(outlier_threshold))
  out_idx <- which(mag > cut) - 1L
  in_idx <- which(mag <= cut) - 1L
  .t1_result(outlier_idx = out_idx, inlier_idx = in_idx, cut = cut,
             estimate = length(out_idx) / length(mag), d = length(mag),
             method = "Outlier channel split for KV quantization")
}
