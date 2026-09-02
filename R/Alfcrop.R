# SPDX-License-Identifier: AGPL-3.0-or-later

#' Contiguous residue cropping of AlphaFold training features
#'
#' Supplement section 1.2.8 of Jumper et al. (2021), pp. 7-8.  Training
#' crops the residue dimension of every feature to one contiguous window.
#' Because the window is contiguous and shared by all features, pair
#' features must be cropped on both axes with the same index set, which is
#' what keeps residue \code{i} of the cropped target aligned with row
#' \code{i} of the cropped pair tensor.
#'
#' The crop start is an argument, not sampled here, so the function is
#' deterministic.  The valid range the spec would sample from is returned
#' alongside the crop.
#'
#' @param seqlen Number of residues before cropping.
#' @param cropsize Number of residues to keep.
#' @param start One-based index of the first residue kept.
#' @param target Optional per-residue feature matrix to crop by row.
#' @param pair Optional pair feature matrix to crop on both axes.
#' @param msa Optional MSA feature matrix, cropped along its residue axis.
#' @param mode Either "clamped" or "unclamped".
#' @return A list with the kept indices \code{idx}, the cropped
#'   \code{target}, \code{pair} and \code{msa}, \code{startmax},
#'   \code{estimate} and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. section 1.2.8
#' @examples
#' rmorie:::Alfcrop(seqlen = 5L, cropsize = 5L)
Alfcrop <- function(seqlen, cropsize, start = 1, target = NULL, pair = NULL,
                    msa = NULL, mode = "clamped") {
  if (!mode %in% c("clamped", "unclamped"))
    stop("mode must be 'clamped' or 'unclamped'")
  if (cropsize > seqlen) stop("cropsize ", cropsize, " exceeds seqlen ", seqlen)
  if (start < 1 || start + cropsize - 1 > seqlen)
    stop("crop [", start, ", ", start + cropsize - 1, "] falls outside 1..", seqlen)

  nn <- seqlen - cropsize
  startmax <- nn + 1
  idx <- seq.int(start, start + cropsize - 1L)

  ct <- if (is.null(target)) NULL else target[idx, , drop = FALSE]
  cp <- if (is.null(pair)) NULL else pair[idx, idx, drop = FALSE]
  cmsa <- if (is.null(msa)) NULL else msa[, idx, drop = FALSE]

  list(idx = idx, target = ct, pair = cp, msa = cmsa, startmax = startmax,
       estimate = as.numeric(cropsize), mode = mode,
       method = "AlphaFold contiguous residue cropping")
}
