# SPDX-License-Identifier: AGPL-3.0-or-later
#' Draw one systematic sample with a random start, and estimate from it.
#'
#' The start comes from a pinned Lehmer generator so the two language
#' arms produce the SAME sample; vary \code{seed} for a different start.
#' Indices are one-based.
#'
#' Formula: r ~ Uniform\{1..k\}; sample = \{r, r+k, r+2k, ...\};
#'   ybar_sy = mean of the sample
#'
#' @param y The whole population, in sampling order.
#' @param k Sampling interval.
#' @param seed Seed for the pinned generator that picks the start.
#' @return List with \code{start}, \code{index}, \code{sample},
#'   \code{estimate}, \code{design_se}, \code{population_mean}, \code{N},
#'   \code{n}, \code{k}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   8: a unit is chosen at random from the first k, and every k-th unit
#'   thereafter. Chapter 8 was NOT in the scanned excerpt available to
#'   this batch, so the standard published form is used. The design
#'   standard error is the exact one computed by the sibling module
#'   systmp.
#' @export
#' @examples
#' Sysrs(y = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), k = 3L)
Sysrs <- function(y, k, seed = 1) {
  y <- .t1_vec(y); N <- length(y); k <- as.integer(k)
  if (k < 1L) stop("the interval k must be at least 1")
  if (N %% k != 0L) stop("length(y) must be an exact multiple of k")
  n <- N %/% k
  if (n < 2L) stop("each systematic sample needs at least two units")
  g <- .t1_lcg(seed)
  r <- as.integer(g$unif() * k)
  if (r >= k) r <- k - 1L
  idx <- seq(r + 1L, N, by = k)
  smp <- y[idx]
  Yb <- mean(y)
  means <- vapply(seq_len(k), function(i) mean(y[seq(i, N, by = k)]), 0)
  V <- sum((means - Yb)^2) / k
  .t1_result(start = r + 1L, index = idx, sample = smp,
             estimate = mean(smp), design_se = sqrt(V),
             population_mean = Yb, N = N, n = n, k = k,
             method = "Systematic sample with a pinned random start")
}
