# SPDX-License-Identifier: AGPL-3.0-or-later
#' Every k-th unit: all k systematic samples and the exact variance.
#'
#' A systematic sample has only k possible outcomes, so its design
#' variance is exact rather than estimated. \code{deff} is its ratio to
#' the simple-random-sampling variance.
#'
#' Formula: V(ybar_sy) = (1/k) sum_\{i=1\}^\{k\} (ybar_i - Ybar)^2;
#'   deff = V(ybar_sy) / \[(1 - f) S^2 / n\]
#'
#' @param y The WHOLE population, in the order it would be sampled;
#'   \code{length(y)} must be an exact multiple of k.
#' @param k Sampling interval.
#' @return List with \code{means}, \code{population_mean},
#'   \code{variance}, \code{se}, \code{srs_variance}, \code{deff},
#'   \code{rho}, \code{N}, \code{n}, \code{k}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   8. Chapter 8 was NOT in the scanned excerpt available to this batch,
#'   so the standard published form is used; rho is reported by inverting
#'   V(ybar_sy) = (S^2/n)\[1 + (n - 1) rho\].
#' @export
#' @examples
#' Sysamp(y = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), k = 3L)
Sysamp <- function(y, k) {
  y <- .t1_vec(y); N <- length(y); k <- as.integer(k)
  if (k < 1L) stop("the interval k must be at least 1")
  if (N %% k != 0L) stop("length(y) must be an exact multiple of k")
  n <- N %/% k
  if (n < 2L) stop("each systematic sample needs at least two units")
  Yb <- mean(y)
  means <- vapply(seq_len(k), function(i) mean(y[seq(i, N, by = k)]), 0)
  V <- sum((means - Yb)^2) / k
  S2 <- stats::var(y)
  Vsrs <- (1 - n / N) * S2 / n
  rho <- if (S2 > 0 && n > 1L) ((V * n / S2) - 1) / (n - 1) else NaN
  .t1_result(means = means, population_mean = Yb, variance = V,
             se = sqrt(V), srs_variance = Vsrs,
             deff = if (Vsrs > 0) V / Vsrs else NaN, rho = rho,
             N = N, n = n, k = k,
             method = "Systematic sampling, exact design variance")
}
