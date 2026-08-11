# SPDX-License-Identifier: AGPL-3.0-or-later
#' Report Noisy Max
#'
#' Adds independent Laplace noise with scale
#' \eqn{\Delta / \epsilon} to each counting query and returns the index
#' of the largest noisy count. For monotone sensitivity-1 counting
#' queries the released index is (epsilon, 0)-differentially private
#' (Dwork-Roth 2014, Claim 3.9); the winning noisy count may be released
#' at no extra privacy cost, the losing counts must not be.
#'
#' Determinism: noise comes from the shared Lehmer minstd stream, so a
#' given seed reproduces the same selection in both language arms.
#' Laplace draws by inverse CDF,
#' \eqn{x = -b\, sign(u - 1/2) \log(1 - 2 |u - 1/2|)}. Ties: first
#' maximum in scan order wins in both arms.
#'
#' @param counts Values of the m counting queries.
#' @param epsilon Privacy budget, positive.
#' @param sensitivity Per-query sensitivity (1 for counting queries).
#' @param seed Seed for the shared deterministic stream.
#' @return List with \code{index} (0-based argmax), \code{winner},
#'   \code{estimate}, \code{epsilon}, \code{scale}, \code{n}.
#' @references Dwork, C., and Roth, A. (2014). The algorithmic
#'   foundations of differential privacy. FnT-TCS 9(3-4), 211-487.
#'   Section 3.3, Report Noisy Max and Claim 3.9.
#'   Local source: fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf
#' @export
Reportm <- function(counts, epsilon, sensitivity = 1, seed = 1) {
  x <- as.numeric(unlist(counts))
  n <- length(x)
  if (n == 0) stop("counts must be non-empty", call. = FALSE)
  eps <- as.numeric(epsilon)
  if (eps <= 0) stop("epsilon must be positive", call. = FALSE)
  b <- as.numeric(sensitivity) / eps
  if (b <= 0) stop("sensitivity must be positive", call. = FALSE)
  g <- .t1_lcg(seed)
  best <- -Inf
  idx <- -1L
  for (i in seq_len(n)) {
    u <- g$unif()
    h <- u - 0.5
    s <- if (h > 0) 1 else if (h < 0) -1 else 0
    noisy <- x[i] - b * s * log(1 - 2 * abs(h))
    if (noisy > best) {
      best <- noisy
      idx <- i - 1L
    }
  }
  .t1_result(index = as.numeric(idx), winner = best,
             estimate = as.numeric(idx), epsilon = eps, scale = b, n = n,
             method = "Report Noisy Max (Dwork-Roth 2014, Claim 3.9)")
}
