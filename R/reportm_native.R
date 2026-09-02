# Report Noisy Max: differentially private argmax of a count vector.
# Source: Dwork, C. and Roth, A. (2014), The Algorithmic Foundations
# of Differential Privacy, Foundations and Trends in Theoretical
# Computer Science 9(3-4), Sec. 3.3: "Add independently generated
# Laplace noise Lap(1/epsilon) to each count and return the index of
# the largest noisy count", whose privacy is their Claim 3.9 (the
# algorithm is (epsilon, 0)-differentially private).  Only the INDEX
# is released -- the information-minimisation point of the section,
# which is why the vector of noisy counts is not returned.
#
# Native implementation mirroring Python morie.fn.reportm exactly: the
# same Lehmer minstd stream and the same inverse-CDF Laplace draw, so
# the selected index is bit-identical across the two arms.

# Lehmer minstd: s <- 48271 s mod (2^31 - 1).  Every intermediate fits
# exactly in a double, which is what makes the streams identical.
#' Lehmer minstd: s <- 48271 s mod (2^31 - 1).  Every intermediate fits
#'
#' exactly in a double, which is what makes the streams identical.
#'
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return The value of \code{e}, as built in the body.
#' @export
.mor_lcg_new <- function(seed = 1) {
  s <- as.numeric(seed) %% 2147483647
  e <- new.env(parent = emptyenv())
  e$s <- if (s > 0) s else 1
  e
}

#' .mor_lcg_unif
#'
#' A step of the reportm_native implementation. Called by \code{morie_reportm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param e A list; the body reads \code{$s} from it.
#' @return A numeric value.
#' @export
.mor_lcg_unif <- function(e) {
  e$s <- (48271 * e$s) %% 2147483647
  e$s / 2147483647
}

#' Report Noisy Max
#'
#' Adds Laplace(\code{sensitivity}/\code{epsilon}) noise to each count
#' and returns the index of the largest noisy count (Dwork and Roth
#' 2014, Sec. 3.3).  By Claim 3.9 the mechanism is
#' \eqn{(\varepsilon, 0)}-differentially private even though the
#' count vector itself has \eqn{\ell_1} sensitivity equal to its
#' length: releasing only the argmax costs no more than one query.
#'
#' @param counts Numeric vector of counts.
#' @param epsilon Privacy parameter, positive.
#' @param sensitivity Per-count sensitivity, default 1.
#' @param seed Seed of the reproducible stream shared with the Python
#'   arm.
#' @return A list with \code{index} (0-based winning index),
#'   \code{winner} (its noisy value), \code{estimate},
#'   \code{epsilon}, \code{scale}, \code{n}, \code{method}.
#' @references Dwork, C. and Roth, A. (2014). The Algorithmic
#'   Foundations of Differential Privacy. Foundations and Trends in
#'   Theoretical Computer Science, 9(3-4), Claim 3.9.
#' @export
#' @examples
#' morie_reportm(counts = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L)
morie_reportm <- function(counts, epsilon, sensitivity = 1, seed = 1) {
  x <- as.numeric(counts)
  n <- length(x)
  if (n == 0L) stop("counts must be non-empty")
  eps <- as.numeric(epsilon)
  if (eps <= 0) stop("epsilon must be positive")
  b <- as.numeric(sensitivity) / eps
  if (b <= 0) stop("sensitivity must be positive")
  g <- .mor_lcg_new(seed)
  best <- -Inf
  idx <- -1L
  for (i in seq_len(n)) {
    u <- .mor_lcg_unif(g)
    h <- u - 0.5
    s <- if (h > 0) 1 else if (h < 0) -1 else 0
    noisy <- x[i] - b * s * log(1 - 2 * abs(h))
    if (noisy > best) { best <- noisy
    idx <- i - 1L }
  }
  list(index = as.numeric(idx), winner = best, estimate = as.numeric(idx),
       epsilon = eps, scale = b, n = n,
       method = "Report Noisy Max (Dwork-Roth 2014, Claim 3.9)")
}
