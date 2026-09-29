#' Attainable exact sizes of a test with a discrete null distribution
#'
#' For a discrete statistic only a finite ladder of significance levels is
#' attainable. Returns the tail probability at every cut point and the exact
#' size of the test, the largest attainable size not above \code{alpha},
#' with its cut point (Gibbons and Chakraborti 2011, section 1.2.9).
#' Identical to the Python arm \code{morie.fn.gb_t1e.exactsize}.
#'
#' @param pmf Null probabilities over the support, in increasing order of the
#'   statistic.
#' @param alpha Nominal level.
#' @param upper \code{TRUE} for an upper-tail rejection region.
#' @return A list with \code{sizes} (tail probability at each cut point),
#'   \code{alpha_exact}, \code{cut} (zero-based index into the support, -1 if
#'   none), \code{nlevels} and \code{method}.
#' @references Gibbons, J. D. and Chakraborti, S. (2011). Nonparametric
#'   Statistical Inference, 5th ed. CRC Press, section 1.2.9.
#' @examples
#' exactsize(choose(5, 0:5) / 32, alpha = 0.2)$alpha_exact
#' @export
exactsize <- function(pmf, alpha = 0.05, upper = TRUE) {
  p <- as.numeric(pmf)
  k <- length(p)
  if (k < 1) stop("pmf must be non-empty.")
  sizes <- if (upper) rev(cumsum(rev(p))) else cumsum(p)
  idx <- if (upper) seq_len(k) else rev(seq_len(k))
  best <- NaN
  cut <- -1L
  for (i in idx) {
    if (sizes[i] <= alpha) {
      best <- sizes[i]
      cut <- i - 1L
      break
    }
  }
  list(sizes = sizes, alpha_exact = best, cut = cut, nlevels = k,
       method = "attainable exact sizes of a discrete test (Sec. 1.2.9)")
}

#' @rdname exactsize
#' @export
gibbons_type1_error <- exactsize
