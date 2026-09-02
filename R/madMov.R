# SPDX-License-Identifier: AGPL-3.0-or-later
#' Moving (rolling) median absolute deviation
#'
#' The MAD of each trailing window of length `window`; output element j covers
#' x\[j:(j+window-1)\].  Source consulted: Hampel (1974), JASA 69(346), 383-393.
#'
#' @param x series.
#' @param window window length.
#' @param constant consistency factor.
#' @return list: estimate, values, centers, window, n, method.
#' @keywords internal
#' @examples
#' madMov(c(1, 2, 3, 4, 5, 6), 3)$values
#' @export
madMov <- function(x, window, constant = 1.4826) {
  v <- as.numeric(x)
  w <- as.integer(window)
  n <- length(v)
  m <- n - w + 1L
  vals <- numeric(max(m, 0L))
  ctrs <- numeric(max(m, 0L))
  if (m >= 1L) for (j in seq_len(m)) {
    seg <- v[j:(j + w - 1L)]
    cc <- stats::median(seg)
    vals[j] <- constant * stats::median(abs(seg - cc))
    ctrs[j] <- cc
  }
  list(estimate = if (m >= 1L) vals[m] else NA_real_, values = vals,
       centers = ctrs, window = w, n = n,
       method = "Moving median absolute deviation (Hampel 1974)")
}

# CANONICAL TEST
# r <- madMov(c(1,2,3,4,5,6), 3)
# stopifnot(length(r$values) == 4L, abs(r$values[1] - 1.4826) < 1e-12)

#' @rdname madMov
#' @keywords internal
#' @export
morie_madMov <- madMov
