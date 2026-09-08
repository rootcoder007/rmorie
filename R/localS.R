# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local-shift sensitivity (Hampel et al. 1986)
#'
#' lambda* = sup over x != y of |IF(y) - IF(x)| / |y - x|, the worst effect of
#' shifting an observation slightly.  Source consulted: Hampel, Ronchetti,
#' Rousseeuw and Stahel (1986), Robust Statistics, section 2.1c.
#'
#' @param IF numeric vector of influence-function values on a grid.
#' @param x optional numeric grid; defaults to 0, 1, ..., n-1.
#' @return list: estimate, lambda_star, i, j, n, method.
#' @keywords internal
#' @examples
#' localS(c(0, 1, 1.5), x = c(0, 1, 2))
#' @export
localS <- function(IF, x = NULL) {
  IF <- as.numeric(IF)
  n <- length(IF)
  xg <- if (is.null(x)) seq_len(n) - 1 else as.numeric(x)
  best <- -Inf
  bi <- 0L
  bj <- 0L
  if (n > 1) for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    dx <- xg[j] - xg[i]
    if (dx != 0) {
      v <- abs(IF[j] - IF[i]) / abs(dx)
      if (v > best) { best <- v
      bi <- i - 1L
      bj <- j - 1L }
    }
  }
  lam <- if (n > 1) as.numeric(best) else NA_real_
  list(estimate = lam, lambda_star = lam, i = as.integer(bi), j = as.integer(bj),
       n = as.integer(n),
       method = "Local-shift sensitivity (Hampel et al. 1986)")
}

# CANONICAL TEST
# r <- localS(c(0, 1, 1.5), x = c(0, 1, 2)); stopifnot(abs(r$estimate - 1) < 1e-12)

#' @rdname localS
#' @keywords internal
#' @export
morie_local_shift <- localS
