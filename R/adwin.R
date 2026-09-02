# SPDX-License-Identifier: AGPL-3.0-or-later
#' ADWIN adaptive windowing for change detection
#'
#' Drops observations from the tail of the window while some split
#' W = W0 . W1 has |mean(W0) - mean(W1)| >= eps_cut, where
#' m = 1/(1/n0 + 1/n1), delta' = delta/n and
#' eps_cut = sqrt(log(4/delta') / (2 m)).
#'
#' @param x Stream values, expected in \[0, 1\].
#' @param delta Confidence parameter in (0, 1).
#'
#' @return List with mean, width, window, ndrops, lastcut, changepoints,
#'   n, delta.
#' @references Bifet and Gavalda (2007), Proc. SIAM SDM, 443-448,
#'   Section 3 and Figure 1.  Read from the authors' own PDF at
#'   www.cs.upc.edu/~gavalda.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Adwin(V)
Adwin <- function(x, delta = 0.05) {
  x <- .t1_vec(x)
  delta <- as.numeric(delta)
  if (!(delta > 0 && delta < 1)) stop("delta must lie in (0, 1)")
  W <- numeric(0)
  drops <- 0L
  cuts <- integer(0)
  last <- NA_real_
  for (pos in seq_along(x)) {
    W <- c(W, x[pos])
    shrunk <- TRUE
    while (shrunk && length(W) >= 2) {
      shrunk <- FALSE
      n <- length(W)
      pre <- c(0, cumsum(W))
      for (n0 in seq_len(n - 1L)) {
        n1 <- n - n0
        m <- 1 / (1 / n0 + 1 / n1)
        dp <- delta / n
        cut <- sqrt(log(4 / dp) / (2 * m))
        d <- abs(pre[n0 + 1L] / n0 - (pre[n + 1L] - pre[n0 + 1L]) / n1)
        if (d >= cut) {
          W <- W[-1L]
          drops <- drops + 1L
          last <- cut
          cuts <- c(cuts, pos - 1L)
          shrunk <- TRUE
          break
        }
      }
    }
  }
  n <- length(W)
  .t1_result(
    mean = if (n > 0) sum(W) / n else NA_real_, width = n,
    window = W, ndrops = drops, lastcut = last,
    changepoints = cuts, n = length(x), delta = delta,
    method = "ADWIN adaptive windowing (Bifet-Gavalda 2007 Sect. 3)"
  )
}
