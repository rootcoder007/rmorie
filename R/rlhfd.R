# SPDX-License-Identifier: AGPL-3.0-or-later

#' RLHF linear reward head (Ouyang 2022)
#'
#' @param x Input matrix (rows = items, cols = features).
#' @param w Optional weight vector.
#' @param b Numeric bias (default 0).
#' @return Named list with value, tensor, w, b, method.
#' @keywords internal
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rlhf_reward(V)
rlhf_reward <- function(x, w = NULL, b = 0) {
  xm <- as.matrix(x)
  d <- ncol(xm)
  if (is.null(w)) w <- rep(1 / d, d)
  if (length(w) != d) stop(sprintf("w must have length %d", d))
  r <- as.numeric(xm %*% w + b)
  list(
    value = r[1L], tensor = r,
    w = as.numeric(w), b = b, method = "rlhf-reward-head"
  )
}
