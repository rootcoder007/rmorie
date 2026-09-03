# SPDX-License-Identifier: AGPL-3.0-or-later
#' Upper record times and counts in a sequence
#'
#' Formula: R_n = sum_i 1\{X_i > max(X_1..X_\{i-1\})\}; E\[R_n\] = sum_i 1/i; Var\[R_n\]
#' = sum_i (1/i - 1/i^2)
#'
#' @param x The sequence, in observation order.

#' @param x See Usage.
#' @return List with ``count``, ``times`` (indices of records), ``values``, ``expected``,
#' ``variance``, ``z``, ``n``.
#' @references Arnold, Balakrishnan and Nagaraja (1998), Records, Wiley. Not held
#' locally; the indicator representation and the resulting harmonic mean and variance are
#' standard published results for iid continuous data.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Evtrecords(V)
Evtrecords <- function(x) {
  x <- .t1_vec(x)
  n <- length(x)
  if (n < 1) stop("need at least one observation")
  cm <- cummax(x)
  isrec <- c(TRUE, x[-1] > cm[-n])
  times <- which(isrec) - 1L
  i <- seq_len(n)
  ev <- sum(1 / i)
  vv <- sum(1 / i - 1 / i^2)
  .t1_result(count = length(times), times = times, values = x[isrec],
             expected = ev, variance = vv,
             z = if (vv > 0) (length(times) - ev) / sqrt(vv) else NA_real_,
             n = n, method = "Upper record times and counts")
}
