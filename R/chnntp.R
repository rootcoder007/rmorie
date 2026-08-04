# SPDX-License-Identifier: AGPL-3.0-or-later
#' Channel capacity by the Blahut-Arimoto algorithm.
#'
#' Formula: q(x|y) = r(x) P(y|x) / sum_x' r(x') P(y|x'); r(x) <- exp(sum_y P(y|x) log q(x|y)) normalised; C = max_r I(X;Y)
#'
#' @param P Channel matrix; row x is the output distribution given input x.
#' @param iters Fixed number of alternations.

#' @return List with ``capacity_bits``, ``capacity_nats``, ``input_dist``, ``trace`` (nats per iteration), ``iterations``.
#' @references Blahut (1972), Computation of channel capacity and rate-distortion functions, IEEE Transactions on Information Theory 18:460-473; Arimoto (1972), same volume, 14-20. Neither is held locally; the alternating update is the standard published form of the algorithm.
#' @export
Chancap <- function(P, iters = 200) {
  P <- as.matrix(P); m <- nrow(P); n <- ncol(P)
  if (any(P < 0)) stop("channel probabilities must be non-negative")
  if (any(abs(rowSums(P) - 1) > 1e-9)) stop("each row of P must sum to 1")
  r <- rep(1 / m, m); trace <- numeric(0)
  lr <- function(r) {
    qy <- as.numeric(t(P) %*% r)
    vapply(seq_len(m), function(i) {
      k <- P[i, ] > 0 & qy > 0
      sum(P[i, k] * log(P[i, k] / qy[k]))
    }, numeric(1))
  }
  for (it in seq_len(as.integer(iters))) {
    lg <- lr(r)
    w <- r * exp(lg - max(lg))
    r <- w / sum(w)
    trace <- c(trace, sum(r * lg))
  }
  qy <- as.numeric(t(P) %*% r)
  cap <- 0
  for (i in seq_len(m)) {
    k <- P[i, ] > 0 & qy > 0
    cap <- cap + r[i] * sum(P[i, k] * log(P[i, k] / qy[k]))
  }
  .t1_result(capacity_bits = cap / log(2), capacity_nats = cap,
             input_dist = r, trace = trace, iterations = as.integer(iters),
             method = "Channel capacity (Blahut-Arimoto)")
}
