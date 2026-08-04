# SPDX-License-Identifier: AGPL-3.0-or-later
#' Priority targets for a prioritized replay buffer
#'
#' Schaul, Quan, Antonoglou and Silver (2016), Prioritized experience
#' replay, ICLR (arXiv:1511.05952 -- FETCHED), section 3.3 and algorithm
#' 1: P(i) = p_i^alpha / sum_k p_k^alpha and w_i = (N P(i))^(-beta) /
#' max_j w_j, with p_i = |delta_i| + eps (proportional) or 1 / rank(i)
#' (rank-based).  For AlphaZero-style training the TD error delta is
#' replaced by the value residual |z - v|, the value head's own error on
#' the stored outcome.
#'
#' @param replay_buffer the buffer; rows of (z, v) when z and v are absent.
#' @param priorities optional raw priorities.
#' @param z,v stored outcomes and value predictions.
#' @param alpha prioritisation exponent.
#' @param beta importance-sampling exponent.
#' @param eps additive constant of the proportional variant.
#' @param variant "proportional" or "rank".
#' @return list: estimate, prob, weight, priority, n, method.
#' @keywords internal
#' @examples
#' Pertarget(NULL, priorities = c(1, 2, 3))$prob
#' @export
Pertarget <- function(replay_buffer, priorities = NULL, z = NULL, v = NULL,
                      alpha = 0.6, beta = 0.4, eps = 1e-6,
                      variant = "proportional") {
  if (!is.null(priorities)) {
    raw <- .s03vec(priorities)
  } else if (!is.null(z) && !is.null(v)) {
    zz <- .s03vec(z); vv <- .s03vec(v)
    raw <- abs(zz - vv)
  } else {
    rows <- .s03mat(replay_buffer)
    raw <- abs(rows[, 1] - rows[, 2])
  }
  n <- length(raw)
  if (identical(variant, "rank")) {
    ord <- order(-raw, seq_len(n))
    p <- numeric(n)
    for (rk in seq_len(n)) p[ord[rk]] <- 1 / rk
  } else {
    p <- raw + as.numeric(eps)
  }
  pa <- p^as.numeric(alpha)
  tot <- 0
  for (x in pa) tot <- tot + x
  prob <- if (tot > 0) pa / tot else rep(0, n)
  w <- numeric(n)
  for (i in seq_len(n)) w[i] <- if (prob[i] > 0) (n * prob[i])^(-as.numeric(beta)) else 0
  mx <- 0
  for (x in w) if (x > mx) mx <- x
  w <- if (mx > 0) w / mx else rep(0, n)
  list(estimate = if (n) prob[1] else NaN, prob = prob, weight = w,
       priority = p, n = n,
       method = "Prioritized replay priorities from the value residual |z - v|")
}
