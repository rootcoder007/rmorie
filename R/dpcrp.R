# SPDX-License-Identifier: AGPL-3.0-or-later
#' Chinese restaurant process seating
#'
#' Formula: P(z_i = k) = n_k / (i - 1 + alpha) for an occupied table k, and alpha / (i -
#' 1 + alpha) for a new one
#'
#' @param n Number of customers.
#' @param alpha Concentration parameter, positive.
#' @param u Caller-supplied uniforms, one per customer.
#' @param seed Seed of the shared minstd stream when ``u`` is omitted.

#' @param n See Usage.
#' @param alpha See Usage.
#' @param u See Usage.
#' @param seed See Usage.
#' @return List with ``table`` (assignment per customer), ``counts``, ``n_tables``,
#' ``expected_tables``, ``alpha``, ``n``.
#' @references Aldous (1985), Exchangeability and related topics, Ecole d'Ete de
#' Probabilites de Saint-Flour XIII; Pitman (2006), Combinatorial Stochastic Processes.
#' Neither is held locally; the seating rule and the E\[K\] = sum_i alpha/(alpha + i - 1)
#' identity are the standard published forms.
#' @export
#' @examples
#' Crp(n = 5L)
Crp <- function(n, alpha = 1, u = NULL, seed = 1) {
  n <- as.integer(n)
  a <- as.numeric(alpha)
  if (a <= 0) stop("alpha must be positive")
  if (n < 1) stop("n must be at least 1")
  us <- if (is.null(u)) NULL else .t1_vec(u)
  g <- if (is.null(us)) .t1_lcg(seed) else NULL
  counts <- numeric(0)
  table <- integer(n)
  for (i in seq_len(n)) {
    draw <- if (is.null(us)) g$unif() else us[i]
    tot <- (i - 1) + a
    pick <- -1L
    if (length(counts)) {
      cw <- cumsum(counts / tot)
      w <- which(draw < cw)
      if (length(w)) pick <- w[1] - 1L
    }
    if (pick < 0L) { counts <- c(counts, 0)
    pick <- length(counts) - 1L }
    counts[pick + 1L] <- counts[pick + 1L] + 1
    table[i] <- pick
  }
  .t1_result(table = table, counts = counts, n_tables = length(counts),
             expected_tables = sum(a / (a + (0:(n - 1)))), alpha = a, n = n,
             method = "Chinese restaurant process")
}
