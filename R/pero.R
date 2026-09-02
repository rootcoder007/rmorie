# SPDX-License-Identifier: AGPL-3.0-or-later
#' Prioritized experience replay
#'
#' Schaul, Quan, Antonoglou and Silver (2016), ICLR (arXiv:1511.05952 --
#' FETCHED).  Section 3.3: P(i) = p_i^alpha / sum_k p_k^alpha (eq. 1),
#' with p_i = |delta_i| + eps (proportional) or 1 / rank(i) (rank-based).
#' Section 3.4: w_i = ((1/N)(1/P(i)))^beta (eq. 2), and, verbatim, "For
#' stability reasons, we always normalize weights by 1/max_i w_i so that
#' they only scale the update downwards"; algorithm 1 writes the same as
#' w_j = (N P(j))^(-beta) / max_i w_i.  The paper's annealing of beta
#' towards 1 is available via t and T.
#'
#' Determinism: an optional draw uses the inverse CDF of P at van der
#' Corput points, never a pseudo-random stream.
#'
#' @param buffer the TD errors delta_i.
#' @param alpha prioritisation exponent.
#' @param beta importance-sampling exponent.
#' @param eps additive constant of the proportional variant.
#' @param variant "proportional" or "rank".
#' @param t,T anneal beta linearly to 1 at t = T.
#' @param n_sample draw this many indices deterministically.
#' @return list: estimate, prob, weight, priority, beta_t, sample, n,
#'   method.
#' @keywords internal
#' @examples
#' Persample(c(0.1, -2, 0.5))$prob
#' @export
Persample <- function(buffer, alpha = 0.6, beta = 0.4, eps = 1e-6,
                      variant = "proportional", t = NULL, T = NULL,
                      n_sample = NULL) {
  d <- .s03vec(buffer)
  n <- length(d)
  if (identical(variant, "rank")) {
    ord <- order(-abs(d), seq_len(n))
    p <- numeric(n)
    for (rk in seq_len(n)) p[ord[rk]] <- 1 / rk
  } else {
    p <- abs(d) + as.numeric(eps)
  }
  pa <- p^as.numeric(alpha)
  tot <- 0
  for (x in pa) tot <- tot + x
  prob <- if (tot > 0) pa / tot else rep(0, n)
  b <- as.numeric(beta)
  if (!is.null(t) && !is.null(T) && as.numeric(T) > 0) {
    b <- b + (1 - b) * (as.numeric(t) / as.numeric(T))
    if (b > 1) b <- 1
  }
  w <- numeric(n)
  for (i in seq_len(n)) w[i] <- if (prob[i] > 0) (n * prob[i])^(-b) else 0
  mx <- 0
  for (x in w) if (x > mx) mx <- x
  w <- if (mx > 0) w / mx else rep(0, n)
  sample_ <- integer(0)
  if (!is.null(n_sample)) {
    cum <- numeric(n)
    cc <- 0
    for (i in seq_len(n)) { cc <- cc + prob[i]
    cum[i] <- cc }
    for (j in seq_len(as.integer(n_sample)) - 1L) {
      u <- .s03vdc(j, 2L)
      idx <- n - 1L
      for (i in seq_len(n)) if (u < cum[i]) { idx <- i - 1L
      break }
      sample_ <- c(sample_, as.integer(idx))
    }
  }
  list(estimate = if (n) prob[1] else NaN, prob = prob, weight = w,
       priority = p, beta_t = b, sample = sample_, n = n,
       method = "Prioritized experience replay (Schaul et al. 2016, eqs. 1-2)")
}
