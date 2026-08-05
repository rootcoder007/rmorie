# SPDX-License-Identifier: AGPL-3.0-or-later
#' MCTS selection: the PUCT and UCT action rules
#'
#' Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED), appendix B,
#' prints the selection rule in full: a = argmax_a \[ Q(s,a) + P(s,a)
#' sqrt(sum_b N(s,b)) / (1 + N(s,a)) (c1 + log((sum_b N(s,b) + c2 + 1) /
#' c2)) ], with c1 = 1.25 and c2 = 19652.  The AlphaGo Zero / AlphaZero
#' rule (Silver et al., Nature 550, 354-359; arXiv:1712.01815 -- FETCHED,
#' which states only that its search is identical to AlphaGo Zero) is the
#' c2 -> infinity limit, U(s,a) = c_puct P(s,a) sqrt(sum_b N(s,b)) / (1 +
#' N(s,a)).  rule = "uct" is Kocsis and Szepesvari's original.  Ties break
#' to the lowest action index, never by a draw.
#'
#' @param Q action values.
#' @param N visit counts.
#' @param P policy priors.
#' @param c c_puct, or c1, or the UCT constant.
#' @param rule one of "puct", "muzero", "uct".
#' @param c2 the MuZero c2 constant.
#' @return list: estimate (0-based action), action, scores, u, n_total,
#'   rule, method.
#' @keywords internal
#' @examples
#' Puctsel(c(0.1, 0.4), c(3, 5), c(0.6, 0.4))$action
#' @export
Puctsel <- function(Q, N, P, c = 1.25, rule = "puct", c2 = 19652) {
  q <- .s03vec(Q); n <- .s03vec(N); p <- .s03vec(P)
  m <- length(q)
  tot <- 0
  for (v in n) tot <- tot + v
  root <- if (tot > 0) sqrt(tot) else 0
  u <- numeric(m); scores <- numeric(m)
  for (a in seq_len(m)) {
    if (identical(rule, "uct")) {
      u[a] <- if (n[a] > 0 && tot > 0) c * sqrt(log(tot) / n[a]) else Inf
    } else {
      base <- p[a] * root / (1 + n[a])
      base <- if (identical(rule, "muzero")) base * (c + log((tot + c2 + 1) / c2)) else base * c
      u[a] <- base
    }
    scores[a] <- q[a] + u[a]
  }
  best <- 1L
  if (m > 1L) for (a in seq(2L, m)) if (scores[a] > scores[best]) best <- a
  list(estimate = as.numeric(best - 1L), action = best - 1L, scores = scores,
       u = u, n_total = tot, rule = rule,
       method = paste0("MCTS selection by ", toupper(rule)))
}
