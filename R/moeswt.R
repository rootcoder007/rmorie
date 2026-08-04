# SPDX-License-Identifier: AGPL-3.0-or-later
#' Switch Transformer top-1 routing with a capacity factor
#'
#' Fedus, Zoph and Shazeer (2022), Switch transformers, JMLR 23(120),
#' 1-39 (arXiv:2101.03961 -- FETCHED), states verbatim that "each token is
#' routed to the expert with the highest router probability, but each
#' expert has a fixed batch size of (total_tokens / num_experts) x
#' capacity_factor.  If the tokens are unevenly dispatched then certain
#' experts will overflow ... resulting in these tokens not being processed
#' by this layer."  Overflowed tokens pass through the residual unchanged
#' here; they are not silently reassigned.  The auxiliary load-balancing
#' loss is eq. (4), loss = alpha N sum_i f_i P_i, with f_i the dispatched
#' fraction (eq. 5) and P_i the mean router probability (eq. 6); alpha =
#' 1e-2 in the paper.
#'
#' @param y the token batch, one row per token.
#' @param x the token batch; wins over y.
#' @param W_g router weights, features in rows, experts in columns.
#' @param experts expert outputs; identity when omitted.
#' @param capacity the capacity factor.
#' @param alpha auxiliary-loss coefficient.
#' @return list: estimate, aux_loss, assign, dropped, f, P,
#'   expert_capacity, out, method.
#' @keywords internal
#' @examples
#' Moeswitch(matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE), W_g = diag(2))$dropped
#' @export
Moeswitch <- function(y, x = NULL, W_g = NULL, experts = NULL,
                      capacity = 1.25, alpha = 1e-2) {
  toks <- .s03mat(if (!is.null(x)) x else y)
  Tn <- nrow(toks)
  Wg <- .s03mat(W_g)
  N <- ncol(Wg)
  cap <- if (N) as.integer(as.numeric(capacity) * Tn / N) else 0L
  probs <- vector("list", Tn)
  for (t in seq_len(Tn)) probs[[t]] <- .s03softmax(.s03matvec(t(Wg), toks[t, ]))
  used <- integer(N); assign <- rep(-1L, Tn)
  for (t in seq_len(Tn)) {
    best <- 1L
    if (N > 1L) for (i in seq(2L, N)) if (probs[[t]][i] > probs[[t]][best]) best <- i
    if (used[best] < cap) { assign[t] <- best - 1L; used[best] <- used[best] + 1L }
  }
  f <- if (Tn) used / Tn else numeric(N)
  P <- numeric(N)
  for (t in seq_len(Tn)) for (i in seq_len(N)) P[i] <- P[i] + probs[[t]][i] / Tn
  s <- 0
  for (i in seq_len(N)) s <- s + f[i] * P[i]
  aux <- as.numeric(alpha) * N * s
  out <- vector("list", Tn)
  for (t in seq_len(Tn)) {
    a <- assign[t]
    if (a < 0L) {
      out[[t]] <- as.numeric(toks[t, ])
    } else if (is.null(experts)) {
      out[[t]] <- probs[[t]][a + 1L] * as.numeric(toks[t, ])
    } else if (is.function(experts[[a + 1L]])) {
      out[[t]] <- probs[[t]][a + 1L] * .s03vec(experts[[a + 1L]](toks[t, ]))
    } else {
      e <- if (is.matrix(experts)) as.numeric(experts[a + 1L, ]) else .s03vec(experts[[a + 1L]])
      out[[t]] <- probs[[t]][a + 1L] * e
    }
  }
  drop <- 0L
  for (a in assign) if (a < 0L) drop <- drop + 1L
  list(estimate = aux, aux_loss = aux, assign = assign, dropped = drop,
       f = f, P = P, expert_capacity = cap, out = out,
       method = "Switch Transformer top-1 routing with capacity factor (Fedus et al. 2022, eqs. 4-6)")
}
