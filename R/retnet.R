# SPDX-License-Identifier: AGPL-3.0-or-later
#' RetNet's retention mechanism, in both its forms
#'
#' Sun et al. (2023), Retentive network: a successor to transformer for
#' large language models, arXiv:2307.08621 (FETCHED).  The recurrent form,
#' eqs. (5)-(6), is S_n = gamma S_(n-1) + K_n' V_n and O_n = Q_n S_n; the
#' parallel form contracts the same computation into a masked matrix
#' product, Retention(X) = (Q K' * D) V with D_(nm) = gamma^(n-m) for n >=
#' m and 0 otherwise.  The paper's claim is that the two agree exactly, so
#' both are computed and their maximum discrepancy is returned as
#' max_gap -- the check that the identity holds in this implementation,
#' not merely in the paper.
#'
#' @param y alternative slot for Q (first, for signature stability).
#' @param Q,K,V query, key and value sequences, one row per step.
#' @param gamma the decay.
#' @return list: estimate, out, out_par, max_gap, state, gamma, method.
#' @keywords internal
#' @examples
#' Retention(matrix(1, 2, 2), K = matrix(1, 2, 2), V = matrix(1, 2, 2))$max_gap
#' @export
Retention <- function(y, Q = NULL, K = NULL, V = NULL, gamma = 0.9) {
  Qm <- .s03mat(if (!is.null(Q)) Q else y)
  Km <- .s03mat(K)
  Vm <- .s03mat(V)
  n <- nrow(Qm)
  dk <- ncol(Qm)
  dv <- ncol(Vm)
  g <- as.numeric(gamma)
  S <- matrix(0, dk, dv)
  out <- matrix(0, n, dv)
  for (t in seq_len(n)) {
    for (a in seq_len(dk)) for (b in seq_len(dv)) {
      S[a, b] <- g * S[a, b] + Km[t, a] * Vm[t, b]
    }
    for (b in seq_len(dv)) {
      s <- 0
      for (a in seq_len(dk)) s <- s + Qm[t, a] * S[a, b]
      out[t, b] <- s
    }
  }
  par <- matrix(0, n, dv)
  for (t in seq_len(n)) {
    for (m in seq_len(t)) {
      qk <- 0
      for (a in seq_len(dk)) qk <- qk + Qm[t, a] * Km[m, a]
      w <- qk * (g^(t - m))
      for (b in seq_len(dv)) par[t, b] <- par[t, b] + w * Vm[m, b]
    }
  }
  gap <- 0
  for (t in seq_len(n)) for (b in seq_len(dv)) {
    d <- abs(out[t, b] - par[t, b])
    if (d > gap) gap <- d
  }
  list(estimate = if (n && dv) out[1, 1] else NaN, out = out, out_par = par,
       max_gap = gap, state = S, gamma = g,
       method = "RetNet retention, recurrent and parallel forms (Sun et al. 2023, eqs. 5-6)")
}
