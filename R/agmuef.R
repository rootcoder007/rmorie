# SPDX-License-Identifier: AGPL-3.0-or-later
#' MuZero pUCT action selection with min-max normalised values
#'
#' score(a) = Qbar(s,a) + P(s,a) sqrt(sum_b N(s,b)) / (1 + N(s,a))
#'            * (c1 + log((sum_b N(s,b) + c2 + 1)/c2)),
#' with Qbar the tree-wide min-max rescaling of Q onto \[0, 1\].
#'
#' @param Q Mean action values at this node.
#' @param N Visit counts, same length as Q.
#' @param P Prior policy, same length as Q.
#' @param c1,c2 Exploration constants; 1.25 and 19652 in the paper.
#' @param qmin,qmax Tree-wide value bounds; NULL uses range(Q).
#'
#' @return List with score, qbar, exploration, best, sumn, k.  best is a
#'   zero-based action index, matching the Python arm.
#' @references Schrittwieser et al. (2020), Nature 588, 604-609;
#'   arXiv:1911.08265, Equation (2) and the paragraph after Equation (4).
#'   Read from the ar5iv rendering of the arXiv source.
#' @export
#' @examples
#' Mzpuct(Q = 0.5, N = 5L, P = 0.5)
Mzpuct <- function(Q, N, P, c1 = 1.25, c2 = 19652, qmin = NULL,
                   qmax = NULL) {
  Q <- .t1_vec(Q)
  N <- .t1_vec(N)
  P <- .t1_vec(P)
  k <- length(Q)
  if (length(N) != k || length(P) != k) {
    stop("Q, N and P must have the same length")
  }
  if (any(N < 0)) stop("visit counts must be non-negative")
  lo <- if (is.null(qmin)) min(Q) else as.numeric(qmin)
  hi <- if (is.null(qmax)) max(Q) else as.numeric(qmax)
  rng <- hi - lo
  qb <- if (rng <= 0) rep(0, k) else (Q - lo) / rng
  sn <- sum(N)
  u <- as.numeric(c1) + log((sn + as.numeric(c2) + 1) / as.numeric(c2))
  ex <- P * sqrt(sn) / (1 + N) * u
  sc <- qb + ex
  .t1_result(
    score = sc, qbar = qb, exploration = ex,
    best = which.max(sc) - 1L, sumn = sn, k = k,
    method = "MuZero pUCT selection (Schrittwieser et al. 2020 eq. 2)"
  )
}
