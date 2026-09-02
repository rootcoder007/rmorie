# SPDX-License-Identifier: AGPL-3.0-or-later
#' Individual fairness audit against a Lipschitz constraint
#'
#' Dwork et al.'s condition is that similar individuals receive similar
#' treatment, made precise as a Lipschitz constraint on the map from
#' individuals to outcomes: \code{|h(x) - h(x')| <= L d(x, x')} for
#' every pair. A pair violates it when the score gap exceeds \code{L}
#' times the distance. The audit reports the worst offending pair, the
#' violation count, and the smallest \code{L} that would make the given
#' scores admissible,
#' \code{L_required = max |h(x) - h(x')| / d(x, x')}, the Lipschitz
#' seminorm of \code{h} on the observed pairs.
#'
#' Pairs at distance zero are handled separately: two individuals the
#' metric cannot tell apart must receive the SAME score, so any gap is
#' a violation and no finite \code{L} repairs it. Dividing by zero
#' there would report Inf for a genuine violation and NaN for a
#' compliant identical pair, which is the wrong way round.
#'
#' Pairs are scanned in the order supplied and the running maximum uses
#' a STRICT comparison, so a tie keeps the earlier pair in both
#' language arms.
#'
#' @param h_values Scores assigned by the classifier, one per person.
#' @param x_pairs Two-column matrix of 0-based index pairs to audit.
#' @param L Lipschitz constant, non-negative.
#' @param metric Distance for each pair, or \code{NULL} for all ones.
#' @return List with \code{estimate}, \code{L_required},
#'   \code{n_violations}, \code{violation_rate}, \code{max_gap},
#'   \code{max_pair_i}, \code{max_pair_j}, \code{fair}, \code{L},
#'   \code{n}, \code{n_pairs}.
#' @references Dwork, C., Hardt, M., Pitassi, T., Reingold, O. &
#'   Zemel, R. (2012). Fairness through awareness. Proceedings of the
#'   3rd Innovations in Theoretical Computer Science Conference,
#'   214-226. doi:10.1145/2090236.2090255
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Objfair(V, M)
Objfair <- function(h_values, x_pairs, L = 1, metric = NULL) {
  h <- as.numeric(h_values)
  n <- length(h)
  if (n == 0L) stop("Objfair: h_values is empty")
  lam <- as.numeric(L)
  if (lam < 0) stop("Objfair: L must be non-negative")
  P <- matrix(as.integer(as.matrix(x_pairs)), ncol = 2)
  m <- nrow(P)
  if (m == 0L) stop("Objfair: x_pairs is empty")
  if (any(P < 0L) || any(P > n - 1L)) stop("Objfair: pair index out of range")
  if (is.null(metric)) {
    d <- rep(1, m)
  } else {
    d <- as.numeric(metric)
    if (length(d) != m) stop("Objfair: metric and x_pairs differ in length")
    if (any(d < 0)) stop("Objfair: distances must be non-negative")
  }
  viol <- 0L; lreq <- 0; maxgap <- 0; bi <- -1L; bj <- -1L
  for (k in seq_len(m)) {
    i <- P[k, 1] + 1L; j <- P[k, 2] + 1L
    gap <- abs(h[i] - h[j])
    if (gap > maxgap) { maxgap <- gap; bi <- P[k, 1]; bj <- P[k, 2] }
    if (d[k] == 0) {
      if (gap > 0) { viol <- viol + 1L; lreq <- Inf }
    } else {
      if (gap > lam * d[k]) viol <- viol + 1L
      ratio <- gap / d[k]
      if (ratio > lreq) lreq <- ratio
    }
  }
  .t1_result(estimate = lreq, L_required = lreq, n_violations = viol,
             violation_rate = viol / m, max_gap = maxgap,
             max_pair_i = bi, max_pair_j = bj,
             fair = if (viol == 0L) 1 else 0, L = lam, n = n, n_pairs = m,
             method = "Individual fairness audit (Dwork et al. 2012)")
}
