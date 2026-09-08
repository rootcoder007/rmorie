# SPDX-License-Identifier: AGPL-3.0-or-later
#' Grover quantum search
#'
#' Each Grover iteration -- oracle sign flip followed by inversion about
#' the mean -- rotates the state by 2 theta in the plane spanned by the
#' marked and unmarked uniform states.  The N = 4, M = 1 case gives
#' theta = pi/6 and success probability exactly 1 after a single query.
#'
#' Formula: P(k) = sin^2((2k + 1) theta), sin theta = sqrt(M/N),
#'   k* = round(pi/(4 theta) - 1/2).
#'
#' @param oracle Length-N vector of 0/1 marks.
#' @param N Search space size.
#' @return List with \code{estimate}, \code{p_success},
#'   \code{p_closed_form}, \code{p_path}, \code{k_opt}, \code{theta},
#'   \code{n}, \code{method}.
#' @references Grover (1996), A fast quantum mechanical algorithm for
#'   database search, STOC '96, pp. 212-219. \doi{10.1145/237814.237866}
#' @export
#' @examples
#' oracle <- rep(0, 8)
#' oracle[4] <- 1
#' GroverS(oracle, N = 8)
GroverS <- function(oracle, N) {
  mark <- as.integer(.s03vec(oracle))
  n <- as.integer(N)
  if (n < 2L) stop("grover_search: N must be at least 2")
  if (length(mark) != n) stop("grover_search: oracle must have N entries")
  if (any(!(mark %in% c(0L, 1L)))) stop("grover_search: oracle entries must be 0 or 1")
  M <- sum(mark)
  if (M == 0L || M == n) stop("grover_search: need at least one marked and one unmarked item")
  theta <- asin(sqrt(M / n))
  kopt <- as.integer(floor(pi / (4 * theta) - 0.5 + 0.5))
  amp <- rep(1 / sqrt(n), n)
  probs <- sum(amp[mark == 1L]^2)
  for (k in seq_len(max(kopt, 1L))) {
    amp <- ifelse(mark == 1L, -amp, amp)
    amp <- 2 * mean(amp) - amp
    probs <- c(probs, sum(amp[mark == 1L]^2))
  }
  .t1_result(estimate = probs[kopt + 1L], p_success = probs[kopt + 1L],
             p_closed_form = sin((2 * kopt + 1) * theta)^2, p_path = probs,
             k_opt = kopt, theta = theta, n = n,
             method = "P(k) = sin^2((2k+1) theta), sin theta = sqrt(M/N), Grover (1996)")
}
