# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sequential composition of k differentially private mechanisms
#'
#' Dwork and Roth's basic composition theorem: if M_i is
#' (eps_i, delta_i)-differentially private for i in 1..k, then the
#' mechanism returning all k outputs is
#' (sum eps_i, sum delta_i)-differentially private -- "the epsilons and
#' the deltas add up".  The budget consumed is therefore the plain sum,
#' whatever the mechanisms are, and the Laplace scale a sensitivity-1
#' query can afford at step i is 1/eps_i.
#'
#' Formula: eps_total = sum_i eps_i, delta_total = sum_i delta_i.
#'
#' @param y Data the mechanisms are run on; only the length is reported.
#' @param epsilons Per-mechanism privacy parameters, strictly positive.
#' @param deltas Per-mechanism delta parameters; \code{NULL} means pure
#'   differential privacy (all zero).
#' @return List with \code{estimate}, \code{epsilon_total},
#'   \code{delta_total}, \code{k}, \code{epsilon_max},
#'   \code{epsilon_min}, \code{epsilon_mean}, \code{laplace_scale},
#'   \code{pure_dp}, \code{n}, \code{method}.
#' @references Dwork and Roth (2014), The Algorithmic Foundations of
#'   Differential Privacy, Foundations and Trends in Theoretical
#'   Computer Science 9(3-4):211-407, section 3.5, Theorem 3.16.
#'   \doi{10.1561/0400000042}
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Kcompo(V, V)
Kcompo <- function(y, epsilons, deltas = NULL) {
  eps <- .s03vec(epsilons)
  if (length(eps) == 0L) stop("k_step_dp_composition: epsilons is empty")
  if (any(eps <= 0)) stop("k_step_dp_composition: epsilons must be positive")
  if (is.null(deltas)) {
    dl <- rep(0, length(eps))
  } else {
    dl <- .s03vec(deltas)
    if (length(dl) != length(eps)) stop("k_step_dp_composition: deltas and epsilons have different lengths")
    if (any(dl < 0 | dl >= 1)) stop("k_step_dp_composition: deltas must lie in [0, 1)")
  }
  n <- if (!is.null(y)) length(.s03vec(y)) else 0L
  tot <- sum(eps)
  .t1_result(estimate = tot, epsilon_total = tot, delta_total = sum(dl),
             k = length(eps), epsilon_max = max(eps), epsilon_min = min(eps),
             epsilon_mean = tot / length(eps), laplace_scale = 1 / eps,
             pure_dp = if (sum(dl) == 0) 1 else 0, n = n,
             method = "eps_total = sum eps_i, delta_total = sum delta_i, Dwork & Roth (2014) Thm 3.16")
}
