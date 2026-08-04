# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior predictive p-value comparing T(y, theta) to T(y_rep, theta).
#'
#' The comparison is made WITHIN each draw, the same theta on both sides.
#' BDA3 flags values below 0.01 or above 0.99 as major failures.
#'
#' Formula: p_B ~= (1/S) sum_s 1\{ T(y_rep^s, theta^s) >= T(y, theta^s) \}
#'
#' @param t_obs T(y, theta^s) per draw, or a scalar.
#' @param t_rep T(y_rep^s, theta^s), one per draw.
#' @return List with \code{p_value}, \code{p_two_sided},
#'   \code{n_extreme}, \code{t_obs_mean}, \code{t_rep_mean}, \code{S},
#'   \code{extreme}.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari & Rubin (2013),
#'   Bayesian Data Analysis, 3rd edition, Section 6.3. Fetched as the full
#'   text of the book from the author's own copy.
#' @export
Ppcrep <- function(t_obs, t_rep) {
  tr <- .t1_vec(t_rep); S <- length(tr)
  if (S < 2L) stop("at least two replicates are required")
  to <- .t1_vec(t_obs)
  if (length(to) == 1L) to <- rep(to, S)
  if (length(to) != S)
    stop("t_obs must be a scalar or have one value per draw")
  k <- sum(tr >= to)
  p <- k / S
  .t1_result(p_value = p, p_two_sided = 2 * min(p, 1 - p),
             n_extreme = as.numeric(k), t_obs_mean = mean(to),
             t_rep_mean = mean(tr), S = as.numeric(S),
             extreme = as.numeric(p < 0.01 || p > 0.99),
             method = "Posterior predictive p-value, BDA3 Section 6.3")
}
