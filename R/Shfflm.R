# SPDX-License-Identifier: AGPL-3.0-or-later
#' Privacy amplification by shuffling: local DP to central DP
#'
#' Central (epsilon, delta)-DP guarantee obtained by shuffling n reports,
#' each produced by an epsilon0-locally-differentially-private randomiser.
#' Erlingsson et al., Theorem 7: for any integer \code{n > 1} and
#' \code{epsilon0, delta > 0} the shuffled mechanism satisfies
#' (epsilon, delta)-DP in the central model with
#' \code{epsilon <= e1 sqrt(2 n log(1/delta)) + n e1 (exp(e1) - 1)} for
#' \code{e1 = 2 exp(2 epsilon0) (exp(epsilon0) - 1) / n}.  In particular,
#' for \code{epsilon0 <= log(n/4)/3},
#' \code{epsilon <= exp(2 epsilon0) (exp(epsilon0) - 1) sqrt(8 log(1/delta)/n)
#' + 6 exp(4 epsilon0) (exp(epsilon0) - 1)^2 / n}, and for any
#' \code{n >= 1000}, \code{0 < epsilon0 < 1/2} and \code{0 < delta < 1/100}
#' the simple form \code{epsilon <= 12 epsilon0 sqrt(log(1/delta)/n)}, which
#' is the sqrt(n) amplification the paper advertises.
#'
#' All three are returned; \code{estimate} is the general bound, the only
#' one whose side conditions always hold.  \code{simple_valid} and
#' \code{refined_valid} say whether each specialised form applies.  For
#' \code{epsilon0 > log(n/2)/3} the bound exceeds epsilon0 and there is no
#' amplification at all; \code{amplifies} flags that case.
#'
#' Equations read from a rendered image of p. 11 of arXiv:1811.12469v2: the
#' text layer drops the square-root signs.
#'
#' @param epsilon0 Per-report local DP parameter, positive.
#' @param n Number of reports shuffled, greater than 1.
#' @param delta Target failure probability, in (0, 1).
#' @return List with \code{estimate} (general bound), \code{epsilon},
#'   \code{epsilon_general}, \code{epsilon_refined}, \code{epsilon_simple},
#'   \code{epsilon1}, \code{epsilon0}, \code{delta}, \code{n},
#'   \code{log_inv_delta}, \code{amplification_factor},
#'   \code{refined_valid}, \code{simple_valid}, \code{amplifies},
#'   \code{method}.
#' @references Erlingsson, U., Feldman, V., Mironov, I., Raghunathan, A.,
#'   Talwar, K. & Thakurta, A. (2019). Amplification by shuffling: from
#'   local to central differential privacy via anonymity. Proceedings of the
#'   Thirtieth Annual ACM-SIAM Symposium on Discrete Algorithms, 2468-2479,
#'   Theorem 7. \doi{10.1137/1.9781611975482.151}
#' @export
#' @examples
#' Shfflm(epsilon0 = 5L, n = 5L, delta = 0.5)
Shfflm <- function(epsilon0, n, delta) {
  e0 <- as.numeric(epsilon0)
  nn <- as.numeric(n)
  d <- as.numeric(delta)
  if (e0 <= 0) stop("shuffle_model: epsilon0 must be positive")
  if (nn <= 1) stop("shuffle_model: n must exceed 1")
  if (d <= 0 || d >= 1) stop("shuffle_model: delta must lie in (0, 1)")

  logd <- log(1 / d)
  e1 <- 2 * exp(2 * e0) * (exp(e0) - 1) / nn
  general <- e1 * sqrt(2 * nn * logd) + nn * e1 * (exp(e1) - 1)
  refined <- exp(2 * e0) * (exp(e0) - 1) * sqrt(8 * logd / nn) +
    6 * exp(4 * e0) * (exp(e0) - 1)^2 / nn
  simple <- 12 * e0 * sqrt(logd / nn)

  .t1_result(estimate = general, epsilon = general, epsilon_general = general,
             epsilon_refined = refined, epsilon_simple = simple,
             epsilon1 = e1, epsilon0 = e0, delta = d, n = nn,
             log_inv_delta = logd,
             amplification_factor = if (general > 0) e0 / general else Inf,
             refined_valid = if (e0 <= log(nn / 4) / 3) 1 else 0,
             simple_valid = if (nn >= 1000 && e0 < 0.5 && d < 0.01) 1 else 0,
             amplifies = if (e0 <= log(nn / 2) / 3) 1 else 0,
             method = "Privacy amplification by shuffling (Erlingsson et al. 2019, Thm 7)")
}
