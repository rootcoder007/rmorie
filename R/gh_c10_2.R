# SPDX-License-Identifier: AGPL-3.0-or-later
#' Universal model weights
#'
#' Weights pi_k proportional to exp(-c k log n) make
#' sum_k pi_k exp(n eps_k^2) converge whenever n eps_k^2 is at most of
#' order k log n.  That summability is the universal-weight condition
#' behind every adaptation theorem in the chapter: it lets one prior
#' serve every smoothness at once.  The series converges exactly when
#' the penalty c beats the rate scale.
#'
#' Formula: S = sum_{k=1}^{K} pi_k exp(n eps_k^2),
#'   log pi_k = -c k log n - log Z,  n eps_k^2 = eps_scale k log n.
#'
#' @param n Sample size entering both the weights and the rate.
#' @param c Penalty scale of the weights.
#' @param K_max Number of terms summed.
#' @param eps_scale Scale of n eps_k^2 in units of k log n.
#' @return List with \code{estimate} (the full sum),
#'   \code{partial_sums}, \code{converges}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.2.1.
#' @export
Ghosalunivweights <- function(n = 100, c = 2, K_max = 200,
                              eps_scale = 1) {
  if (K_max < 1) stop("K_max must be at least 1")
  if (n <= 1) stop("n must exceed 1")
  k <- seq_len(K_max)
  log_pis <- -c * k * log(n)
  mx <- max(log_pis)
  Z <- sum(exp(log_pis - mx))
  terms <- exp(log_pis - mx - log(Z) + eps_scale * k * log(n))
  run <- cumsum(terms)
  total <- run[K_max]
  keep <- unique(c(10, 50, K_max))
  keep <- keep[keep <= K_max]
  .t1_result(estimate = total,
             partial_sums = run[keep],
             converges = is.finite(total) && c > eps_scale,
             method = "universal weights (GvdV 2017 sec. 10.2.1)")
}
