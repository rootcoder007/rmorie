# SPDX-License-Identifier: AGPL-3.0-or-later
#' Solve the kriging system for the weights
#'
#' Simple kriging solves Sigma lambda = sigma. Ordinary kriging adds the
#' unbiasedness constraint sum(lambda) = 1 through a Lagrange multiplier,
#' bordering the system with a row and column of ones.
#'
#' @param cov_matrix Sigma, the (n by n) covariance among observations.
#' @param cov_target sigma, covariance between observations and the
#'   target, length n.
#' @param coords Unused; accepted for call-site compatibility.
#' @param unbiased Impose the sum-to-one constraint (ordinary rather than
#'   simple kriging).
#' @return Named list: weights, weight_sum, lagrange, unbiased.
#' @references Schabenberger & Gotway (2005), Sec 5.2.
#' @examples
#' spkwt(diag(3) * 2, c(1, 0.5, 0.25), unbiased = TRUE)
#' @export
spkwt <- function(cov_matrix, cov_target, coords = NULL, unbiased = FALSE) {
  Sigma <- as.matrix(cov_matrix)
  sig <- as.numeric(cov_target)
  n <- nrow(Sigma)
  if (nrow(Sigma) != ncol(Sigma)) stop("`cov_matrix` must be square")
  if (length(sig) != n) {
    stop("`cov_target` must have one entry per observation")
  }
  if (!unbiased) {
    lam <- as.numeric(solve(Sigma, sig))
    return(list(weights = lam, weight_sum = sum(lam), lagrange = NULL,
                unbiased = FALSE))
  }
  A <- matrix(0, n + 1, n + 1)
  A[seq_len(n), seq_len(n)] <- Sigma
  A[seq_len(n), n + 1] <- 1
  A[n + 1, seq_len(n)] <- 1
  sol <- as.numeric(solve(A, c(sig, 1)))
  lam <- sol[seq_len(n)]
  list(weights = lam, weight_sum = sum(lam), lagrange = sol[n + 1],
       unbiased = TRUE)
}
