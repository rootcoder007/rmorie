# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean-field ADVI evidence lower bound.
#'
#' L(mu, omega) = mean_s log p(x, mu + exp(omega) eta_s)
#'                + sum_k omega_k + (K/2)(1 + log 2 pi).
#'
#' @param mu Variational means, length K.
#' @param omega Log standard deviations, length K.
#' @param eta Standard-normal draws, S x K, supplied by the caller.
#' @param logjoint Function of a length-K vector returning log p(x, zeta).
#'
#' @return List with elbo, entropy, meanlogjoint, logjoints, K, S.
#' @references Kucukelbir, Tran, Ranganath, Gelman and Blei (2017), JMLR
#'   18(14); arXiv:1603.00788, Sects. 2.3-2.5 and Equation (5), read from
#'   the ar5iv rendering of the arXiv source.
#' @export
#' @examples
#' set.seed(1)
#' Advielbo(mu = c(0, 0), omega = c(0, 0), eta = matrix(rnorm(4), 2, 2),
#'          logjoint = function(x) -0.5 * sum(x^2))
Advielbo <- function(mu, omega, eta, logjoint) {
  mu <- .t1_vec(mu); omega <- .t1_vec(omega)
  E <- .t1_mat(eta)
  K <- length(mu)
  if (length(omega) != K) stop("mu and omega must have the same length")
  if (ncol(E) != K) stop("eta must have K columns")
  S <- nrow(E)
  lj <- numeric(S)
  for (s in seq_len(S)) {
    lj[s] <- as.numeric(logjoint(mu + exp(omega) * E[s, ]))
  }
  ent <- sum(omega) + 0.5 * K * (1 + log(2 * pi))
  mlj <- sum(lj) / S
  .t1_result(elbo = mlj + ent, entropy = ent, meanlogjoint = mlj,
             logjoints = lj, K = K, S = S,
             method = "Mean-field ADVI ELBO (Kucukelbir et al. 2017 eq. 5)")
}
