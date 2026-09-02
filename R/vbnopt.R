# SPDX-License-Identifier: AGPL-3.0-or-later
#' Variational inference by ELBO maximisation -- alias of \code{\link{Vinfer}}
#'
#' DUPLICATE, resolved by aliasing (the wave-2 DUPMAP pairs vbnopt and
#' vinfer with each other; one is implemented, the other re-exports it).
#' Both name the same procedure from the same paper: Jordan,
#' Ghahramani, Jaakkola and Saul (1999), Machine Learning 37(2):183-233,
#' doi:10.1023/A:1007665907178 -- maximise ELBO(q) = E_q\[log p(x,z)\] -
#' E_q\[log q(z)\] over a factorised q by the coordinate update
#' log q*_j = E_{q_{-j}}\[log p(x,z)\] + const. "ELBO max" and
#' "mean-field" are two names for one thing: the factorisation is the
#' constraint, the ELBO is the objective.
#'
#' @param log_p Name of the joint; only "normal-gamma" is implemented.
#' @param q Variational family; only "meanfield" is implemented.
#' @param x Observed sample.
#' @param mu0,lambda0,a0,b0 Normal-Gamma prior hyperparameters.
#' @param max_iter Maximum coordinate sweeps.
#' @param tol Stop when E[tau] moves by less than this.
#' @return As \code{\link{Vinfer}}.
#' @references Jordan, M.I., Ghahramani, Z., Jaakkola, T.S. and Saul, L.K.
#'   (1999). Machine Learning 37(2):183-233. doi:10.1023/A:1007665907178.
#' @examples
#' Vbnopt("normal-gamma", "meanfield", c(1, 2, 3, 4, 5))$e_tau
#' @export
Vbnopt <- function(log_p = "normal-gamma", q = "meanfield", x = NULL,
                   mu0 = 0, lambda0 = 0, a0 = 0, b0 = 0,
                   max_iter = 200, tol = 1e-12) {
  Vinfer(log_p, q, x, mu0, lambda0, a0, b0, max_iter, tol)
}
