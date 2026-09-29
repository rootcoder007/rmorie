#' E-BFMI: energy Bayesian fraction of missing information
#'
#' Betancourt's estimator of how well the momentum resampling of an HMC or
#' NUTS sampler explores the energy distribution,
#' \eqn{\sum_{n \ge 1} (E_n - E_{n-1})^2 / \sum_n (E_n - \bar E)^2}; values
#' below 0.3 flag poor exploration. Identical to the Python arm
#' \code{morie.fn.bfmi.bayesian_fmi}.
#'
#' @param energy Numeric vector of Hamiltonian energies, one per iteration
#'   (at least three).
#' @return A list with \code{bfmi}, \code{adequate} (\code{bfmi >= 0.3}),
#'   \code{energy_var} (divisor n - 1) and \code{transition_var} (mean squared
#'   transition, divisor n - 1).
#' @references Betancourt, M. (2016). Diagnosing suboptimal cotangent
#'   disintegrations in Hamiltonian Monte Carlo. arXiv:1604.00695.
#' @examples
#' bayesian_fmi(c(1, 3, 2, 5, 4))$bfmi
#' @export
bayesian_fmi <- function(energy) {
  e <- as.numeric(energy)
  n <- length(e)
  if (n < 3) stop("Need at least 3 energy values.")
  ss <- sum((e - sum(e) / n)^2)
  sd2 <- sum(diff(e)^2)
  b <- if (ss < 1e-30) 1 else sd2 / ss
  list(bfmi = b, adequate = b >= 0.3, energy_var = ss / (n - 1),
       transition_var = sd2 / (n - 1))
}
