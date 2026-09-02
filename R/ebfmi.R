# SPDX-License-Identifier: AGPL-3.0-or-later
#' Energy Bayesian fraction of missing information.
#'
#' Formula: EBFMI = sum_{n=1}^{N} (E_n - E_{n-1})^2 / sum_{n=0}^{N} (E_n - Ebar)^2
#'
#' @param energy Energies per iteration; a list of lists is treated as one chain per row.

#' @return List with ``ebfmi`` (per chain), ``min_ebfmi``, ``n_chains``, ``n``.
#' @references Betancourt (2016), Diagnosing Suboptimal Cotangent Disintegrations in Hamiltonian Monte Carlo, arXiv:1604.00695. Verified against the paper: the estimator is the displayed equation for BFMI-hat.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ebfmi(V)
Ebfmi <- function(energy) {
  E <- if (is.matrix(energy)) lapply(seq_len(nrow(energy)), function(i) energy[i, ])
       else if (is.list(energy)) lapply(energy, .t1_vec) else list(.t1_vec(energy))
  out <- vapply(E, function(e) {
    N <- length(e)
    if (N < 2) stop("need at least two energies per chain")
    num <- sum(diff(e)^2); den <- sum((e - mean(e))^2)
    if (den > 0) num / den else NA_real_
  }, numeric(1))
  .t1_result(ebfmi = out, min_ebfmi = min(out), n_chains = length(E),
             n = length(E[[1]]),
             method = "Energy Bayesian fraction of missing information")
}
