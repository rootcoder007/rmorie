# SPDX-License-Identifier: AGPL-3.0-or-later
#' Advanced composition theorem for differential privacy
#'
#' Dwork, Rothblum and Vadhan (2010), "Boosting and differential privacy", 51st
#' IEEE Symposium on Foundations of Computer Science (FOCS), 51-60,
#' doi:10.1109/FOCS.2010.12.  The full text was not retrievable here, so the
#' theorem is written in the standard published form the module specification
#' states: the k-fold adaptive composition of mechanisms each
#' (epsilon, delta)-differentially private is (epsilon', k delta + delta')-
#' differentially private with
#' epsilon' = sqrt(2 k ln(1/delta')) epsilon + k epsilon (e^epsilon - 1).
#'
#' Two facts make the result worth having and both are checked as anchors.  The
#' leading term scales as sqrt(k), not k: basic sequential composition gives
#' k epsilon, while advanced composition trades a small delta' for a privacy
#' loss growing like the square root of the number of queries.  As epsilon goes
#' to zero the second term is order k epsilon^2 and vanishes relative to the
#' first, so the ratio of epsilon' to epsilon sqrt(2 k ln(1/delta')) tends to 1.
#'
#' Advanced composition is not uniformly better.  For small k or large epsilon
#' the quadratic correction dominates and k epsilon is the tighter bound;
#' epsilon_basic and tighter are returned so the crossover is visible rather
#' than assumed, because quoting the advanced bound where the basic one is
#' smaller overstates the privacy loss.
#'
#' @param epsilon per-mechanism epsilon, positive.
#' @param delta per-mechanism delta, in [0, 1).
#' @param k number of compositions, at least one.
#' @param delta_prime slack traded for the sqrt(k) rate, in (0, 1].
#' @return list: epsilon_total, estimate, delta_total, epsilon_basic,
#'   delta_basic, leading_term, quadratic_term, tighter, epsilon_effective, k,
#'   method.
#' @keywords internal
#' @examples
#' Advcmp(0.1, 1e-6, 100, 1e-5)$epsilon_total
#' @export
Advcmp <- function(epsilon, delta = 0, k = 1, delta_prime = 1e-6) {
  e <- as.numeric(epsilon)
  if (!(e > 0)) stop("advanced_composition: epsilon must be positive")
  d <- as.numeric(delta)
  if (!(d >= 0 && d < 1)) stop("advanced_composition: delta must lie in [0, 1)")
  kk <- as.integer(k)
  if (kk < 1L) stop("advanced_composition: k must be at least one")
  dp <- as.numeric(delta_prime)
  if (!(dp > 0 && dp <= 1)) stop("advanced_composition: delta_prime must lie in (0, 1]")
  lead <- sqrt(2 * kk * log(1 / dp)) * e
  quad <- kk * e * (exp(e) - 1)
  et <- lead + quad
  eb <- kk * e
  dt <- kk * d + dp
  list(epsilon_total = et, estimate = et, delta_total = dt, epsilon_basic = eb,
       delta_basic = kk * d, leading_term = lead, quadratic_term = quad,
       tighter = if (et < eb) "advanced" else "basic",
       epsilon_effective = if (et < eb) et else eb, k = kk,
       method = "Dwork, Rothblum and Vadhan (2010) advanced composition")
}
