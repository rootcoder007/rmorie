# SPDX-License-Identifier: AGPL-3.0-or-later
#' Le Cam's bound on the posterior mass of an alternative set
#'
#' The last term divides by Pi(U), so a prior that starves the
#' neighbourhood of the truth destroys the bound however good the test is.
#'
#' Formula: P_0 Pi(V | X) <= d_TV(P_0, P_U) + P_0 phi
#'   + (1/Pi(U)) int_V P(1 - phi) dPi(P)
#'
#' @param dtv d_TV(P_0, P_U), in \[0, 1\].
#' @param p0_phi P_0 phi, the type I error, in \[0, 1\].
#' @param prior_mass Pi(U), strictly positive.
#' @param integral int_V P(1 - phi) dPi(P), non-negative.
#' @return List with \code{bound}, \code{term_tv}, \code{term_test},
#'   \code{term_prior}, \code{informative}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Lemma 6.46 (Le Cam). Read from the
#'   copy of the book held in the corpus. NOTE: the worklist filed this
#'   under "Appendix D"; in the book it is Lemma 6.46 in Section 6.8.2.
#' @export
#' @examples
#' Lecam(dtv = 0.3, p0_phi = 0.4, prior_mass = 0.5, integral = 0.2)
Lecam <- function(dtv, p0_phi, prior_mass, integral) {
  dtv <- as.numeric(dtv)
  p0 <- as.numeric(p0_phi)
  pm <- as.numeric(prior_mass)
  it <- as.numeric(integral)
  if (dtv < 0 || dtv > 1) stop("dtv must lie in [0, 1]")
  if (p0 < 0 || p0 > 1) stop("P_0 phi must lie in [0, 1]")
  if (pm <= 0) stop("the prior mass Pi(U) must be positive")
  if (it < 0) stop("the integral must be non-negative")
  t3 <- it / pm
  b <- dtv + p0 + t3
  .t1_result(bound = b, term_tv = dtv, term_test = p0, term_prior = t3,
             informative = as.numeric(b < 1),
             method = "Le Cam posterior inequality, Ghosal Lemma 6.46")
}
