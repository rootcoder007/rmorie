# SPDX-License-Identifier: AGPL-3.0-or-later
#' Check the extended Schwartz conditions and return the resulting bound
#'
#' Prior mass on a Kullback-Leibler neighbourhood AND a test rate beating
#' its radius are both required; either alone is insufficient. The margin
#' C - c governs the bound.
#'
#' Formula: Pi(P_0) > 0 and K(p_0; P_0) <= c and C > c imply consistency;
#'   the working bound is e^\{-(C - c) n\} / Pi(P_0)
#'
#' @param prior_mass Pi(P_0), in (0, 1].
#' @param kl_radius c, the Kullback-Leibler radius.
#' @param test_rate C, the exponential rate of the test.
#' @param n Sample size.
#' @return List with \code{holds}, \code{margin}, \code{bound},
#'   \code{prior_mass}, \code{n}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Theorem 6.17 (Extended Schwartz)
#'   together with Theorem 6.16. Read from the copy of the book held in
#'   the corpus.
#' @export
#' @examples
#' Kldsupp(prior_mass = 0.5, kl_radius = 0.1, test_rate = 0.2, n = 100)
Kldsupp <- function(prior_mass, kl_radius, test_rate, n) {
  pm <- as.numeric(prior_mass); cc <- as.numeric(kl_radius)
  Cc <- as.numeric(test_rate); n <- as.integer(n)
  if (pm <= 0 || pm > 1) stop("the prior mass must lie in (0, 1]")
  if (cc < 0) stop("the Kullback-Leibler radius must be non-negative")
  if (n < 1L) stop("n must be at least 1")
  margin <- Cc - cc
  .t1_result(holds = as.numeric(margin > 0), margin = margin,
             bound = exp(-margin * n) / pm, prior_mass = pm,
             n = as.numeric(n),
             method = "Extended Schwartz conditions, Ghosal Theorem 6.17")
}
