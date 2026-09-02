# SPDX-License-Identifier: AGPL-3.0-or-later
#' Check the three conditions that deliver a posterior contraction rate
#'
#' Note the asymmetry: the prior-mass and sieve-mass conditions use
#' epsilon-bar, the entropy condition uses epsilon, and the theorem also
#' needs n epsilon-bar^2 -> infinity.
#'
#' Formula: (i) Pi_n(B_2(p_0, ebar_n)) >= e^\{-C n ebar_n^2\};
#'   (ii) log N(xi eps_n, P_\{n,1\}, d) <= n eps_n^2;
#'   (iii) Pi_n(P_\{n,2\}) <= e^\{-(C + 4) n ebar_n^2\}
#'
#' @param prior_ball Pi_n(B_2(p_0, ebar_n)), in (0, 1].
#' @param log_entropy log N(xi eps_n, P_\{n,1\}, d), non-negative.
#' @param sieve_mass Pi_n(P_\{n,2\}), in \[0, 1\].
#' @param eps_bar ebar_n, positive.
#' @param eps eps_n, at least eps_bar.
#' @param n Sample size.
#' @param Cconst The constant C > 0.
#' @return List with \code{holds}, \code{cond_prior}, \code{cond_entropy},
#'   \code{cond_sieve}, \code{slack_prior}, \code{slack_entropy},
#'   \code{slack_sieve}, \code{n_eps_bar_sq}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Theorem 8.9, Section 8.2,
#'   conditions (8.4), (8.5) and (8.6). Read from the copy of the book
#'   held in the corpus.
#' @export
#' @examples
#' Testcond(prior_ball = 0.5, log_entropy = 10, sieve_mass = 0.1,
#'          eps_bar = 0.1, eps = 0.2, n = 100, Cconst = 1)
Testcond <- function(prior_ball, log_entropy, sieve_mass, eps_bar, eps, n,
                     Cconst) {
  pb <- as.numeric(prior_ball); le <- as.numeric(log_entropy)
  sm <- as.numeric(sieve_mass); eb <- as.numeric(eps_bar)
  ep <- as.numeric(eps); n <- as.integer(n); Cc <- as.numeric(Cconst)
  if (pb <= 0 || pb > 1) stop("the prior ball mass must lie in (0, 1]")
  if (le < 0) stop("the log entropy must be non-negative")
  if (sm < 0 || sm > 1) stop("the sieve mass must lie in [0, 1]")
  if (eb <= 0 || ep <= 0) stop("the rates must be positive")
  if (ep < eb) stop("eps_n must be at least eps_bar_n")
  if (n < 1L) stop("n must be at least 1")
  if (Cc <= 0) stop("C must be positive")
  neb <- n * eb^2
  s1 <- log(pb) + Cc * neb
  s2 <- n * ep^2 - le
  s3 <- if (sm > 0) -(Cc + 4) * neb - log(sm) else Inf
  c1 <- as.numeric(s1 >= 0); c2 <- as.numeric(s2 >= 0); c3 <- as.numeric(s3 >= 0)
  .t1_result(holds = as.numeric(c1 && c2 && c3), cond_prior = c1,
             cond_entropy = c2, cond_sieve = c3, slack_prior = s1,
             slack_entropy = s2, slack_sieve = s3, n_eps_bar_sq = neb,
             method = "Contraction-rate conditions, Ghosal Theorem 8.9")
}
