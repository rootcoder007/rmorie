# SPDX-License-Identifier: AGPL-3.0-or-later
#' Beta-process Levy measure
#'
#' BP(c, H0) has Levy measure nu(du, dt) = c u^(-1) (1 - u)^(c-1) du
#' dH0(t).  The u^(-1) factor makes nu infinite near zero -- infinitely
#' many infinitesimal jumps -- yet the expected total jump mass is
#' finite and equals H0 exactly, because the u in "u dnu" cancels it.
#' That cancellation is what the quadrature here confirms.
#'
#' Formula: int_0^1 u nu(du) = int_0^1 c (1 - u)^(c-1) du = 1,
#'   so the expected mass on \[0, t_max\] is H0(t_max) = t_max.
#'
#' @param c Concentration, positive.
#' @param t_max Upper time limit, with H0(t) = t.
#' @param n_grid Number of midpoint cells.
#' @return List with \code{estimate} (expected total jump mass),
#'   \code{H0_t_max}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.3.2.
#' @export
#' @examples
#' Ghosalbpcont()
Ghosalbpcont <- function(c = 2, t_max = 1, n_grid = 2000) {
  n_grid <- as.integer(n_grid)
  if (c <= 0) stop("c must be positive")
  if (n_grid < 1L) stop("n_grid must be positive")
  u <- (seq_len(n_grid) - 0.5) / n_grid
  expected_mass <- sum(u * c / u * (1 - u)^(c - 1) / n_grid) * t_max
  .t1_result(estimate = expected_mass, H0_t_max = t_max,
             gap = abs(expected_mass - t_max),
             method = "BP Levy measure (GvdV 2017 sec. 13.3.2)")
}
