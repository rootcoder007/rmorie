# SPDX-License-Identifier: AGPL-3.0-or-later
#' Augmented Lagrangian for equality-constrained minimisation.
#'
#' L_A = f(x) + sum_i lambda_i g_i(x) + (mu/2) sum_i g_i(x)^2, with the
#' first-order multiplier update lambda <- lambda + mu g(x).
#'
#' @param f Objective, a function of x returning a scalar.
#' @param g Constraint map, returning the vector g(x).
#' @param x Point at which to evaluate.
#' @param lam Current multipliers; NULL starts them at zero.
#' @param mu Penalty parameter, strictly positive.
#'
#' @return List with value, objective, linear, penalty, violation,
#'   lambda, mu, m.
#' @references Hestenes (1969), JOTA 4(5), 303-320; Powell (1969), in
#'   Fletcher (ed.), Optimization, 283-298.  Standard published form;
#'   neither source is in the local corpus and neither was read.
#' @export
#' @examples
#' f <- function(x) sum(x^2)
#' g <- function(x) sum(x) - 1
#' Auglag(f, g, x = c(0.5, 0.5))
Auglag <- function(f, g, x, lam = NULL, mu = 1) {
  x <- .t1_vec(x); gv <- .t1_vec(g(x)); m <- length(gv)
  lm <- if (is.null(lam)) rep(0, m) else .t1_vec(lam)
  if (length(lm) != m) stop("lam must have one entry per constraint")
  mu <- as.numeric(mu)
  if (mu <= 0) stop("mu must be strictly positive")
  fv <- as.numeric(f(x))
  lin <- sum(lm * gv)
  pen <- 0.5 * mu * sum(gv^2)
  .t1_result(value = fv + lin + pen, objective = fv, linear = lin,
             penalty = pen, violation = if (m > 0L) max(abs(gv)) else 0,
             lambda = lm + mu * gv, mu = mu, m = m,
             method = "Augmented Lagrangian (Hestenes 1969; Powell 1969)")
}
