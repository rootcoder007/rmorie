# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayes theorem posterior for genomic parameters
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 171-208\], Chapter 6, Section 6.1 "Bayes Theorem and Bayesian
#' Linear Regression", pp. 171-172, read as a rendered page.  The section
#' gives, unnumbered, immediately before equation (6.1),
#'
#'   f(theta|y) = f(y, theta)/f(y) = f(theta) f(y|theta) / f(y)
#'              proportional to f(theta) L(theta; y),
#'
#' "where f(y) = integral f(y|theta) f(theta) d theta = E_theta\[f(y|theta)\] is
#' the marginal distribution".  It adds that "once a sample of the posterior
#' distribution is obtained, estimation of a parameter is often found by
#' averaging the sample values", which is the posterior mean reported here.
#'
#' DETERMINISM.  Nothing is sampled.  The marginal f(y) and the posterior
#' moments are quadratures on a fixed equally spaced grid by the composite
#' Simpson rule, which is exact for the polynomial integrands and converges to
#' machine precision on the conjugate case the anchor uses.
#'
#' @param y the data, passed through to likelihood_f.
#' @param prior_f function of theta giving f(theta), the prior density.
#' @param likelihood_f function of (theta, y) giving L(theta; y).
#' @param grid pair (lo, hi), the support over which theta is integrated.
#' @param n_grid number of grid points; forced odd for Simpson's rule.
#' @return list: estimate, posterior, theta, marginal, post_mean, post_var, n,
#'   method.
#' @keywords internal
#' @examples
#' Bayth(c(1, 0, 1), function(t) 1, function(t, y) t^sum(y) * (1 - t)^(length(y) - sum(y)))$estimate
#' @export
Bayth <- function(y, prior_f, likelihood_f, grid = c(0, 1), n_grid = 2001L) {
  yy <- .s03vec(y)
  if (!is.function(prior_f) || !is.function(likelihood_f)) {
    stop("bayes_theorem_genomic: prior_f and likelihood_f must be callables")
  }
  lo <- as.numeric(grid[1L])
  hi <- as.numeric(grid[2L])
  if (!(hi > lo)) stop("bayes_theorem_genomic: the grid must have positive width")
  m <- as.integer(n_grid)
  if (m < 3L) stop("bayes_theorem_genomic: n_grid must be at least 3")
  if (m %% 2L == 0L) m <- m + 1L
  h <- (hi - lo) / (m - 1L)
  th <- lo + (seq_len(m) - 1L) * h
  un <- numeric(m)
  for (i in seq_len(m)) {
    p <- as.numeric(prior_f(th[i]))
    if (p < 0) stop("bayes_theorem_genomic: the prior returned a negative density")
    un[i] <- p * as.numeric(likelihood_f(th[i], yy))
  }
  simpson <- function(v) {
    s <- v[1L] + v[m]
    if (m > 2L) for (i in seq(2L, m - 1L)) s <- s + (if ((i - 1L) %% 2L == 1L) 4 else 2) * v[i]
    s * h / 3
  }
  marg <- simpson(un)
  if (!(marg > 0)) stop("bayes_theorem_genomic: the marginal f(y) is not positive")
  post <- un / marg
  m1 <- simpson(th * post)
  m2 <- simpson(th * th * post)
  list(estimate = m1, posterior = post, theta = th, marginal = marg,
       post_mean = m1, post_var = m2 - m1 * m1, n = length(yy),
       method = paste0("f(theta|y) = f(theta)L(theta;y)/f(y), f(y) = int ",
                       "f(y|theta)f(theta)dtheta, Chapter 6 Sect. 6.1"))
}
