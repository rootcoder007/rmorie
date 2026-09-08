# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian bootstrap: Dirichlet reweighting of the observed sample
#'
#' Rubin, D. B. (1981), "The Bayesian Bootstrap", The Annals of Statistics
#' 9(1), 130-134, doi:10.1214/aos/1176345338 (verified against Crossref).
#'
#' Each replicate draws w ~ Dirichlet(1,...,1) over the n observed values and
#' evaluates the statistic on the reweighted sample, giving a posterior sample
#' of the functional rather than a sampling distribution.  Unlike Efron's
#' bootstrap the weights are continuous, so no observation is ever dropped and
#' the replicate distribution has no atoms.
#'
#' The default statistic is the weighted mean, for which the posterior
#' variance is available in closed form: with w ~ Dirichlet(1,...,1),
#' Var(w_i) = (n-1)/(n^2 (n+1)) and Cov(w_i, w_j) = -1/(n^2 (n+1)), so
#' Var(sum w_i x_i) = sum_i (x_i - xbar)^2 / (n (n + 1)).  That is the anchor
#' for this module and it does not run through the resampling loop at all;
#' var_closed reports it whenever the default statistic is in use.
#'
#' @param x the observed sample.
#' @param stat function(x, w) giving the statistic on the reweighted sample;
#'   NULL uses the weighted mean.
#' @param B number of posterior draws.
#' @param seed seed for the shared deterministic stream.
#' @return list: theta_b, estimate, se, lo, hi, var_closed, n, B, method.
#' @keywords internal
#' @examples
#' Btbayes(c(1, 2, 3, 4, 5), B = 50)$var_closed
#' @export
Btbayes <- function(x, stat = NULL, B = 200, seed = 1) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 1L) stop("boot_bayesian: need at least one observation")
  if (as.integer(B) < 2L) stop("boot_bayesian: need at least two replicates")
  W <- .btdir_rows(n, B, seed)
  f <- if (is.null(stat)) function(x, w) sum(w * x) else stat
  theta <- vapply(W, function(w) as.numeric(f(xx, w)), 0)
  m <- .s03mean(theta)
  vc <- if (n > 1L) sum((xx - .s03mean(xx))^2) / (n * (n + 1)) else 0
  list(theta_b = theta, estimate = m,
       se = if (length(theta) > 1L) .s03sd(theta, 1L) else NaN,
       lo = .s03quantile7(theta, 0.025), hi = .s03quantile7(theta, 0.975),
       var_closed = if (is.null(stat)) vc else NaN,
       n = n, B = as.integer(B),
       method = "Rubin (1981) Ann. Statist. 9(1):130-134")
}
