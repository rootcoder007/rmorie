# SPDX-License-Identifier: AGPL-3.0-or-later
#' Parametric bootstrap: simulate from the fitted model, not the data
#'
#' Davison, A. C. and Hinkley, D. V. (1997), Bootstrap Methods and their
#' Application, Cambridge University Press, chapter 2 (parametric simulation
#' from F_hat = F(. | theta_hat)).
#'
#' Where the nonparametric bootstrap resamples the empirical distribution, the
#' parametric bootstrap draws fresh samples from the fitted member of the
#' assumed family: x*_b ~ F(. | theta_hat), theta*_b = T(x*_b).  The trade is
#' stark and worth naming: if the family is right this is strictly more
#' efficient -- the replicates are continuous, the tails are extrapolated
#' rather than truncated at the sample maximum, and B can exceed the number of
#' distinct nonparametric resamples -- and if the family is wrong the
#' bootstrap is confidently wrong, with no diagnostic inside the procedure
#' that would say so.
#'
#' rvs_fn(theta, n, g) supplies the simulator and receives the shared Lehmer
#' stream g so both language arms draw the same numbers; the default is the
#' normal family with theta = (mu, sigma).
#'
#' Anchor: for the normal family and the sample mean the conditional variance
#' of the replicates is exactly sigma_hat^2 / n, a closed form that never
#' enters the resampling loop; and sigma = 0 makes every replicate exactly mu.
#'
#' @param theta_hat the fitted parameter, passed straight to rvs_fn.
#' @param rvs_fn function(theta, n, g) returning a simulated sample; NULL uses
#'   the normal family with theta = (mu, sigma).
#' @param stat statistic of a sample; NULL uses the mean.
#' @param B replicates.
#' @param n simulated sample size; required.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, estimate, se, lo, hi, var_closed, n, B, method.
#' @keywords internal
#' @examples
#' Btparm(c(3, 2), NULL, NULL, 50, 10)$var_closed
#' @export
Btparm <- function(theta_hat, rvs_fn = NULL, stat = NULL, B = 200, n = NULL,
                   seed = 1, alpha = 0.05) {
  th <- .s03vec(theta_hat)
  if (is.null(n)) stop("boot_parametric: n (the simulated sample size) is required")
  n <- as.integer(n)
  if (n < 1L) stop("boot_parametric: n must be at least 1")
  if (as.integer(B) < 2L) stop("boot_parametric: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_parametric: alpha must lie strictly between 0 and 1")
  if (is.null(rvs_fn)) {
    if (length(th) < 2L)
      stop("boot_parametric: the default normal family needs theta = (mu, sigma)")
    if (th[2] < 0) stop("boot_parametric: sigma must be non-negative")
  }
  f <- if (is.null(stat)) .s03mean else stat
  r <- if (is.null(rvs_fn)) .btparm_normal else rvs_fn
  g <- .t1_lcg(seed)
  reps <- vapply(seq_len(as.integer(B)), function(b) as.numeric(f(r(th, n, g))), 0)
  dflt <- is.null(rvs_fn) && is.null(stat)
  list(theta_b = reps, estimate = .s03mean(reps), se = .s03sd(reps, 1L),
       lo = .s03quantile7(reps, a / 2), hi = .s03quantile7(reps, 1 - a / 2),
       var_closed = if (dflt) th[2] * th[2] / n else NaN,
       n = n, B = as.integer(B),
       method = "Davison and Hinkley (1997) Bootstrap Methods and their Application, ch. 2")
}

#' @noRd
.btparm_normal <- function(theta, n, g) {
  mu <- as.numeric(theta[1]); sd <- as.numeric(theta[2])
  vapply(seq_len(as.integer(n)), function(i) mu + sd * g$norm(), 0)
}
