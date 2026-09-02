# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical influence values by numerical perturbation
#'
#' The quotient is the one of Hampel, F. R. (1974), "The influence curve and
#' its role in robust estimation", \emph{Journal of the American Statistical
#' Association} 69(346), 383-393, doi:10.1080/01621459.1974.10482962 (closed
#' access, no open copy in any repository per Unpaywall), evaluated at finite
#' eps at each observed point rather than in the limit:
#'
#' \deqn{U_i = \[T((1-\epsilon) F_n + \epsilon \delta_{x_i}) - T(F_n)\]/\epsilon.}{U_i = \[T((1-eps) F_n + eps delta_{x_i}) - T(F_n)\]/eps.}
#'
#' These U_i are the empirical influence values the nonparametric delta method
#' uses; estimate is that variance estimate, sum U_i^2 / n^2, which for the
#' mean is exactly (n-1) s^2 / n^2, the usual variance of the sample mean with
#' the population-variance denominator.
#'
#' T is a functional of a weighted sample, T(values, weights), or one of the
#' names "mean", "var", "median"; see Infcrv.
#'
#' @param x The sample.
#' @param stat function(values, weights) or "mean", "var", "median".
#' @param eps Contamination weight.
#' @return list: estimate, infl, tf, eps, n, method.
#' @keywords internal
#' @examples
#' Btvinf(c(2, 4, 4, 5, 7))$infl
#' @export
Btvinf <- function(x, stat = "mean", eps = 1e-3) {
  T <- .if_resolve(stat, "boot_influence_fn")
  v <- .s03vec(x)
  n <- length(v)
  if (n == 0L) stop("boot_influence_fn: x is empty")
  e <- as.numeric(eps)
  if (!(e > 0 && e < 1)) {
    stop("boot_influence_fn: eps must lie strictly between 0 and 1")
  }
  base <- T(v, rep(1 / n, n))
  u <- numeric(n)
  for (i in seq_len(n)) {
    u[i] <- (T(c(v, v[i]), c(rep((1 - e) / n, n), e)) - base) / e
  }
  list(estimate = sum(u * u) / (n * n), infl = u, tf = base, eps = e, n = n,
       method = "Influence function via numerical perturbation")
}
