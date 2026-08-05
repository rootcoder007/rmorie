# SPDX-License-Identifier: AGPL-3.0-or-later
#' Power of a two-sided test under a complex survey design
#'
#' A clustered design does not reduce the sample size, it reduces the
#' information: the effective sample size is \code{n / DEFF}. Both tails
#' are kept, so a zero effect returns alpha exactly rather than alpha/2.
#'
#' Formula: \code{n_eff = n / DEFF};
#' \code{power = Phi(d sqrt(n_eff) - z) + Phi(-d sqrt(n_eff) - z)} with
#' \code{z = Phi^-1(1 - alpha/2)}.
#'
#' @param effect_size Standardised effect size (Cohen's d).
#' @param alpha Two-sided significance level in (0, 1).
#' @param DEFF Design effect, positive.
#' @param n Nominal sample size, at least 1.
#' @return List with \code{estimate}, \code{power}, \code{n_eff},
#'   \code{ncp}, \code{z_crit}, \code{DEFF}, \code{n}.
#' @references Lumley, T. (2010). Complex Surveys: A Guide to Analysis
#'   Using R, Wiley. \doi{10.1002/9780470580066}. Standard form; the book
#'   is not held locally.
#' @export
Powsrv <- function(effect_size, alpha = 0.05, DEFF = 1, n = 100) {
  d <- as.numeric(effect_size); alpha <- as.numeric(alpha)
  deff <- as.numeric(DEFF); n <- as.integer(n)
  if (!(alpha > 0 && alpha < 1)) stop("Powsrv: alpha must lie in (0, 1)")
  if (deff <= 0) stop("Powsrv: DEFF must be positive")
  if (n < 1L) stop("Powsrv: n must be at least 1")
  neff <- n / deff
  z <- .s03qnorm(1 - alpha / 2)
  ncp <- d * sqrt(neff)
  power <- .s03pnorm(ncp - z) + .s03pnorm(-ncp - z)
  .t1_result(estimate = power, power = power, n_eff = neff, ncp = ncp,
             z_crit = z, DEFF = deff, n = n,
             method = "Two-sided z power with the design effect discount")
}
