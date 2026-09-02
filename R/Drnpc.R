# SPDX-License-Identifier: AGPL-3.0-or-later
#' Negative-control falsification for the doubly robust DiD estimator
#'
#' A negative control outcome cannot plausibly be affected by the
#' exposure but shares its confounding structure, so its estimated
#' effect is an estimate of the bias.  Both outcomes go through the same
#' doubly robust moment of Sant'Anna and Zhao (2020), equation (2.6);
#' the design is refuted when
#' \code{|tau_neg| > qnorm(1 - alpha/2) se_neg}, and
#' \code{tau_adj = tau_main - tau_neg} subtracts the estimated bias.
#'
#' @param y_main Outcome change for the outcome of interest.
#' @param y_neg Outcome change for the negative control outcome.
#' @param D Binary treatment indicator.
#' @param X Optional baseline covariates.
#' @param alpha Two-sided level of the falsification test.
#' @return List with \code{estimate}, \code{tau_main}, \code{tau_neg},
#'   \code{se_main}, \code{se_neg}, \code{z_neg}, \code{crit},
#'   \code{falsified}, \code{tau_adj}, \code{n}.
#' @references Lipsitch, M., Tchetgen Tchetgen, E. and Cohen, T. (2010).
#'   Negative controls. Epidemiology 21(3), 383-388.  Sant'Anna,
#'   P. H. C. and Zhao, J. (2020). Journal of Econometrics 219(1),
#'   101-122.
#' @export
#' @examples
#' set.seed(1)
#' r <- Drnpc(y_main = rnorm(10), y_neg = rnorm(10), D = rbinom(10, 1, 0.5)); TRUE
Drnpc <- function(y_main, y_neg, D, X = NULL, alpha = 0.05) {
  ym <- .s03vec(y_main)
  yn <- .s03vec(y_neg)
  dv <- .s03vec(D)
  n <- length(ym)
  if (n == 0L) stop("Drnpc: empty input, y_main has no observations")
  if (length(yn) != n || length(dv) != n)
    stop("Drnpc: y_main, y_neg and D must have the same length")
  if (!(alpha > 0 && alpha < 1))
    stop("Drnpc: alpha must lie strictly between 0 and 1")
  s <- sum(dv)
  if (s <= 0 || s >= n)
    stop("Drnpc: D must contain both treated and control units")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  fm <- .s03drdid(ym, dv, Xr)
  fn <- .s03drdid(yn, dv, Xr)
  crit <- .s03qnorm(1 - alpha / 2)
  z <- if (fn$se > 0) fn$tau / fn$se else NaN
  bad <- if (!is.na(z) && abs(z) > crit) 1 else 0
  .t1_result(estimate = fm$tau, tau_main = fm$tau, tau_neg = fn$tau,
             se_main = fm$se, se_neg = fn$se, z_neg = z, crit = crit,
             falsified = bad, tau_adj = fm$tau - fn$tau, n = n,
             method = "DR-DiD with negative control outcome")
}
