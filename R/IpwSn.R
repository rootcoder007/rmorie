# SPDX-License-Identifier: AGPL-3.0-or-later
#' IPW sensitivity analysis for a non-ignorable selection mechanism
#'
#' The Robins-Rotnitzky-Scharfstein selection-bias function q(y) indexes
#' departures from missing at random.  With the exponential tilt
#' q(y) = lambda y the observed-data weights become
#' \code{C exp(-lambda Y) / pi(X)}, and the resulting Hajek mean traces
#' out a sensitivity curve.  \code{lambda = 0} is exactly the MAR
#' inverse-probability-weighted estimator; positive lambda encodes
#' "large Y are less likely to be observed".
#'
#' Formula: mu(lam) = sum C exp(-lam Y) Y / pi / sum C exp(-lam Y) / pi.
#'
#' @param Y Outcome; entries with C = 0 are unused.
#' @param X Covariate block for the selection model; intercept added.
#' @param C Selection indicator, 0 or 1.
#' @param lam_grid Values of the selection-bias parameter.
#' @return List with \code{estimate} (mu at lambda 0), \code{mu},
#'   \code{lambda}, \code{mu_min}, \code{mu_max}, \code{range},
#'   \code{propensity}, \code{gamma}, \code{n_observed}, \code{n},
#'   \code{method}.
#' @references Robins, Rotnitzky and Scharfstein (2000), Sensitivity
#'   analysis for selection bias and unmeasured confounding, IMA Volumes
#'   in Mathematics and its Applications 116, Springer, pp. 1-94,
#'   \doi{10.1007/978-1-4612-1284-3_1}; Scharfstein, Rotnitzky and
#'   Robins (1999), JASA 94(448):1096-1120.
#'   \doi{10.1080/01621459.1999.10473862}
#' @export
IpwSn <- function(Y, X, C, lam_grid) {
  yv <- .s03vec(Y); n <- length(yv)
  if (n == 0L) stop("ipw_sensitivity: Y is empty")
  cc <- .s03vec(C)
  if (length(cc) != n) stop("ipw_sensitivity: Y and C have different lengths")
  if (any(cc != 0 & cc != 1)) stop("ipw_sensitivity: C must be 0 or 1")
  if (sum(cc) == 0) stop("ipw_sensitivity: no observed units")
  lam <- .s03vec(lam_grid)
  if (length(lam) == 0L) stop("ipw_sensitivity: lam_grid is empty")
  Z <- .s03design(X, n)
  if (nrow(Z) != n) stop("ipw_sensitivity: X and Y have different lengths")
  gam <- .s03logit(Z, cc, 60L)
  pi <- vapply(as.numeric(.s03matvec(Z, gam)), .s03sigmoid, 0)
  obs <- cc == 1
  mus <- vapply(lam, function(L) {
    u <- exp(-L * yv[obs]) / pi[obs]
    den <- sum(u)
    if (den <= 0) stop("ipw_sensitivity: tilted weights vanished; lambda too extreme")
    sum(u * yv[obs]) / den
  }, 0)
  zero <- if (any(lam == 0)) mus[which(lam == 0)[1]] else mus[1]
  .t1_result(estimate = zero, mu = mus, lambda = lam, mu_min = min(mus),
             mu_max = max(mus), range = max(mus) - min(mus),
             propensity = pi, gamma = gam, n_observed = sum(cc), n = n,
             method = "mu(lam) = sum C exp(-lam Y) Y / pi / sum C exp(-lam Y) / pi, Robins, Rotnitzky & Scharfstein (2000)")
}
