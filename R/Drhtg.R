# SPDX-License-Identifier: AGPL-3.0-or-later
#' Heterogeneous conditional ATT: the DR-DiD moment inside each stratum
#'
#' The doubly robust moment of Sant'Anna and Zhao (2020), equation (2.6),
#' is evaluated separately on each stratum of a discrete covariate, with
#' the weights renormalised within the stratum, so every \code{CATT(x)}
#' is a self-contained doubly robust estimate.  A single stratum
#' therefore reproduces the pooled estimator exactly.  The reported
#' heterogeneity statistic is the size-weighted variance of \code{CATT}
#' across strata, exactly zero when all strata share an effect.
#'
#' @param y Outcome change, one entry per unit.
#' @param D Binary treatment indicator.
#' @param X Optional baseline covariates used inside each stratum fit.
#' @param strata Discrete stratum label per unit; \code{NULL} is one
#'   stratum.
#' @return List with \code{estimate}, \code{strata}, \code{catt},
#'   \code{se}, \code{n_stratum}, \code{pooled}, \code{hetero_var},
#'   \code{n}.
#' @references Sant'Anna, P. H. C. and Zhao, J. (2020). Journal of
#'   Econometrics 219(1), 101-122, equation (2.6).  Athey, S. and
#'   Imbens, G. (2016). PNAS 113(27), 7353-7360.
#' @export
Drhtg <- function(y, D, X = NULL, strata = NULL) {
  yv <- .s03vec(y); dv <- .s03vec(D); n <- length(yv)
  if (n == 0L) stop("Drhtg: empty input, y has no observations")
  if (length(dv) != n) stop("Drhtg: y and D must have the same length")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  st <- if (is.null(strata)) rep("all", n) else as.character(strata)
  if (length(st) != n) stop("Drhtg: strata must have the same length as y")
  labels <- sort(unique(st))
  catt <- numeric(0); sev <- numeric(0); ns <- numeric(0)
  for (s in labels) {
    idx <- which(st == s); ds <- dv[idx]
    if (length(idx) < 3L || sum(ds) <= 0 || sum(ds) >= length(ds)) {
      catt <- c(catt, NaN); sev <- c(sev, NaN); ns <- c(ns, length(idx)); next
    }
    xm <- if (is.null(Xr)) NULL else Xr[idx, , drop = FALSE]
    fit <- .s03drdid(yv[idx], ds, xm)
    catt <- c(catt, fit$tau); sev <- c(sev, fit$se); ns <- c(ns, length(idx))
  }
  good <- !is.na(catt)
  den <- sum(ns[good]); num <- sum(ns[good] * catt[good])
  est <- if (den > 0) num / den else NaN
  hv <- if (den > 0) sum(ns[good] * (catt[good] - est)^2) / den else 0
  pooled <- if (sum(dv) > 0 && sum(dv) < n) .s03drdid(yv, dv, Xr)$tau else NaN
  .t1_result(estimate = est, strata = labels, catt = catt, se = sev,
             n_stratum = ns, pooled = pooled, hetero_var = hv, n = n,
             method = "DR-DiD heterogeneous CATT")
}
