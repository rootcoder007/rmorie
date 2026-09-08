# SPDX-License-Identifier: AGPL-3.0-or-later
#' DiD with a continuous treatment: ATT(d|d) levels and ACRT slopes
#'
#' \code{ATT(d|d') = E\[Y(d) - Y(0) | D = d'\]} is a level effect and
#' \code{ACRT(d_j|d_k) = E\[Y(d_j) - Y(d_{j-1}) | D = d_k\]/(d_j - d_{j-1})}
#' is a slope; the paper is explicit that the two can have different
#' signs, so both curves are returned and neither stands in for the
#' other.  Each \code{ATT(d|d)} is the doubly robust panel moment of
#' Sant'Anna and Zhao (2020) comparing the units at dose \code{d}
#' against the zero-dose units.
#'
#' @param y Outcome change, one entry per unit.
#' @param D_dose Non-negative treatment intensity; zero marks untreated.
#' @param X Optional baseline covariates.
#' @return List with \code{estimate}, \code{doses}, \code{att},
#'   \code{se}, \code{acrt}, \code{acrt_dose}, \code{n_zero}, \code{n}.
#' @references Callaway, B., Goodman-Bacon, A. and Sant'Anna, P. H. C.
#'   (2024). Difference-in-differences with a continuous treatment.
#'   arXiv:2107.02637, Section 3.  Sant'Anna, P. H. C. and Zhao, J.
#'   (2020). Journal of Econometrics 219(1), 101-122, equation (2.6).
#' @export
Drctf <- function(y, D_dose, X = NULL) {
  yv <- .s03vec(y)
  dv <- .s03vec(D_dose)
  n <- length(yv)
  if (n == 0L) stop("Drctf: empty input, y has no observations")
  if (length(dv) != n) stop("Drctf: y and D_dose must have the same length")
  if (any(dv < 0)) stop("Drctf: D_dose must be non-negative")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  zero <- which(dv == 0)
  if (!length(zero)) stop("Drctf: no zero-dose units to compare against")
  doses <- sort(unique(dv[dv > 0]))
  if (!length(doses)) stop("Drctf: no treated unit, every dose is zero")
  att <- numeric(0)
  sev <- numeric(0)
  nd <- numeric(0)
  for (d in doses) {
    hit <- which(dv == d)
    idx <- c(hit, zero)
    lab <- c(rep(1, length(hit)), rep(0, length(zero)))
    nd <- c(nd, length(hit))
    if (length(idx) < 3L) { att <- c(att, NaN)
    sev <- c(sev, NaN)
    next }
    xm <- if (is.null(Xr)) NULL else Xr[idx, , drop = FALSE]
    f <- .s03drdid(yv[idx], lab, xm)
    att <- c(att, f$tau)
    sev <- c(sev, f$se)
  }
  acrt <- numeric(0)
  adose <- numeric(0)
  if (length(doses) > 1L) for (j in seq_along(doses)[-1L]) {
    acrt <- c(acrt, (att[j] - att[j - 1L]) / (doses[j] - doses[j - 1L]))
    adose <- c(adose, doses[j])
  }
  good <- !is.na(att)
  den <- sum(nd[good])
  .t1_result(estimate = if (den > 0) sum(nd[good] * att[good]) / den else NaN,
             doses = doses, att = att, se = sev, acrt = acrt,
             acrt_dose = adose, n_zero = length(zero), n = n,
             method = "DR-DiD with continuous treatment intensity")
}
