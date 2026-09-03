# SPDX-License-Identifier: AGPL-3.0-or-later
#' LP-DiD: local-projection difference-in-differences with clean controls
#'
#' For each horizon \code{h} the contrast
#' \code{y_{i,t+h} - y_{i,t-1}} is taken between newly treated
#' observations (\code{dD_it = 1}) and clean controls
#' (\code{D_{i,t+h} = 0}), the sample restriction of equation (8) of the
#' paper.  Excluding the already-treated units from the comparison group
#' removes the negative weights that make dynamic two-way fixed effects
#' uninterpretable under heterogeneous effects.  Each horizon is
#' estimated by the doubly robust moment of Sant'Anna and Zhao (2020).
#'
#' @param y Outcome, long format, one entry per unit-period.
#' @param D Binary treatment indicator.
#' @param unit,time Unit and period identifiers.
#' @param horizon Largest horizon reported; non-negative.
#' @param X Optional baseline covariates, one row per unit-period.
#' @return List with \code{estimate}, \code{horizons}, \code{beta},
#'   \code{se}, \code{n_cells}, \code{n}.
#' @references Dube, A., Girardi, D., Jorda, O. and Taylor, A. M. (2023).
#'   A local projections approach to difference-in-differences. NBER
#'   Working Paper 31184, equation (8); Journal of Applied Econometrics
#'   40(7), 741-758 (2025).  Sant'Anna, P. H. C. and Zhao, J. (2020).
#'   Journal of Econometrics 219(1), 101-122, equation (2.6).
#' @export
#' @examples
#' Drlp1(y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), unit = c(1, 2, 3,
#' 4, 5, 6, 7, 8), time = c(1, 2, 3, 4, 5, 6, 7, 8))
Drlp1 <- function(y, D, unit, time, horizon = 3L, X = NULL) {
  yv <- .s03vec(y)
  dv <- .s03vec(D)
  n <- length(yv)
  if (n == 0L) stop("Drlp1: empty input, y has no observations")
  u <- as.character(unit)
  tt <- as.numeric(time)
  if (length(dv) != n || length(u) != n || length(tt) != n)
    stop("Drlp1: y, D, unit and time must have the same length")
  H <- as.integer(horizon)
  if (H < 0L) stop("Drlp1: horizon must be non-negative")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  key <- paste(u, tt, sep = "\r")
  units <- unique(u)
  per <- sort(unique(tt))
  hs <- 0L:H
  beta <- numeric(0)
  sev <- numeric(0)
  ncell <- numeric(0)
  for (h in hs) {
    dys <- numeric(0)
    ds <- numeric(0)
    xs <- list()
    for (j in seq_along(per)[-1L]) {
      if (j + h > length(per)) next
      tc <- per[j]
      tp <- per[j - 1L]
      th <- per[j + h]
      for (z in units) {
        ih <- match(paste(z, th, sep = "\r"), key)
        ip <- match(paste(z, tp, sep = "\r"), key)
        ic <- match(paste(z, tc, sep = "\r"), key)
        if (is.na(ih) || is.na(ip) || is.na(ic)) next
        new <- dv[ic] >= 0.5 && dv[ip] < 0.5
        clean <- dv[ih] < 0.5
        if (!(new || clean)) next
        dys <- c(dys, yv[ih] - yv[ip])
        ds <- c(ds, if (new) 1 else 0)
        if (!is.null(Xr)) xs[[length(xs) + 1L]] <- Xr[ic, ]
      }
    }
    ncell <- c(ncell, length(dys))
    if (length(dys) < 3L || sum(ds) <= 0 || sum(ds) >= length(ds)) {
      beta <- c(beta, NaN)
      sev <- c(sev, NaN)
      next
    }
    xm <- if (is.null(Xr)) NULL else do.call(rbind, xs)
    f <- .s03drdid(dys, ds, xm)
    beta <- c(beta, f$tau)
    sev <- c(sev, f$se)
  }
  .t1_result(estimate = if (length(beta)) beta[1L] else NaN,
             horizons = hs, beta = beta, se = sev, n_cells = ncell, n = n,
             method = "DR-DiD via local projection")
}
