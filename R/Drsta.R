# SPDX-License-Identifier: AGPL-3.0-or-later
#' Doubly robust DiD over a staggered adoption design: the ATT(g,t) table
#'
#' One doubly robust estimate per (cohort, event-time) cell the panel can
#' support, each between the base period \code{g - 1} and period
#' \code{g + e}, against the never-treated units.  Pre-treatment cells
#' are computed on identical footing and returned rather than suppressed.
#'
#' @param y Outcome, long format, one entry per unit-period.
#' @param D Treatment indicator, carried for the interface.
#' @param unit,time Unit and period identifiers.
#' @param cohort First treated period of the row's unit; 0 or Inf marks a
#'   never-treated unit.
#' @param X Optional baseline covariates, one row per unit-period.
#' @return List with \code{estimate}, \code{cohorts}, \code{event_time},
#'   \code{att} (row-major cohort x event-time), \code{n_cells},
#'   \code{n_post}, \code{n}.
#' @references Callaway, B. and Sant'Anna, P. H. C. (2021). Journal of
#'   Econometrics 225(2), 200-230.  Sant'Anna, P. H. C. and Zhao, J.
#'   (2020). Journal of Econometrics 219(1), 101-122, equation (2.6).
#'   Roth, J. and Sant'Anna, P. H. C. (2023). Journal of Political
#'   Economy Microeconomics 1(4), 669-709.
#' @export
#' @examples
#' set.seed(1)
#' r <- Drsta(y = rnorm(10), D = rbinom(10, 1, 0.5), unit = rnorm(10), time = sort(runif(10)), cohort = rnorm(10)); TRUE
Drsta <- function(y, D, unit, time, cohort, X = NULL) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("Drsta: empty input, y has no observations")
  u <- as.character(unit); tt <- as.numeric(time); gg <- as.numeric(cohort)
  if (length(u) != n || length(tt) != n || length(gg) != n)
    stop("Drsta: y, unit, time and cohort must have the same length")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  key <- paste(u, tt, sep = "\r")
  units <- unique(u); per <- sort(unique(tt))
  gof <- gg[match(units, u)]; names(gof) <- units
  treated <- gof > 0 & is.finite(gof)
  cohorts <- sort(unique(gof[treated]))
  if (!length(cohorts)) stop("Drsta: no treated cohort in the panel")
  never <- units[!treated]
  if (!length(never)) stop("Drsta: no never-treated units to compare against")
  lo <- as.integer(round(per[1L] - cohorts[length(cohorts)]))
  hi <- as.integer(round(per[length(per)] - cohorts[1L]))
  es <- lo:hi
  att <- numeric(0); ncell <- numeric(0)
  num <- 0; den <- 0; npost <- 0L
  for (cc in cohorts) {
    sz <- sum(gof == cc)
    for (e in es) {
      dys <- numeric(0); ds <- numeric(0); xs <- list()
      if ((cc - 1) %in% per && (cc + e) %in% per) {
        for (z in units) {
          k1 <- paste(z, cc + e, sep = "\r"); k0 <- paste(z, cc - 1, sep = "\r")
          i1 <- match(k1, key); i0 <- match(k0, key)
          if (is.na(i1) || is.na(i0)) next
          if (gof[[z]] == cc) lab <- 1 else if (z %in% never) lab <- 0 else next
          dys <- c(dys, yv[i1] - yv[i0]); ds <- c(ds, lab)
          if (!is.null(Xr)) xs[[length(xs) + 1L]] <- Xr[i1, ]
        }
      }
      if (length(dys) < 3L || sum(ds) <= 0 || sum(ds) >= length(ds)) {
        att <- c(att, NaN); ncell <- c(ncell, length(dys)); next
      }
      xm <- if (is.null(Xr)) NULL else do.call(rbind, xs)
      fit <- .s03drdid(dys, ds, xm)
      att <- c(att, fit$tau); ncell <- c(ncell, length(dys))
      if (e >= 0L) { num <- num + sz * fit$tau; den <- den + sz; npost <- npost + 1L }
    }
  }
  .t1_result(estimate = if (den > 0) num / den else NaN,
             cohorts = cohorts, event_time = es, att = att,
             n_cells = ncell, n_post = npost, n = n,
             method = "DR-DiD for staggered adoption")
}
