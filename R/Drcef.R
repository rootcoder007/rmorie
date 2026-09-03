# SPDX-License-Identifier: AGPL-3.0-or-later
#' Callaway-Sant'Anna event-study aggregation of doubly robust ATT(g,t)
#'
#' The group-time effects at a fixed length of exposure \code{e = t - g}
#' are averaged with the cohort-share weights of the cohorts actually
#' observed at that horizon,
#' \code{theta_es(e) = sum_g 1{g + e <= T} P(G = g | G + e <= T) ATT(g, g + e)},
#' equation (3.4) of the paper.  Each \code{ATT(g, g + e)} is the doubly
#' robust panel moment of Sant'Anna and Zhao (2020) between the base
#' period \code{g - 1} and period \code{g + e}, against the never-treated.
#'
#' @param y Outcome, long format, one entry per unit-period.
#' @param D Treatment indicator, carried for the interface.
#' @param unit,time Unit and period identifiers.
#' @param cohort First treated period of the row's unit; 0 or Inf marks a
#'   never-treated unit.
#' @param X Optional baseline covariates, one row per unit-period.
#' @return List with \code{estimate}, \code{event_time}, \code{theta},
#'   \code{n_cohorts}, \code{weight_sum}, \code{n}.
#' @references Callaway, B. and Sant'Anna, P. H. C. (2021).
#'   Difference-in-differences with multiple time periods. Journal of
#'   Econometrics 225(2), 200-230, equation (3.4).
#'   Sant'Anna, P. H. C. and Zhao, J. (2020). Journal of Econometrics
#'   219(1), 101-122, equation (2.6).
#' @export
#' @examples
#' set.seed(1)
#' r <- Drcef(y = rnorm(10), D = rbinom(10, 1, 0.5), unit = rnorm(10), time =
#' sort(runif(10)), cohort = rnorm(10)); TRUE
Drcef <- function(y, D, unit, time, cohort, X = NULL) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("Drcef: empty input, y has no observations")
  u <- as.character(unit)
  tt <- as.numeric(time)
  gg <- as.numeric(cohort)
  if (length(u) != n || length(tt) != n || length(gg) != n)
    stop("Drcef: y, unit, time and cohort must have the same length")
  Xr <- if (is.null(X)) NULL else .s03mat(X)
  key <- paste(u, tt, sep = "\r")
  units <- unique(u)
  per <- sort(unique(tt))
  Tm <- per[length(per)]
  gof <- gg[match(units, u)]
  names(gof) <- units
  treated <- gof > 0 & is.finite(gof)
  cohorts <- sort(unique(gof[treated]))
  if (!length(cohorts)) stop("Drcef: no treated cohort in the panel")
  never <- units[!treated]
  if (!length(never)) stop("Drcef: no never-treated units to compare against")
  size <- vapply(cohorts, function(cc) sum(gof == cc), 0)
  es <- integer(0)
  for (cc in cohorts) if ((cc - 1) %in% per)
    es <- union(es, as.integer(round(per - cc)))
  es <- sort(es)
  ev <- integer(0)
  theta <- numeric(0)
  wsum <- numeric(0)
  for (e in es) {
    ok <- cohorts + e <= Tm & (cohorts + e) %in% per & (cohorts - 1) %in% per
    elig <- cohorts[ok]
    if (!length(elig)) next
    tot <- sum(size[ok])
    acc <- 0
    hit <- FALSE
    for (ci in seq_along(elig)) {
      cc <- elig[ci]
      dys <- numeric(0)
      ds <- numeric(0)
      xs <- list()
      for (z in units) {
        k1 <- paste(z, cc + e, sep = "\r")
        k0 <- paste(z, cc - 1, sep = "\r")
        i1 <- match(k1, key)
        i0 <- match(k0, key)
        if (is.na(i1) || is.na(i0)) next
        if (gof[[z]] == cc) lab <- 1 else if (z %in% never) lab <- 0 else next
        dys <- c(dys, yv[i1] - yv[i0])
        ds <- c(ds, lab)
        if (!is.null(Xr)) xs[[length(xs) + 1L]] <- Xr[i1, ]
      }
      if (length(dys) < 3L || sum(ds) <= 0 || sum(ds) >= length(ds)) next
      xm <- if (is.null(Xr)) NULL else do.call(rbind, xs)
      fit <- .s03drdid(dys, ds, xm)
      acc <- acc + (size[ok][ci] / tot) * fit$tau
      hit <- TRUE
    }
    if (!hit) next
    ev <- c(ev, e)
    theta <- c(theta, acc)
    wsum <- c(wsum, tot)
  }
  post <- theta[ev >= 0L]
  .t1_result(estimate = if (length(post)) .s03mean(post) else NaN,
             event_time = ev, theta = theta, n_cohorts = length(cohorts),
             weight_sum = wsum, n = n,
             method = "DR Callaway-Sant'Anna event-study aggregation")
}
