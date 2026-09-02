# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dynamic (event-study) doubly robust DiD
#'
#' Callaway and Sant'Anna (2021), Difference-in-differences with multiple
#' time periods, Journal of Econometrics 225(2), 200-230
#' (arXiv:1803.09015 -- FETCHED), define ATT(g, t) = E\[Y_t(g) - Y_t(0) |
#' G_g = 1] and the event-time aggregation over cohorts observed at each
#' horizon; Sant'Anna and Zhao (2020), Journal of Econometrics 219(1),
#' 101-122 (arXiv:1812.01723 -- FETCHED), equation (2.6) supplies the
#' doubly robust estimator for each (g, t) cell, with never-treated units
#' as the comparison group and g-1 as the base period.  Negative horizons
#' are pre-treatment placebo cells computed on the same footing; their
#' being near zero IS the evidence for parallel trends, so they are
#' reported rather than suppressed.
#'
#' @param y outcome in long format.
#' @param D treatment indicator (unused when cohort is given).
#' @param unit,time unit and period identifiers.
#' @param cohort first treated period per row; 0 or Inf = never treated.
#' @param horizon largest |event time| reported.
#' @param X baseline covariates, one row per unit-period.
#' @return list: estimate, event_time, att, n_cells, method.
#' @keywords internal
#' @examples
#' # see the parity harness for a worked panel
#' Drdiddyn(c(1, 2, 1, 2), NULL, c("a", "a", "b", "b"), c(1, 2, 1, 2),
#'          c(2, 2, 0, 0), 1)$event_time
#' @export
Drdiddyn <- function(y, D = NULL, unit = NULL, time = NULL, cohort = NULL,
                     horizon = 3, X = NULL) {
  yv <- .s03vec(y)
  u <- as.character(unit)
  t <- as.numeric(time)
  g <- as.numeric(cohort)
  Xr <- if (!is.null(X)) .s03mat(X) else NULL
  units <- character(0)
  for (x in u) if (!(x %in% units)) units <- c(units, x)
  per <- sort(unique(t))
  key <- paste(u, t, sep = "|")
  gof <- setNames(g[match(units, u)], units)
  hs <- seq(-as.integer(horizon), as.integer(horizon))
  att <- numeric(length(hs))
  ncell <- integer(length(hs))
  post <- numeric(0)
  treatedg <- sort(unique(g[g > 0 & is.finite(g)]))
  for (hi in seq_along(hs)) {
    e <- hs[hi]
    dys <- numeric(0)
    ds <- numeric(0)
    xs <- list()
    for (uu in units) {
      gg <- gof[[uu]]
      treated <- gg > 0 && is.finite(gg)
      if (treated) {
        p <- gg + e
        base <- gg - 1
        i1 <- match(paste(uu, p, sep = "|"), key)
        i0 <- match(paste(uu, base, sep = "|"), key)
        if (!is.na(i1) && !is.na(i0)) {
          dys <- c(dys, yv[i1] - yv[i0])
          ds <- c(ds, 1)
          if (!is.null(Xr)) xs[[length(xs) + 1L]] <- Xr[i1, ]
        }
      } else {
        for (gg2 in treatedg) {
          p <- gg2 + e
          base <- gg2 - 1
          i1 <- match(paste(uu, p, sep = "|"), key)
          i0 <- match(paste(uu, base, sep = "|"), key)
          if (!is.na(i1) && !is.na(i0)) {
            dys <- c(dys, yv[i1] - yv[i0])
            ds <- c(ds, 0)
            if (!is.null(Xr)) xs[[length(xs) + 1L]] <- Xr[i1, ]
          }
        }
      }
    }
    if (length(dys) < 3L || sum(ds) <= 0 || sum(ds) >= length(ds)) {
      att[hi] <- NaN
      ncell[hi] <- length(dys)
      next
    }
    fit <- .s03drdid(dys, ds, if (!is.null(Xr)) do.call(rbind, xs) else NULL)
    att[hi] <- fit$tau
    ncell[hi] <- length(dys)
    if (e >= 0) post <- c(post, fit$tau)
  }
  good <- post[!is.nan(post)]
  list(estimate = if (length(good)) .s03mean(good) else NaN,
       event_time = hs, att = att, n_cells = ncell,
       method = "Event-time ATT(g, g+e) (Callaway and Sant'Anna 2021) estimated by DR-DiD (Sant'Anna and Zhao 2020)")
}
