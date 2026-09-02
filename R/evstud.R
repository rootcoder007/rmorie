# SPDX-License-Identifier: AGPL-3.0-or-later

#' Event-study leads + lags coefficients
#'
#' Formula: the dynamic two-way fixed effects specification
#' \preformatted{
#'   y_it = alpha_i + lambda_t + sum_{e != ref} mu_e 1{t - g_i = e} + eps
#' }
#' where g_i is unit i's treatment cohort and e = t - g_i is event time.
#' One event-time indicator must be dropped for identification against
#' the unit and period effects; e = ref (conventionally -1) is omitted,
#' so every mu_e is read relative to it.  Never-treated units (cohort NA
#' or infinite) enter only through alpha_i and lambda_t and act as a
#' clean control group.
#'
#' CAVEAT, not a defect of the code: with heterogeneous, dynamic
#' treatment effects these mu_e are contaminated -- weighted sums of
#' cohort-specific effects at OTHER relative periods, with weights that
#' can be negative.  Sun & Abraham (2021) prove this and give the
#' interaction-weighted alternative.
#'
#' @param y Outcome, one entry per unit-period.
#' @param D Treatment indicator (carried for reference; the event
#'   dummies are built from \code{cohort} and \code{time}).
#' @param unit,time Unit and period labels.
#' @param cohort First treated period per observation; NA or Inf marks a
#'   never-treated unit.
#' @param max_lead,max_lag Truncate the window to \[-max_lead, max_lag\].
#' @param ref Omitted event time.
#' @return List with \code{estimate}, \code{event_times}, \code{coef},
#'   \code{se}, \code{sigma2}, \code{resid_df}, \code{n_units},
#'   \code{n_periods}, \code{n}, \code{method}.
#' @references Sun & Abraham (2021), Journal of Econometrics
#'   225(2):175-199, doi:10.1016/j.jeconom.2020.09.006; Borusyak &
#'   Jaravel (2017), "Revisiting Event Study Designs", working paper.
#' @export
Evstud <- function(y, D, unit, time, cohort, max_lead = NULL, max_lag = NULL,
                   ref = -1) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  D <- as.numeric(D); unit <- as.character(unit)
  time <- as.numeric(time); coh <- as.numeric(cohort)
  if (!(length(D) == n && length(unit) == n && length(time) == n &&
        length(coh) == n))
    stop("y, D, unit, time and cohort must have equal length")
  us <- sort(unique(unit)); ts <- sort(unique(time))
  U <- length(us); Tn <- length(ts)
  if (U < 2L || Tn < 2L) stop("need at least two units and two periods")
  ui <- match(unit, us) - 1L
  ti <- match(time, ts) - 1L
  ev <- ifelse(is.na(coh) | is.infinite(coh), NA_integer_,
               as.integer(round(time - coh)))
  seen <- sort(unique(ev[!is.na(ev)]))
  if (!is.null(max_lead)) seen <- seen[seen >= -as.integer(max_lead)]
  if (!is.null(max_lag)) seen <- seen[seen <= as.integer(max_lag)]
  ref <- as.integer(ref)
  ets <- seen[seen != ref]
  if (length(ets) == 0L)
    stop("no event-time indicators left after dropping ref")
  p <- 1L + (U - 1L) + (Tn - 1L) + length(ets)
  if (n <= p)
    stop(sprintf("more parameters (%d) than observations (%d)", p, n))
  X <- matrix(0, n, p)
  X[, 1L] <- 1
  for (k in seq_len(U - 1L)) X[, 1L + k] <- as.numeric(ui == k)
  for (k in seq_len(Tn - 1L)) X[, U + k] <- as.numeric(ti == k)
  off <- 1L + (U - 1L) + (Tn - 1L)
  for (k in seq_along(ets))
    X[, off + k] <- as.numeric(!is.na(ev) & ev == ets[k])
  b <- .s03lstsq(X, y, 0)
  resid <- y - as.numeric(X %*% b)
  dof <- n - p
  s2 <- sum(resid * resid) / dof
  XtX <- matrix(0, p, p)
  for (a in seq_len(p)) for (cc in seq_len(p)) XtX[a, cc] <- sum(X[, a] * X[, cc])
  coef <- numeric(length(ets)); se <- numeric(length(ets))
  for (k in seq_along(ets)) {
    j <- off + k
    e <- numeric(p); e[j] <- 1
    col <- .s03cholsolve(XtX, e)
    coef[k] <- b[j]
    se[k] <- sqrt(s2 * col[j])
  }
  est <- if (any(ets == 0L)) coef[which(ets == 0L)[1]] else NaN
  .t1_result(estimate = est, event_times = ets, coef = coef, se = se,
             sigma2 = s2, resid_df = dof, n_units = U, n_periods = Tn,
             n = n, method = "Event-study leads + lags coefficients")
}
