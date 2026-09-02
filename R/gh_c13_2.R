# SPDX-License-Identifier: AGPL-3.0-or-later
#' DP posterior survival and its Kaplan-Meier limit
#'
#' With F ~ DP(alpha G0) and right-censored data the posterior mean
#' survival interpolates between prior and data, and as alpha tends to
#' zero it converges to the KAPLAN-MEIER estimator.  That limit is the
#' point of the section: it shows the DP prior smuggles in no
#' information when alpha is small, and it gives the classical
#' frequentist estimator a Bayesian reading.  The discrepancy is
#' computed rather than asserted, so shrinking alpha must shrink it.
#'
#' Formula: S_KM(t) = prod_\{t_j <= t\} (1 - d_j / R_j);
#'   S_DP = w S_KM + (1 - w) exp(-rate t), w = n / (alpha + n).
#'
#' @param x Observed times, non-negative.
#' @param event Event indicators 0/1; all events when NULL.
#' @param alpha DP concentration, positive.
#' @param g0_rate Rate of the exponential base measure; from the data
#'   when NULL.
#' @return List with \code{times}, \code{survival_dp},
#'   \code{survival_km}, \code{max_abs_diff_to_km}, \code{alpha},
#'   \code{limit_note}, \code{n_events}, \code{n}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.2; Susarla &
#'   Van Ryzin (1976); Kaplan & Meier (1958).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalsurvdpkm(V)
Ghosalsurvdpkm <- function(x, event = NULL, alpha = 1, g0_rate = NULL) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) stop(sprintf("need at least 2 observations, got %d.", n))
  if (any(xv < 0)) stop("times must be non-negative.")
  ev <- if (is.null(event)) rep(1, n) else as.numeric(event)
  if (length(ev) != n)
    stop(sprintf("event has %d entries for %d times.", length(ev), n))
  if (!all(ev %in% c(0, 1))) stop("event must be binary 0/1.")
  a <- as.numeric(alpha)
  if (a <= 0) stop(sprintf("alpha must be positive, got %g.", a))
  ord <- order(xv)
  ts <- xv[ord]; es <- ev[ord]
  uniq <- sort(unique(ts))
  km <- numeric(length(uniq))
  surv <- 1
  for (i in seq_along(uniq)) {
    t <- uniq[i]
    at_risk <- sum(ts >= t)
    deaths <- sum(ts == t & es == 1)
    if (at_risk > 0 && deaths > 0) surv <- surv * (1 - deaths / at_risk)
    km[i] <- surv
  }
  rate <- if (is.null(g0_rate)) 1 / max(mean(xv), 1e-12) else as.numeric(g0_rate)
  wt <- n / (a + n)
  dp <- wt * km + (1 - wt) * exp(-rate * uniq)
  .t1_result(times = uniq, survival_dp = dp, survival_km = km,
             max_abs_diff_to_km = max(abs(dp - km)), alpha = a,
             limit_note = paste0("alpha -> 0 gives Kaplan-Meier exactly; ",
                                 "alpha -> infinity gives the base measure"),
             n_events = sum(ev), n = n,
             method = "DP posterior survival (Sec. 13.2); Kaplan-Meier is the alpha -> 0 limit")
}
