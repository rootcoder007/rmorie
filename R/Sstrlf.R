# SPDX-License-Identifier: AGPL-3.0-or-later
#' Left-truncated survival adjustment
#'
#' The risk set at t is conditional on entry < t, which is exactly the
#' delayed-entry Kaplan-Meier estimator.  A thin alias for \code{Lftrt};
#' the estimator already exists and is not duplicated here.
#'
#' @param entry Left-truncation (entry) times.
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @return List with \code{estimate} (survival at the last event time),
#'   \code{times}, \code{survival}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{n_obs}, \code{n_events}, \code{method}.
#' @references Klein, J. P. and Moeschberger, M. L. (2003). Survival
#'   Analysis: Techniques for Censored and Truncated Data, 2nd ed.
#'   Springer, section 4.2.
#' @examples
#' Sstrlf(c(0, 0, 1, 2), c(5, 6, 7, 8), c(1, 1, 0, 1))
#' @export
Sstrlf <- function(entry, time, event) {
  r <- Lftrt(entry, time, event)
  k <- length(r$survival)
  c(list(estimate = if (k) as.numeric(r$survival[k]) else NaN), r,
    list(method = "delayed-entry Kaplan-Meier [Klein & Moeschberger 2003]"))
}
