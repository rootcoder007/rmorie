# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kaplan-Meier estimator with left truncation (delayed entry)
#'
#' Under delayed entry a subject is not at risk before its entry time, so
#' the risk set at t counts only those with entry < t <= observed time.
#' Ignoring the entry times inflates the early risk sets and biases
#' survival upwards.  The variance is Greenwood's, accumulated over the
#' same truncated risk sets.
#'
#' R arm of the existing Python \code{lftrt} module.
#'
#' Formula: S(t) = prod_\{t_j <= t\} (1 - d_j / n_j) with
#' n_j = #\{i : entry_i < t_j <= time_i\}.
#'
#' @param entry Left-truncation (entry) times.
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param alpha Significance level for the confidence interval.
#' @return List with \code{times}, \code{survival}, \code{se},
#'   \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{n_events}.
#' @references Klein, J. P. and Moeschberger, M. L. (2003). Survival
#'   Analysis: Techniques for Censored and Truncated Data, 2nd ed.
#'   Springer, section 4.2 and chapter 3.
#' @examples
#' Lftrt(c(0, 0, 1, 2), c(5, 6, 7, 8), c(1, 1, 0, 1))
#' @export
Lftrt <- function(entry, time, event, alpha = 0.05) {
  entry <- .s03vec(entry)
  time <- .s03vec(time)
  event <- .s03vec(event)
  n <- length(time)
  unique_t <- sort(unique(time[event == 1]))
  nt <- length(unique_t)
  surv <- numeric(nt)
  se_arr <- numeric(nt)
  z <- stats::qnorm(1 - alpha / 2)
  s <- 1
  gw <- 0
  for (j in seq_len(nt)) {
    tj <- unique_t[j]
    nj <- sum(entry < tj & time >= tj)
    dj <- sum(time == tj & event == 1)
    if (nj > 0) {
      s <- s * (1 - dj / nj)
      if (nj > dj) gw <- gw + dj / (nj * (nj - dj))
    }
    surv[j] <- s
    se_arr[j] <- s * sqrt(max(gw, 0))
  }
  list(times = unique_t, survival = surv, se = se_arr,
       ci_lower = pmax(surv - z * se_arr, 0),
       ci_upper = pmin(surv + z * se_arr, 1),
       n_obs = as.integer(n), n_events = as.integer(sum(event)))
}

# CANONICAL TEST
# r <- Lftrt(rep(0, 4), c(5, 6, 7, 8), c(1, 1, 1, 1))
# stopifnot(abs(r$survival[1] - 0.75) < 1e-12)
