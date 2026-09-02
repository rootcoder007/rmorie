# SPDX-License-Identifier: AGPL-3.0-or-later
#' Concordance statistic (C-index) for survival models
#'
#' A pair (i, j) is comparable only if subject i had an event and either
#' t_i < t_j, or the two times are equal and j also had an event.  The
#' index is (concordant + 0.5 tied) / comparable, with higher risk score
#' meaning shorter expected survival.  Harrell's version weights every
#' comparable pair equally and is therefore dependent on the censoring
#' distribution; Uno's version reweights each pair by the inverse squared
#' censoring survival and truncates at the 75th percentile of the event
#' times, which removes that dependence.
#'
#' R arm of the existing Python \code{cstat} module.
#'
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param risk_score Predicted risk; higher means shorter survival.
#' @param method Either "harrell" (default) or "uno".
#' @return List with \code{c_statistic}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{concordant}, \code{discordant}, \code{tied},
#'   \code{comparable}.
#' @references Harrell, F. E. et al. (1982). JAMA 247(18):2543-2546.
#'   \doi{10.1001/jama.1982.03320430047030}
#'   Uno, H. et al. (2011). Statistics in Medicine 30(10):1105-1117.
#'   \doi{10.1002/sim.4154}
#' @examples
#' Cstat(c(1, 2, 3, 4), c(1, 1, 1, 0), c(4, 3, 2, 1))
#' @export
Cstat <- function(time, event, risk_score, method = "harrell") {
  time <- .s03vec(time)
  event <- .s03vec(event)
  risk_score <- .s03vec(risk_score)
  n <- length(time)
  if (length(event) != n || length(risk_score) != n)
    stop("time, event, and risk_score must have the same length.")
  if (!(method %in% c("harrell", "uno")))
    stop("method must be 'harrell' or 'uno'.")
  if (method == "uno") return(.cstat_uno(time, event, risk_score))

  concordant <- 0
  discordant <- 0
  tied <- 0
  comparable <- 0
  for (i in seq_len(n)) {
    if (event[i] == 0) next
    for (j in seq_len(n)) {
      if (i == j) next
      cmp <- (time[i] < time[j]) || (time[i] == time[j] && event[j] == 1)
      if (!cmp) next
      comparable <- comparable + 1
      if (risk_score[i] > risk_score[j]) concordant <- concordant + 1
      else if (risk_score[i] < risk_score[j]) discordant <- discordant + 1
      else tied <- tied + 1
    }
  }
  if (comparable == 0)
    return(list(c_statistic = 0.5, se = NaN, ci_lower = NaN, ci_upper = NaN,
                concordant = as.integer(concordant),
                discordant = as.integer(discordant),
                tied = as.integer(tied), comparable = as.integer(comparable)))
  c <- (concordant + 0.5 * tied) / comparable
  se <- sqrt(max(2 * c * (1 - c) / comparable, 0))
  z <- 1.959963985
  list(c_statistic = as.numeric(c), se = as.numeric(se),
       ci_lower = as.numeric(max(c - z * se, 0)),
       ci_upper = as.numeric(min(c + z * se, 1)),
       concordant = as.integer(concordant), discordant = as.integer(discordant),
       tied = as.integer(tied), comparable = as.integer(comparable))
}

#' .cstat_uno
#'
#' A step of the Cstat implementation. Called by \code{Cstat}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param time A vector; its length is taken and its elements indexed.
#' @param event A vector; indexed elementwise.
#' @param risk_score A vector; indexed elementwise.
#' @return A list with \code{c_statistic}, \code{se}, \code{ci_lower}, \code{ci_upper}, \code{concordant}, \code{discordant}, \code{tied}, \code{comparable}.
#' @export
.cstat_uno <- function(time, event, risk_score) {
  n <- length(time)
  cen_event <- as.numeric(event == 0)
  ord <- order(time)
  t_s <- time[ord]
  e_s <- cen_event[ord]
  S_c <- 1
  km_c_times <- numeric(0)
  km_c_vals <- numeric(0)
  for (t_j in sort(unique(t_s[e_s == 1]))) {
    n_r <- sum(t_s >= t_j)
    n_e <- sum(t_s == t_j & e_s == 1)
    km_c_times <- c(km_c_times, t_j)
    km_c_vals <- c(km_c_vals, S_c)
    S_c <- S_c * (if (n_r > 0) 1 - n_e / n_r else 1)
  }
  tau <- if (sum(event) > 0) .s03quantile7(time[event == 1], 0.75) else max(time)
  get_km_c <- function(t) {
    if (length(km_c_times) == 0L) return(1)
    idx <- sum(km_c_times <= t) - 1L
    idx <- max(idx, 0L)
    km_c_vals[min(idx, length(km_c_vals) - 1L) + 1L]
  }
  num <- 0
  denom <- 0
  for (i in seq_len(n)) {
    if (event[i] == 0 || time[i] > tau) next
    w_i <- 1 / max(get_km_c(time[i])^2, 1e-10)
    for (j in seq_len(n)) {
      if (i == j) next
      if (time[i] < time[j]) {
        denom <- denom + w_i
        if (risk_score[i] > risk_score[j]) num <- num + w_i
        else if (risk_score[i] == risk_score[j]) num <- num + 0.5 * w_i
      }
    }
  }
  c <- if (denom > 0) num / denom else 0.5
  se <- sqrt(max(2 * c * (1 - c) / max(n, 1), 0))
  z <- 1.959963985
  list(c_statistic = as.numeric(c), se = as.numeric(se),
       ci_lower = as.numeric(max(c - z * se, 0)),
       ci_upper = as.numeric(min(c + z * se, 1)),
       concordant = as.integer(trunc(num)),
       discordant = as.integer(trunc(denom - num)),
       tied = 0L, comparable = as.integer(trunc(denom)))
}

# CANONICAL TEST
# perfectly ordered risk: C == 1
# stopifnot(abs(Cstat(c(1,2,3,4), c(1,1,1,0), c(4,3,2,1))$c_statistic - 1) < 1e-12)
