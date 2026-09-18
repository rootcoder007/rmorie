# SPDX-License-Identifier: AGPL-3.0-or-later

# Aalen-Johansen estimator of F_k(t) plus the overall KM survival.
# F_k(t) = sum_{u <= t} S(u-) dN_k(u) / Y(u).  The S(u-) factor is the
# ALL-CAUSE Kaplan-Meier: leaving it out gives 1 - KM computed on the
# cause alone, which over-states the incidence whenever a competing
# event can happen first.
#' Aalen-Johansen estimator of F_k(t) plus the overall KM survival
#'
#' F_k(t) = sum_\{u <= t\} S(u-) dN_k(u) / Y(u).  The S(u-) factor is the
#' ALL-CAUSE Kaplan-Meier: leaving it out gives 1 - KM computed on the
#' cause alone, which over-states the incidence whenever a competing
#' event can happen first.
#'
#' @param time Passed to \code{.s03vec}.
#' @param event_type Passed to \code{.s03vec}.
#' @param cause Coerced to numeric by the body, with \code{as.numeric}.
#'   Defaults to \code{1}.
#' @return A list with \code{times}, \code{F}, \code{S}, \code{Y}, \code{dk}, \code{n}.
#' @export
.aalen_johansen <- function(time, event_type, cause = 1) {
  t <- .s03vec(time)
  n <- length(t)
  if (n == 0L) stop("empty input: time has no observations")
  ev <- .s03vec(event_type)
  if (length(ev) != n) stop("time and event_type must have the same length")
  if (any(t < 0)) stop("times must be non-negative")
  cause <- as.numeric(cause)
  o <- order(t, seq_len(n))
  ts <- t[o]
  es <- ev[o]
  times <- c()
  F <- c()
  S <- c()
  atrisk <- c()
  dk <- c()
  surv <- 1
  cif <- 0
  i <- 1L
  while (i <= n) {
    u <- ts[i]
    j <- i
    d_all <- 0L
    d_k <- 0L
    while (j <= n && ts[j] == u) {
      if (es[j] != 0) {
        d_all <- d_all + 1L
        if (es[j] == cause) d_k <- d_k + 1L
      }
      j <- j + 1L
    }
    Y <- n - i + 1L
    if (d_all > 0L) {
      cif <- cif + surv * d_k / Y
      surv <- surv * (1 - d_all / Y)
      times <- c(times, u)
      F <- c(F, cif)
      S <- c(S, surv)
      atrisk <- c(atrisk, Y)
      dk <- c(dk, d_k)
    }
    i <- j
  }
  list(times = times, F = F, S = S, Y = atrisk, dk = dk, n = n)
}

#' Cumulative incidence function under competing risks
#'
#' Formula: F_k(t) = integral S(u-) lambda_k(u) du
#'
#' The Aalen-Johansen estimator.  With a single cause and no competing
#' event it collapses to exactly 1 - KM(t), the identity used to check
#' it; with competing events it stays below that, because a subject who
#' fails of another cause can never fail of this one.
#'
#' @param time Follow-up time per subject.
#' @param event_type 0 for censored, otherwise the cause label.
#' @param cause Cause of interest.
#' @return List with \code{estimate}, \code{time}, \code{cif},
#'   \code{surv}, \code{n_risk}, \code{n_event}, \code{n},
#'   \code{method}.
#' @references Kalbfleisch & Prentice (2002), The Statistical Analysis
#'   of Failure Time Data, 2nd ed., Wiley, section 8.2; Aalen &
#'   Johansen (1978), Scand. J. Statist. 5(3):141-150.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Crrcim(V, V)
Crrcim <- function(time, event_type, cause = 1) {
  a <- .aalen_johansen(time, event_type, cause)
  .t1_result(estimate = if (length(a$F)) a$F[length(a$F)] else 0,
             time = a$times, cif = a$F, surv = a$S, n_risk = a$Y,
             n_event = a$dk, n = a$n,
             method = "Aalen-Johansen cumulative incidence function")
}
