# SPDX-License-Identifier: AGPL-3.0-or-later
#' Renewal-equation reproduction number with reporting delay
#'
#' Formula: I_t = R_t sum_\{tau=1\}^\{gmax\} g(tau) I_\{t-tau\}, so R_t = I_t / sum_tau g(tau) I_\{t-tau\}
#'
#' @param incidence Reported cases per time step.
#' @param gen_int Discretised generation-time pmf over lags 1..gmax.
#' @param delays Reporting-delay pmf over lags 0..xi_max.

#' @param incidence See Usage.
#' @param gen_int See Usage.
#' @param delays See Usage.
#' @return List with ``rt``, ``time``, ``infections``, ``shift``, ``mean_rt``, ``n``.
#' @references Abbott, Hellewell, Sherratt et al (2020), EpiNow2. Model definition verified against the package's own estimate_infections() vignette, which states I_t = R_t sum_tau g(tau) I_\{t-tau\} and the delay convolution D_t = xi sum_tau xi(tau) I_\{t-tau\}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Rtrenew(V, V)
Rtrenew <- function(incidence, gen_int, delays = NULL) {
  y <- .t1_vec(incidence)
  w <- .t1_vec(gen_int)
  if (sum(w) <= 0) stop("gen_int must have positive mass")
  w <- w / sum(w)
  shift <- 0L
  if (!is.null(delays)) {
    d <- .t1_vec(delays)
    if (sum(d) <= 0) stop("delays must have positive mass")
    d <- d / sum(d)
    shift <- as.integer(round(sum((seq_along(d) - 1) * d)))
  }
  infections <- if (shift > 0) y[-seq_len(shift)] else y
  n <- length(infections)
  s <- length(w)
  times <- integer(0)
  rt <- numeric(0)
  if (n > s) for (t in (s + 1L):n) {
    force <- sum(w * infections[(t - 1L):(t - s)])
    times <- c(times, t - 1L)
    rt <- c(rt, if (force > 0) infections[t] / force else NA_real_)
  }
  .t1_result(rt = rt, time = times, infections = infections, shift = shift,
             mean_rt = mean(rt[!is.na(rt)]), n = n,
             method = "Renewal-equation Rt with reporting delay")
}
