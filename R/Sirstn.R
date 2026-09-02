# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stochastic SIR by Gillespie's direct method
#'
#' Simulates the SIR continuous-time Markov chain exactly.  Two reaction
#' channels act on integer counts (S, I, R):
#' \code{a1 = beta S I / N} taking \code{(S, I) -> (S - 1, I + 1)}, and
#' \code{a2 = gamma I} taking \code{(I, R) -> (I - 1, R + 1)}, with total
#' propensity \code{a0 = a1 + a2}.  Given two independent uniforms u1, u2 on
#' (0, 1) the direct method draws the waiting time and the channel as
#' \code{tau = -log(u1) / a0} and channel 1 if and only if
#' \code{u2 a0 < a1}, which is exact -- no time discretisation is involved.
#' The chain stops when \code{I = 0} (the absorbing state) or when the clock
#' passes T.
#'
#' The uniform stream is the Lehmer minstd generator
#' \code{s <- 48271 s mod (2^31 - 1)} shared with every other arm of this
#' package, so a given seed reproduces the same trajectory in R and in
#' Python bit for bit.
#'
#' @param S0,I0 Initial susceptible and infectious counts; N = S0 + I0.
#' @param beta Transmission rate.
#' @param gamma Recovery rate.
#' @param T Time horizon.
#' @param seed Seed for the shared minstd stream. Default 1.
#' @return List with \code{estimate} (final size), \code{S}, \code{I},
#'   \code{R}, \code{N}, \code{final_size}, \code{attack_rate},
#'   \code{peak_I}, \code{peak_time}, \code{t_end}, \code{extinction_time},
#'   \code{n_events}, \code{n_infections}, \code{R0}, \code{beta},
#'   \code{gamma}, \code{T}, \code{seed}, \code{method}.
#' @references Gillespie, D. T. (1977). Exact stochastic simulation of
#'   coupled chemical reactions. Journal of Physical Chemistry 81(25),
#'   2340-2361. \doi{10.1021/j100540a008}
#' @export
#' @examples
#' Sirstn(S0 = 990, I0 = 10, beta = 0.3, gamma = 0.1, T = 100)
Sirstn <- function(S0, I0, beta, gamma, T, seed = 1) {
  S <- as.numeric(S0)
  I <- as.numeric(I0)
  R <- 0
  beta <- as.numeric(beta)
  gamma <- as.numeric(gamma)
  T <- as.numeric(T)
  if (S < 0 || I < 0) stop("sir_stochastic: S0 and I0 must be non-negative")
  if (beta < 0 || gamma < 0) stop("sir_stochastic: beta and gamma must be non-negative")
  if (T < 0) stop("sir_stochastic: T must be non-negative")
  N <- S + I
  if (N <= 0) stop("sir_stochastic: total population must be positive")

  rng <- .t1_lcg(seed)
  t <- 0
  peak_I <- I
  peak_time <- 0
  n_events <- 0
  n_infections <- 0
  extinction_time <- NA_real_

  repeat {
    a1 <- beta * S * I / N
    a2 <- gamma * I
    a0 <- a1 + a2
    if (a0 <= 0) { extinction_time <- t
    break }
    tau <- -log(rng$unif()) / a0
    if (t + tau > T) { t <- T
    break }
    t <- t + tau
    if (rng$unif() * a0 < a1) {
      S <- S - 1
      I <- I + 1
      n_infections <- n_infections + 1
      if (I > peak_I) { peak_I <- I
      peak_time <- t }
    } else {
      I <- I - 1
      R <- R + 1
    }
    n_events <- n_events + 1
  }

  r0 <- if (gamma > 0) beta / gamma else Inf
  .t1_result(estimate = R, S = S, I = I, R = R, N = N, final_size = R,
             attack_rate = R / N, peak_I = peak_I, peak_time = peak_time,
             t_end = t, extinction_time = extinction_time,
             n_events = n_events, n_infections = n_infections, R0 = r0,
             beta = beta, gamma = gamma, T = T, seed = seed,
             method = "Stochastic SIR, Gillespie direct method (Gillespie 1977)")
}
