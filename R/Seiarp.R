# SPDX-License-Identifier: AGPL-3.0-or-later
#' SEIRA: SEIR with a parallel asymptomatic infectious compartment
#'
#' The latent class splits into two infectious streams, symptomatic and
#' asymptomatic:
#' \code{dS/dt = -S (beta I + kappa beta A) / N},
#' \code{dE/dt = S (beta I + kappa beta A) / N - sigma E},
#' \code{dI/dt = p sigma E - gamma I},
#' \code{dA/dt = (1 - p) sigma E - gamma_a A},
#' \code{dR/dt = gamma I + gamma_a A}.
#'
#' A fraction p of those leaving the latent class become symptomatic and a
#' fraction \code{1 - p} asymptomatic; asymptomatic carriers transmit at a
#' relative infectiousness kappa and clear at their own rate gamma_a.
#' Because the two infectious streams are in parallel, Anderson & May's
#' average over the infectious classes gives
#' \code{R0 = beta (p / gamma + kappa (1 - p) / gamma_a)}, the sum over
#' routes of (transmission rate) x (probability of entering the route) x
#' (mean time spent in it).  Setting \code{p = 1} collapses the A
#' compartment and recovers the SEIR value \code{R0 = beta / gamma}.
#'
#' @param S,E,I,A,R Initial counts in the five compartments.
#' @param params Six rates in this order: beta, sigma, gamma, p, kappa,
#'   gamma_a.
#' @param t_max Integration horizon. Default 160.
#' @param dt Runge-Kutta step. Default 0.1.
#' @return List with \code{estimate} (final size), \code{S}, \code{E},
#'   \code{I}, \code{A}, \code{R}, \code{N}, \code{R0},
#'   \code{R0_symptomatic}, \code{R0_asymptomatic},
#'   \code{asymptomatic_fraction}, \code{peak_I}, \code{peak_time},
#'   \code{final_size}, \code{conservation_error}, the six rates,
#'   \code{t_max}, \code{dt}, \code{method}.
#' @references Anderson, R. M. & May, R. M. (1991). Infectious Diseases of
#'   Humans: Dynamics and Control. Oxford University Press.
#'   ISBN 0-19-854040-X.
#' @export
#' @examples
#' Seiarp(S = 990, E = 10, I = 0, A = 0, R = 0,
#'        params = c(0.3, 0.2, 0.1, 0.05, 0.1, 0.5))
Seiarp <- function(S, E, I, A, R, params, t_max = 160, dt = 0.1) {
  pr <- .s03vec(params)
  if (length(pr) != 6L)
    stop("seira_asymptomatic: params must be (beta, sigma, gamma, p, kappa, gamma_a)")
  beta <- pr[1]
  sigma <- pr[2]
  gamma <- pr[3]
  p <- pr[4]
  kappa <- pr[5]
  gamma_a <- pr[6]
  if (beta < 0 || sigma < 0 || gamma < 0 || kappa < 0 || gamma_a < 0)
    stop("seira_asymptomatic: rates must be non-negative")
  if (p < 0 || p > 1) stop("seira_asymptomatic: p must lie in [0, 1]")
  y <- c(as.numeric(S), as.numeric(E), as.numeric(I), as.numeric(A), as.numeric(R))
  if (any(y < 0)) stop("seira_asymptomatic: compartment sizes must be non-negative")
  t_max <- as.numeric(t_max)
  dt <- as.numeric(dt)
  if (dt <= 0 || t_max < 0) stop("seira_asymptomatic: need dt > 0 and t_max >= 0")
  N <- y[1] + y[2] + y[3] + y[4] + y[5]
  if (N <= 0) stop("seira_asymptomatic: total population must be positive")

  deriv <- function(v) {
    f <- v[1] * (beta * v[3] + kappa * beta * v[4]) / N
    c(-f,
      f - sigma * v[2],
      p * sigma * v[2] - gamma * v[3],
      (1 - p) * sigma * v[2] - gamma_a * v[4],
      gamma * v[3] + gamma_a * v[4])
  }

  nsteps <- as.integer(round(t_max / dt))
  peak_I <- y[3]
  peak_time <- 0
  for (step in seq_len(nsteps)) {
    k1 <- deriv(y)
    k2 <- deriv(y + 0.5 * dt * k1)
    k3 <- deriv(y + 0.5 * dt * k2)
    k4 <- deriv(y + dt * k3)
    y <- y + (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    if (y[3] > peak_I) { peak_I <- y[3]
    peak_time <- step * dt }
  }

  sym <- if (gamma > 0) p / gamma else Inf
  asym <- if (gamma_a > 0) kappa * (1 - p) / gamma_a else Inf
  r0 <- beta * (sym + asym)

  .t1_result(estimate = y[5], S = y[1], E = y[2], I = y[3], A = y[4], R = y[5],
             N = N, R0 = r0, R0_symptomatic = beta * sym,
             R0_asymptomatic = beta * asym, asymptomatic_fraction = 1 - p,
             peak_I = peak_I, peak_time = peak_time, final_size = y[5],
             conservation_error = abs(y[1] + y[2] + y[3] + y[4] + y[5] - N),
             beta = beta, sigma = sigma, gamma = gamma, p = p,
             kappa = kappa, gamma_a = gamma_a, t_max = t_max, dt = dt,
             method = "SEIRA with asymptomatic compartment (Anderson & May 1991)")
}
