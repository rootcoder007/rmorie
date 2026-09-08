# SPDX-License-Identifier: AGPL-3.0-or-later
#' SEIR compartmental model with an exposed (latent) class
#'
#' Integrates the deterministic SEIR system
#' \code{dS/dt = -beta S I / N}, \code{dE/dt = beta S I / N - sigma E},
#' \code{dI/dt = sigma E - gamma I}, \code{dR/dt = gamma I}.
#'
#' The exposed class E holds individuals who are infected but not yet
#' infectious; they progress at rate sigma, so the mean latent period is
#' \code{1 / sigma}.  The basic reproduction number is \code{R0 = beta /
#' gamma}: the latent stage delays but does not alter the number of
#' secondary cases, because every exposed individual eventually becomes
#' infectious.  Population \code{N = S + E + I + R} is conserved.
#'
#' Integrated with classical fourth-order Runge-Kutta at a fixed step, so the
#' result is deterministic and identical in every language arm.
#'
#' @param S,E,I,R Initial counts in the four compartments.
#' @param beta Transmission rate.
#' @param sigma Rate of progression from exposed to infectious.
#' @param gamma Recovery rate.
#' @param t_max Integration horizon. Default 160.
#' @param dt Runge-Kutta step. Default 0.1.
#' @return List with \code{estimate} (final size), \code{S}, \code{E},
#'   \code{I}, \code{R}, \code{N}, \code{R0}, \code{latent_period},
#'   \code{infectious_period}, \code{peak_I}, \code{peak_time},
#'   \code{final_size}, \code{conservation_error}, \code{beta},
#'   \code{sigma}, \code{gamma}, \code{t_max}, \code{dt}, \code{method}.
#' @references Hethcote, H. W. (2000). The mathematics of infectious
#'   diseases. SIAM Review 42(4), 599-653.
#'   \doi{10.1137/S0036144500371907}
#' @export
#' @examples
#' Seirep(S = 990, E = 10, I = 0, R = 0, beta = 0.3, sigma = 0.2, gamma = 0.1)
Seirep <- function(S, E, I, R, beta, sigma, gamma, t_max = 160, dt = 0.1) {
  y <- c(as.numeric(S), as.numeric(E), as.numeric(I), as.numeric(R))
  beta <- as.numeric(beta)
  sigma <- as.numeric(sigma)
  gamma <- as.numeric(gamma)
  t_max <- as.numeric(t_max)
  dt <- as.numeric(dt)
  if (any(y < 0)) stop("seir_compartmental: compartment sizes must be non-negative")
  if (beta < 0 || sigma < 0 || gamma < 0) stop("seir_compartmental: rates must be non-negative")
  if (dt <= 0 || t_max < 0) stop("seir_compartmental: need dt > 0 and t_max >= 0")
  N <- y[1] + y[2] + y[3] + y[4]
  if (N <= 0) stop("seir_compartmental: total population must be positive")

  deriv <- function(v) {
    f <- beta * v[1] * v[3] / N
    c(-f, f - sigma * v[2], sigma * v[2] - gamma * v[3], gamma * v[3])
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

  r0 <- if (gamma > 0) beta / gamma else Inf
  .t1_result(estimate = y[4], S = y[1], E = y[2], I = y[3], R = y[4], N = N,
             R0 = r0,
             latent_period = if (sigma > 0) 1 / sigma else Inf,
             infectious_period = if (gamma > 0) 1 / gamma else Inf,
             peak_I = peak_I, peak_time = peak_time, final_size = y[4],
             conservation_error = abs(y[1] + y[2] + y[3] + y[4] - N),
             beta = beta, sigma = sigma, gamma = gamma,
             t_max = t_max, dt = dt,
             method = "SEIR compartmental model (Hethcote 2000)")
}
