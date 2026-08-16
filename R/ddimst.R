# SPDX-License-Identifier: AGPL-3.0-or-later

# Nichol-Dhariwal cosine schedule for the cumulative alpha.
#' Nichol-Dhariwal cosine schedule for the cumulative alpha
#'
#' A step of the ddimst implementation. Called by \code{Ddimst}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param t Passed to \code{f}.
#' @param T Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.ddim_alpha_bar <- function(t, T) {
  f <- function(u) cos((u / T + 0.008) / 1.008 * pi / 2)^2
  f(t) / f(0)
}

#' One DDIM reverse step
#'
#' Formula: non-Markovian deterministic reverse
#'
#' x_{t-1} = sqrt(a_{t-1}) x0_hat + sqrt(1 - a_{t-1} - sigma^2) eps
#' + sigma z, with x0_hat = (x_t - sqrt(1 - a_t) eps) / sqrt(a_t) and
#' sigma = eta sqrt((1 - a_{t-1})/(1 - a_t)) sqrt(1 - a_t/a_{t-1}).  At
#' eta = 0 the update is fully deterministic (that is DDIM); at eta = 1
#' it reproduces the DDPM ancestral step.  The noise z is taken as zero
#' so the map is a function of its arguments alone.
#'
#' @param x_t Current latent.
#' @param t Current timestep, at least 1.
#' @param eps_theta Predicted noise, same length as x_t.
#' @param eta Stochasticity in [0, 1].
#' @param T Total number of steps for the cosine schedule.
#' @param alpha_bar_t,alpha_bar_prev Optional explicit cumulative alphas.
#' @return List with \code{estimate}, \code{x_prev}, \code{x0_pred},
#'   \code{sigma}, \code{alpha_bar_t}, \code{alpha_bar_prev}, \code{n},
#'   \code{method}.
#' @references Song, Meng & Ermon (2021), Denoising Diffusion Implicit
#'   Models, ICLR 2021.
#' @export
Ddimst <- function(x_t, t, eps_theta, eta = 0, T = 1000,
                   alpha_bar_t = NULL, alpha_bar_prev = NULL) {
  x <- .s03vec(x_t); e <- .s03vec(eps_theta)
  n <- length(x)
  if (n == 0L) stop("empty input: x_t has no entries")
  if (length(e) != n) stop("x_t and eps_theta must have the same length")
  t <- as.integer(t)
  if (t < 1L) stop("t must be at least 1")
  if (!(eta >= 0 && eta <= 1)) stop("eta must lie in [0, 1]")
  at <- if (is.null(alpha_bar_t)) .ddim_alpha_bar(t, T) else as.numeric(alpha_bar_t)
  ap <- if (is.null(alpha_bar_prev)) .ddim_alpha_bar(t - 1, T) else
    as.numeric(alpha_bar_prev)
  if (!(at > 0 && at <= 1 && ap > 0 && ap <= 1))
    stop("cumulative alphas must lie in (0, 1]")
  sigma <- eta * sqrt((1 - ap) / (1 - at)) * sqrt(max(1 - at / ap, 0))
  x0 <- (x - sqrt(1 - at) * e) / sqrt(at)
  cc <- sqrt(max(1 - ap - sigma * sigma, 0))
  xp <- sqrt(ap) * x0 + cc * e
  s <- 0
  for (v in xp) s <- s + v
  .t1_result(estimate = s / n, x_prev = xp, x0_pred = x0, sigma = sigma,
             alpha_bar_t = at, alpha_bar_prev = ap, n = n,
             method = "DDIM reverse step")
}
