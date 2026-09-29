.sir_rhs <- function(s, i, beta, gamma, N) {
  inf <- beta * s * i / N
  c(-inf, inf - gamma * i, gamma * i)
}

#' SIR compartmental model by fourth-order Runge-Kutta
#'
#' R arm of \code{morie.fn.sirepi}: Kermack-McKendrick SIR, \eqn{dS/dt =
#' -\beta SI/N}, \eqn{dI/dt = \beta SI/N - \gamma I}, \eqn{dR/dt = \gamma I},
#' integrated by classical RK4 with step \code{dt} (last step shortened to
#' land on \code{T}); basic reproduction number, peak and the asymptotic
#' final size solving \eqn{S_\infty = S_0 \exp(-(\beta/(\gamma N))(N -
#' S_\infty - R_0))}.
#'
#' @param S0,I0,R0 Initial counts.
#' @param beta Transmission rate.
#' @param gamma Removal rate.
#' @param T Horizon.
#' @param dt Step.
#' @return List with \code{t}, \code{S}, \code{I}, \code{R},
#'   \code{R0_basic}, \code{peak_I}, \code{peak_time}, \code{final_S},
#'   \code{attack_rate}.
#' @references Kermack, W. O. and McKendrick, A. G. (1927). A contribution to
#'   the mathematical theory of epidemics. Proceedings of the Royal Society A
#'   115, 700-721.
#' @examples
#' Sirepi(990, 10, 0, 0.3, 0.1, 50, dt = 0.5)$final_S
#' @export
Sirepi <- function(S0, I0, R0, beta, gamma, T, dt = 0.1) {
  if (min(S0, I0, R0) < 0 || beta < 0 || gamma <= 0 || T <= 0 || dt <= 0) {
    stop("need non-negative counts, beta >= 0, gamma, T, dt > 0")
  }
  s <- S0
  i <- I0
  rr <- R0
  N <- s + i + rr
  t <- 0
  ts <- 0
  S <- s
  I <- i
  R <- rr
  while (t < T - 1e-12) {
    h <- min(dt, T - t)
    k1 <- .sir_rhs(s, i, beta, gamma, N)
    k2 <- .sir_rhs(s + h / 2 * k1[1], i + h / 2 * k1[2], beta, gamma, N)
    k3 <- .sir_rhs(s + h / 2 * k2[1], i + h / 2 * k2[2], beta, gamma, N)
    k4 <- .sir_rhs(s + h * k3[1], i + h * k3[2], beta, gamma, N)
    s <- s + h / 6 * (k1[1] + 2 * k2[1] + 2 * k3[1] + k4[1])
    i <- i + h / 6 * (k1[2] + 2 * k2[2] + 2 * k3[2] + k4[2])
    rr <- rr + h / 6 * (k1[3] + 2 * k2[3] + 2 * k3[3] + k4[3])
    t <- t + h
    ts <- c(ts, t)
    S <- c(S, s)
    I <- c(I, i)
    R <- c(R, rr)
  }
  k <- which.max(I)
  a <- beta / (gamma * N)
  lo <- 0
  hi <- S0
  for (it in 1:200) {
    mid <- 0.5 * (lo + hi)
    if (mid - S0 * exp(-a * (N - mid - R0)) < 0) lo <- mid else hi <- mid
  }
  fin <- 0.5 * (lo + hi)
  att <- if (S0 > 0) 1 - fin / S0 else 0
  list(t = ts, S = S, I = I, R = R, R0_basic = beta / gamma, peak_I = I[k], peak_time = ts[k], final_S = fin,
       attack_rate = att, estimate = att)
}
