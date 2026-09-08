# SPDX-License-Identifier: AGPL-3.0-or-later
#' Network SI epidemic, individual-based mean field
#'
#' Susceptible-Infected spreading on a fixed network.  Each node carries a
#' probability rho_i(t) of being infected and obeys the mean-field reaction
#' rate equation
#' \code{d rho_i / dt = beta (1 - rho_i) sum_j A_ij rho_j},
#' which is Pastor-Satorras & Vespignani's eq. (7) written per node rather
#' than per degree class: with \code{k_i = sum_j A_ij} and
#' \code{Theta_i = sum_j A_ij rho_j / k_i} the probability that a link out of
#' node i points at an infected node, the right-hand side is
#' \code{beta k_i (1 - rho_i) Theta_i}.  Dropping the recovery term
#' \code{-rho_i} of eq. (7) gives SI rather than SIS.
#'
#' Integrated with classical fourth-order Runge-Kutta on a fixed step, so the
#' result is deterministic and identical in every language arm.
#'
#' @param G Square adjacency matrix, one row/column per node.
#' @param beta Per-edge infection rate.
#' @param initial Initial infection probability of each node, in \[0, 1\].
#' @param t_max Integration horizon. Default 20.
#' @param dt Runge-Kutta step. Default 0.01.
#' @return List with \code{estimate} (final prevalence), \code{prevalence},
#'   \code{rho_final}, \code{rho_initial}, \code{theta} (eq. (9)),
#'   \code{mean_degree}, \code{second_moment}, \code{half_time}, \code{n},
#'   \code{beta}, \code{t_max}, \code{dt}, \code{method}.
#' @references Pastor-Satorras, R. & Vespignani, A. (2001). Epidemic dynamics
#'   and endemic states in complex networks. Physical Review E 63, 066117,
#'   eqs. (7)-(10). \doi{10.1103/PhysRevE.63.066117}
#' @export
#' @examples
#' G <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' Siepid(G, beta = 0.3, initial = c(1, 0, 0))
Siepid <- function(G, beta, initial, t_max = 20, dt = 0.01) {
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L || ncol(A) != n) stop("si_epidemic: G must be a square adjacency matrix")
  p <- .s03vec(initial)
  if (length(p) != n) stop("si_epidemic: initial must have one entry per node")
  if (any(p < 0) || any(p > 1)) stop("si_epidemic: initial probabilities must lie in [0, 1]")
  beta <- as.numeric(beta)
  t_max <- as.numeric(t_max)
  dt <- as.numeric(dt)
  if (beta < 0) stop("si_epidemic: beta must be non-negative")
  if (dt <= 0 || t_max < 0) stop("si_epidemic: need dt > 0 and t_max >= 0")

  deriv <- function(x) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0
      for (j in seq_len(n)) s <- s + A[i, j] * x[j]
      out[i] <- beta * (1 - x[i]) * s
    }
    out
  }

  nsteps <- as.integer(round(t_max / dt))
  rho0 <- sum(p) / n
  half_time <- NA_real_
  prev_rho <- rho0
  for (step in seq_len(nsteps)) {
    k1 <- deriv(p)
    k2 <- deriv(p + 0.5 * dt * k1)
    k3 <- deriv(p + 0.5 * dt * k2)
    k4 <- deriv(p + dt * k3)
    p <- p + (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    rho <- sum(p) / n
    if (prev_rho < 0.5 && rho >= 0.5) half_time <- step * dt
    prev_rho <- rho
  }

  deg <- numeric(n)
  for (i in seq_len(n)) deg[i] <- sum(A[i, ])
  kbar <- sum(deg) / n
  k2bar <- sum(deg * deg) / n
  theta <- if (kbar > 0) sum(deg * p) / (n * kbar) else NA_real_
  rho <- sum(p) / n

  .t1_result(estimate = rho, prevalence = p, rho_final = rho,
             rho_initial = rho0, theta = theta, mean_degree = kbar,
             second_moment = k2bar, half_time = half_time, n = n,
             beta = beta, t_max = t_max, dt = dt,
             method = "Network SI epidemic, individual-based mean field (Pastor-Satorras & Vespignani 2001, eq. 7)")
}
