# SPDX-License-Identifier: AGPL-3.0-or-later
#' Network SIS epidemic, individual-based mean field
#'
#' Susceptible-Infected-Susceptible spreading on a fixed network, the
#' individual-based form of Pastor-Satorras & Vespignani eq. (7),
#' \code{d rho_i / dt = -gamma rho_i + beta (1 - rho_i) sum_j A_ij rho_j},
#' the recovered node returning directly to the susceptible pool.  With
#' \code{k_i = sum_j A_ij} and \code{Theta_i = sum_j A_ij rho_j / k_i} the
#' creation term is \code{beta k_i (1 - rho_i) Theta_i}, which is eq. (7)
#' verbatim; PSV set gamma = 1 so that only lambda = beta / gamma matters.
#'
#' \code{lambda_c} is the heterogeneous mean-field epidemic threshold
#' \code{<k> / <k^2>} obtained by linearising eqs. (8)-(9) about Theta = 0;
#' the endemic phase is lambda > lambda_c.  For a k-regular graph this is
#' \code{1 / k}, and PSV's homogeneous result \code{<k> lambda_c = 1} follows.
#'
#' @param G Square adjacency matrix, one row/column per node.
#' @param beta Per-edge infection rate.
#' @param gamma Recovery rate back to susceptible.
#' @param initial Initial infection probability of each node, in [0, 1].
#' @param t_max Integration horizon. Default 50.
#' @param dt Runge-Kutta step. Default 0.01.
#' @return List with \code{estimate} (final prevalence), \code{prevalence},
#'   \code{rho_final}, \code{rho_initial}, \code{theta}, \code{mean_degree},
#'   \code{second_moment}, \code{lambda}, \code{lambda_c}, \code{endemic},
#'   \code{n}, \code{beta}, \code{gamma}, \code{t_max}, \code{dt},
#'   \code{method}.
#' @references Pastor-Satorras, R. & Vespignani, A. (2001). Epidemic dynamics
#'   and endemic states in complex networks. Physical Review E 63, 066117,
#'   eqs. (7)-(10). \doi{10.1103/PhysRevE.63.066117}
#' @export
Sietrt <- function(G, beta, gamma, initial, t_max = 50, dt = 0.01) {
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L || ncol(A) != n) stop("sis_epidemic: G must be a square adjacency matrix")
  p <- .s03vec(initial)
  if (length(p) != n) stop("sis_epidemic: initial must have one entry per node")
  if (any(p < 0) || any(p > 1)) stop("sis_epidemic: initial probabilities must lie in [0, 1]")
  beta <- as.numeric(beta); gamma <- as.numeric(gamma)
  t_max <- as.numeric(t_max); dt <- as.numeric(dt)
  if (beta < 0 || gamma < 0) stop("sis_epidemic: beta and gamma must be non-negative")
  if (dt <= 0 || t_max < 0) stop("sis_epidemic: need dt > 0 and t_max >= 0")

  deriv <- function(x) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0
      for (j in seq_len(n)) s <- s + A[i, j] * x[j]
      out[i] <- -gamma * x[i] + beta * (1 - x[i]) * s
    }
    out
  }

  rho0 <- sum(p) / n
  nsteps <- as.integer(round(t_max / dt))
  for (step in seq_len(nsteps)) {
    k1 <- deriv(p)
    k2 <- deriv(p + 0.5 * dt * k1)
    k3 <- deriv(p + 0.5 * dt * k2)
    k4 <- deriv(p + dt * k3)
    p <- p + (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
  }

  deg <- numeric(n)
  for (i in seq_len(n)) deg[i] <- sum(A[i, ])
  kbar <- sum(deg) / n
  k2bar <- sum(deg * deg) / n
  lam <- if (gamma > 0) beta / gamma else Inf
  lam_c <- if (k2bar > 0) kbar / k2bar else NA_real_
  theta <- if (kbar > 0) sum(deg * p) / (n * kbar) else NA_real_
  rho <- sum(p) / n

  .t1_result(estimate = rho, prevalence = p, rho_final = rho,
             rho_initial = rho0, theta = theta, mean_degree = kbar,
             second_moment = k2bar, lambda = lam, lambda_c = lam_c,
             endemic = if (!is.na(lam_c) && lam > lam_c) 1 else 0,
             n = n, beta = beta, gamma = gamma, t_max = t_max, dt = dt,
             method = "Network SIS epidemic, individual-based mean field (Pastor-Satorras & Vespignani 2001, eq. 7)")
}
