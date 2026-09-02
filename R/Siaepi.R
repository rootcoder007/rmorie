# SPDX-License-Identifier: AGPL-3.0-or-later
#' Network SIR epidemic, individual-based mean field
#'
#' Susceptible-Infected-Recovered spreading on a fixed network.  The
#' individual-based mean-field system, obtained from Pastor-Satorras &
#' Vespignani's eq. (7) by adding an absorbing recovered compartment instead
#' of returning recovered nodes to the susceptible pool:
#' \code{dS_i/dt = -beta S_i sum_j A_ij I_j},
#' \code{dI_i/dt = beta S_i sum_j A_ij I_j - gamma I_i},
#' \code{dR_i/dt = gamma I_i}.
#'
#' With \code{gamma = 0} the R compartment never fills and the system is
#' exactly the SI model of \code{\link{Siepid}}.  The per-node total
#' \code{S_i + I_i + R_i} is conserved by construction and is reported as
#' \code{conservation_error} so the integration can be checked.
#'
#' @param G Square adjacency matrix, one row/column per node.
#' @param beta Per-edge infection rate.
#' @param gamma Recovery rate into the absorbing R compartment.
#' @param initial Initial infection probability of each node, in \[0, 1\].
#' @param t_max Integration horizon. Default 50.
#' @param dt Runge-Kutta step. Default 0.01.
#' @return List with \code{estimate} (final attack rate), \code{S},
#'   \code{I}, \code{R}, \code{attack_rate}, \code{prevalence},
#'   \code{susceptible}, \code{peak_prevalence}, \code{peak_time},
#'   \code{mean_degree}, \code{conservation_error}, \code{n}, \code{beta},
#'   \code{gamma}, \code{t_max}, \code{dt}, \code{method}.
#' @references Pastor-Satorras, R. & Vespignani, A. (2001). Epidemic dynamics
#'   and endemic states in complex networks. Physical Review E 63, 066117,
#'   eq. (7). \doi{10.1103/PhysRevE.63.066117}
#' @export
#' @examples
#' G <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' Siaepi(G, beta = 0.3, gamma = 0.1, initial = c(1, 0, 0))
Siaepi <- function(G, beta, gamma, initial, t_max = 50, dt = 0.01) {
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L || ncol(A) != n) stop("sir_epidemic: G must be a square adjacency matrix")
  I <- .s03vec(initial)
  if (length(I) != n) stop("sir_epidemic: initial must have one entry per node")
  if (any(I < 0) || any(I > 1)) stop("sir_epidemic: initial probabilities must lie in [0, 1]")
  beta <- as.numeric(beta)
  gamma <- as.numeric(gamma)
  t_max <- as.numeric(t_max)
  dt <- as.numeric(dt)
  if (beta < 0 || gamma < 0) stop("sir_epidemic: beta and gamma must be non-negative")
  if (dt <= 0 || t_max < 0) stop("sir_epidemic: need dt > 0 and t_max >= 0")

  S <- 1 - I
  R <- numeric(n)

  deriv <- function(s, i, r) {
    ds <- numeric(n)
    di <- numeric(n)
    dr <- numeric(n)
    for (a in seq_len(n)) {
      f <- 0
      for (b in seq_len(n)) f <- f + A[a, b] * i[b]
      f <- beta * s[a] * f
      ds[a] <- -f
      di[a] <- f - gamma * i[a]
      dr[a] <- gamma * i[a]
    }
    list(ds, di, dr)
  }

  nsteps <- as.integer(round(t_max / dt))
  peak_I <- sum(I) / n
  peak_time <- 0
  for (step in seq_len(nsteps)) {
    k1 <- deriv(S, I, R)
    k2 <- deriv(S + 0.5 * dt * k1[[1]], I + 0.5 * dt * k1[[2]], R + 0.5 * dt * k1[[3]])
    k3 <- deriv(S + 0.5 * dt * k2[[1]], I + 0.5 * dt * k2[[2]], R + 0.5 * dt * k2[[3]])
    k4 <- deriv(S + dt * k3[[1]], I + dt * k3[[2]], R + dt * k3[[3]])
    S <- S + (dt / 6) * (k1[[1]] + 2 * k2[[1]] + 2 * k3[[1]] + k4[[1]])
    I <- I + (dt / 6) * (k1[[2]] + 2 * k2[[2]] + 2 * k3[[2]] + k4[[2]])
    R <- R + (dt / 6) * (k1[[3]] + 2 * k2[[3]] + 2 * k3[[3]] + k4[[3]])
    cur <- sum(I) / n
    if (cur > peak_I) { peak_I <- cur
    peak_time <- step * dt }
  }

  deg <- numeric(n)
  for (i in seq_len(n)) deg[i] <- sum(A[i, ])
  kbar <- sum(deg) / n
  cons <- max(abs(S + I + R - 1))

  .t1_result(estimate = sum(R) / n, S = S, I = I, R = R,
             attack_rate = sum(R) / n, prevalence = sum(I) / n,
             susceptible = sum(S) / n, peak_prevalence = peak_I,
             peak_time = peak_time, mean_degree = kbar,
             conservation_error = cons, n = n, beta = beta, gamma = gamma,
             t_max = t_max, dt = dt,
             method = "Network SIR epidemic, individual-based mean field (Pastor-Satorras & Vespignani 2001, eq. 7)")
}
