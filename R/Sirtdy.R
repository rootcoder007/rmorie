# SPDX-License-Identifier: AGPL-3.0-or-later
#' Age-structured SIR driven by a social contact matrix
#'
#' SIR stratified into age groups, with mixing supplied by a POLYMOD-style
#' contact matrix.  For group i with \code{N_i = S_i + I_i + R_i},
#' \code{dS_i/dt = -S_i sum_j C_ij I_j / N_j},
#' \code{dI_i/dt = S_i sum_j C_ij I_j / N_j - gamma I_i},
#' \code{dR_i/dt = gamma I_i},
#' where \code{C_ij} is the effective transmission rate from an infectious
#' member of group j to a susceptible in group i, that is, the mean number
#' of contacts per unit time scaled by the per-contact transmission
#' probability.  Mossong et al. report the contact counts; multiplying them
#' by the transmission probability gives C.
#'
#' The basic reproduction number is the spectral radius of the
#' next-generation matrix \code{K_ij = S_i(0) C_ij / (gamma N_j)}, computed
#' by power iteration from a uniform start vector, which is deterministic and
#' so identical across language arms.  With one age group this collapses to
#' the scalar \code{R0 = C S0 / (gamma N)}.
#'
#' @param S,I,R Initial counts per age group; all three the same length.
#' @param contact_matrix Square matrix C, one row/column per age group.
#' @param gamma Recovery rate, common to all groups.
#' @param t_max Integration horizon. Default 160.
#' @param dt Runge-Kutta step. Default 0.1.
#' @return List with \code{estimate} (overall attack rate), \code{S},
#'   \code{I}, \code{R}, \code{N}, \code{attack_rate}, \code{R0},
#'   \code{peak_prevalence}, \code{peak_time}, \code{n_groups},
#'   \code{population}, \code{conservation_error}, \code{gamma},
#'   \code{t_max}, \code{dt}, \code{method}.
#' @references Mossong, J. et al. (2008). Social contacts and mixing
#'   patterns relevant to the spread of infectious diseases. PLoS Medicine
#'   5(3), e74. \doi{10.1371/journal.pmed.0050074}
#' @export
Sirtdy <- function(S, I, R, contact_matrix, gamma, t_max = 160, dt = 0.1) {
  s <- .s03vec(S); i0 <- .s03vec(I); r0 <- .s03vec(R)
  m <- length(s)
  if (m == 0L || length(i0) != m || length(r0) != m)
    stop("sir_age_structured: S, I and R must have the same non-zero length")
  C <- .s03mat(contact_matrix)
  if (nrow(C) != m || ncol(C) != m)
    stop("sir_age_structured: contact_matrix must be m x m for m age groups")
  gamma <- as.numeric(gamma)
  if (gamma < 0) stop("sir_age_structured: gamma must be non-negative")
  if (any(c(s, i0, r0) < 0)) stop("sir_age_structured: compartment sizes must be non-negative")
  t_max <- as.numeric(t_max); dt <- as.numeric(dt)
  if (dt <= 0 || t_max < 0) stop("sir_age_structured: need dt > 0 and t_max >= 0")

  N <- s + i0 + r0
  if (any(N <= 0)) stop("sir_age_structured: every age group must have positive size")

  S0 <- s
  sv <- s; iv <- i0; rv <- r0

  deriv <- function(a, b, cc) {
    da <- numeric(m); db <- numeric(m); dc <- numeric(m)
    for (k in seq_len(m)) {
      lam <- 0
      for (j in seq_len(m)) lam <- lam + C[k, j] * b[j] / N[j]
      f <- a[k] * lam
      da[k] <- -f
      db[k] <- f - gamma * b[k]
      dc[k] <- gamma * b[k]
    }
    list(da, db, dc)
  }

  nsteps <- as.integer(round(t_max / dt))
  Ntot <- sum(N)
  peak_I <- sum(iv) / Ntot
  peak_time <- 0
  for (step in seq_len(nsteps)) {
    k1 <- deriv(sv, iv, rv)
    k2 <- deriv(sv + 0.5 * dt * k1[[1]], iv + 0.5 * dt * k1[[2]], rv + 0.5 * dt * k1[[3]])
    k3 <- deriv(sv + 0.5 * dt * k2[[1]], iv + 0.5 * dt * k2[[2]], rv + 0.5 * dt * k2[[3]])
    k4 <- deriv(sv + dt * k3[[1]], iv + dt * k3[[2]], rv + dt * k3[[3]])
    sv <- sv + (dt / 6) * (k1[[1]] + 2 * k2[[1]] + 2 * k3[[1]] + k4[[1]])
    iv <- iv + (dt / 6) * (k1[[2]] + 2 * k2[[2]] + 2 * k3[[2]] + k4[[2]])
    rv <- rv + (dt / 6) * (k1[[3]] + 2 * k2[[3]] + 2 * k3[[3]] + k4[[3]])
    cur <- sum(iv) / Ntot
    if (cur > peak_I) { peak_I <- cur; peak_time <- step * dt }
  }

  K <- matrix(0, m, m)
  for (a in seq_len(m)) for (b in seq_len(m))
    K[a, b] <- if (gamma > 0) S0[a] * C[a, b] / (gamma * N[b]) else Inf
  if (gamma > 0) {
    v <- rep(1 / m, m)
    lam <- 0
    for (it in seq_len(2000L)) {
      w <- numeric(m)
      for (a in seq_len(m)) {
        acc <- 0
        for (b in seq_len(m)) acc <- acc + K[a, b] * v[b]
        w[a] <- acc
      }
      nrm <- sum(abs(w))
      if (nrm <= 0) { lam <- 0; break }
      v <- w / nrm
      lam <- nrm
    }
  } else {
    lam <- Inf
  }

  .t1_result(estimate = sum(rv) / Ntot, S = sv, I = iv, R = rv, N = N,
             attack_rate = sum(rv) / Ntot, R0 = lam,
             peak_prevalence = peak_I, peak_time = peak_time,
             n_groups = m, population = Ntot,
             conservation_error = max(abs(sv + iv + rv - N)),
             gamma = gamma, t_max = t_max, dt = dt,
             method = "Age-structured SIR with contact matrix (Mossong et al. 2008)")
}
