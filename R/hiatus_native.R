# Many-strain pathogen dynamics without the combinatorial blow-up.
#
# Sources:
#   Gog, J. R. & Grenfell, B. T. (2002) "Dynamics and selection of
#   many-strain pathogens", PNAS 99(26), 17209-17214.
#   Gog, J. R. & Swinton, J. (2002) "A status-based approach to
#   multiple strain dynamics", Journal of Mathematical Biology 44(2),
#   169-184.

._check <- function(beta, nu, mu, sigma) {
  b <- as.numeric(beta)
  n <- length(b)
  nv <- if (length(nu) == 1L) rep(as.numeric(nu), n) else as.numeric(nu)
  if (length(nv) != n) {
    stop(sprintf("hiatus: %d recovery rates for %d strains",
                 length(nv), n))
  }
  if (n < 1L) stop("hiatus: at least one strain is needed")
  if (any(b <= 0.0)) {
    stop("hiatus: transmission rates must be positive")
  }
  if (any(nv < 0.0)) {
    stop("hiatus: recovery rates must be non-negative")
  }
  if (as.numeric(mu) < 0.0) {
    stop("hiatus: the birth/death rate must be non-negative")
  }
  Sm <- as.matrix(sigma)
  storage.mode(Sm) <- "double"
  if (nrow(Sm) != n || ncol(Sm) != n) {
    stop(sprintf("hiatus: sigma must be %d by %d", n, n))
  }
  for (i in 1:n) {
    for (j in 1:n) {
      v <- Sm[i, j]
      if (!(0.0 <= v && v <= 1.0)) {
        stop(sprintf("hiatus: sigma entries are probabilities and must lie in [0, 1], got %r", v))
      }
    }
  }
  list(b = b, nv = nv, m = as.numeric(mu), sg = Sm, n = n)
}

basic_reproduction_numbers <- function(beta, nu, mu) {
  b <- as.numeric(beta)
  n <- length(b)
  nv <- if (length(nu) == 1L) rep(as.numeric(nu), n) else as.numeric(nu)
  m <- as.numeric(mu)
  out <- numeric(n)
  for (i in 1:n) {
    d <- nv[i] + m
    if (d <= 1e-14) {
      stop(sprintf("hiatus: strain %d never leaves the infectious class (nu + mu = 0)", i - 1L))
    }
    out[i] <- b[i] / d
  }
  out
}

endemic_equilibrium <- function(beta, nu, mu, strain = 0L) {
  R0 <- basic_reproduction_numbers(beta, nu, mu)[as.integer(strain) + 1L]
  if (R0 <= 1.0) {
    return(list(R0 = R0, S = 1.0, I = 0.0,
                note = paste("R0 <= 1, so the disease-free state is the only equilibrium")))
  }
  b <- as.numeric(beta)
  n <- length(b)
  nv <- if (length(nu) == 1L) rep(as.numeric(nu), n) else as.numeric(nu)
  d <- nv[as.integer(strain) + 1L] + as.numeric(mu)
  list(R0 = R0, S = 1.0 / R0,
       I = as.numeric(mu) * (1.0 - 1.0 / R0) / d)
}

derivatives <- function(S, I, beta, nu, mu, sigma) {
  chk <- ._check(beta, nu, mu, sigma)
  b <- chk$b
  nv <- chk$nv
  m <- chk$m
  sg <- chk$sg
  n <- chk$n
  Sv <- as.numeric(S)
  Iv <- as.numeric(I)
  if (length(Sv) != n || length(Iv) != n) {
    stop(sprintf("hiatus: S and I must have one entry per strain (%d, %d, %d)", length(Sv), length(Iv), n))
  }
  dI <- numeric(n)
  dS <- numeric(n)
  for (i in 1:n) {
    dI[i] <- b[i] * Sv[i] * Iv[i] - nv[i] * Iv[i] - m * Iv[i]
  }
  for (i in 1:n) {
    acc <- 0.0
    for (j in 1:n) {
      acc <- acc + b[j] * Sv[i] * sg[i, j] * Iv[j]
    }
    dS[i] <- m - acc - m * Sv[i]
  }
  list(dS = dS, dI = dI)
}

simulate <- function(beta, nu, mu, sigma, S0 = NULL, I0 = NULL,
                     t_end = 2000.0, dt = 0.05, mutation = 0.0,
                     record_every = 100L) {
  chk <- ._check(beta, nu, mu, sigma)
  b <- chk$b
  nv <- chk$nv
  m <- chk$m
  sg <- chk$sg
  n <- chk$n
  Sv <- if (is.null(S0)) rep(1.0, n) else as.numeric(S0)
  Iv <- if (is.null(I0)) rep(1e-4, n) else as.numeric(I0)
  if (length(Sv) != n || length(Iv) != n) {
    stop("hiatus: the initial state must have one entry per strain")
  }
  if (as.numeric(dt) <= 0.0) {
    stop("hiatus: dt must be positive")
  }
  mu_rate <- as.numeric(mutation)
  if (!(0.0 <= mu_rate && mu_rate < 1.0)) {
    stop("hiatus: mutation must lie in [0, 1)")
  }

  rhs <- function(Sx, Ix) {
    der <- derivatives(Sx, Ix, b, nv, m, sg)
    dS <- der$dS
    dI <- der$dI
    if (mu_rate > 0.0 && n > 1L) {
      born <- numeric(n)
      for (i in 1:n) {
        born[i] <- b[i] * Sx[i] * Ix[i]
      }
      leak <- mu_rate * born
      for (i in 1:n) {
        dI[i] <- dI[i] - leak[i]
        nb <- c(i - 1L, i + 1L)
        nb <- nb[nb >= 1L & nb <= n]
        if (length(nb) > 0L) {
          for (q in nb) {
            dI[q] <- dI[q] + leak[i] / length(nb)
          }
        }
      }
    }
    list(dS = dS, dI = dI)
  }

  steps <- as.integer(t_end / as.numeric(dt))
  h <- as.numeric(dt)
  traj_t <- numeric(0)
  traj_S <- list()
  traj_I <- list()
  for (s in 0:steps) {
    if (s %% as.integer(record_every) == 0L) {
      traj_t <- c(traj_t, s * h)
      traj_S[[length(traj_S) + 1L]] <- Sv
      traj_I[[length(traj_I) + 1L]] <- Iv
    }
    if (s == steps) break
    r1 <- rhs(Sv, Iv)
    aS <- Sv + 0.5 * h * r1$dS
    aI <- Iv + 0.5 * h * r1$dI
    r2 <- rhs(aS, aI)
    bS <- Sv + 0.5 * h * r2$dS
    bI <- Iv + 0.5 * h * r2$dI
    r3 <- rhs(bS, bI)
    cS <- Sv + h * r3$dS
    cI <- Iv + h * r3$dI
    r4 <- rhs(cS, cI)
    for (i in 1:n) {
      Sv[i] <- Sv[i] + h / 6.0 * (r1$dS[i] + 2 * r2$dS[i] +
                                    2 * r3$dS[i] + r4$dS[i])
      Iv[i] <- Iv[i] + h / 6.0 * (r1$dI[i] + 2 * r2$dI[i] +
                                    2 * r3$dI[i] + r4$dI[i])
      Sv[i] <- min(max(Sv[i], 0.0), 1.0)
      Iv[i] <- max(Iv[i], 0.0)
    }
  }
  list(estimate = as.numeric(Iv), S = as.numeric(Sv),
       I = as.numeric(Iv),
       t = traj_t, S_traj = traj_S, I_traj = traj_I,
       n_strains = n, R0 = basic_reproduction_numbers(b, nv, m),
       n_variables = 2L * n,
       n_variables_history_based = if (n <= 30L) sprintf("2^%d = %d",
                                                          n, 2^n) else
                                                          sprintf("2^%d", n),
       surviving = which(Iv > 1e-8) - 1L,
       method = paste("status-based many-strain model, Gog & Grenfell (2002), integrated by RK4"))
}

linear_strain_space <- function(n, width = 2.0, floor = 0.0) {
  if (as.integer(n) < 1L) stop("hiatus: n must be at least 1")
  if (as.numeric(width) <= 0.0) stop("hiatus: width must be positive")
  w <- as.numeric(width)
  N <- as.integer(n)
  out <- matrix(0.0, N, N)
  for (i in 1:N) {
    for (j in 1:N) {
      v <- exp(-((i - j)^2) / (w * w))
      out[i, j] <- max(as.numeric(floor), v)
    }
  }
  out
}

twostrainhiatus <- simulate
hiatus_model <- simulate
hiatusmodel <- simulate

morie_hiatus <- function(beta, nu, mu, sigma, S0 = NULL, I0 = NULL,
                         t_end = 2000.0, dt = 0.05, mutation = 0.0,
                         record_every = 100L) {
  simulate(beta, nu, mu, sigma, S0, I0, t_end, dt, mutation,
           record_every)
}

.hiatus_cheatsheet <- function() {
  paste("hiatus: many-strain dynamics in 2n variables, not 2^n. Status-based + reduced transmission + POLARIZED immunity (some hosts fully immune, not all partly) means one variable per host per strain. dI_i = b_i S_i I_i - (v_i + mu) I_i; dS_i = mu - sum_j b_j S_i sigma_ij I_j - mu S_i. sigma_ij = P(infection by j immunises against i). Off-diagonal sigma = 0 decouples the strains exactly; sigma = 1 everywhere gives competitive exclusion.")
}
