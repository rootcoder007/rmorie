# Peaks-over-threshold GPD analysis (Pickands 1975; Davison & Smith 1990).
# R translation of morie.fn.potM.

.potM_loglik <- function(z, sigma, xi) {
  k <- length(z)
  if (sigma <= 0) return(-Inf)
  if (abs(xi) < 1e-9) {
    # Exponential limit (xi -> 0): Gumbel
    return(-k * log(sigma) - sum(z) / sigma)
  } else {
    # General GPD
    if (any(1 + xi * z / sigma <= 0)) return(-Inf)
    return(-k * log(sigma) - (1 + 1/xi) * sum(log(1 + xi * z / sigma)))
  }
}

.potM_neg_loglik <- function(par, z) {
  sigma <- par[1]
  xi <- par[2]
  ll <- .potM_loglik(z, sigma, xi)
  if (is.na(ll) || !is.finite(ll)) return(1e10)
  -ll
}

.potM_gpd_mle <- function(exc) {
  z <- as.numeric(exc)
  k <- length(z)
  mean_z <- mean(z)

  # Initial values: exponential MLE
  sigma0 <- mean_z
  xi0 <- 0.0

  # Numerical optimization
  result <- tryCatch(
    optim(
      par = c(sigma0, xi0),
      fn = .potM_neg_loglik,
      z = z,
      method = "BFGS",
      control = list(fnscale = 1, maxit = 1000, reltol = 1e-8)
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    # Fallback to Nelder-Mead
    result <- optim(
      par = c(sigma0, xi0),
      fn = .potM_neg_loglik,
      z = z,
      method = "Nelder-Mead",
      control = list(fnscale = 1, maxit = 5000)
    )
  }

  sigma <- result$par[1]
  xi <- result$par[2]
  loglik <- .potM_loglik(z, sigma, xi)
  converged <- isTRUE(result$convergence == 0)

  # Compute Hessian for covariance matrix
  eps <- 1e-5
  f0 <- .potM_neg_loglik(c(sigma, xi), z)
  H <- matrix(0, 2, 2)

  # Diagonal elements
  f_pp <- .potM_neg_loglik(c(sigma + eps, xi), z)
  f_mm <- .potM_neg_loglik(c(sigma - eps, xi), z)
  H[1, 1] <- (f_pp - 2 * f0 + f_mm) / (eps * eps)

  f_pp <- .potM_neg_loglik(c(sigma, xi + eps), z)
  f_mm <- .potM_neg_loglik(c(sigma, xi - eps), z)
  H[2, 2] <- (f_pp - 2 * f0 + f_mm) / (eps * eps)

  # Off-diagonal elements
  f_pp <- .potM_neg_loglik(c(sigma + eps, xi + eps), z)
  f_pm <- .potM_neg_loglik(c(sigma + eps, xi - eps), z)
  f_mp <- .potM_neg_loglik(c(sigma - eps, xi + eps), z)
  f_mm <- .potM_neg_loglik(c(sigma - eps, xi - eps), z)
  H[1, 2] <- H[2, 1] <- (f_pp - f_pm - f_mp + f_mm) / (4 * eps * eps)

  # Covariance = inverse of observed information (Hessian of neg loglik)
  cov_mat <- tryCatch(solve(H), error = function(e) matrix(NA_real_, 2, 2))

  list(
    sigma = sigma,
    xi = xi,
    loglik = loglik,
    cov = cov_mat,
    converged = converged
  )
}

morie_potM <- function(y, u, return_periods = c(10.0, 100.0)) {
  yv <- as.numeric(y)
  u <- as.numeric(u)
  n <- length(yv)
  exc <- yv[yv > u] - u
  k <- length(exc)
  if (k < 2) {
    stop("need at least two exceedances above u")
  }
  fit <- .potM_gpd_mle(exc)
  sigma <- fit$sigma
  xi <- fit$xi
  rate <- k / n

  # Return levels (Coles Eq. 4.13)
  rp <- as.numeric(return_periods)
  rl <- numeric(length(rp))
  names(rl) <- as.character(rp)
  for (i in seq_along(rp)) {
    m <- rp[i]
    if (m * rate <= 1.0) {
      rl[i] <- NA_real_
    } else if (abs(xi) < 1e-9) {
      rl[i] <- u + sigma * log(m * rate)
    } else {
      rl[i] <- u + (sigma / xi) * ((m * rate)^xi - 1.0)
    }
  }

  list(
    sigma = sigma,
    xi = xi,
    loglik = fit$loglik,
    cov = fit$cov,
    n_exceedances = k,
    n = n,
    rate = rate,
    return_levels = rl,
    threshold = u,
    converged = fit$converged,
    method = "POT/GPD (Davison-Smith 1990; Coles Eq. 4.13)"
  )
}

# Long descriptive alias (stub-era name)
peaks_over_threshold <- morie_potM

morie_potM_cheatsheet <- function() {
  "potM: GPD MLE on y-u | y>u; x_m = u + sigma/xi ((m zeta)^xi - 1)"
}

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_potm <- morie_potM
