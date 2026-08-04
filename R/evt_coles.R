# Coles (2001) extreme-value shelf -- R mirror of morie/fn/_evt_core
# and the 29 Python shelf modules. Every equation checked against the
# library PDF of Coles, An Introduction to Statistical Modeling of
# Extreme Values, Springer: GEV eq. (3.2) p.47, quantiles/return
# levels eq. (3.4) p.49, log-likelihood eq. (3.7)-(3.9) p.55, delta
# method eq. (3.10)-(3.11) p.56, GPD eq. (4.2)-(4.4) pp.75-76,
# likelihood (4.10) p.80, POT return level (4.11)-(4.14) pp.81-82,
# chi/chibar sec. 8.4 pp.163-165, profile likelihood sec. 2.6.5,
# Bayesian recipe sec. 9.1.3, nonstationary trend sec. 6.2.
#
# Complements R/evt_native.R (Hill/Pickands/L-moments), which argues
# L-moments for the non-regular region xi <= -0.5 (Smith 1985); the
# ML fits here are the book's ch. 3-4 workhorses, regular for
# xi > -0.5 (Coles p. 55).

.evt_xi_tiny <- 1e-8

#' GEV distribution function (Coles 2001 eq. 3.2)
#' @param x quantile(s)
#' @param mu,sigma,xi GEV location, scale (> 0), shape
#' @return numeric vector of probabilities
#' @export
morie_evt_gev_cdf <- function(x, mu, sigma, xi) {
  t <- (x - mu) / sigma
  if (abs(xi) < .evt_xi_tiny) {
    return(exp(-exp(-t)))
  }
  arg <- 1 + xi * t
  out <- ifelse(arg <= 0, ifelse(xi > 0, 0, 1),
    exp(-pmax(arg, 1e-300)^(-1 / xi))
  )
  out
}

#' GEV log-density (Coles 2001 eq. 3.7 integrand; Gumbel eq. 3.9)
#' @inheritParams morie_evt_gev_cdf
#' @return numeric vector of log-densities (-Inf off support, eq. 3.8)
#' @export
morie_evt_gev_logpdf <- function(x, mu, sigma, xi) {
  if (sigma <= 0) {
    return(rep(-Inf, length(x)))
  }
  t <- (x - mu) / sigma
  if (abs(xi) < .evt_xi_tiny) {
    return(-log(sigma) - t - exp(-t))
  }
  arg <- 1 + xi * t
  out <- rep(-Inf, length(x))
  ok <- arg > 0
  out[ok] <- -log(sigma) - (1 + 1 / xi) * log(arg[ok]) -
    arg[ok]^(-1 / xi)
  out
}

#' GEV log-likelihood (Coles 2001 eq. 3.7-3.9)
#' @param x sample of block maxima
#' @inheritParams morie_evt_gev_cdf
#' @return scalar log-likelihood
#' @export
morie_evt_gev_loglik <- function(x, mu, sigma, xi) {
  sum(morie_evt_gev_logpdf(x, mu, sigma, xi))
}

#' GEV quantile (Coles 2001 eq. 3.4, non-exceedance p)
#' @param p probability in (0, 1)
#' @inheritParams morie_evt_gev_cdf
#' @export
morie_evt_gev_quantile <- function(p, mu, sigma, xi) {
  stopifnot(all(p > 0), all(p < 1))
  yp <- -log(p)
  if (abs(xi) < .evt_xi_tiny) {
    return(mu - sigma * log(yp))
  }
  mu + (sigma / xi) * (yp^(-xi) - 1)
}

#' GEV maximum-likelihood fit (Coles 2001 sec. 3.3.2)
#' @param x block maxima
#' @return list(mu, sigma, xi, loglik, cov, n, converged)
#' @export
morie_evt_gev_mle <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  stopifnot(n >= 2)
  s <- stats::sd(x)
  sigma0 <- s * sqrt(6) / pi
  mu0 <- mean(x) - 0.5772156649015329 * sigma0
  nll <- function(th) -morie_evt_gev_loglik(x, th[1], exp(th[2]), th[3])
  fit <- stats::optim(c(mu0, log(sigma0), 0.1), nll,
    method = "Nelder-Mead",
    control = list(maxit = 4000)
  )
  mu <- fit$par[1]
  sigma <- exp(fit$par[2])
  xi <- fit$par[3]
  nll_nat <- function(th) -morie_evt_gev_loglik(x, th[1], th[2], th[3])
  H <- .evt_num_hessian(nll_nat, c(mu, sigma, xi))
  covm <- tryCatch(solve(H), error = function(e) MASS_ginv_fallback(H))
  list(
    mu = mu, sigma = sigma, xi = xi, loglik = -fit$value,
    cov = covm, n = n, converged = fit$convergence == 0L
  )
}

# central-difference observed information (the numeric-differencing
# route Coles p. 56 prescribes)
.evt_num_hessian <- function(f, theta, h = 1e-4) {
  k <- length(theta)
  H <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in i:k) {
      hi <- h * max(1, abs(theta[i]))
      hj <- h * max(1, abs(theta[j]))
      tpp <- tpm <- tmp <- tmm <- theta
      tpp[i] <- tpp[i] + hi
      tpp[j] <- tpp[j] + hj
      tpm[i] <- tpm[i] + hi
      tpm[j] <- tpm[j] - hj
      tmp[i] <- tmp[i] - hi
      tmp[j] <- tmp[j] + hj
      tmm[i] <- tmm[i] - hi
      tmm[j] <- tmm[j] - hj
      H[i, j] <- H[j, i] <- (f(tpp) - f(tpm) - f(tmp) + f(tmm)) /
        (4 * hi * hj)
    }
  }
  H
}

# pseudo-inverse fallback for a singular observed information
MASS_ginv_fallback <- function(H) {
  e <- eigen(H, symmetric = TRUE)
  pos <- e$values > max(e$values) * 1e-12
  e$vectors[, pos, drop = FALSE] %*%
    diag(1 / e$values[pos], sum(pos)) %*%
    t(e$vectors[, pos, drop = FALSE])
}

#' GPD distribution function (Coles 2001 eq. 4.2-4.4)
#' @param y excess(es) over the threshold, y >= 0
#' @param sigma,xi GPD scale (> 0) and shape
#' @export
morie_evt_gpd_cdf <- function(y, sigma, xi) {
  out <- numeric(length(y))
  neg <- y < 0
  if (abs(xi) < .evt_xi_tiny) {
    out <- 1 - exp(-y / sigma)
  } else {
    arg <- 1 + xi * y / sigma
    out <- ifelse(arg <= 0, 1, 1 - pmax(arg, 1e-300)^(-1 / xi))
  }
  out[neg] <- 0
  out
}

#' GPD log-likelihood over excesses (Coles 2001 eq. 4.10)
#' @inheritParams morie_evt_gpd_cdf
#' @export
morie_evt_gpd_loglik <- function(y, sigma, xi) {
  if (sigma <= 0 || any(y < 0)) {
    return(-Inf)
  }
  if (abs(xi) < .evt_xi_tiny) {
    return(sum(-log(sigma) - y / sigma))
  }
  arg <- 1 + xi * y / sigma
  if (any(arg <= 0)) {
    return(-Inf)
  }
  sum(-log(sigma) - (1 + 1 / xi) * log(arg))
}

#' GPD quantile (inverse of Coles 2001 eq. 4.2)
#' @param p probability in [0, 1)
#' @inheritParams morie_evt_gpd_cdf
#' @export
morie_evt_gpd_quantile <- function(p, sigma, xi) {
  stopifnot(all(p >= 0), all(p < 1))
  if (abs(xi) < .evt_xi_tiny) {
    return(-sigma * log(1 - p))
  }
  (sigma / xi) * ((1 - p)^(-xi) - 1)
}

#' GPD maximum-likelihood fit (Coles 2001 sec. 4.3.2)
#' @param y threshold excesses
#' @export
morie_evt_gpd_mle <- function(y) {
  y <- as.numeric(y)
  n <- length(y)
  stopifnot(n >= 2)
  ybar <- mean(y)
  s2 <- stats::var(y)
  xi0 <- 0.5 * (1 - ybar^2 / s2)
  sigma0 <- max(if (xi0 < 1) ybar * (1 - xi0) else ybar, 1e-8)
  nll <- function(th) -morie_evt_gpd_loglik(y, exp(th[1]), th[2])
  fit <- stats::optim(
    c(
      log(sigma0),
      if (abs(xi0) < 0.9) xi0 else 0.1
    ),
    nll,
    method = "Nelder-Mead",
    control = list(maxit = 4000)
  )
  sigma <- exp(fit$par[1])
  xi <- fit$par[2]
  nll_nat <- function(th) -morie_evt_gpd_loglik(y, th[1], th[2])
  H <- .evt_num_hessian(nll_nat, c(sigma, xi))
  covm <- tryCatch(solve(H), error = function(e) MASS_ginv_fallback(H))
  list(
    sigma = sigma, xi = xi, loglik = -fit$value, cov = covm,
    n = n, converged = fit$convergence == 0L
  )
}

#' GEV T-period return level (Coles 2001 eq. 3.4/3.10)
#' @param T return period (> 1)
#' @inheritParams morie_evt_gev_cdf
#' @export
morie_evt_return_level <- function(mu, sigma, xi, T) {
  stopifnot(T > 1)
  morie_evt_gev_quantile(1 - 1 / T, mu, sigma, xi)
}

#' Delta-method CI for a GEV return level (Coles 2001 eq. 3.10-3.11)
#' @param x block maxima to fit
#' @param T return period
#' @param alpha 1 - confidence level
#' @export
morie_evt_return_level_ci <- function(x, T, alpha = 0.05) {
  f <- morie_evt_gev_mle(x)
  z <- morie_evt_return_level(f$mu, f$sigma, f$xi, T)
  yp <- -log(1 - 1 / T)
  g <- if (abs(f$xi) < .evt_xi_tiny) {
    c(1, -log(yp), 0)
  } else {
    c(
      1,
      -(1 / f$xi) * (1 - yp^(-f$xi)),
      f$sigma * f$xi^(-2) * (1 - yp^(-f$xi)) -
        (f$sigma / f$xi) * yp^(-f$xi) * log(yp)
    )
  }
  se <- sqrt(max(drop(t(g) %*% f$cov %*% g), 0))
  zc <- stats::qnorm(1 - alpha / 2)
  list(z_T = z, ci_lo = z - zc * se, ci_hi = z + zc * se, se = se)
}

#' POT m-observation return level (Coles 2001 eq. 4.12-4.14)
#' @param u threshold; sigma,xi GPD parameters; zeta_u = P(X > u);
#'   m observations
#' @export
morie_evt_return_level_pot <- function(u, sigma, xi, zeta_u, m) {
  stopifnot(m * zeta_u > 1)
  if (abs(xi) < .evt_xi_tiny) {
    return(u + sigma * log(m * zeta_u))
  }
  u + (sigma / xi) * ((m * zeta_u)^xi - 1)
}

#' Empirical chi(u) tail dependence (Coles 2001 sec. 8.4 p.164)
#' @param x,y equal-length series; u quantile level
#' @export
morie_evt_chi <- function(x, y, u = 0.95) {
  n <- length(x)
  stopifnot(length(y) == n, n >= 4)
  rx <- rank(x) / (n + 1)
  ry <- rank(y) / (n + 1)
  joint <- sum(rx < u & ry < u) / n
  joint <- min(max(joint, 1 / (2 * n)), 1 - 1 / (2 * n))
  min(max(2 - log(joint) / log(u), 0), 1)
}

#' Empirical chibar(u) (Coles 2001 sec. 8.4 p.164)
#' @inheritParams morie_evt_chi
#' @param u_grid quantile grid
#' @export
morie_evt_chibar <- function(x, y, u_grid = seq(0.5, 0.95,
                               length.out = 20
                             )) {
  n <- length(x)
  stopifnot(length(y) == n, n >= 4)
  rx <- rank(x) / (n + 1)
  ry <- rank(y) / (n + 1)
  vapply(u_grid, function(u) {
    joint <- sum(rx > u & ry > u) / n
    joint <- min(max(joint, 1 / (2 * n)), 1 - 1 / (2 * n))
    min(max(2 * log(1 - u) / log(joint) - 1, -1), 1)
  }, numeric(1))
}

#' Profile-likelihood CI for the shape xi (Coles 2001 sec. 2.6.5)
#' @param x data (block maxima or excesses); model "gev" or "gpd"
#' @param alpha 1 - confidence level
#' @export
morie_evt_xi_ci_profile <- function(x, alpha = 0.05, model = "gev") {
  crit <- stats::qchisq(1 - alpha, 1) / 2
  if (model == "gev") {
    fit <- morie_evt_gev_mle(x)
    prof <- function(xi) {
      nll <- function(th) {
        -morie_evt_gev_loglik(
          x, th[1],
          exp(th[2]), xi
        )
      }
      -stats::optim(c(fit$mu, log(fit$sigma)), nll,
        method = "Nelder-Mead",
        control = list(maxit = 2000)
      )$value
    }
  } else {
    fit <- morie_evt_gpd_mle(x)
    prof <- function(xi) {
      nll <- function(th) -morie_evt_gpd_loglik(x, exp(th[1]), xi)
      -stats::optim(log(fit$sigma), nll,
        method = "Brent",
        lower = log(fit$sigma) - 6,
        upper = log(fit$sigma) + 6
      )$value
    }
  }
  l_hat <- fit$loglik
  xi_hat <- fit$xi
  edge <- function(dir) {
    xi <- xi_hat
    step <- 0.01 * dir
    for (i in 1:400) {
      xi <- xi + step
      if (l_hat - prof(xi) > crit) {
        lo <- xi - step
        hi <- xi
        for (j in 1:40) {
          mid <- (lo + hi) / 2
          if (l_hat - prof(mid) > crit) hi <- mid else lo <- mid
        }
        return((lo + hi) / 2)
      }
    }
    xi
  }
  lo <- edge(-1)
  hi <- edge(1)
  list(ci_lo = min(lo, hi), ci_hi = max(lo, hi), xi_hat = xi_hat)
}

#' Bayesian GEV posterior via random-walk Metropolis
#' (Coles 2001 sec. 9.1.3 vague-normal-prior recipe)
#' @param x block maxima; n_draws retained draws; seed RNG seed
#' @param prior_sd prior standard deviations for (mu, log sigma, xi)
#' @export
morie_evt_bayes_gev <- function(x, n_draws = 2000, seed = 42,
                                prior_sd = c(100, 10, 1)) {
  set.seed(seed)
  f <- morie_evt_gev_mle(x)
  logpost <- function(th) {
    morie_evt_gev_loglik(x, th[1], exp(th[2]), th[3]) -
      th[1]^2 / (2 * prior_sd[1]^2) -
      th[2]^2 / (2 * prior_sd[2]^2) -
      th[3]^2 / (2 * prior_sd[3]^2)
  }
  th <- c(f$mu, log(f$sigma), f$xi)
  step <- c(0.1, 0.05, 0.05)
  lp <- logpost(th)
  warm <- max(200, n_draws %/% 2)
  draws <- matrix(0, n_draws, 3)
  acc <- 0L
  tot <- 0L
  kept <- 0L
  for (it in seq_len(warm + n_draws)) {
    for (j in 1:3) {
      prop <- th
      prop[j] <- prop[j] + step[j] * stats::rnorm(1)
      lp_new <- logpost(prop)
      tot <- tot + 1L
      if (log(stats::runif(1)) < lp_new - lp) {
        th <- prop
        lp <- lp_new
        acc <- acc + 1L
        if (it <= warm) step[j] <- step[j] * 1.05
      } else if (it <= warm) step[j] <- step[j] * 0.97
    }
    if (it > warm) {
      kept <- kept + 1L
      draws[kept, ] <- c(th[1], exp(th[2]), th[3])
    }
  }
  list(draws = draws, accept_rate = acc / tot)
}

#' Nonstationary GEV with a linear trend in location
#' (Coles 2001 sec. 6.2)
#' @param x series of maxima; t optional time index
#' @export
morie_evt_gev_trend <- function(x, t = seq_along(x) - 1) {
  n <- length(x)
  tz <- (t - mean(t)) / max(stats::sd(t) * sqrt((n - 1) / n), 1e-12)
  f0 <- morie_evt_gev_mle(x)
  nll <- function(th) {
    s <- exp(th[3])
    -sum(morie_evt_gev_logpdf(x, th[1] + th[2] * tz, s, th[4]))
  }
  fit <- stats::optim(c(f0$mu, 0, log(f0$sigma), f0$xi), nll,
    method = "Nelder-Mead",
    control = list(maxit = 6000)
  )
  tsd <- max(stats::sd(t) * sqrt((n - 1) / n), 1e-12)
  beta1 <- fit$par[2] / tsd
  list(
    beta0 = fit$par[1], beta1 = beta1,
    sigma = exp(fit$par[3]), xi = fit$par[4],
    loglik = -fit$value,
    lr_vs_stationary = 2 * (-fit$value - f0$loglik)
  )
}
