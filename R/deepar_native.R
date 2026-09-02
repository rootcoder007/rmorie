# DeepAR: autoregressive probabilistic forecasting.
# Sources: Salinas, D., Flunkert, V., Gasthaus, J. & Januschowski, T.
# (2020) "DeepAR: Probabilistic forecasting with autoregressive
# recurrent networks", International Journal of Forecasting 36(3),
# 1181-1191, doi:10.1016/j.ijforecast.2019.07.001, arXiv:1704.04110,
# Secs. 3.2-3.3 (likelihood, scale handling, ancestral sampling);
# Hochreiter, S. & Schmidhuber, J. (1997) "Long Short-Term Memory",
# Neural Computation 9(8), 1735-1780; Gasthaus, J. et al. (2019)
# "Probabilistic Forecasting with Spline Quantile Function RNNs",
# AISTATS 22, PMLR 89, 1901-1910, for the quantile alternative.
#
# Native implementation mirroring Python morie.fn.deepar exactly: the
# same scale factor, the same Gaussian and negative binomial
# log-likelihoods, the same Gamma-Poisson mixture sampler, the same
# fitted autoregression, and the same ancestral sampling with the
# shared generator so the same seed reproduces the same draws.

#' Per-series scale factor
#'
#' \code{nu = 1 + (1/t_0) sum_t z_t}. The +1 is what keeps the divisor
#' away from zero for a series that is all zeros, which intermittent
#' series often are.
#'
#' @param z Numeric vector.
#' @param t0 Optional integer, number of leading terms to use.
#' @return A single numeric.
#' @export
morie_deepar_scale_factor <- function(z, t0 = NULL) {
  zv <- as.numeric(z)
  n <- if (is.null(t0)) length(zv) else as.integer(t0)
  if (n < 1L) stop("deepar: need at least one observation")
  1 + sum(zv[seq_len(n)]) / n
}

#' Gaussian log-likelihood
#'
#' For real-valued series.
#'
#' @param z Numeric observation.
#' @param mu Numeric mean.
#' @param sigma Numeric standard deviation.
#' @return Numeric log-density.
#' @export
morie_deepar_gaussian_loglik <- function(z, mu, sigma) {
  s <- max(as.numeric(sigma), 1e-12)
  z <- as.numeric(z)
  mu <- as.numeric(mu)
  -log(s) - 0.5 * log(2 * pi) - 0.5 * ((z - mu) / s) ^ 2
}

#' Negative binomial log-likelihood
#'
#' Variance is \code{mu (1 + mu alpha)}, so \code{alpha} IS the
#' overdispersion and \code{alpha -> 0} recovers Poisson.
#'
#' @param z Numeric observation.
#' @param mu Numeric mean.
#' @param alpha Numeric overdispersion.
#' @return Numeric log-density.
#' @export
morie_deepar_negative_binomial_loglik <- function(z, mu, alpha) {
  zz <- as.numeric(z)
  m <- max(as.numeric(mu), 1e-12)
  a <- as.numeric(alpha)
  if (zz < 0)
    stop(sprintf("deepar: the negative binomial needs a non-negative count, got %s",
                 format(z)))
  if (a < 0)
    stop(sprintf("deepar: alpha must be non-negative, got %s", format(alpha)))
  if (a < 1e-10) {
    return(zz * log(m) - m - lgamma(zz + 1))
  }
  r <- 1 / a
  lgamma(zz + r) - lgamma(r) - lgamma(zz + 1) +
    r * log(r / (r + m)) + zz * log(m / (r + m))
}

#' Gamma-Poisson mixture sampler
#'
#' The standard construction: \code{NB(mu, alpha)} as a Poisson with
#' rate drawn from \code{Gamma(1/alpha, mu * alpha)}. Uses the shared
#' generator so the same seed reproduces the same draws.
#'
#' @param mu Numeric mean.
#' @param alpha Numeric overdispersion.
#' @param e Generator environment from \code{.ghc_rng}.
#' @return Numeric draw.
#' @keywords internal
#' @noRd
.sample_neg_bin <- function(mu, alpha, e) {
  m <- max(mu, 1e-12)
  a <- alpha
  if (a < 1e-10) {
    lam <- m
  } else {
    r <- 1 / a
    lam <- .rgamma(r, m / r, e)
  }
  .rpois(lam, e)
}

#' Marsaglia-Tsang gamma sampler
#'
#' @param shape Numeric shape.
#' @param scale Numeric scale.
#' @param e Generator environment.
#' @return Numeric draw.
#' @keywords internal
#' @noRd
.rgamma <- function(shape, scale, e) {
  s <- shape
  if (s < 1) {
    u <- .ghc_unif(e, 1L)
    return(.rgamma(s + 1, scale, e) * u ^ (1 / s))
  }
  d <- s - 1 / 3
  c <- 1 / sqrt(9 * d)
  repeat {
    x <- .ghc_norm(e, 1L)
    v <- (1 + c * x) ^ 3
    if (v <= 0) next
    u <- .ghc_unif(e, 1L)
    if (log(u) < 0.5 * x * x + d - d * v + d * log(v))
      return(d * v * scale)
  }
}

#' Poisson sampler
#'
#' Direct product for small means, normal approximation with a
#' continuity correction for large means.
#'
#' @param lam Numeric rate.
#' @param e Generator environment.
#' @return Numeric draw.
#' @keywords internal
#' @noRd
.rpois <- function(lam, e) {
  if (lam <= 0) return(0)
  if (lam > 30) {
    v <- lam + sqrt(lam) * .ghc_norm(e, 1L)
    return(max(0, floor(v + 0.5)))
  }
  ll <- exp(-lam)
  p <- 1
  n <- 0
  repeat {
    p <- p * .ghc_unif(e, 1L)
    if (p <= ll) return(n)
    n <- n + 1L
  }
}

#' Fit the conditional mean by a scaled autoregression
#'
#' A linear autoregression stands in for the recurrent network: the
#' scaling, the likelihood and the sampling are what this module is
#' about, and they are identical either way.
#'
#' @param z Numeric vector.
#' @param n_lags Integer number of lags.
#' @param likelihood One of \code{"negative-binomial"} or
#'   \code{"gaussian"}.
#' @param ridge Numeric ridge penalty for the least-squares fit.
#' @return A list with the fitted parameters and the same field
#'   names as the Python arm.
#' @export
morie_deepar_fit <- function(z, n_lags = 2L,
                             likelihood = "negative-binomial",
                             ridge = 1e-6) {
  if (!(likelihood %in% c("negative-binomial", "gaussian")))
    stop(sprintf("deepar: likelihood must be negative-binomial or gaussian, got %s",
                 likelihood))
  zv <- as.numeric(z)
  n <- length(zv)
  p <- as.integer(n_lags)
  if (n < p + 4L)
    stop(sprintf("deepar: %d observations is too few for %d lags", n, p))
  nu <- morie_deepar_scale_factor(zv)
  zs <- zv / nu
  X <- matrix(0, n - p, p + 1L)
  for (t in (p + 1L):n) {
    X[t - p, 1] <- 1
    for (j in seq_len(p)) X[t - p, 1L + j] <- zs[t - j]
  }
  yv <- zs[(p + 1L):n]
  XtX <- crossprod(X)
  Xty <- crossprod(X, yv)
  beta <- as.numeric(solve(XtX + diag(ridge, p + 1L), Xty))
  fitted <- pmax(as.numeric(X %*% beta), 0)
  resid <- yv - fitted
  if (likelihood == "negative-binomial") {
    mbar <- max(mean(fitted), 1e-12)
    vbar <- if (length(yv) > 1L) var(yv) else mbar
    alpha <- max((vbar - mbar) / (mbar * mbar), 1e-8)
  } else {
    alpha <- if (length(resid) > 1L) max(sd(resid), 1e-12) else 1
  }
  list(estimate = beta, beta = beta, nu = nu, n_lags = p,
       likelihood = likelihood, alpha = alpha,
       fitted_scaled = fitted, residual = resid, scaled = zs, n = n,
       method = paste("DeepAR autoregressive probabilistic",
                      "forecaster, Salinas, Flunkert, Gasthaus &",
                      "Januschowski (2020)"))
}

#' Ancestral sampling: feed each draw back in
#'
#' Multi-step uncertainty has no closed form once the model consumes
#' its own output, which is why the intervals widen with horizon
#' without being told to.
#'
#' @param fit A fit object from \code{morie_deepar_fit}.
#' @param z_history Numeric vector of history observations.
#' @param horizon Integer forecast horizon.
#' @param n_samples Integer number of trajectories.
#' @param seed Integer seed.
#' @return A list of trajectories, each a length-\code{horizon} numeric
#'   vector.
#' @export
morie_deepar_sample <- function(fit, z_history, horizon, n_samples = 200L,
                                seed = 0L) {
  beta <- fit$beta
  p <- fit$n_lags
  nu <- fit$nu
  alpha <- fit$alpha
  H <- as.integer(horizon)
  hist <- tail(as.numeric(z_history) / nu, p)
  if (length(hist) < p)
    stop(sprintf("deepar: need at least %d history points, got %d",
                 p, length(hist)))
  e <- .ghc_rng(as.integer(seed))
  paths <- vector("list", as.integer(n_samples))
  for (s in seq_along(paths)) {
    st <- as.numeric(hist)
    path <- numeric(H)
    for (h in seq_len(H)) {
      mu_s <- beta[1] + sum(beta[seq_len(p) + 1L] * rev(st)[seq_len(p)])
      mu_s <- max(mu_s, 0)
      if (fit$likelihood == "negative-binomial") {
        draw <- .sample_neg_bin(mu_s * nu, alpha, e) / nu
      } else {
        draw <- mu_s + (alpha / nu) * .ghc_norm(e, 1L)
      }
      st <- c(st, draw)
      path[h] <- draw * nu
    }
    paths[[s]] <- path
  }
  paths
}

#' Fit, sample, and read the quantiles off the trajectories
#'
#' @param z Numeric vector of history observations.
#' @param horizon Integer forecast horizon.
#' @param n_lags Integer number of lags.
#' @param likelihood One of \code{"negative-binomial"} or
#'   \code{"gaussian"}.
#' @param n_samples Integer number of trajectories.
#' @param quantiles Numeric vector of quantiles in (0, 1).
#' @param seed Integer seed.
#' @return A list with \code{mean}, \code{quantiles}, \code{paths},
#'   \code{width} and the same metadata as the Python arm.
#' @export
morie_deepar_forecast <- function(z, horizon, n_lags = 2L,
                                  likelihood = "negative-binomial",
                                  n_samples = 300L,
                                  quantiles = c(0.1, 0.5, 0.9),
                                  seed = 0L) {
  fit <- morie_deepar_fit(z, n_lags = n_lags, likelihood = likelihood)
  paths <- morie_deepar_sample(fit, z, horizon, n_samples = n_samples,
                                seed = as.integer(seed))
  H <- as.integer(horizon)
  qs <- list()
  for (q in quantiles) {
    if (q <= 0 || q >= 1)
      stop(sprintf("deepar: quantiles must be in (0, 1), got %s", format(q)))
    qs[[as.character(q)]] <- vapply(seq_len(H), function(h) {
      v <- vapply(paths, function(pp) pp[h], numeric(1))
      unname(quantile(v, probs = q, type = 7, names = FALSE))
    }, numeric(1))
  }
  mean <- vapply(seq_len(H), function(h) {
    mean(vapply(paths, function(pp) pp[h], numeric(1)))
  }, numeric(1))
  width <- vapply(seq_len(H), function(h) {
    qs[[as.character(max(quantiles))]][h] -
      qs[[as.character(min(quantiles))]][h]
  }, numeric(1))
  list(estimate = mean, mean = mean, quantiles = qs, paths = paths,
       width = width, horizon = H, nu = fit$nu, alpha = fit$alpha,
       likelihood = likelihood, n_samples = as.integer(n_samples),
       method = paste("DeepAR probabilistic forecast by ancestral",
                      "sampling, Salinas et al. (2020)"))
}

# house entry point: the package exports one morie_<module>
morie_deepar <- morie_deepar_forecast
