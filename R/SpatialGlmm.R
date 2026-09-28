#' Latent Gaussian fields and spatial GLMM simulation
#'
#' \code{CarPrecision}: proper CAR precision \eqn{\tau (D - \rho W)}.
#' \code{SarCovariance}: SAR covariance
#' \eqn{\sigma^2 [(I - \rho W)'(I - \rho W)]^{-1}}. \code{GmrfSimulate}: GMRF
#' samples by \eqn{L' x = e} for \eqn{Q = L L'} (Rue and Held 2005).
#' \code{SpatialGlmmSimulate}: responses given a latent field for Gaussian,
#' Poisson, binomial, negative binomial (NB2) and gamma families, drawn by
#' inversion of Philox uniforms with exact pmf recurrences. Identical to the
#' Python arm \code{morie.fn.sglmm}.
#'
#' @param W Adjacency matrix.
#' @param rho Spatial dependence.
#' @param tau Precision scale.
#' @param sigma2 Innovation variance.
#' @param Q Precision matrix.
#' @param nsim Number of samples.
#' @param seed Philox seed.
#' @param X Design matrix.
#' @param beta Coefficients.
#' @param latent Latent field values.
#' @param family Response family.
#' @param trials Binomial trials.
#' @param size Negative binomial size.
#' @param shape Gamma shape.
#' @param sigma Gaussian noise standard deviation.
#' @return Matrix or list.
#' @references Besag, J. (1974). Spatial interaction and the statistical
#'   analysis of lattice systems. JRSS B 36, 192-236.
#'
#'   Rue, H. and Held, L. (2005). Gaussian Markov Random Fields: Theory and
#'   Applications. Chapman and Hall/CRC.
#'
#'   Diggle, P. J., Tawn, J. A. and Moyeed, R. A. (1998). Model-based
#'   geostatistics. Applied Statistics 47, 299-350.
#' @examples
#' CarPrecision(rbind(c(0, 1), c(1, 0)), 0.5, 2)
#' SpatialGlmmSimulate(matrix(1, 2, 1), 0, c(0, 0), family = "binomial")$mu
#' @export
CarPrecision <- function(W, rho, tau = 1) {
  W <- as.matrix(W) * 1
  tau * (diag(rowSums(W), nrow(W)) - rho * W)
}

#' @rdname CarPrecision
#' @export
SarCovariance <- function(W, rho, sigma2 = 1) {
  B <- diag(nrow(W)) - rho * as.matrix(W)
  sigma2 * solve(crossprod(B))
}

#' @rdname CarPrecision
#' @export
GmrfSimulate <- function(Q, nsim = 1L, seed = 1) {
  R <- chol(as.matrix(Q))
  list(samples = lapply(seq_len(nsim) - 1, function(s) as.vector(backsolve(R, .morie_random_normal(nrow(R), seed = seed, stream = s)))))
}

.sg_draw <- function(family, mu, u, trials, size, shape, sigma, z) {
  if (family == "gaussian") return(mu + sigma * z)
  if (family == "poisson") {
    k <- 0
    p <- exp(-mu)
    F <- p
    while (u > F && p > 0) {
      k <- k + 1
      p <- p * mu / k
      F <- F + p
    }
    return(k)
  }
  if (family == "binomial") {
    n <- trials
    if (mu <= 0) return(0)
    if (mu >= 1) return(n)
    k <- 0
    p <- (1 - mu)^n
    F <- p
    while (u > F && k < n) {
      p <- p * (n - k) / (k + 1) * mu / (1 - mu)
      k <- k + 1
      F <- F + p
    }
    return(k)
  }
  if (family == "negbin") {
    k <- 0
    p <- exp(size * log(size / (size + mu)))
    F <- p
    while (u > F && p > 0) {
      p <- p * (k + size) / (k + 1) * mu / (size + mu)
      k <- k + 1
      F <- F + p
    }
    return(k)
  }
  if (family == "gamma") return(stats::qgamma(u, shape, shape / mu))
  stop("family must be gaussian, poisson, binomial, negbin or gamma", call. = FALSE)
}

#' @rdname CarPrecision
#' @export
SpatialGlmmSimulate <- function(X, beta, latent, family = "poisson", trials = 1, size = NULL, shape = NULL, sigma = 1,
                                seed = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  eta <- as.vector(X %*% beta) + latent
  mu <- switch(family, binomial = 1 / (1 + exp(-eta)), gaussian = eta, exp(eta))
  u <- .morie_random_uniform(n, seed = seed, stream = 0)
  z <- if (family == "gaussian") .morie_random_normal(n, seed = seed, stream = 1) else rep(NA_real_, n)
  tr <- rep_len(trials, n)
  y <- vapply(seq_len(n), function(i) .sg_draw(family, mu[i], u[i], tr[i], size, shape, sigma, z[i]), 0)
  list(y = y, eta = eta, mu = mu, family = family)
}

.sg_terms <- function(family, y, eta, m) {
  if (family == "poisson") {
    mu <- exp(eta)
    return(list(ll = y * eta - mu - lgamma(y + 1), g = y - mu, w = mu))
  }
  p <- 1 / (1 + exp(-eta))
  ll <- ifelse(eta < 30, y * eta - m * log1p(exp(eta)), y * eta - m * (eta + log1p(exp(-eta)))) +
    lgamma(m + 1) - lgamma(y + 1) - lgamma(m - y + 1)
  list(ll = ll, g = y - m * p, w = m * p * (1 - p))
}

.sg_laplace <- function(y, X, Z, Sigma, beta, family, m, u0) {
  Si <- solve(Sigma)
  off <- as.vector(X %*% beta)
  u <- u0
  for (it in 1:100) {
    eta <- off + as.vector(Z %*% u)
    tm <- .sg_terms(family, y, eta, m)
    g <- as.vector(crossprod(Z, tm$g)) - as.vector(Si %*% u)
    H <- Si + crossprod(Z, tm$w * Z)
    step <- as.vector(solve(H, g))
    u <- u + step
    if (max(abs(step)) < 1e-12 * (1 + max(abs(u)))) break
  }
  eta <- off + as.vector(Z %*% u)
  tm <- .sg_terms(family, y, eta, m)
  H <- Si + crossprod(Z, tm$w * Z)
  ld <- function(A) 2 * sum(log(diag(chol(A))))
  list(ll = sum(tm$ll) - 0.5 * sum(u * (Si %*% u)) - 0.5 * (ld(Sigma) + ld(H)), u = u, H = H, eta = eta)
}

#' Spatial GLMM fitting, prediction, residuals and scoring
#'
#' \code{SpatialGlmmFit}: maximises the Laplace approximation of the GLMM
#' marginal likelihood (Newton for the latent mode, L-BFGS-B for
#' \eqn{\beta}, \eqn{\log\sigma} and \eqn{\log} range) with iid group
#' intercepts (as \code{lme4::glmer} with \code{nAGQ = 1}) or a spatial field
#' (\code{Exp}, \code{Gau}, \code{Sph}); returns the Gaussian approximation
#' of the latent posterior (mode, sd, covariance). \code{SpatialGlmmPredict}:
#' kriging of the latent mode and variance, response mean by the lognormal
#' formula (Poisson) or 20-point Gauss-Hermite (binomial).
#' \code{GlmmResiduals}: Pearson, deviance and randomised quantile residuals.
#' \code{CrpsGaussian}, \code{CrpsPoisson}, \code{CrpsSample}: continuous
#' ranked probability scores. Identical to the Python arm
#' \code{morie.fn.sglmm}.
#'
#' @param y Responses.
#' @param X Design matrix.
#' @param family \code{"poisson"} or \code{"binomial"} (residuals also
#'   \code{"negbin"}).
#' @param coords,groups Spatial coordinates or grouping factor.
#' @param model Spatial correlation model.
#' @param trials Binomial trials.
#' @param start Starting parameters.
#' @param fit A \code{SpatialGlmmFit} result with \code{coords}.
#' @param X0,coords0 Prediction design and coordinates.
#' @param mu Fitted means (probabilities for binomial).
#' @param size Negative binomial size.
#' @param seed Philox seed.
#' @param sigma,lam,samples Predictive parameters or samples.
#' @return List or numeric vector.
#' @references Breslow, N. E. and Clayton, D. G. (1993). Approximate
#'   inference in generalized linear mixed models. JASA 88, 9-25.
#'
#'   Dunn, P. K. and Smyth, G. K. (1996). Randomized quantile residuals. JCGS
#'   5, 236-244.
#'
#'   Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring rules,
#'   prediction, and estimation. JASA 102, 359-378.
#' @examples
#' CrpsGaussian(0, 0, 1)
#' GlmmResiduals(c(2, 0), c(2, 1))$pearson
#' @export
SpatialGlmmFit <- function(y, X, family = "poisson", coords = NULL, groups = NULL, model = "Exp", trials = NULL,
                           start = NULL) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)
  p <- ncol(X)
  m <- if (is.null(trials)) rep(1, n) else rep_len(as.numeric(trials), n)
  D <- NULL
  if (!is.null(groups)) {
    lv <- unique(groups)
    Z <- outer(groups, lv, "==") * 1
  } else if (!is.null(coords)) {
    Z <- diag(n)
    D <- as.matrix(stats::dist(as.matrix(coords)))
  } else {
    stop("give groups or coords", call. = FALSE)
  }
  q <- ncol(Z)
  sigma_of <- function(theta) {
    s2 <- exp(2 * theta[1])
    if (is.null(D)) return(diag(s2, q))
    matrix(KrigingCovariance(D, list(model = model, psill = s2, range = exp(theta[2]))), q) + diag(1e-10 * s2, q)
  }
  ntheta <- if (is.null(D)) 1 else 2
  if (is.null(start)) {
    mbar <- sum(y) / sum(m)
    link <- if (family == "poisson") log(max(mbar, 1e-8)) else log(max(mbar, 1e-8) / max(1 - mbar, 1e-8))
    start <- c(link, rep(0, p - 1), log(0.5), if (!is.null(D)) log(sum(D[, n]) / n))
  }
  st <- new.env()
  st$u <- rep(0, q)
  obj <- function(par) {
    r <- .sg_laplace(y, X, Z, sigma_of(par[-seq_len(p)]), par[seq_len(p)], family, m, st$u)
    st$u <- r$u
    -r$ll
  }
  res <- LbfgsbMinimize(obj, start, pgtol = 1e-7, factr = 1e3, max_iter = 500)
  par <- res$x
  theta <- par[-seq_len(p)]
  r <- .sg_laplace(y, X, Z, sigma_of(theta), par[seq_len(p)], family, m, st$u)
  V <- solve(r$H)
  list(beta = par[seq_len(p)], sigma2 = exp(2 * theta[1]), range = if (is.null(D)) NULL else exp(theta[2]),
       loglik = r$ll, aic = -2 * r$ll + 2 * (p + ntheta), u = r$u, posterior_sd = sqrt(diag(V)), posterior_cov = V,
       eta = r$eta, family = family, model = model, converged = res$converged, coords = coords, trials = m)
}

.sg_gh <- function(n = 20) {
  x <- w <- numeric(n)
  z <- 0
  for (i in seq_len((n + 1) %/% 2)) {
    z <- if (i == 1) sqrt(2 * n + 1) - 1.85575 * (2 * n + 1)^(-1 / 6) else if (i == 2) z - 1.14 * n^0.426 / z else
      if (i == 3) 1.86 * z - 0.86 * x[1] else if (i == 4) 1.91 * z - 0.91 * x[2] else 2 * z - x[i - 2]
    for (it in 1:100) {
      p1 <- pi^-0.25
      p2 <- 0
      for (j in seq_len(n)) {
        p3 <- p2
        p2 <- p1
        p1 <- z * sqrt(2 / j) * p2 - sqrt((j - 1) / j) * p3
      }
      pp <- sqrt(2 * n) * p2
      z1 <- z
      z <- z1 - p1 / pp
      if (abs(z - z1) <= 1e-15) break
    }
    x[i] <- z
    x[n + 1 - i] <- -z
    w[i] <- w[n + 1 - i] <- 2 / pp^2
  }
  list(x = x, w = w)
}

#' @rdname SpatialGlmmFit
#' @export
SpatialGlmmPredict <- function(fit, X0, coords0) {
  P <- as.matrix(fit$coords)
  Q <- as.matrix(coords0)
  comp <- list(model = fit$model, psill = fit$sigma2, range = fit$range)
  n <- nrow(P)
  Sigma <- matrix(KrigingCovariance(as.matrix(stats::dist(P)), comp), n) + diag(1e-10 * fit$sigma2, n)
  Si <- solve(Sigma)
  Dq <- sqrt(outer(Q[, 1], P[, 1], "-")^2 + outer(Q[, 2], P[, 2], "-")^2)
  C <- matrix(KrigingCovariance(Dq, comp), nrow(Q))
  Wt <- C %*% Si
  mstar <- as.vector(Wt %*% fit$u)
  v <- fit$sigma2 - rowSums(Wt * C) + rowSums((Wt %*% fit$posterior_cov) * Wt)
  eta <- as.vector(as.matrix(X0) %*% fit$beta) + mstar
  gh <- .sg_gh()
  mean <- if (fit$family == "poisson") exp(eta + v / 2) else
    vapply(seq_along(eta), function(k) sum(gh$w / (1 + exp(-(eta[k] + sqrt(2) * sqrt(max(v[k], 0)) * gh$x)))) / sqrt(pi), 0)
  list(eta = eta, variance = v, mean = mean)
}

#' @rdname SpatialGlmmFit
#' @export
GlmmResiduals <- function(y, mu, family = "poisson", trials = 1, size = NULL, seed = 1) {
  y <- as.numeric(y)
  mu <- as.numeric(mu)
  n <- length(y)
  tr <- rep_len(as.numeric(trials), n)
  v <- .morie_random_uniform(n, seed = seed, stream = 0)
  cdf <- function(k, i) {
    if (k < 0) return(0)
    switch(family, poisson = stats::ppois(k, mu[i]), binomial = stats::pbinom(k, tr[i], mu[i]),
           negbin = stats::pnbinom(k, size = size, mu = mu[i]))
  }
  out <- t(vapply(seq_len(n), function(i) {
    yi <- y[i]
    if (family == "poisson") {
      mean <- mu[i]
      var <- mu[i]
      d <- 2 * ((if (yi > 0) yi * log(yi / mu[i]) else 0) - (yi - mu[i]))
    } else if (family == "binomial") {
      mean <- tr[i] * mu[i]
      var <- mean * (1 - mu[i])
      d <- 2 * ((if (yi > 0) yi * log(yi / mean) else 0) +
                  (if (tr[i] - yi > 0) (tr[i] - yi) * log((tr[i] - yi) / (tr[i] - mean)) else 0))
    } else {
      mean <- mu[i]
      var <- mu[i] + mu[i]^2 / size
      d <- 2 * ((if (yi > 0) yi * log(yi / mu[i]) else 0) - (yi + size) * log((yi + size) / (mu[i] + size)))
    }
    a <- cdf(yi - 1, i)
    b <- cdf(yi, i)
    uu <- min(max(a + v[i] * (b - a), 1e-300), 1 - 1e-16)
    c((yi - mean) / sqrt(var), sign(yi - mean) * sqrt(max(d, 0)), stats::qnorm(uu))
  }, numeric(3)))
  list(pearson = out[, 1], deviance = out[, 2], quantile = out[, 3])
}

#' @rdname SpatialGlmmFit
#' @export
CrpsGaussian <- function(y, mu, sigma) {
  z <- (y - mu) / sigma
  sigma * (z * (2 * stats::pnorm(z) - 1) + 2 * stats::dnorm(z) - 1 / sqrt(pi))
}

#' @rdname SpatialGlmmFit
#' @export
CrpsPoisson <- function(y, lam) {
  vapply(seq_along(y), function(i) {
    p <- exp(-lam[i])
    F <- p
    k <- 0
    s <- 0
    repeat {
      s <- s + (F - (y[i] <= k))^2
      if (k >= max(y[i], lam[i]) && p < 1e-20) break
      k <- k + 1
      p <- p * lam[i] / k
      F <- F + p
    }
    s
  }, 0)
}

#' @rdname SpatialGlmmFit
#' @export
CrpsSample <- function(y, samples) {
  vapply(seq_along(y), function(i) {
    s <- as.numeric(samples[[i]])
    mean(abs(s - y[i])) - sum(abs(outer(s, s, "-"))) / (2 * length(s)^2)
  }, 0)
}
