.es_qnbinom <- function(p, size, prob) {
  target <- p * (1 - 64 * .Machine$double.eps)
  f <- exp(size * log(prob))
  cdf <- f
  y <- 0
  while (cdf < target) {
    f <- f * (y + size) / (y + 1) * (1 - prob)
    y <- y + 1
    cdf <- cdf + f
  }
  y
}

#' Spatial epidemiology and disease surveillance
#'
#' \code{BayesOutbreak}: the Bayes outbreak detector of
#' \code{surveillance::algo.bayes}. \code{CusumSurveillance}: CUSUM of
#' standardised counts (\code{algo.cusum}); with expected counts per region
#' the spatial surveillance CUSUM. \code{EcologicalRegression}: Poisson,
#' negative binomial and zero-inflated Poisson regression with expected-count
#' offsets. \code{LerouxPrecision} and \code{Bym2Structure}: Leroux and BYM2
#' area-effect structures. \code{BufferExposure} and \code{KernelExposure}:
#' exposure assessment. Identical to the Python arm \code{morie.fn.episurv}.
#'
#' @param observed Counts (vector, or times by regions matrix).
#' @param freq Observations per year.
#' @param b Number of past years.
#' @param w Half-window width.
#' @param act_y Use the preceding w counts.
#' @param alpha Test level.
#' @param time_points 1-based time points to evaluate.
#' @param expected Expected counts (same shape as observed).
#' @param start First monitored time point when no expected counts are given.
#' @param k,h CUSUM reference value and threshold.
#' @param trans \code{"standard"}, \code{"rossi"}, \code{"anscombe"} or
#'   \code{"none"}.
#' @param reset Reset the sum after an alarm.
#' @param counts Area counts.
#' @param X Area covariates (no intercept).
#' @param family \code{"poisson"}, \code{"negbin"} or \code{"zip"}.
#' @param Z Zero-inflation covariates (no intercept).
#' @param tol,maxit Convergence controls.
#' @param A Symmetric adjacency matrix.
#' @param rho Leroux spatial parameter.
#' @param tau Precision.
#' @param phi Share of structured variance.
#' @param receptors,sources Coordinates (rows).
#' @param radius Buffer radius.
#' @param weights Source weights.
#' @param metric \code{"euclidean"} or \code{"haversine"}.
#' @param bandwidth Kernel bandwidth.
#' @param kernel \code{"gaussian"} or \code{"quartic"}.
#' @return A list or vector.
#' @references Hoehle, M. (2007). surveillance: an R package for the
#'   monitoring of infectious diseases. Computational Statistics 22, 571-582.
#'
#'   Rogerson, P. A. and Yamada, I. (2004). Approaches to syndromic
#'   surveillance when data consist of small regional counts. MMWR 53, 79-85.
#'
#'   Lambert, D. (1992). Zero-inflated Poisson regression. Technometrics 34,
#'   1-14.
#'
#'   Leroux, B. G., Lei, X. and Breslow, N. (2000). Estimation of disease
#'   rates in small areas: a new mixed model for spatial dependence.
#'   Springer, 179-191.
#'
#'   Riebler, A., Sorbye, S. H., Simpson, D. and Rue, H. (2016). An intuitive
#'   Bayesian spatial model for disease mapping that accounts for scaling.
#'   Statistical Methods in Medical Research 25, 1145-1165.
#' @examples
#' BayesOutbreak(c(3, 5, 2, 4, 6, 3, 4, 12), w = 6, time_points = 8)$alarm
#' LerouxPrecision(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)), 0.5, 2)
#' @export
BayesOutbreak <- function(observed, freq = 52, b = 0, w = 6, act_y = TRUE, alpha = 0.05, time_points = NULL) {
  x <- observed
  tps <- if (is.null(time_points)) (b * freq + w + 1):length(x) else time_points
  ub <- numeric(length(tps))
  al <- logical(length(tps))
  for (q in seq_along(tps)) {
    t <- tps[q]
    if (t - b * freq - w < 1) stop("the series is too short for the reference window")
    base <- if (act_y) x[(t - w):(t - 1)] else numeric(0)
    for (i in seq_len(b)) base <- c(base, x[(t - i * freq - w):(t - i * freq + w)])
    s <- sum(base, na.rm = TRUE)
    n <- sum(!is.na(base))
    ub[q] <- .es_qnbinom(1 - alpha, s + 0.5, n / (n + 1))
    al[q] <- x[t] > ub[q]
  }
  list(upperbound = ub, alarm = al, time_points = tps)
}

#' @rdname BayesOutbreak
#' @export
CusumSurveillance <- function(observed, expected = NULL, start = NULL, k = 1.04, h = 2.26, trans = "standard",
                              reset = FALSE) {
  multi <- is.matrix(observed)
  X <- as.matrix(observed)
  E <- if (is.null(expected)) NULL else as.matrix(expected)
  cs <- NULL
  al <- NULL
  for (cc in seq_len(ncol(X))) {
    x <- X[, cc]
    if (!is.null(E)) {
      idx <- seq_along(x)
      m <- E[, cc]
    } else {
      idx <- start:length(x)
      m <- rep(sum(x[seq_len(start - 1)]) / (start - 1), length(idx))
    }
    v <- x[idx]
    z <- switch(trans,
      standard = (v - m) / sqrt(m),
      rossi = (v - 3 * m + 2 * sqrt(v * m)) / (2 * sqrt(m)),
      anscombe = 1.5 * (v^(2 / 3) - m^(2 / 3)) / m^(1 / 6),
      none = v,
      stop("trans must be standard, rossi, anscombe or none")
    )
    s <- 0
    sc <- numeric(length(z))
    ac <- logical(length(z))
    for (i in seq_along(z)) {
      s <- max(0, s + (z[i] - k))
      ac[i] <- s >= h
      if (reset && ac[i]) s <- 0
      sc[i] <- s
    }
    cs <- cbind(cs, sc)
    al <- cbind(al, ac)
  }
  dimnames(cs) <- NULL
  dimnames(al) <- NULL
  if (!multi) return(list(cusum = as.vector(cs), alarm = as.vector(al)))
  list(cusum = cs, alarm = al)
}

.es_irls <- function(y, X, off, wfun, maxit = 100, tol = 1e-12) {
  beta <- numeric(ncol(X))
  beta[1] <- log(max(sum(y), 0.5) / sum(exp(off)))
  for (it in seq_len(maxit)) {
    eta <- as.vector(X %*% beta) + off
    mu <- exp(eta)
    w <- wfun(mu)
    z <- eta - off + (y - mu) / mu
    new <- as.vector(solve(crossprod(X * w, X), crossprod(X * w, z)))
    done <- max(abs(new - beta)) < tol * (1 + max(abs(beta)))
    beta <- new
    if (done) break
  }
  beta
}

.es_digamma <- function(x) {
  acc <- 0
  while (x < 12) {
    acc <- acc - 1 / x
    x <- x + 1
  }
  inv <- 1 / x
  i2 <- inv^2
  acc + log(x) - 0.5 * inv - i2 * (1 / 12 - i2 * (1 / 120 - i2 * (1 / 252 - i2 / 240)))
}

.es_trigamma <- function(x) {
  acc <- 0
  while (x < 12) {
    acc <- acc + 1 / x^2
    x <- x + 1
  }
  inv <- 1 / x
  i2 <- inv^2
  acc + inv + i2 / 2 + inv * i2 * (1 / 6 - i2 * (1 / 30 - i2 * (1 / 42 - i2 / 30)))
}

#' @rdname BayesOutbreak
#' @export
EcologicalRegression <- function(counts, expected, X = NULL, family = "poisson", Z = NULL, tol = 1e-12, maxit = 500) {
  y <- counts
  off <- log(expected)
  n <- length(y)
  Xm <- if (is.null(X)) matrix(1, n, 1) else cbind(1, as.matrix(X))
  fitted <- function(beta) exp(as.vector(Xm %*% beta) + off)
  if (family == "poisson") {
    beta <- .es_irls(y, Xm, off, function(mu) mu)
    mu <- fitted(beta)
    return(list(coefficients = beta, fitted = mu, loglik = sum(y * log(mu) - mu - lgamma(y + 1))))
  }
  if (family == "negbin") {
    beta <- .es_irls(y, Xm, off, function(mu) mu)
    mu <- fitted(beta)
    th <- n / sum((y / mu - 1)^2)
    for (it in seq_len(maxit)) {
      for (j in seq_len(100)) {
        sc <- sum(vapply(y + th, .es_digamma, 0) - .es_digamma(th) + log(th) + 1 - log(th + mu) - (y + th) / (mu + th))
        inf <- sum(-vapply(y + th, .es_trigamma, 0) + .es_trigamma(th) - 1 / th + 2 / (mu + th) - (y + th) / (mu + th)^2)
        if (!(inf > 0)) {
          th <- min(2 * th, 1e10)
          if (th >= 1e10) break
          next
        }
        d <- sc / inf
        th <- max(th + d, 1e-8)
        if (abs(d) < tol * (1 + th)) break
      }
      nb <- .es_irls(y, Xm, off, function(m) m / (1 + m / th))
      done <- max(abs(nb - beta)) < 1e-10
      beta <- nb
      mu <- fitted(beta)
      if (done) break
    }
    ll <- sum(lgamma(y + th) - lgamma(th) - lgamma(y + 1) + th * log(th / (th + mu)) + y * log(mu / (th + mu)))
    return(list(coefficients = beta, theta = th, fitted = mu, loglik = ll))
  }
  if (family == "zip") {
    Zm <- if (is.null(Z)) matrix(1, n, 1) else cbind(1, as.matrix(Z))
    beta <- .es_irls(y, Xm, off, function(mu) mu)
    gam <- numeric(ncol(Zm))
    ll_old <- -Inf
    for (it in seq_len(maxit * 20)) {
      mu <- fitted(beta)
      pi <- 1 / (1 + exp(-as.vector(Zm %*% gam)))
      zz <- ifelse(y == 0, pi / (pi + (1 - pi) * exp(-mu)), 0)
      wts <- 1 - zz
      b <- beta
      for (j in seq_len(50)) {
        m2 <- fitted(b)
        st <- as.vector(solve(crossprod(Xm * (wts * m2), Xm), crossprod(Xm, wts * (y - m2))))
        b <- b + st
        if (max(abs(st)) < 1e-13) break
      }
      beta <- b
      gm <- gam
      for (j in seq_len(50)) {
        p2 <- 1 / (1 + exp(-as.vector(Zm %*% gm)))
        st <- as.vector(solve(crossprod(Zm * (p2 * (1 - p2)), Zm), crossprod(Zm, zz - p2)))
        gm <- gm + st
        if (max(abs(st)) < 1e-13) break
      }
      gam <- gm
      mu <- fitted(beta)
      pi <- 1 / (1 + exp(-as.vector(Zm %*% gam)))
      ll <- sum(ifelse(y == 0, log(pi + (1 - pi) * exp(-mu)), log(1 - pi) + y * log(mu) - mu - lgamma(y + 1)))
      if (abs(ll - ll_old) < tol * (1 + abs(ll))) break
      ll_old <- ll
    }
    return(list(coefficients = beta, zero_coefficients = gam, fitted = mu, loglik = ll))
  }
  stop("family must be 'poisson', 'negbin' or 'zip'")
}

#' @rdname BayesOutbreak
#' @export
LerouxPrecision <- function(A, rho, tau = 1) tau * (rho * (diag(rowSums(A)) - A) + (1 - rho) * diag(nrow(A)))

#' @rdname BayesOutbreak
#' @export
Bym2Structure <- function(A, tau = 1, phi = 0.5, tol = 1e-10) {
  Q <- diag(rowSums(A)) - A
  e <- eigen(Q, symmetric = TRUE)
  keep <- e$values > tol * max(e$values)
  G <- e$vectors[, keep, drop = FALSE] %*% (t(e$vectors[, keep, drop = FALSE]) / e$values[keep])
  s <- exp(mean(log(diag(G))))
  list(scaling_factor = s, generalized_inverse = G, covariance = ((1 - phi) * diag(nrow(A)) + phi * G / s) / tau)
}

.es_dist <- function(p, q, metric) {
  if (metric == "euclidean") return(sqrt(sum((p - q)^2)))
  r <- pi / 180
  a <- sin((q[1] - p[1]) * r / 2)^2 + cos(p[1] * r) * cos(q[1] * r) * sin((q[2] - p[2]) * r / 2)^2
  2 * 6371 * asin(min(1, sqrt(a)))
}

#' @rdname BayesOutbreak
#' @export
BufferExposure <- function(receptors, sources, radius, weights = NULL, metric = "euclidean") {
  R <- as.matrix(receptors)
  S <- as.matrix(sources)
  w <- if (is.null(weights)) rep(1, nrow(S)) else weights
  d <- t(apply(R, 1, function(p) apply(S, 1, function(q) .es_dist(p, q, metric))))
  if (nrow(S) == 1) d <- matrix(d, ncol = 1)
  inside <- d <= radius
  list(count = rowSums(inside), weighted = as.vector(inside %*% w), nearest = apply(d, 1, min))
}

#' @rdname BayesOutbreak
#' @export
KernelExposure <- function(receptors, sources, bandwidth, weights = NULL, kernel = "gaussian") {
  R <- as.matrix(receptors)
  S <- as.matrix(sources)
  w <- if (is.null(weights)) rep(1, nrow(S)) else weights
  apply(R, 1, function(p) {
    u <- sqrt(colSums((t(S) - p)^2)) / bandwidth
    kv <- switch(kernel,
      gaussian = exp(-u^2 / 2) / (2 * pi),
      quartic = ifelse(u < 1, 3 / pi * (1 - u^2)^2, 0),
      stop("kernel must be 'gaussian' or 'quartic'")
    )
    sum(w * kv) / bandwidth^2
  })
}
