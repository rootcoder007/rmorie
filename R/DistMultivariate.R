# SPDX-License-Identifier: AGPL-3.0-or-later
.mv_points <- function(x) if (is.matrix(x)) x else matrix(x, nrow = 1)

.mv_log_bessel_i <- function(nu, z) {
  if (z <= 0) return(if (nu == 0) 0 else -Inf)
  k <- 0:max(60, ceiling(2 * z + 60))
  t <- (2 * k + nu) * log(z / 2) - lgamma(k + 1) - lgamma(k + nu + 1)
  m <- max(t)
  m + log(sum(exp(t - m)))
}

#' Multivariate normal density
#'
#' log f = -(d/2) log(2 pi) - (1/2) log|Sigma| - (1/2) squared Mahalanobis distance;
#' draws mu + L z with Philox normals.
#'
#' @param x Point (vector) or matrix of points (rows).
#' @param mean Mean vector.
#' @param cov Covariance matrix.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, mahalanobis, random).
#' @references Anderson, T. W. (2003). An Introduction to Multivariate Statistical Analysis, ch. 2.
#' @examples
#' MvNormDens(c(0, 0))$pdf
#' @export
MvNormDens <- function(x = NULL, mean = c(0, 0), cov = diag(2), n = 0, seed = 0) {
  if (!is.null(x)) .morie_arg(x, "n")
  d <- length(mean)
  L <- t(chol(cov))
  ld <- 2 * sum(log(diag(L)))
  res <- list()
  if (!is.null(x)) {
    X <- .mv_points(x)
    z <- forwardsolve(L, t(X) - mean)
    m2 <- colSums(z^2)
    res$logpdf <- -0.5 * d * log(2 * pi) - 0.5 * ld - 0.5 * m2
    res$pdf <- exp(res$logpdf)
    res$mahalanobis <- m2
  }
  if (n > 0) {
    zs <- matrix(.morie_random_normal(n * d, seed = seed, stream = 0), d)
    res$random <- t(mean + L %*% zs)
  }
  res
}

#' Multivariate t density
#'
#' @param x Point or matrix of points.
#' @param loc Location vector.
#' @param scale Scale matrix.
#' @param df Degrees of freedom.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, random).
#' @references Kotz, S. & Nadarajah, S. (2004). Multivariate t Distributions and Their Applications, ch. 1.
#' @examples
#' MvtDens(c(0, 0), df = 1)$pdf
#' @export
MvtDens <- function(x = NULL, loc = c(0, 0), scale = diag(2), df = 1, n = 0, seed = 0) {
  d <- length(loc)
  if (!(df > 0)) stop("df must be > 0", call. = FALSE)
  L <- t(chol(scale))
  cc <- lgamma((df + d) / 2) - lgamma(df / 2) - d / 2 * log(df * pi) - sum(log(diag(L)))
  res <- list()
  if (!is.null(x)) {
    X <- .mv_points(x)
    q <- colSums(forwardsolve(L, t(X) - loc)^2)
    res$logpdf <- cc - (df + d) / 2 * log1p(q / df)
    res$pdf <- exp(res$logpdf)
  }
  if (n > 0) {
    zs <- matrix(.morie_random_normal(n * d, seed = seed, stream = 0), d)
    w <- qchisq(.morie_random_uniform(n, seed = seed, stream = 1), df)
    res$random <- t(loc + sweep(L %*% zs, 2, sqrt(w / df), "/"))
  }
  res
}

#' Bivariate normal density and distribution function
#'
#' Phi_2 by Gauss-Legendre quadrature of phi(t) Phi((k - rho t)/sqrt(1 - rho^2)).
#'
#' @param x,y Evaluation point.
#' @param mean1,mean2,sd1,sd2 Marginal parameters.
#' @param rho Correlation.
#' @return list(pdf, cdf).
#' @references Owen, D. B. (1956). Annals of Mathematical Statistics 27, 1075-1090.
#' @examples
#' BvNormDist(0, 0, rho = 0.5)$cdf
#' @export
BvNormDist <- function(x, y, mean1 = 0, mean2 = 0, sd1 = 1, sd2 = 1, rho = 0) {
  if (!(sd1 > 0 && sd2 > 0 && rho >= -1 && rho <= 1)) stop("need sd1, sd2 > 0 and -1 <= rho <= 1", call. = FALSE)
  h <- (x - mean1) / sd1
  k <- (y - mean2) / sd2
  if (abs(rho) == 1) return(list(pdf = 0, cdf = if (rho == 1) min(pnorm(h), pnorm(k)) else max(0, pnorm(h) + pnorm(k) - 1)))
  s <- sqrt(1 - rho^2)
  pdf <- exp(-(h^2 - 2 * rho * h * k + k^2) / (2 * s^2)) / (2 * pi * sd1 * sd2 * s)
  cdf <- if (h <= -9) 0 else .dist_gl(function(t) dnorm(t) * pnorm((k - rho * t) / s), -9, h, n = max(64L, as.integer(8 * (h + 9))))
  list(pdf = pdf, cdf = cdf)
}

#' Multivariate skew-normal density
#'
#' @param x Point or matrix of points.
#' @param xi Location.
#' @param omega Scale matrix.
#' @param alpha Slant vector.
#' @return list(pdf, logpdf).
#' @references Azzalini, A. & Dalla Valle, A. (1996). Biometrika 83, 715-726.
#' @examples
#' MvSkewNorm(c(0, 0), alpha = c(3, -1))$pdf
#' @export
MvSkewNorm <- function(x, xi = c(0, 0), omega = diag(2), alpha = c(0, 0)) {
  X <- .mv_points(x)
  d <- length(xi)
  L <- t(chol(omega))
  E <- t(X) - xi
  z <- forwardsolve(L, E)
  lin <- colSums(alpha / sqrt(diag(omega)) * E)
  lp <- log(2) - 0.5 * d * log(2 * pi) - sum(log(diag(L))) - 0.5 * colSums(z^2) + pnorm(lin, log.p = TRUE)
  list(pdf = exp(lp), logpdf = lp)
}

#' Bivariate Poisson distribution
#'
#' @param x,y Counts.
#' @param a,b Rates of the independent parts.
#' @param c Common rate.
#' @return list(pmf, cov).
#' @references Holgate, P. (1964). Biometrika 51, 241-245.
#' @examples
#' BvPois(0, 0, 1, 2, 0.5)$pmf
#' @export
BvPois <- function(x, y, a = 1, b = 1, c = 0) {
  if (!(a > 0 && b > 0 && c >= 0) || x < 0 || y < 0) stop("need a, b > 0, c >= 0 and non-negative x, y", call. = FALSE)
  base <- -(a + b + c) + x * log(a) + y * log(b) - lgamma(x + 1) - lgamma(y + 1)
  k <- 0:(if (c == 0) 0 else min(x, y))
  lt <- lchoose(x, k) + lchoose(y, k) + lgamma(k + 1) + ifelse(k > 0, k * log(c / (a * b)), 0)
  list(pmf = exp(base) * sum(exp(lt)), cov = c)
}

.mv_lmvgamma <- function(a, p) p * (p - 1) / 4 * log(pi) + sum(lgamma(a - (0:(p - 1)) / 2))

#' Wishart and inverse Wishart densities
#'
#' @param W Positive-definite matrix.
#' @param df Degrees of freedom.
#' @param S Scale matrix.
#' @param inverse Inverse Wishart when TRUE.
#' @return list(pdf, logpdf).
#' @references Anderson, T. W. (2003). An Introduction to Multivariate Statistical Analysis, ch. 7.
#' @examples
#' WishartDens(matrix(1), 3, matrix(1))$pdf
#' @export
WishartDens <- function(W, df, S, inverse = FALSE) {
  W <- as.matrix(W)
  S <- as.matrix(S)
  p <- nrow(W)
  if (nrow(S) != p || !(df > p - 1)) stop("W and S must have the same dimension and df > p - 1", call. = FALSE)
  ldw <- 2 * sum(log(diag(chol(W))))
  lds <- 2 * sum(log(diag(chol(S))))
  lp <- if (inverse) {
    df / 2 * lds - df * p / 2 * log(2) - .mv_lmvgamma(df / 2, p) - (df + p + 1) / 2 * ldw - sum(diag(S %*% chol2inv(chol(W)))) / 2
  } else {
    (df - p - 1) / 2 * ldw - sum(diag(chol2inv(chol(S)) %*% W)) / 2 - df * p / 2 * log(2) - df / 2 * lds - .mv_lmvgamma(df / 2, p)
  }
  list(pdf = exp(lp), logpdf = lp)
}

#' LKJ density on correlation matrices
#'
#' @param R Correlation matrix.
#' @param eta Shape.
#' @return list(pdf, logpdf, log_normalizer).
#' @references Lewandowski, Kurowicka & Joe (2009). JMVA 100, 1989-2001.
#' @examples
#' LkjCorr(diag(2), 1)$pdf
#' @export
LkjCorr <- function(R, eta = 1) {
  R <- as.matrix(R)
  d <- nrow(R)
  if (!(eta > 0) || any(abs(diag(R) - 1) > 1e-12)) stop("R must be a correlation matrix and eta > 0", call. = FALSE)
  k <- seq_len(d - 1)
  b <- eta + (d - k - 1) / 2
  logc <- sum((2 * eta - 2 + d - k) * (d - k) * log(2) + (d - k) * (2 * lgamma(b) - lgamma(2 * b)))
  lp <- (eta - 1) * 2 * sum(log(diag(chol(R)))) - logc
  list(pdf = exp(lp), logpdf = lp, log_normalizer = logc)
}

#' Von Mises-Fisher density
#'
#' @param x Unit vector or matrix of unit vectors (rows).
#' @param mu Mean direction.
#' @param kappa Concentration.
#' @return list(pdf, logpdf, log_normalizer).
#' @references Mardia, K. V. & Jupp, P. E. (2000). Directional Statistics, Sec 9.3.2.
#' @examples
#' VmfDens(c(0, 0, 1), c(0, 0, 1), 2)$pdf
#' @export
VmfDens <- function(x, mu, kappa) {
  p <- length(mu)
  if (p < 2 || abs(sum(mu^2) - 1) > 1e-9 || kappa < 0) stop("mu must be a unit vector in dimension >= 2 and kappa >= 0", call. = FALSE)
  logc <- if (kappa == 0) lgamma(p / 2) - log(2) - p / 2 * log(pi) else (p / 2 - 1) * log(kappa) - p / 2 * log(2 * pi) - .mv_log_bessel_i(p / 2 - 1, kappa)
  lp <- logc + kappa * drop(.mv_points(x) %*% mu)
  list(pdf = exp(lp), logpdf = lp, log_normalizer = logc)
}

#' Kent (FB5) density on the sphere
#'
#' @param x Unit 3-vector or matrix of them.
#' @param kappa,beta Concentration and ovalness.
#' @param g1,g2,g3 Orthonormal axes.
#' @return list(pdf, logpdf, log_normalizer).
#' @references Kent, J. T. (1982). JRSS B 44, 71-80.
#' @examples
#' KentDens(c(0, 0, 1), 2, 0.5)$pdf
#' @export
KentDens <- function(x, kappa, beta, g1 = c(0, 0, 1), g2 = c(1, 0, 0), g3 = c(0, 1, 0)) {
  if (!(kappa > 0 && beta >= 0 && 2 * beta < kappa)) stop("need kappa > 0 and 0 <= 2 beta < kappa", call. = FALSE)
  G <- rbind(g1, g2, g3)
  if (max(abs(G %*% t(G) - diag(3))) > 1e-9) stop("g1, g2, g3 must be orthonormal", call. = FALSE)
  terms <- numeric(0)
  j <- 0
  repeat {
    t <- lgamma(j + 0.5) - lgamma(j + 1) + (if (beta > 0) 2 * j * log(beta) else if (j == 0) 0 else -Inf) -
      (2 * j + 0.5) * log(kappa / 2) + .mv_log_bessel_i(2 * j + 0.5, kappa)
    terms <- c(terms, t)
    if (j > 3 && (t < max(terms) - 40 || beta == 0)) break
    j <- j + 1
  }
  m <- max(terms)
  logc <- log(2 * pi) + m + log(sum(exp(terms - m)))
  P <- .mv_points(x) %*% t(G)
  lp <- unname(kappa * P[, 1] + beta * (P[, 2]^2 - P[, 3]^2) - logc)
  list(pdf = exp(lp), logpdf = lp, log_normalizer = logc)
}

#' Bivariate copula densities
#'
#' @param u,v Points in (0, 1).
#' @param family "gaussian", "t", "clayton", "gumbel", "frank", "joe" or "bb1"
#'   (Joe 2014, sec 4.17; delta = 1 is Clayton, theta to 0 is Gumbel(delta)).
#' @param theta Copula parameter.
#' @param df Degrees of freedom for the t copula.
#' @param delta Second BB1 parameter, delta >= 1.
#' @return list(density, logdensity).
#' @references Joe, H. (2014). Dependence Modeling with Copulas, ch. 4.
#' @examples
#' CopulaDens(0.3, 0.7, "frank", 3)$density
#' @export
CopulaDens <- function(u, v, family = c("gaussian", "t", "clayton", "gumbel", "frank", "joe", "bb1"), theta = 0.5, df = 4, delta = 1.5) {
  family <- match.arg(family)
  if (!(u > 0 && u < 1 && v > 0 && v < 1)) stop("u and v must be in (0, 1)", call. = FALSE)
  d <- switch(family,
    gaussian = {
      a <- qnorm(u)
      b <- qnorm(v)
      exp(-(theta^2 * (a^2 + b^2) - 2 * theta * a * b) / (2 * (1 - theta^2))) / sqrt(1 - theta^2)
    },
    t = {
      a <- qt(u, df)
      b <- qt(v, df)
      q <- (a^2 - 2 * theta * a * b + b^2) / (1 - theta^2)
      exp(lgamma((df + 2) / 2) - lgamma(df / 2) - log(df * pi) - 0.5 * log(1 - theta^2) - (df + 2) / 2 * log1p(q / df)) / (stats::dt(a, df) * stats::dt(b, df))
    },
    clayton = (1 + theta) * (u * v)^(-theta - 1) * (u^-theta + v^-theta - 1)^(-2 - 1 / theta),
    gumbel = {
      x <- -log(u)
      y <- -log(v)
      A <- x^theta + y^theta
      exp(-A^(1 / theta)) * (x * y)^(theta - 1) / (u * v) * A^(1 / theta - 2) * (A^(1 / theta) + theta - 1)
    },
    frank = {
      e <- -expm1(-theta)
      den <- e - (-expm1(-theta * u)) * (-expm1(-theta * v))
      theta * e * exp(-theta * (u + v)) / den^2
    },
    joe = {
      ub <- 1 - u
      vb <- 1 - v
      s <- ub^theta + vb^theta - (ub * vb)^theta
      s^(1 / theta - 2) * ub^(theta - 1) * vb^(theta - 1) * (theta - 1 + s)
    },
    bb1 = {
      if (!(theta > 0 && delta >= 1)) stop("need theta > 0 and delta >= 1", call. = FALSE)
      a <- u^-theta - 1
      b <- v^-theta - 1
      s <- a^delta + b^delta
      w <- s^(1 / delta)
      xu <- delta * a^(delta - 1) * theta * u^(-theta - 1)
      yv <- delta * b^(delta - 1) * theta * v^(-theta - 1)
      h1 <- -(1 + w)^(-1 / theta - 1) / theta
      h2 <- (1 / theta) * (1 / theta + 1) * (1 + w)^(-1 / theta - 2)
      xu * yv * (h2 * (w / (delta * s))^2 + h1 * (1 / delta) * (1 / delta - 1) * w / s^2)
    })
  list(density = d, logdensity = log(d))
}

#' Multinomial distribution
#'
#' @param x Counts.
#' @param size Number of trials.
#' @param probs Category probabilities (normalised).
#' @param n Number of random count vectors.
#' @param seed Philox seed.
#' @return list(pmf, logpmf, mean, cov, random).
#' @references Johnson, Kotz & Balakrishnan (1997). Discrete Multivariate Distributions, ch. 35.
#' @examples
#' MultinomialDist(c(1, 2, 1), probs = c(0.2, 0.5, 0.3))$pmf
#' @export
MultinomialDist <- function(x = NULL, size = NULL, probs = c(0.5, 0.5), n = 0, seed = 0) {
  if (!length(probs) || min(probs) < 0 || sum(probs) <= 0) stop("probs must be non-negative with a positive sum", call. = FALSE)
  p <- probs / sum(probs)
  if (is.null(size)) size <- sum(x)
  res <- list(mean = size * p, cov = size * (diag(p, length(p)) - tcrossprod(p)))
  if (!is.null(x)) {
    lp <- lgamma(size + 1) + sum(-lgamma(x + 1) + ifelse(x > 0, x * log(p), 0))
    res$logpmf <- lp
    res$pmf <- exp(lp)
  }
  if (n > 0) {
    cum <- cumsum(p)
    cum[length(cum)] <- 1
    u <- matrix(.morie_random_uniform(n * size, seed = seed, stream = 0), nrow = size)
    res$random <- t(apply(u, 2, function(col) tabulate(vapply(col, function(w) which(w <= cum)[1], numeric(1)), length(p))))
  }
  res
}
