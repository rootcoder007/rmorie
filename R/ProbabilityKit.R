# SPDX-License-Identifier: AGPL-3.0-or-later
# Probability and inference toolkit.
# Identical to the Python arm morie.fn.probkit.

#' Probability toolkit: product variance, Weibull and folded-normal laws, evidence, CRPS, maximum entropy, EV simulation, DDM, KING
#'
#' \code{ProductVariance}: \code{Var(XY) = Var(X) Var(Y) + (EX)^2 Var(Y) + (EY)^2 Var(X)}
#' for independent factors. \code{WeibullMoments}: mean
#' \code{Gamma(1 + 1/gamma) / lam^(1/gamma)} and variance of
#' \code{T = X^(1/gamma)}, \code{X ~ Expo(lam)}. \code{FoldedNormal}: CDF,
#' density, mean and variance of \code{|X|}, \code{X ~ N(mu, sigma^2)}.
#' \code{Chisq1Cdf}: \code{2 Phi(sqrt(x)) - 1}. \code{HarmonicMeanEvidence}:
#' Newton-Raftery \code{log m = log S - logsumexp(-l_s)}. \code{CrpsCdf}:
#' \code{int (F(z) - 1(z >= y))^2 dz} by exp-sinh tails and tanh-sinh pieces
#' between \code{y} and the \code{breaks}. \code{MaxEntropyDiscrete}:
#' \code{p_i = q_i exp(sum_k lambda_k g_k(x_i)) / Z} by Newton steps on the dual.
#' \code{BvLogisticSimulate}: Shi's mixture method for the bivariate logistic
#' extreme-value law (the algorithm of evd::rbvevd) on Philox streams 0-3,
#' with GEV margins. \code{DdmDrift}: Gama et al. (2004) drift detection (0-based
#' indices). \code{KingKinship}: KING robust (between-family) or within-family kinship.
#'
#' @param mean_x,var_x,mean_y,var_y Means and variances of the two factors.
#' @param lam,gamma Weibull rate and shape (Blitzstein-Hwang parameterisation).
#' @param y Evaluation points, or the observation for \code{CrpsCdf}.
#' @param mu,sigma Normal mean and standard deviation.
#' @param x Values.
#' @param log_lik Log-likelihoods at posterior draws.
#' @param cdf A CDF function.
#' @param breaks Points where the CDF is not smooth.
#' @param h Double-exponential step.
#' @param support_values Matrix (constraints by support points) of g_k(x_i).
#' @param targets Target moments.
#' @param prior Prior probabilities, or NULL for uniform.
#' @param tol,max_iter Newton tolerance and iterations.
#' @param n Number of draws.
#' @param alpha Logistic dependence (0, 1].
#' @param loc,scale,shape GEV margin parameters (length 2).
#' @param seed Philox seed.
#' @param errors 0/1 error stream.
#' @param min_instances,warning_level,drift_level DDM settings.
#' @param genotypes Matrix individuals by SNPs of minor-allele counts (NA missing).
#' @param method "robust" or "within".
#' @return A number, vector or list.
#' @references Blitzstein, J. K. and Hwang, J. (2019). Introduction to
#'   Probability, 2nd ed. Leone, F. C. et al. (1961). Technometrics 3,
#'   543-550. Newton, M. A. and Raftery, A. E. (1994). JRSS B 56, 3-48.
#'   Matheson, J. E. and Winkler, R. L. (1976). Management Science 22,
#'   1087-1096. Jaynes, E. T. (1957). Physical Review 106, 620-630. Shi, D.
#'   (1995). Biometrika 82, 644-649. Stephenson, A. (2003). Extremes 6,
#'   49-59. Gama, J. et al. (2004). SBIA 2004, 286-295. Manichaikul, A. et al.
#'   (2010). Bioinformatics 26, 2867-2873.
#' @examples
#' ProductVariance(1, 2, 3, 4)
#' CrpsCdf(pnorm, 0.3)
#' MaxEntropyDiscrete(rbind(1:6), 4.5)$p
#' @export
ProductVariance <- function(mean_x, var_x, mean_y, var_y) var_x * var_y + mean_x^2 * var_y + mean_y^2 * var_x

#' @rdname ProductVariance
#' @export
WeibullMoments <- function(lam, gamma) {
  list(mean = base::gamma(1 + 1 / gamma) / lam^(1 / gamma),
       var = (base::gamma(1 + 2 / gamma) - base::gamma(1 + 1 / gamma)^2) / lam^(2 / gamma))
}

#' @rdname ProductVariance
#' @export
FoldedNormal <- function(y, mu = 0, sigma = 1) {
  y <- as.numeric(y)
  a <- (y - mu) / sigma
  b <- (y + mu) / sigma
  cdf <- ifelse(y < 0, 0, stats::pnorm(a) + stats::pnorm(b) - 1)
  pdf <- ifelse(y < 0, 0, (exp(-a * a / 2) + exp(-b * b / 2)) / (sigma * sqrt(2 * pi)))
  m <- sigma * sqrt(2 / pi) * exp(-mu * mu / (2 * sigma * sigma)) + mu * (1 - 2 * stats::pnorm(-mu / sigma))
  list(cdf = cdf, pdf = pdf, mean = m, var = mu * mu + sigma * sigma - m * m)
}

#' @rdname ProductVariance
#' @export
Chisq1Cdf <- function(x) {
  x <- as.numeric(x)
  ifelse(x > 0, 2 * stats::pnorm(sqrt(pmax(x, 0))) - 1, 0)
}

#' @rdname ProductVariance
#' @export
HarmonicMeanEvidence <- function(log_lik) {
  ll <- as.numeric(log_lik)
  m <- max(-ll)
  le <- log(length(ll)) - (m + log(sum(exp(-ll - m))))
  list(log_evidence = le, evidence = exp(le))
}

.pk_exp_sinh <- function(g, h = 1 / 64, tmax = 4) {
  n <- round(tmax / h)
  s <- 0
  for (k in -n:n) {
    t <- k * h
    u <- exp(pi / 2 * sinh(t))
    if (u == 0 || is.infinite(u)) next
    s <- s + g(u) * u * pi / 2 * cosh(t)
  }
  s * h
}

.pk_tanh_sinh <- function(g, a, b, h = 1 / 64, tmax = 3.5) {
  n <- round(tmax / h)
  cc <- (a + b) / 2
  r <- (b - a) / 2
  s <- 0
  for (k in -n:n) {
    t <- k * h
    v <- pi / 2 * sinh(t)
    x <- tanh(v)
    if (abs(x) >= 1) next
    s <- s + g(cc + r * x) * pi / 2 * cosh(t) / cosh(v)^2
  }
  s * h * r
}

#' @rdname ProductVariance
#' @export
CrpsCdf <- function(cdf, y, breaks = numeric(0), h = 1 / 64) {
  pts <- sort(unique(c(as.numeric(y), as.numeric(breaks))))
  g <- function(z) (cdf(z) - (if (z >= y) 1 else 0))^2
  np <- length(pts)
  total <- .pk_exp_sinh(function(u) g(pts[1] - u), h) + .pk_exp_sinh(function(u) g(pts[np] + u), h)
  for (i in seq_len(np - 1)) total <- total + .pk_tanh_sinh(g, pts[i], pts[i + 1], h)
  total
}

#' @rdname ProductVariance
#' @export
MaxEntropyDiscrete <- function(support_values, targets, prior = NULL, tol = 1e-12, max_iter = 200) {
  G <- matrix(as.numeric(support_values), nrow = NROW(support_values))
  m <- as.numeric(targets)
  K <- nrow(G)
  n <- ncol(G)
  q <- if (is.null(prior)) rep(1 / n, n) else as.numeric(prior)
  dual <- function(lm) {
    a <- log(q) + as.numeric(crossprod(G, lm))
    mx <- max(a)
    z <- sum(exp(a - mx))
    list(f = mx + log(z) - sum(lm * m), p = exp(a - mx) / z)
  }
  lam <- numeric(K)
  d0 <- dual(lam)
  f <- d0$f
  p <- d0$p
  it <- 0
  for (it in seq_len(max_iter)) {
    mu <- as.numeric(G %*% p)
    g <- mu - m
    if (max(abs(g)) <= tol) break
    Gc <- G - mu
    H <- Gc %*% (t(Gc) * p)
    d <- as.numeric(solve(H, -g))
    step <- 1
    gd <- sum(g * d)
    gmax <- max(abs(g))
    repeat {
      nw <- lam + step * d
      dn <- dual(nw)
      gn <- max(abs(as.numeric(G %*% dn$p) - m))
      if (dn$f <= f + 1e-4 * step * gd || gn < gmax || step < 1e-12) break
      step <- step / 2
    }
    lam <- nw
    f <- dn$f
    p <- dn$p
  }
  list(p = p, lambdas = lam, entropy = -sum(p[p > 0] * log(p[p > 0])), iterations = it)
}

#' @rdname ProductVariance
#' @export
BvLogisticSimulate <- function(n, alpha, loc = c(0, 0), scale = c(1, 1), shape = c(0, 0), seed = 0) {
  u <- .morie_random_uniform(n, seed = seed, stream = 0)
  mix <- .morie_random_uniform(n, seed = seed, stream = 1)
  z <- -log(.morie_random_uniform(n, seed = seed, stream = 2))
  z <- ifelse(mix < alpha, z - log(.morie_random_uniform(n, seed = seed, stream = 3)), z)
  f1 <- 1 / (z * u^alpha)
  f2 <- 1 / (z * (1 - u)^alpha)
  tr <- function(f, k) if (shape[k] == 0) loc[k] + scale[k] * log(f) else loc[k] + scale[k] * (f^shape[k] - 1) / shape[k]
  list(x = tr(f1, 1), y = tr(f2, 2))
}

#' @rdname ProductVariance
#' @export
DdmDrift <- function(errors, min_instances = 30, warning_level = 2, drift_level = 3) {
  warns <- drifts <- integer(0)
  t <- 0
  p <- 1
  pmin <- smin <- psmin <- Inf
  for (i in seq_along(errors)) {
    t <- t + 1
    p <- p + (errors[i] - p) / t
    s <- sqrt(p * (1 - p) / t)
    if (t < min_instances) next
    if (p + s <= psmin) {
      pmin <- p
      smin <- s
      psmin <- p + s
    }
    if (p + s > pmin + drift_level * smin) {
      drifts <- c(drifts, i - 1L)
      t <- 0
      p <- 1
      pmin <- smin <- psmin <- Inf
    } else if (p + s > pmin + warning_level * smin) {
      warns <- c(warns, i - 1L)
    }
  }
  list(warning_points = warns, drifts = drifts)
}

#' @rdname ProductVariance
#' @export
KingKinship <- function(genotypes, method = "robust") {
  G <- as.matrix(genotypes)
  n <- nrow(G)
  K <- diag(0.5, n)
  for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    ok <- !is.na(G[i, ]) & !is.na(G[j, ])
    a <- G[i, ok]
    b <- G[j, ok]
    hi <- sum(a == 1)
    hj <- sum(b == 1)
    hh <- sum(a == 1 & b == 1)
    oo <- sum((a == 0 & b == 2) | (a == 2 & b == 0))
    nm <- min(hi, hj)
    phi <- if (hi + hj == 0 || (method != "within" && nm == 0)) NaN else if (method == "within") {
      (hh - 2 * oo) / (hi + hj)
    } else {
      (hh - 2 * oo) / (2 * nm) + 0.5 - (hi + hj) / (4 * nm)
    }
    K[i, j] <- K[j, i] <- phi
  }
  list(kinship = K)
}
