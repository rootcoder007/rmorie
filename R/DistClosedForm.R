# SPDX-License-Identifier: AGPL-3.0-or-later
.dist_gl <- function(f, a, b, n = 64) {
  xs <- c(0.0950125098376374, 0.2816035507792589, 0.4580167776572274, 0.6178762444026438,
          0.7554044083550030, 0.8656312023878318, 0.9445750230732326, 0.9894009349916499)
  ws <- c(0.1894506104550685, 0.1826034150449236, 0.1691565193950025, 0.1495959888165767,
          0.1246289712555339, 0.0951585116824928, 0.0622535239386479, 0.0271524594117541)
  h <- (b - a) / n
  s <- 0
  for (i in seq_len(n)) {
    cc <- a + (i - 0.5) * h
    s <- s + sum(ws * (f(cc + xs * h / 2) + f(cc - xs * h / 2)))
  }
  s * h / 2
}

.dist_bisect <- function(cdf, p, lo, hi, tol = 1e-14) {
  if (p <= 0 || p >= 1) stop("p must be in (0, 1)", call. = FALSE)
  a <- lo
  b <- hi
  if (is.infinite(a)) {
    a <- -1
    while (cdf(a) > p) a <- a * 2
  }
  if (is.infinite(b)) {
    b <- if (a < 1) 1 else 2 * a
    while (cdf(b) < p) b <- if (b > 0) b * 2 else b + 1
  }
  for (i in 1:300) {
    m <- (a + b) / 2
    if (cdf(m) < p) a <- m else b <- m
    if (b - a <= tol * (1 + abs(m))) break
  }
  (a + b) / 2
}

#' Laplace distribution
#'
#' f(x) = exp(-|x - loc|/scale) / (2 scale).
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param loc Parameter.
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Johnson, N. L., Kotz, S. & Balakrishnan, N. (1995). Continuous Univariate Distributions, Vol. 2, ch. 24.
#' @examples
#' LaplaceDist(0.5)$cdf
#' @export
LaplaceDist <- function(x = NULL, loc = 0.0, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(scale > 0)) stop("invalid parameters: need scale > 0", call. = FALSE)
  pdf <- function(v) exp(-abs(v - loc) / scale) / (2 * scale)
  cdf <- function(v) ifelse(v < loc, 0.5 * exp((v - loc) / scale), 1 - 0.5 * exp(-(v - loc) / scale))
  qf <- function(u) ifelse(u < 0.5, loc + scale * log(2 * u), loc - scale * log(2 - 2 * u))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Asymmetric Laplace distribution
#'
#' f(x) = (sqrt2/scale) kappa/(1 + kappa^2) exp(-sqrt2 kappa (x - loc)/scale) for x >= loc, exp(-sqrt2 (loc - x)/(scale kappa)) below.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param loc Parameter.
#' @param scale Parameter.
#' @param kappa Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Kotz, S., Kozubowski, T. J. & Podgorski, K. (2001). The Laplace Distribution and Generalizations. Birkhauser, ch. 3.
#' @examples
#' AsyLaplace(0.5)$cdf
#' @export
AsyLaplace <- function(x = NULL, loc = 0.0, scale = 1.0, kappa = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(scale > 0 && kappa > 0)) stop("invalid parameters: need scale > 0 && kappa > 0", call. = FALSE)
  pdf <- function(v) (sqrt(2) / scale) * kappa / (1 + kappa^2) * ifelse(v >= loc, exp(-sqrt(2) * kappa / scale * (v - loc)), exp(-sqrt(2) / (scale * kappa) * (loc - v)))
  cdf <- function(v) ifelse(v < loc, kappa^2 / (1 + kappa^2) * exp(-sqrt(2) / (scale * kappa) * (loc - v)), 1 - exp(-sqrt(2) * kappa / scale * (v - loc)) / (1 + kappa^2))
  qf <- function(u) ifelse(u <= kappa^2 / (1 + kappa^2), loc + scale * kappa / sqrt(2) * log(u * (1 + kappa^2) / kappa^2), loc - scale / (sqrt(2) * kappa) * log((1 - u) * (1 + kappa^2)))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Burr (type XII) distribution
#'
#' F(x) = 1 - (1 + (x/scale)^shape2)^(-shape1), x > 0.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param shape1 Parameter (required).
#' @param shape2 Parameter (required).
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Burr, I. W. (1942). Cumulative frequency functions. Annals of Mathematical Statistics 13, 215-232.
#' @examples
#' BurrDist(0.5, shape1 = 2, shape2 = 2)$cdf
#' @export
BurrDist <- function(x = NULL, shape1 = NULL, shape2 = NULL, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (is.null(shape1) || is.null(shape2)) stop("shape1, shape2 required", call. = FALSE)
  if (!(shape1 > 0 && shape2 > 0 && scale > 0)) stop("invalid parameters: need shape1 > 0 && shape2 > 0 && scale > 0", call. = FALSE)
  pdf <- function(v) ifelse(v <= 0, 0, shape1 * shape2 * (v / scale)^shape2 / (v * (1 + (v / scale)^shape2)^(shape1 + 1)))
  cdf <- function(v) ifelse(v <= 0, 0, 1 - (1 + (v / scale)^shape2)^(-shape1))
  qf <- function(u) scale * ((1 - u)^(-1 / shape1) - 1)^(1 / shape2)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Dagum distribution
#'
#' F(x) = (1 + (x/scale)^(-a))^(-p), x > 0.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param shape1a Parameter (required).
#' @param shape2p Parameter (required).
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Dagum, C. (1977). A new model of personal income distribution. Economie Appliquee 30, 413-437.
#' @examples
#' DagumDist(0.5, shape1a = 2, shape2p = 2)$cdf
#' @export
DagumDist <- function(x = NULL, shape1a = NULL, shape2p = NULL, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (is.null(shape1a) || is.null(shape2p)) stop("shape1a, shape2p required", call. = FALSE)
  if (!(shape1a > 0 && shape2p > 0 && scale > 0)) stop("invalid parameters: need shape1a > 0 && shape2p > 0 && scale > 0", call. = FALSE)
  pdf <- function(v) ifelse(v <= 0, 0, shape1a * shape2p * v^(shape1a * shape2p - 1) / (scale^(shape1a * shape2p) * (1 + (v / scale)^shape1a)^(shape2p + 1)))
  cdf <- function(v) ifelse(v <= 0, 0, (1 + (v / scale)^(-shape1a))^(-shape2p))
  qf <- function(u) scale * (u^(-1 / shape2p) - 1)^(-1 / shape1a)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Fisk (log-logistic) distribution
#'
#' F(x) = 1 / (1 + (x/scale)^(-shape)), x > 0.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param shape Parameter (required).
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Fisk, P. R. (1961). The graduation of income distributions. Econometrica 29, 171-185.
#' @examples
#' FiskDist(0.5, shape = 2)$cdf
#' @export
FiskDist <- function(x = NULL, shape = NULL, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (is.null(shape)) stop("shape required", call. = FALSE)
  if (!(shape > 0 && scale > 0)) stop("invalid parameters: need shape > 0 && scale > 0", call. = FALSE)
  pdf <- function(v) ifelse(v <= 0, 0, (shape / scale) * (v / scale)^(shape - 1) / (1 + (v / scale)^shape)^2)
  cdf <- function(v) ifelse(v <= 0, 0, 1 / (1 + (v / scale)^(-shape)))
  qf <- function(u) scale * (u / (1 - u))^(1 / shape)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Frechet distribution
#'
#' F(x) = exp(-((x - loc)/scale)^(-shape)), x > loc.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param shape Parameter (required).
#' @param loc Parameter.
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Frechet, M. (1927). Sur la loi de probabilite de l'ecart maximum. Ann. Soc. Polon. Math. 6, 93-116.
#' @examples
#' FrechetDist(0.5, shape = 2)$cdf
#' @export
FrechetDist <- function(x = NULL, shape = NULL, loc = 0.0, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (is.null(shape)) stop("shape required", call. = FALSE)
  if (!(shape > 0 && scale > 0)) stop("invalid parameters: need shape > 0 && scale > 0", call. = FALSE)
  pdf <- function(v) ifelse(v <= loc, 0, shape / scale * ((v - loc) / scale)^(-1 - shape) * exp(-((v - loc) / scale)^(-shape)))
  cdf <- function(v) ifelse(v <= loc, 0, exp(-((v - loc) / scale)^(-shape)))
  qf <- function(u) loc + scale * (-log(u))^(-1 / shape)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Gumbel (type I extreme value) distribution
#'
#' F(x) = exp(-exp(-(x - loc)/scale)).
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param loc Parameter.
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Gumbel, E. J. (1958). Statistics of Extremes. Columbia University Press.
#' @examples
#' GumbelDist(0.5)$cdf
#' @export
GumbelDist <- function(x = NULL, loc = 0.0, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(scale > 0)) stop("invalid parameters: need scale > 0", call. = FALSE)
  pdf <- function(v) exp(-(v - loc) / scale - exp(-(v - loc) / scale)) / scale
  cdf <- function(v) exp(-exp(-(v - loc) / scale))
  qf <- function(u) loc - scale * log(-log(u))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Rayleigh distribution
#'
#' F(x) = 1 - exp(-x^2/(2 sigma^2)), x >= 0.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param sigma Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Rayleigh, Lord (1880). On the resultant of a large number of vibrations. Phil. Mag. 10, 73-78.
#' @examples
#' RayleighDist(0.5)$cdf
#' @export
RayleighDist <- function(x = NULL, sigma = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(sigma > 0)) stop("invalid parameters: need sigma > 0", call. = FALSE)
  pdf <- function(v) ifelse(v < 0, 0, v / sigma^2 * exp(-v^2 / (2 * sigma^2)))
  cdf <- function(v) ifelse(v < 0, 0, 1 - exp(-v^2 / (2 * sigma^2)))
  qf <- function(u) sigma * sqrt(-2 * log(1 - u))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Power function distribution
#'
#' F(x) = (x/alpha)^beta, 0 < x < alpha.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param alpha Parameter.
#' @param beta Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Johnson, Kotz & Balakrishnan (1995). Continuous Univariate Distributions, Vol. 2, ch. 25.
#' @examples
#' PowerDist(0.5)$cdf
#' @export
PowerDist <- function(x = NULL, alpha = 1.0, beta = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(alpha > 0 && beta > 0)) stop("invalid parameters: need alpha > 0 && beta > 0", call. = FALSE)
  pdf <- function(v) ifelse(v > 0 & v < alpha, beta * v^(beta - 1) / alpha^beta, 0)
  cdf <- function(v) ifelse(v <= 0, 0, ifelse(v >= alpha, 1, (v / alpha)^beta))
  qf <- function(u) alpha * u^(1 / beta)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Nakagami distribution
#'
#' f(x) = 2 m^m x^(2m-1) exp(-m x^2/Omega) / (Gamma(m) Omega^m); F(x) = P(m, m x^2/Omega).
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param shape Parameter (required).
#' @param scale Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Nakagami, M. (1960). The m-distribution. In Statistical Methods in Radio Wave Propagation, 3-36.
#' @examples
#' Nakagami(0.5, shape = 2)$cdf
#' @export
Nakagami <- function(x = NULL, shape = NULL, scale = 1.0, p = NULL, n = 0, seed = 0) {
  if (is.null(shape)) stop("shape required", call. = FALSE)
  if (!(shape >= 0.5 && scale > 0)) stop("invalid parameters: need shape >= 0.5 && scale > 0", call. = FALSE)
  pdf <- function(v) ifelse(v <= 0, 0, exp(log(2) + shape * log(shape) - lgamma(shape) - shape * log(scale) + (2 * shape - 1) * log(pmax(v, 1e-300)) - shape * v^2 / scale))
  cdf <- function(v) ifelse(v <= 0, 0, pgamma(shape * v^2 / scale, shape))
  qf <- function(u) sqrt(scale * stats::qgamma(u, shape) / shape)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Three-parameter Weibull distribution
#'
#' F(x) = 1 - exp(-((x - loc)/scale)^shape), x > loc.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param shape Parameter (required).
#' @param scale Parameter.
#' @param loc Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Weibull, W. (1951). A statistical distribution function of wide applicability. J. Appl. Mech. 18, 293-297.
#' @examples
#' Weibull3(0.5, shape = 2)$cdf
#' @export
Weibull3 <- function(x = NULL, shape = NULL, scale = 1.0, loc = 0.0, p = NULL, n = 0, seed = 0) {
  if (is.null(shape)) stop("shape required", call. = FALSE)
  if (!(shape > 0 && scale > 0)) stop("invalid parameters: need shape > 0 && scale > 0", call. = FALSE)
  pdf <- function(v) ifelse(v <= loc, 0, shape / scale * ((v - loc) / scale)^(shape - 1) * exp(-((v - loc) / scale)^shape))
  cdf <- function(v) ifelse(v <= loc, 0, 1 - exp(-((v - loc) / scale)^shape))
  qf <- function(u) loc + scale * (-log(1 - u))^(1 / shape)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Johnson SU distribution
#'
#' Z = gamma + delta asinh((x - xi)/lam) ~ N(0, 1).
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param gamma Parameter.
#' @param delta Parameter.
#' @param xi Parameter.
#' @param lam Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Johnson, N. L. (1949). Systems of frequency curves generated by methods of translation. Biometrika 36, 149-176.
#' @examples
#' JohnsonSU(0.5)$cdf
#' @export
JohnsonSU <- function(x = NULL, gamma = 0.0, delta = 1.0, xi = 0.0, lam = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(delta > 0 && lam > 0)) stop("invalid parameters: need delta > 0 && lam > 0", call. = FALSE)
  pdf <- function(v) delta / (lam * sqrt(2 * pi) * sqrt(1 + ((v - xi) / lam)^2)) * exp(-0.5 * (gamma + delta * asinh((v - xi) / lam))^2)
  cdf <- function(v) pnorm(gamma + delta * asinh((v - xi) / lam))
  qf <- function(u) xi + lam * sinh((qnorm(u) - gamma) / delta)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Johnson SB distribution
#'
#' Z = gamma + delta log(y/(1 - y)), y = (x - xi)/lam in (0, 1), Z ~ N(0, 1).
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param gamma Parameter.
#' @param delta Parameter.
#' @param xi Parameter.
#' @param lam Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Johnson, N. L. (1949). Systems of frequency curves generated by methods of translation. Biometrika 36, 149-176.
#' @examples
#' JohnsonSB(0.5)$cdf
#' @export
JohnsonSB <- function(x = NULL, gamma = 0.0, delta = 1.0, xi = 0.0, lam = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(delta > 0 && lam > 0)) stop("invalid parameters: need delta > 0 && lam > 0", call. = FALSE)
  pdf <- function(v) ifelse(v > xi & v < xi + lam, delta / (lam * sqrt(2 * pi) * ((v - xi) / lam) * (1 - (v - xi) / lam)) * exp(-0.5 * (gamma + delta * log(((v - xi) / lam) / (1 - (v - xi) / lam)))^2), 0)
  cdf <- function(v) ifelse(v <= xi, 0, ifelse(v >= xi + lam, 1, pnorm(gamma + delta * log(((v - xi) / lam) / (1 - (v - xi) / lam)))))
  qf <- function(u) xi + lam / (1 + exp(-(qnorm(u) - gamma) / delta))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Sinh-arcsinh distribution
#'
#' Z = sinh(delta asinh((x - loc)/scale) - eps) ~ N(0, 1).
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param loc Parameter.
#' @param scale Parameter.
#' @param eps Parameter.
#' @param delta Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Jones, M. C. & Pewsey, A. (2009). Sinh-arcsinh distributions. Biometrika 96, 761-780.
#' @examples
#' SinhArcsinh(0.5)$cdf
#' @export
SinhArcsinh <- function(x = NULL, loc = 0.0, scale = 1.0, eps = 0.0, delta = 1.0, p = NULL, n = 0, seed = 0) {
  if (!(scale > 0 && delta > 0)) stop("invalid parameters: need scale > 0 && delta > 0", call. = FALSE)
  pdf <- function(v) delta * cosh(delta * asinh((v - loc) / scale) - eps) / (scale * sqrt(2 * pi) * sqrt(1 + ((v - loc) / scale)^2)) * exp(-0.5 * sinh(delta * asinh((v - loc) / scale) - eps)^2)
  cdf <- function(v) pnorm(sinh(delta * asinh((v - loc) / scale) - eps))
  qf <- function(u) loc + scale * sinh((asinh(qnorm(u)) + eps) / delta)
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Cardioid distribution on the circle
#'
#' f(t) = (1 + 2 rho cos(t - mu)) / (2 pi), 0 <= t < 2 pi, |rho| <= 1/2.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param mu Parameter.
#' @param rho Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Jeffreys, H. (1961). Theory of Probability, 3rd ed.; Mardia, K. V. & Jupp, P. E. (2000). Directional Statistics, Sec 3.5.5.
#' @examples
#' Cardioid(0.5)$cdf
#' @export
Cardioid <- function(x = NULL, mu = 0.0, rho = 0.25, p = NULL, n = 0, seed = 0) {
  if (!(abs(rho) <= 0.5)) stop("invalid parameters: need abs(rho) <= 0.5", call. = FALSE)
  pdf <- function(v) ifelse(v >= 0 & v <= 2 * pi, (1 + 2 * rho * cos(v - mu)) / (2 * pi), 0)
  cdf <- function(v) ifelse(v <= 0, 0, ifelse(v >= 2 * pi, 1, (v + 2 * rho * (sin(v - mu) + sin(mu))) / (2 * pi)))
  qf <- function(u) vapply(u, function(w) .dist_bisect(cdf, w, 0.0, 2 * pi), numeric(1))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Circular uniform distribution
#'
#' f(t) = 1/(2 pi), 0 <= t < 2 pi.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Mardia, K. V. & Jupp, P. E. (2000). Directional Statistics. Wiley, Sec 3.5.3.
#' @examples
#' CircUnif(0.5)$cdf
#' @export
CircUnif <- function(x = NULL, p = NULL, n = 0, seed = 0) {
  if (!(TRUE)) stop("invalid parameters: need TRUE", call. = FALSE)
  pdf <- function(v) ifelse(v >= 0 & v <= 2 * pi, 1 / (2 * pi), 0)
  cdf <- function(v) pmin(1, pmax(0, v / (2 * pi)))
  qf <- function(u) 2 * pi * u
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Wrapped Cauchy distribution
#'
#' f(t) = (1 - rho^2) / (2 pi (1 + rho^2 - 2 rho cos(t - mu))), 0 <= t < 2 pi.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param mu Parameter.
#' @param rho Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Mardia, K. V. & Jupp, P. E. (2000). Directional Statistics. Wiley, Sec 3.5.7.
#' @examples
#' WrapCauchy(0.5)$cdf
#' @export
WrapCauchy <- function(x = NULL, mu = 0.0, rho = 0.5, p = NULL, n = 0, seed = 0) {
  if (!(rho >= 0 && rho < 1)) stop("invalid parameters: need rho >= 0 && rho < 1", call. = FALSE)
  pdf <- function(v) ifelse(v >= 0 & v <= 2 * pi, (1 - rho^2) / (2 * pi * (1 + rho^2 - 2 * rho * cos(v - mu))), 0)
  cdf <- function(v) vapply(v, function(t) if (t <= 0.0) 0 else if (t >= 2 * pi) 1 else min(1, .dist_gl(pdf, 0.0, t)), numeric(1))
  qf <- function(u) vapply(u, function(w) .dist_bisect(cdf, w, 0.0, 2 * pi), numeric(1))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Wrapped normal distribution
#'
#' f(t) = sum_k phi((t - mu + 2 pi k)/sigma)/sigma, sigma^2 = -2 log rho.
#' Density, distribution function, quantile and draws by inversion of the
#' morie Philox uniforms (identical in the Python arm).
#'
#' @param x Evaluation points.
#' @param mu Parameter.
#' @param rho Parameter.
#' @param p Probabilities for the quantile function.
#' @param n Number of random draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Mardia, K. V. & Jupp, P. E. (2000). Directional Statistics. Wiley, Sec 3.5.7; rho = exp(-sigma^2/2).
#' @examples
#' WrapNorm(0.5)$cdf
#' @export
WrapNorm <- function(x = NULL, mu = 0.0, rho = 0.5, p = NULL, n = 0, seed = 0) {
  if (!(rho > 0 && rho < 1)) stop("invalid parameters: need rho > 0 && rho < 1", call. = FALSE)
  sd <- sqrt(-2 * log(rho))
  kmax <- as.integer(10 + 6 * sd)
  pdf <- function(v) ifelse(v >= 0 & v <= 2 * pi, vapply(v, function(t) sum(dnorm((t - mu + 2 * pi * (-kmax:kmax)) / sd)) / sd, numeric(1)), 0)
  cdf <- function(v) ifelse(v <= 0, 0, ifelse(v >= 2 * pi, 1, vapply(v, function(t) sum(pnorm((t - mu + 2 * pi * (-kmax:kmax)) / sd) - pnorm((-mu + 2 * pi * (-kmax:kmax)) / sd)), numeric(1))))
  qf <- function(u) vapply(u, function(w) .dist_bisect(cdf, w, 0.0, 2 * pi), numeric(1))
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

.dist_log_i0 <- function(z) {
  z <- abs(z)
  if (z < 50) return(log(besselI(z, 0, expon.scaled = TRUE)) + z)
  k <- 1:29
  t <- cumprod((2 * k - 1)^2 / (k * 8 * z))
  z - 0.5 * log(2 * pi * z) + log(1 + sum(t[t >= 1e-17]))
}

#' Rice distribution
#'
#' f(x) = (x/sigma^2) exp(-(x^2 + v^2)/(2 sigma^2)) I0(x v/sigma^2); distribution
#' function by quadrature, quantile by bisection, draws by inversion of the
#' morie Philox uniforms.
#'
#' @param x Evaluation points.
#' @param sigma Scale.
#' @param vee Non-centrality.
#' @param p Probabilities.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random).
#' @references Rice, S. O. (1945). Bell System Technical Journal 24, 46-156.
#' @examples
#' RiceDist(1, sigma = 1, vee = 0)$cdf
#' @export
RiceDist <- function(x = NULL, sigma = 1, vee = 0, p = NULL, n = 0, seed = 0) {
  if (!(sigma > 0 && vee >= 0)) stop("need sigma > 0 and vee >= 0", call. = FALSE)
  s2 <- sigma^2
  logpdf <- function(v) vapply(v, function(t) if (t <= 0) -Inf else log(t) - log(s2) - (t^2 + vee^2) / (2 * s2) + .dist_log_i0(t * vee / s2), numeric(1))
  pdf <- function(v) ifelse(v > 0, exp(logpdf(v)), 0)
  cdf <- function(v) vapply(v, function(t) if (t <= 0) 0 else min(1, .dist_gl(pdf, 0, t)), numeric(1))
  qf <- function(u) vapply(u, function(w) .dist_bisect(cdf, w, 0, Inf), numeric(1))
  res <- list()
  if (!is.null(x)) {
    res$pdf <- pdf(x)
    res$logpdf <- logpdf(x)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

.dist_u_of_x <- function(Q, x, lo, hi) {
  if (x <= lo) return(0)
  if (x >= hi) return(1)
  a <- 0
  b <- 1
  for (i in 1:200) {
    m <- (a + b) / 2
    if (Q(m) < x) a <- m else b <- m
    if (b - a < 1e-16) break
  }
  (a + b) / 2
}

#' Generalized lambda distribution (Ramberg-Schmeiser)
#'
#' Q(u) = l1 + (u^l3 - (1 - u)^l4)/l2 with density 1/Q'(u).
#'
#' @param x Evaluation points.
#' @param l1,l2,l3,l4 Parameters.
#' @param p Probabilities.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random, support).
#' @references Ramberg, J. S. & Schmeiser, B. W. (1974). Communications of the ACM 17, 78-82.
#' @examples
#' GenLambda(p = 0.5, l1 = 1, l2 = 0.2, l3 = 0.13, l4 = 0.13)$quantile
#' @export
GenLambda <- function(x = NULL, l1 = 0, l2 = 1, l3 = 0.1, l4 = 0.1, p = NULL, n = 0, seed = 0) {
  if (l2 == 0) stop("l2 must be non-zero", call. = FALSE)
  Q <- function(u) l1 + (u^l3 - (1 - u)^l4) / l2
  dQ <- function(u) (l3 * u^(l3 - 1) + l4 * (1 - u)^(l4 - 1)) / l2
  if (!all(dQ(c(0.001, 0.25, 0.5, 0.75, 0.999)) > 0)) stop("invalid parameters: Q'(u) must be positive on (0, 1)", call. = FALSE)
  lo <- if (l3 > 0) Q(0) else -Inf
  hi <- if (l4 > 0) Q(1) else Inf
  cdf <- function(v) vapply(v, function(t) .dist_u_of_x(Q, t, lo, hi), numeric(1))
  pdf <- function(v) ifelse(v <= lo | v >= hi, 0, 1 / dQ(cdf(v)))
  res <- list(support = c(lo, hi))
  if (!is.null(x)) {
    d <- pdf(x)
    res$pdf <- d
    res$logpdf <- ifelse(d > 0, log(d), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- Q(p)
  if (n > 0) res$random <- Q(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Tukey lambda distribution
#'
#' Q(u) = (u^lam - (1 - u)^lam)/lam, logistic at lam = 0.
#'
#' @param x Evaluation points.
#' @param lam Shape.
#' @param p Probabilities.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(pdf, logpdf, cdf, quantile, random, support).
#' @references Joiner, B. L. & Rosenblatt, J. R. (1971). JASA 66, 394-399.
#' @examples
#' TukeyLambda(0.25, lam = 1)$cdf
#' @export
TukeyLambda <- function(x = NULL, lam = 0.14, p = NULL, n = 0, seed = 0) {
  if (lam == 0) {
    Q <- function(u) log(u / (1 - u))
    dQ <- function(u) 1 / (u * (1 - u))
  } else {
    Q <- function(u) (u^lam - (1 - u)^lam) / lam
    dQ <- function(u) u^(lam - 1) + (1 - u)^(lam - 1)
  }
  lo <- if (lam > 0) -1 / lam else -Inf
  hi <- if (lam > 0) 1 / lam else Inf
  cdf <- function(v) vapply(v, function(t) .dist_u_of_x(Q, t, lo, hi), numeric(1))
  pdf <- function(v) ifelse(v <= lo | v >= hi, 0, 1 / dQ(cdf(v)))
  res <- list(support = c(lo, hi))
  if (!is.null(x)) {
    d <- pdf(x)
    res$pdf <- d
    res$logpdf <- ifelse(d > 0, log(d), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- Q(p)
  if (n > 0) res$random <- Q(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Ordered logistic distribution
#'
#' P(Y = j) = F(c_j - eta) - F(c_(j-1) - eta) for categories 0..J-1.
#'
#' @param k Categories (0-based) at which to evaluate.
#' @param eta Linear predictor.
#' @param cuts Increasing cut points.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(probs, pmf, cdf, random).
#' @references McCullagh, P. (1980). JRSS B 42, 109-142.
#' @examples
#' OrderedLogis(eta = 0.3, cuts = c(-1, 0.5, 2))$probs
#' @export
OrderedLogis <- function(k = NULL, eta = 0, cuts = c(-1, 1), n = 0, seed = 0) {
  if (!length(cuts) || any(diff(cuts) <= 0)) stop("cuts must be non-empty and strictly increasing", call. = FALSE)
  cum <- c(plogis(cuts - eta), 1)
  probs <- c(cum[1], diff(cum))
  res <- list(probs = probs)
  if (!is.null(k)) {
    if (any(k < 0 | k >= length(probs))) stop("categories must be in 0..J-1", call. = FALSE)
    res$pmf <- probs[k + 1]
    res$cdf <- cum[k + 1]
  }
  if (n > 0) res$random <- vapply(.morie_random_uniform(n, seed = seed, stream = 0), function(u) which(u <= cum)[1] - 1, numeric(1))
  res
}

#' Categorical distribution
#'
#' @param k Categories (0-based) at which to evaluate.
#' @param probs Non-negative weights (normalised).
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return list(probs, mean, var, pmf, cdf, random).
#' @references Johnson, Kemp & Kotz (2005). Univariate Discrete Distributions, 3rd ed., ch. 10.
#' @examples
#' CategoricalDist(1, probs = c(1, 3))$pmf
#' @export
CategoricalDist <- function(k = NULL, probs = c(0.5, 0.5), n = 0, seed = 0) {
  if (!length(probs) || min(probs) < 0 || sum(probs) <= 0) stop("probs must be non-negative with a positive sum", call. = FALSE)
  pr <- probs / sum(probs)
  cum <- cumsum(pr)
  cum[length(cum)] <- 1
  j <- seq_along(pr) - 1
  m <- sum(j * pr)
  res <- list(probs = pr, mean = m, var = sum((j - m)^2 * pr))
  if (!is.null(k)) {
    if (any(k < 0 | k >= length(pr))) stop("categories must be in 0..K-1", call. = FALSE)
    res$pmf <- pr[k + 1]
    res$cdf <- cum[k + 1]
  }
  if (n > 0) res$random <- vapply(.morie_random_uniform(n, seed = seed, stream = 0), function(u) which(u <= cum)[1] - 1, numeric(1))
  res
}
