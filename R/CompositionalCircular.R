.cd_closure <- function(x) {
  if (any(x <= 0)) stop("compositions must be strictly positive")
  x / sum(x)
}

.cd_clr <- function(x) {
  l <- log(.cd_closure(x))
  l - mean(l)
}

#' Compositional summaries, biplot and Dirichlet models
#'
#' \code{AitchisonClrCovariance}: clr covariance, total variance, variation
#' matrix and centre (Aitchison 1986). \code{CompositionalMad}: MAD of each
#' clr coordinate. \code{CompositionalPielou}: Pielou (1966) evenness of a
#' composition. \code{CompositionalQuantileDist}: Aitchison distance between
#' the part-wise upper and lower quartiles. \code{AitchisonBiplot}: SVD of
#' the centred clr matrix (Aitchison and Greenacre 2002).
#' \code{DirichletFitMom}: method of moments (Minka 2000).
#' \code{DirichletSample}: gamma draws by inversion of Philox uniforms (part
#' i on stream i - 1). Identical to the Python arm \code{morie.fn.compdir}.
#'
#' @param X Sample (rows are compositions).
#' @param x Composition.
#' @param alpha Dirichlet parameters.
#' @param n Number of draws.
#' @param seed Philox seed.
#' @return List, vector, number or matrix.
#' @references Aitchison, J. (1986). The Statistical Analysis of
#'   Compositional Data. Chapman and Hall.
#'
#'   Aitchison, J. and Greenacre, M. (2002). Biplots of compositional data.
#'   Journal of the Royal Statistical Society C 51, 375-392.
#'
#'   Minka, T. P. (2000). Estimating a Dirichlet distribution. Technical
#'   report, MIT.
#' @examples
#' AitchisonClrCovariance(rbind(c(1, 2, 4), c(2, 2, 2), c(4, 2, 1)))$total_variance
#' CompositionalPielou(c(1, 1, 2))
#' DirichletFitMom(rbind(c(1, 2, 7), c(2, 2, 6), c(3, 1, 6)))$alpha
#' DirichletSample(c(2, 3), 1, seed = 4)
#' @export
AitchisonClrCovariance <- function(X) {
  X <- as.matrix(X)
  D <- ncol(X)
  L <- log(t(apply(X, 1, .cd_closure)))
  Tm <- outer(seq_len(D), seq_len(D), Vectorize(function(a, b) stats::var(L[, a] - L[, b])))
  cv <- stats::cov(t(apply(X, 1, .cd_clr)))
  list(clr_cov = unname(cv), total_variance = sum(diag(cv)), variation = unname(Tm),
       center = .cd_closure(exp(colMeans(L))))
}

#' @rdname AitchisonClrCovariance
#' @export
CompositionalMad <- function(X) {
  C <- t(apply(as.matrix(X), 1, .cd_clr))
  unname(apply(C, 2, function(v) stats::median(abs(v - stats::median(v)))))
}

#' @rdname AitchisonClrCovariance
#' @export
CompositionalPielou <- function(x) {
  .morie_arg(x, "n")
  p <- .cd_closure(x)
  -sum(p * log(p)) / log(length(p))
}

#' @rdname AitchisonClrCovariance
#' @export
CompositionalQuantileDist <- function(X) {
  X <- as.matrix(X)
  q1 <- apply(X, 2, stats::quantile, 0.25, names = FALSE)
  q3 <- apply(X, 2, stats::quantile, 0.75, names = FALSE)
  sqrt(sum((.cd_clr(q3) - .cd_clr(q1))^2))
}

#' @rdname AitchisonClrCovariance
#' @export
AitchisonBiplot <- function(X) {
  .morie_arg(X, "m")
  C <- t(apply(as.matrix(X), 1, .cd_clr))
  s <- svd(sweep(C, 2, colMeans(C)))
  for (k in seq_along(s$d)) {
    if (s$v[which.max(abs(s$v[, k])), k] < 0) {
      s$v[, k] <- -s$v[, k]
      s$u[, k] <- -s$u[, k]
    }
  }
  list(singular_values = s$d, scores = s$u %*% diag(s$d, length(s$d)), loadings = s$v,
       explained = s$d^2 / sum(s$d^2))
}

#' @rdname AitchisonClrCovariance
#' @export
DirichletFitMom <- function(X) {
  R <- t(apply(as.matrix(X), 1, .cd_closure))
  m <- colMeans(R)
  s <- m[1] * (1 - m[1]) / stats::var(R[, 1]) - 1
  list(alpha = unname(m * s), precision = unname(s), mean = m)
}

#' @rdname AitchisonClrCovariance
#' @export
DirichletSample <- function(alpha, n, seed = 1L) {
  U <- matrix(vapply(seq_along(alpha), function(i) .morie_random_uniform(n, seed = seed, stream = i - 1), numeric(n)), n)
  G <- matrix(sapply(seq_along(alpha), function(i) stats::qgamma(U[, i], alpha[i], 1)), n)
  G / rowSums(G)
}

.cc_a1inv <- function(r) {
  if (r < 0.53) return(2 * r + r^3 + 5 * r^5 / 6)
  if (r < 0.85) return(-0.4 + 1.39 * r + 0.43 / (1 - r))
  1 / (r^3 - 4 * r^2 + 3 * r)
}

#' Circular summaries and von Mises maximum likelihood (circular package conventions)
#'
#' \code{CircularSummary}: mean direction, mean resultant length, circular
#' variance and standard deviation, Rayleigh test p-value with the
#' small-sample correction (Mardia and Jupp 2000). \code{VonmisesMle}: mean
#' direction and concentration by the Best and Fisher (1981) inverse of
#' \eqn{A_1}, optional bias correction, standard errors and log-likelihood,
#' as \code{circular::mle.vonmises}.
#'
#' @param theta Angles (radians).
#' @param bias Apply the small-sample bias correction.
#' @return List.
#' @references Mardia, K. V. and Jupp, P. E. (2000). Directional Statistics.
#'   Wiley.
#'
#'   Best, D. J. and Fisher, N. I. (1981). The bias of the maximum likelihood
#'   estimators of the von Mises-Fisher concentration parameters.
#'   Communications in Statistics - Simulation and Computation 10, 493-502.
#' @examples
#' CircularSummary(c(0.1, 0.3, 6.2))$rbar
#' VonmisesMle(c(0.1, 0.3, 6.2, 0.05))$kappa
#' @export
CircularSummary <- function(theta) {
  n <- length(theta)
  C <- mean(cos(theta))
  S <- mean(sin(theta))
  R <- sqrt(C^2 + S^2)
  z <- n * R^2
  p <- exp(-z)
  if (n < 50) p <- p * (1 + (2 * z - z^2) / (4 * n) - (24 * z - 132 * z^2 + 76 * z^3 - 9 * z^4) / (288 * n^2))
  list(mean = atan2(S, C) %% (2 * pi), rbar = R, variance = 1 - R, sd = if (R > 0) sqrt(-2 * log(R)) else Inf,
       rayleigh_p = min(max(p, 0), 1))
}

#' @rdname CircularSummary
#' @export
VonmisesMle <- function(theta, bias = FALSE) {
  n <- length(theta)
  mu <- atan2(sum(sin(theta)), sum(cos(theta)))
  V <- mean(cos(theta - mu))
  k <- if (V > 0) .cc_a1inv(V) else 0
  if (bias) k <- if (k < 2) max(k - 2 / (n * k), 0) else (n - 1)^3 * k / (n^3 + n)
  a1 <- besselI(k, 1) / besselI(k, 0)
  list(mu = mu %% (2 * pi), kappa = k, se_mu = if (k > 0) sqrt(1 / (n * k * a1)) else Inf,
       se_kappa = if (k > 0) sqrt(1 / (n * (1 - a1 / k - a1^2))) else NaN,
       loglik = k * sum(cos(theta - mu)) - n * log(2 * pi * besselI(k, 0)))
}
