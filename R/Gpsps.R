# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spectral mixture kernel GP
#'
#' Modelling the spectral density as a mixture of Gaussians and
#' inverting Bochner's theorem gives the kernel below.  The stub this
#' function replaces printed the exponent as exp(-tau^2 v), dropping the
#' 2 pi^2; the arXiv PDF was checked and the factor is there.  It
#' matters: without it the mu = 0 case is not a squared exponential with
#' lengthscale 1/(2 pi sqrt(v)), and that mapping is the anchor the
#' tests use.
#'
#' Formula: k(tau) = sum_q w_q exp(-2 pi^2 tau^2 v_q) cos(2 pi tau mu_q).
#'
#' @param X One-dimensional training inputs.
#' @param y Training responses.
#' @param X_test Test inputs; the training inputs by default.
#' @param Q Number of mixture components.
#' @param weights,variances,means Length-Q mixture parameters.
#' @param noise Observation noise variance.
#' @return List with \code{estimate}, \code{mean}, \code{variance},
#'   \code{k_zero}, \code{loglik}, \code{n}, \code{method}.
#' @references Wilson and Adams (2013), Gaussian process kernels for
#'   pattern discovery and extrapolation, ICML 2013, PMLR
#'   28(3):1067-1075. arXiv:1302.4245
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Gpsps(V, V)
Gpsps <- function(X, y, X_test = NULL, Q = 1, weights = NULL, variances = NULL,
                  means = NULL, noise = 0.01) {
  xs <- .s03vec(X)
  yv <- .s03vec(y)
  n <- length(xs)
  if (n == 0L) stop("gp_spectral_mixture: X is empty")
  if (length(yv) != n) stop("gp_spectral_mixture: X and y have different lengths")
  q <- as.integer(Q)
  if (q < 1L) stop("gp_spectral_mixture: Q must be at least 1")
  w <- if (is.null(weights)) rep(1 / q, q) else .s03vec(weights)
  v <- if (is.null(variances)) rep(1 / (2 * pi)^2, q) else .s03vec(variances)
  m <- if (is.null(means)) 0.1 * seq_len(q) else .s03vec(means)
  if (length(w) != q || length(v) != q || length(m) != q)
    stop("gp_spectral_mixture: weights, variances and means must each have Q entries")
  if (any(c(w, v) <= 0)) stop("gp_spectral_mixture: weights and variances must be positive")
  s2 <- as.numeric(noise)
  if (s2 < 0) stop("gp_spectral_mixture: noise must be non-negative")
  xt <- if (is.null(X_test)) xs else .s03vec(X_test)
  sm <- function(t) sum(w * exp(-2 * pi^2 * t * t * v) * cos(2 * pi * t * m))
  K <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) K[i, j] <- sm(xs[i] - xs[j])
  K <- K + diag(s2, n)
  alpha <- .s03cholsolve(K, yv)
  Ks <- matrix(0, length(xt), n)
  for (j in seq_along(xt)) for (i in seq_len(n)) Ks[j, i] <- sm(xt[j] - xs[i])
  mu <- as.numeric(Ks %*% alpha)
  k0 <- sm(0)
  sd <- numeric(length(xt))
  for (j in seq_along(xt)) {
    z <- .s03cholsolve(K, Ks[j, ])
    sd[j] <- max(k0 - sum(Ks[j, ] * z), 0)
  }
  L <- .s03chol(K)
  ll <- -0.5 * sum(yv * alpha) - sum(log(diag(L))) - 0.5 * n * log(2 * pi)
  .t1_result(estimate = mu[1], mean = mu, variance = sd, k_zero = k0,
             loglik = ll, n = n,
             method = "k(tau) = sum_q w_q exp(-2 pi^2 tau^2 v_q) cos(2 pi tau mu_q), Wilson & Adams (2013)")
}
