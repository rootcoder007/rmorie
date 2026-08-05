# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian process classification by the Laplace approximation
#'
#' With the probit likelihood, Newton's method on
#' Psi(f) = log p(y|f) - 0.5 f' K^{-1} f gives the posterior mode; the
#' averaged predictive probability is Phi(mu*/sqrt(1 + var*)).  The mode
#' is where the gradient vanishes, and the tests check that directly by
#' differentiating Psi numerically rather than trusting the iteration.
#'
#' Formula: f <- (I + K W)^{-1} K (W f + grad log p(y|f));
#'   p* = Phi(mu* / sqrt(1 + var*)).
#'
#' @param X Training inputs, one row per point.
#' @param y Binary labels, 0 or 1.
#' @param X_test Test inputs; the training inputs by default.
#' @param kernel Unused placeholder kept for the interface.
#' @param lengthscale,variance RBF kernel hyperparameters.
#' @param iters Newton iterations.
#' @return List with \code{estimate}, \code{p}, \code{predicted},
#'   \code{f_mode}, \code{latent_mean}, \code{latent_var},
#'   \code{objective}, \code{n}, \code{method}.
#' @references Williams and Barber (1998), Bayesian classification with
#'   Gaussian processes, IEEE TPAMI 20(12):1342-1351,
#'   \doi{10.1109/34.735807}; Rasmussen and Williams (2006), ch. 3,
#'   Algorithm 3.1 and eq. (3.82).
#' @export
Gpcla <- function(X, y, X_test = NULL, kernel = NULL, lengthscale = 1,
                  variance = 1, iters = 40) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("gp_classification: X is empty")
  yv <- as.integer(.s03vec(y))
  if (length(yv) != n) stop("gp_classification: X and y have different lengths")
  if (any(!(yv %in% c(0L, 1L)))) stop("gp_classification: labels must be 0 or 1")
  ell <- as.numeric(lengthscale); var <- as.numeric(variance)
  if (ell <= 0 || var <= 0) stop("gp_classification: lengthscale and variance must be positive")
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  kf <- function(P, Q) {
    out <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      out[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    out
  }
  dphi <- function(z) exp(-0.5 * z * z) / sqrt(2 * pi)
  gh <- function(f) {
    s <- ifelse(yv == 1L, 1, -1)
    z <- s * f
    P <- pmax(vapply(z, .s03pnorm, 0), 1e-300)
    r <- vapply(z, dphi, 0) / P
    list(g = s * r, w = r * r + z * r)
  }
  K <- kf(A, A)
  f <- rep(0, n)
  obj <- numeric(0)
  for (it in seq_len(as.integer(iters))) {
    gw <- gh(f)
    B <- K %*% diag(gw$w, n) + diag(1, n)
    rhs <- as.numeric(K %*% (gw$w * f + gw$g))
    f <- as.numeric(solve(B, rhs))
    s <- ifelse(yv == 1L, 1, -1)
    ll <- sum(log(pmax(vapply(s * f, .s03pnorm, 0), 1e-300)))
    a <- .s03cholsolve(K, f)
    obj <- c(obj, ll - 0.5 * sum(f * a))
  }
  gw <- gh(f)
  alpha <- .s03cholsolve(K, f)
  Ks <- kf(Xs, A)
  mu <- as.numeric(Ks %*% alpha)
  M <- K + diag(ifelse(gw$w > 0, 1 / gw$w, 1e12), n)
  sd <- numeric(nrow(Xs))
  for (j in seq_len(nrow(Xs))) {
    v <- .s03cholsolve(M, Ks[j, ])
    sd[j] <- max(var - sum(Ks[j, ] * v), 0)
  }
  p <- vapply(mu / sqrt(1 + sd), .s03pnorm, 0)
  .t1_result(estimate = p[1], p = p, predicted = as.integer(p >= 0.5), f_mode = f,
             latent_mean = mu, latent_var = sd, objective = obj, n = n,
             method = "Newton mode of Psi(f) with probit likelihood; averaged prediction R&W eq. (3.82)")
}
