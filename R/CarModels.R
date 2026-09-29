.carm_sym <- function(W) {
  W <- unname(as.matrix(W)) * 1
  if (any(W != t(W))) stop("W must be symmetric")
  W
}

.carm_profile <- function(t2, lam, phi) {
  a <- phi / lam + 1 - phi
  m <- length(lam)
  s <- sum(t2 / a)
  ll <- -0.5 * m * (log(2 * pi * s / m) + 1) - 0.5 * sum(log(a))
  ds <- -sum(t2 * (1 / lam - 1) / (a * a))
  score <- -0.5 * m * ds / s - 0.5 * sum((1 / lam - 1) / a)
  list(ll = ll, score = score, s = s / m)
}

#' Gaussian conditional autoregressive (CAR) models
#'
#' `carvar` is the CAR covariance `sigma2 (I - rho W)^-1 diag(m)`, `carjac`
#' the log-likelihood Jacobian term `0.5 log|I - rho W|`, `carsim` draws
#' `x = L'^-1 z` from the precision `(I - rho W)/sigma2 = L L'` with Philox
#' normals (stream `k` of `seed` for draw `k`), `cargmm` the moment
#' (conditional least squares) estimator `e'We / e'W^2 e` of `rho`,
#' `caricar` the intrinsic CAR log-density with the generalised determinant
#' of `D - W`, `carres` Moran's I of CAR residuals (randomisation moments),
#' and `carbym` the REML variance components of the Gaussian
#' Besag-York-Mollie model `y = mu + u + v` (ICAR `u`, iid `v`), solved by
#' bisection on the profile score of the spatial share `phi`.
#'
#' @param W Spatial weights (symmetric adjacency for `carsim`, `caricar`,
#'   `carbym`).
#' @param rho CAR autocorrelation parameter.
#' @param sigma2 Conditional variance scale.
#' @param m Optional vector of conditional variance multipliers.
#' @param nsim Number of simulated fields.
#' @param seed Philox seed.
#' @param y Numeric response.
#' @param X Optional regressor matrix (intercept only when NULL).
#' @param phi Field values for the ICAR density.
#' @param tau ICAR precision.
#' @param tol Relative eigenvalue threshold for the null space.
#' @param resid Residual vector.
#' @param alternative Alternative hypothesis for the Moran test.
#' @return `carvar` a list with `covariance`, `marginal_variance` and
#'   `mean_marginal_variance`; `carjac`, `cargmm`, `caricar` and `carres`
#'   lists with `statistic`; `carsim` a list with the `draws` matrix (one
#'   row per draw); `carbym` a list with `sigma2_spatial`,
#'   `sigma2_unstructured`, `phi`, `mu` and `reml_loglik`.
#' @references Besag, J. (1974). Spatial interaction and the statistical
#'   analysis of lattice systems. Journal of the Royal Statistical Society B
#'   36, 192-236. Besag, J., York, J. and Mollie, A. (1991). Bayesian image
#'   restoration, with two applications in spatial statistics. Annals of the
#'   Institute of Statistical Mathematics 43, 1-20. Rue, H. and Held, L.
#'   (2005). Gaussian Markov Random Fields. Chapman and Hall/CRC. Haining, R.
#'   (1990). Spatial Data Analysis in the Social and Environmental Sciences.
#'   Cambridge University Press.
#' @examples
#' W <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
#' carvar(W, 0.4, 2)$covariance
#' carjac(W, 0.4)$statistic
#' caricar(c(0.5, -0.2, -0.3), W)$statistic
#' @export
carvar <- function(W, rho, sigma2, m = NULL) {
  W <- unname(as.matrix(W)) * 1
  n <- nrow(W)
  if (is.null(m)) m <- rep(1, n)
  S <- sigma2 * solve(diag(n) - rho * W) %*% diag(m, n)
  if (max(abs(S - t(S))) > 1e-8 * max(abs(S))) stop("(I - rho W)^-1 M is not symmetric: not a valid CAR specification")
  list(covariance = S, marginal_variance = diag(S), mean_marginal_variance = sum(diag(S)) / n)
}

#' @rdname carvar
#' @export
carjac <- function(W, rho) {
  W <- unname(as.matrix(W)) * 1
  list(statistic = 0.5 * as.numeric(determinant(diag(nrow(W)) - rho * W, logarithm = TRUE)$modulus), rho = rho)
}

#' @rdname carvar
#' @export
carsim <- function(W, rho, sigma2, nsim = 9, seed = 0) {
  W <- .carm_sym(W)
  n <- nrow(W)
  R <- chol((diag(n) - rho * W) / sigma2)
  draws <- t(vapply(seq_len(nsim) - 1, function(k) {
    backsolve(R, .morie_random_normal(n, seed = seed, stream = k))
  }, numeric(n)))
  list(draws = matrix(draws, nsim, n), nsim = nsim, seed = seed)
}

#' @rdname carvar
#' @export
cargmm <- function(y, W, X = NULL) {
  list(statistic = car_rho_ols(y, W, X), estimator = "conditional least squares moment")
}

#' @rdname carvar
#' @export
caricar <- function(phi, W, tau = 1, tol = 1e-9) {
  W <- .carm_sym(W)
  x <- as.numeric(phi)
  n <- length(x)
  Q <- diag(rowSums(W), n) - W
  ev <- .s03jacobi(Q)$values
  pos <- ev[ev > tol * max(ev)]
  r <- length(pos)
  quad <- 0
  for (i in seq_len(n - 1)) for (j in (i + 1):n) quad <- quad + W[i, j] * (x[i] - x[j])^2
  logp <- -0.5 * r * log(2 * pi) + 0.5 * r * log(tau) + 0.5 * sum(log(pos)) - 0.5 * tau * quad
  list(statistic = logp, rank = r, quadratic_form = quad, tau = tau)
}

#' @rdname carvar
#' @export
carres <- function(resid, W, alternative = "greater") miml(resid, W, alternative = alternative)

#' @rdname carvar
#' @export
carbym <- function(y, W, tol = 1e-9) {
  W <- .carm_sym(W)
  y <- as.numeric(y)
  n <- length(y)
  Q <- diag(rowSums(W), n) - W
  e <- .s03jacobi(Q)
  keep <- which(e$values > tol * max(e$values))
  lam <- e$values[keep]
  t2 <- as.vector(crossprod(e$vectors[, keep, drop = FALSE], y))^2
  if (.carm_profile(t2, lam, 0)$score <= 0) {
    phi <- 0
  } else if (.carm_profile(t2, lam, 1)$score >= 0) {
    phi <- 1
  } else {
    lo <- 0
    hi <- 1
    for (it in 1:200) {
      mid <- 0.5 * (lo + hi)
      if (.carm_profile(t2, lam, mid)$score > 0) lo <- mid else hi <- mid
      if (hi - lo <= 1e-15) break
    }
    phi <- 0.5 * (lo + hi)
  }
  p <- .carm_profile(t2, lam, phi)
  list(sigma2_spatial = p$s * phi, sigma2_unstructured = p$s * (1 - phi), phi = phi, mu = mean(y),
       reml_loglik = p$ll, rank = length(lam))
}
