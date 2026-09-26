# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian kriging with a normal-inverse-gamma prior
#'
#' Schabenberger & Gotway (2005) eqs (6.94)-(6.98), after Kitanidis (1986) and
#' Le & Zidek (1992): with Z ~ G(X beta, sigma2 V), V known, and (beta,
#' sigma2) ~ NIG(a, d, m, Q), Z(s0) | Z is multivariate t with mean X0 m* +
#' V0z V^-1 (Z - X m*) and variance a* / (nu - 2) {V00 - V0z V^-1 Vz0 + (X0 -
#' V0z V^-1 X) Q* (X0 - V0z V^-1 X)'}, where Q* = (Q^-1 + X' V^-1 X)^-1, m* =
#' Q* (Q^-1 m + X' V^-1 Z), a* = a + m' Q^-1 m + Z' V^-1 Z - m*' Q*^-1 m*, and
#' nu = n + d (n + d - p for the flat prior, Qinv = NULL, whose mean is the
#' universal kriging predictor). The printed nu* of (6.98) weights beta_gls - m
#' by X' V^-1 X; the exact a* weights it by (Q + (X' V^-1 X)^-1)^-1.
#'
#' @param X,X0 Design matrices for the data and the prediction sites.
#' @param z Numeric responses.
#' @param V,V0z,V00 Correlation blocks: data, prediction-data, prediction.
#' @param a,d Inverse-gamma hyperparameters.
#' @param m Prior mean of beta (proper prior).
#' @param Qinv Prior precision of beta up to sigma2; NULL is flat.
#' @return Named list: mean, variance, scale, df, a_star, m_star, Q_star.
#' @references Kitanidis, P. K. (1986). WRR 22, 499-507. Le, N. D. & Zidek,
#'   J. V. (1992). JMVA 43, 351-374. Schabenberger & Gotway (2005), eqs
#'   (6.94)-(6.98), pp. 391-394.
#' @examples
#' xy <- cbind(1:6, c(2, 4, 1, 5, 3, 6))
#' v <- exp(-as.matrix(dist(xy)))
#' v0 <- exp(-sqrt(colSums((t(xy) - c(3.5, 3.5))^2)))
#' bkrnig(cbind(1, xy[, 1]), sin(1:6), cbind(1, 3.5), v, matrix(v0, 1), matrix(1))$mean
#' @export
bkrnig <- function(X, z, X0, V, V0z, V00, a = 0, d = 0, m = NULL, Qinv = NULL) {
  X <- as.matrix(X)
  z <- as.numeric(z)
  X0 <- matrix(X0, ncol = ncol(X))
  V0z <- matrix(V0z, ncol = nrow(X))
  V00 <- as.matrix(V00)
  n <- nrow(X)
  p <- ncol(X)
  flat <- is.null(Qinv)
  qi <- if (flat) matrix(0, p, p) else as.matrix(Qinv)
  mm <- if (is.null(m)) rep(0, p) else as.numeric(m)
  vi <- solve(V)
  A <- crossprod(X, vi %*% X)
  qs <- solve(qi + A)
  ms <- drop(qs %*% (qi %*% mm + crossprod(X, vi %*% z)))
  astar <- drop(a + crossprod(mm, qi %*% mm) + crossprod(z, vi %*% z) - crossprod(ms, (qi + A) %*% ms))
  nu <- n + d - (if (flat) p else 0)
  if (nu <= 2) stop("the predictive t needs more than 2 degrees of freedom", call. = FALSE)
  k <- V0z %*% vi
  mean <- drop(X0 %*% ms + k %*% (z - X %*% ms))
  dm <- X0 - k %*% X
  cc <- V00 - k %*% t(V0z) + dm %*% qs %*% t(dm)
  cc <- unname((cc + t(cc)) / 2)
  list(mean = mean, variance = cc * astar / (nu - 2), scale = cc * astar / nu, df = nu,
       a_star = astar, m_star = ms, Q_star = qs)
}

#' Bayesian kriging over the Matern class (Handcock and Stein 1993)
#'
#' Z ~ G(X beta, sigma2 V(theta, nu)) with V the Matern correlation (4.9),
#' prior pi(theta, nu) / sigma2 with bounded uniform priors on the scale theta
#' and smoothness nu (Schabenberger & Gotway 2005, p. 394). Integrating out
#' beta and sigma2 leaves p(theta, nu | Z) proportional to pi(theta, nu)
#' |V|^-1/2 |X' V^-1 X|^-1/2 (S2)^-(n - p)/2; at each grid point the predictive
#' is the flat-prior t of (6.97)-(6.98). There is no closed form over (theta,
#' nu), so the posterior is evaluated on the grids and the mixture's mean and
#' variance are returned.
#'
#' @param coords,coords0 Coordinates of the data and the prediction sites.
#' @param z Numeric responses.
#' @param X,X0 Design matrices.
#' @param theta_grid,nu_grid Grid points of the uniform priors.
#' @param prior_weights Optional matrix of prior quadrature weights.
#' @return Named list: mean, variance, posterior, theta_grid, nu_grid.
#' @references Handcock, M. S. & Stein, M. L. (1993). Technometrics 35,
#'   403-410. Berger, J. O., De Oliveira, V. & Sanso, B. (2001). JASA 96,
#'   1361-1374. Schabenberger & Gotway (2005), p. 394.
#' @examples
#' xy <- cbind(1:8, c(2, 4, 1, 5, 3, 6, 8, 7))
#' hsbkrg(xy, sin(1:8), cbind(1, xy[, 1]), rbind(c(4, 4)), cbind(1, 4), c(0.5, 1, 2), 1.5)$mean
#' @export
hsbkrg <- function(coords, z, X, coords0, X0, theta_grid, nu_grid, prior_weights = NULL) {
  coords <- as.matrix(coords)
  coords0 <- matrix(coords0, ncol = 2)
  X <- as.matrix(X)
  X0 <- matrix(X0, ncol = ncol(X))
  z <- as.numeric(z)
  n <- nrow(X)
  p <- ncol(X)
  if (n - p <= 2) stop("need n - p > 2", call. = FALSE)
  dd <- as.matrix(stats::dist(coords))
  d0 <- sqrt(outer(coords0[, 1], coords[, 1], "-")^2 + outer(coords0[, 2], coords[, 2], "-")^2)
  d00 <- as.matrix(stats::dist(coords0))
  mat <- function(dm, nu, th) matrix(spmatr(as.vector(dm), 1, nu, th)$covariance, nrow(dm))
  pw <- if (is.null(prior_weights)) matrix(1, length(theta_grid), length(nu_grid)) else prior_weights
  logw <- numeric(0)
  means <- list()
  vars <- list()
  for (i in seq_along(theta_grid)) for (j in seq_along(nu_grid)) {
    th <- theta_grid[i]
    nu <- nu_grid[j]
    v <- mat(dd, nu, th)
    f <- bkrnig(X, z, X0, v, mat(d0, nu, th), mat(d00, nu, th))
    ld <- as.numeric(determinant(v)$modulus) + as.numeric(determinant(crossprod(X, solve(v, X)))$modulus)
    logw <- c(logw, log(pw[i, j]) - 0.5 * ld - 0.5 * (n - p) * log(f$a_star))
    means[[length(means) + 1L]] <- f$mean
    vars[[length(vars) + 1L]] <- diag(as.matrix(f$variance))
  }
  w <- exp(logw - max(logw))
  w <- w / sum(w)
  mu <- Reduce(`+`, Map(`*`, means, w))
  ex2 <- Reduce(`+`, Map(function(mm, vv, ww) ww * (vv + mm^2), means, vars, w))
  list(mean = mu, variance = ex2 - mu^2,
       posterior = matrix(w, length(theta_grid), length(nu_grid), byrow = TRUE),
       theta_grid = theta_grid, nu_grid = nu_grid)
}
