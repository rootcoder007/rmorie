# SPDX-License-Identifier: AGPL-3.0-or-later

## Dormand-Prince 5(4) coefficients (the RK45 of scipy's solve_ivp)
.morie_dp5 <- list(
  c = c(0, 1 / 5, 3 / 10, 4 / 5, 8 / 9, 1, 1),
  a = list(
    numeric(0),
    c(1 / 5),
    c(3 / 40, 9 / 40),
    c(44 / 45, -56 / 15, 32 / 9),
    c(19372 / 6561, -25360 / 2187, 64448 / 6561, -212 / 729),
    c(9017 / 3168, -355 / 33, 46732 / 5247, 49 / 176, -5103 / 18656),
    c(35 / 384, 0, 500 / 1113, 125 / 192, -2187 / 6784, 11 / 84)
  ),
  b = c(35 / 384, 0, 500 / 1113, 125 / 192, -2187 / 6784, 11 / 84, 0),
  e = c(71 / 57600, 0, -71 / 16695, 71 / 1920, -17253 / 339200, 22 / 525, -1 / 40)
)

## adaptive Dormand-Prince integration that lands exactly on every
## requested time, error-controlled per component by atol + rtol |y|
.morie_rk45 <- function(f, t_eval, y0, rtol = 1e-10, atol = 1e-12) {
  tab <- .morie_dp5
  out <- matrix(NA_real_, length(t_eval), length(y0))
  out[1L, ] <- y0
  t <- t_eval[1L]
  y <- y0
  h <- (t_eval[length(t_eval)] - t) / 100
  for (k in seq_along(t_eval)[-1L]) {
    target <- t_eval[k]
    while (t < target - 1e-15 * max(1, abs(target))) {
      h <- min(h, target - t)
      K <- matrix(0, 7L, length(y))
      K[1L, ] <- f(t, y)
      for (s in 2:7) {
        yy <- y + h * colSums(tab$a[[s]] * K[seq_len(s - 1L), , drop = FALSE])
        K[s, ] <- f(t + tab$c[s] * h, yy)
      }
      ynew <- y + h * colSums(tab$b * K)
      err <- h * colSums(tab$e * K)
      sc <- atol + rtol * pmax(abs(y), abs(ynew))
      en <- sqrt(mean((err / sc)^2))
      if (en <= 1) {
        t <- t + h
        y <- ynew
      }
      h <- h * min(5, max(0.2, 0.9 * en^(-1 / 5)))
    }
    out[k, ] <- y
  }
  out
}

#' Geodesics of a four-dimensional metric
#'
#' Integrates the geodesic equation
#' d^2 x^mu / d tau^2 + Gamma^mu_ab (dx^a / d tau)(dx^b / d tau) = 0 with
#' Gamma^l_mn = 1/2 g^ls (d_m g_sn + d_n g_sm - d_s g_mn), the metric
#' derivatives taken by central differences of step \code{h}, by an
#' adaptive Dormand-Prince 5(4) scheme at rtol 1e-10, atol 1e-12.
#'
#' @param metric_func Function of a length-4 position returning the 4 x 4
#'   metric.
#' @param x0 Initial position (length 4).
#' @param u0 Initial velocity (length 4).
#' @param tau_span Proper-time interval.
#' @param n_points Number of output points.
#' @param h Finite-difference step for the metric derivatives.
#' @return List with \code{tau}, \code{position} (n x 4), \code{velocity}
#'   (n x 4).
#' @examples
#' flat <- function(x) diag(c(-1, 1, 1, 1))
#' r <- morie_geods(flat, c(0, 0, 0, 0), c(1, 0.5, 0, 0), c(0, 2), 5)
#' r$position[5, ]
#' @export
morie_geods <- function(metric_func, x0, u0, tau_span = c(0, 10), n_points = 500L, h = 1e-5) {
  x0 <- as.numeric(x0)
  u0 <- as.numeric(u0)
  if (length(x0) != 4L || length(u0) != 4L) stop("x0 and u0 must be length-4.", call. = FALSE)
  christoffel <- function(x) {
    ginv <- solve(metric_func(x))
    dg <- array(0, c(4L, 4L, 4L))
    for (mu in 1:4) {
      dx <- numeric(4)
      dx[mu] <- h
      dg[mu, , ] <- (metric_func(x + dx) - metric_func(x - dx)) / (2 * h)
    }
    G <- array(0, c(4L, 4L, 4L))
    for (l in 1:4) for (m in 1:4) for (n in 1:4) {
      G[l, m, n] <- 0.5 * sum(ginv[l, ] * (dg[m, , n] + dg[n, , m] - dg[, m, n]))
    }
    G
  }
  rhs <- function(tau, y) {
    u <- y[5:8]
    G <- christoffel(y[1:4])
    acc <- vapply(1:4, function(mu) -sum(G[mu, , ] * outer(u, u)), numeric(1))
    c(u, acc)
  }
  tau <- seq(tau_span[1L], tau_span[2L], length.out = n_points)
  Y <- .morie_rk45(rhs, tau, c(x0, u0))
  list(tau = tau, position = Y[, 1:4, drop = FALSE], velocity = Y[, 5:8, drop = FALSE])
}
