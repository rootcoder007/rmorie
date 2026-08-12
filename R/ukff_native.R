# Unscented Kalman filter.
# Source: Julier & Uhlmann (1997), A new extension of the Kalman
# filter to nonlinear systems, Proc. SPIE 3068, Eqs. 12-14
# (fetched-wave3/julier-uhlmann-1997-ukf.pdf).  Mirrors Python
# morie.fn.ukfF exactly (same sigma-point construction via Cholesky,
# fresh points before the measurement update, symmetrized P).

.ukf_sigma <- function(x, P, kappa) {
  n <- length(x)
  scale <- n + kappa
  L <- t(chol(scale * (P + t(P)) / 2))
  pts <- matrix(0, 2 * n + 1, n)
  pts[1, ] <- x
  w <- c(kappa / scale, rep(1 / (2 * scale), 2 * n))
  for (i in seq_len(n)) {
    pts[1 + 2 * i - 1, ] <- x + L[, i]
    pts[1 + 2 * i, ] <- x - L[, i]
  }
  list(pts = pts, w = w)
}

.ukf_ut <- function(pts, w, fun) {
  ys <- t(apply(pts, 1, fun))
  if (ncol(ys) == nrow(pts) && nrow(ys) == 1) ys <- t(ys)
  mean <- as.numeric(colSums(ys * w))
  d <- sweep(ys, 2, mean)
  cov <- t(d) %*% (d * w)
  list(ys = ys, mean = mean, cov = cov)
}

#' Unscented Kalman filter (additive noise)
#'
#' Julier-Uhlmann sigma-point filter: 2n+1 deterministic points
#' (Eq. 12) propagate through the process and measurement models;
#' predicted moments are the weighted statistics of the transformed
#' points (Eqs. 13-14) plus the noise covariances, and the standard
#' Kalman gain update follows.  Exact for linear models.
#'
#' @param f Process model function (vector -> vector).
#' @param h Measurement model function (vector -> vector).
#' @param Q,R Additive process and measurement noise covariances.
#' @param x0,P0 Initial mean and covariance.
#' @param measurements Matrix (rows = observations) or list.
#' @param kappa Sigma-point spread (default 3 - n as recommended,
#'   floored to keep n + kappa > 0).
#' @return A list with elements \code{states}, \code{covariances},
#'   \code{innovations}, \code{kappa}, \code{method}.
#' @references Julier, S. J. and Uhlmann, J. K. (1997). A new
#'   extension of the Kalman filter to nonlinear systems. Proc. SPIE
#'   3068, 182-193.
#' @export
morie_ukff <- function(f, h, Q, R, x0, P0, measurements,
                       kappa = NULL) {
  x <- as.numeric(x0)
  n <- length(x)
  P <- as.matrix(P0)
  Q <- as.matrix(Q)
  R <- as.matrix(R)
  if (is.null(kappa)) {
    kappa <- 3 - n
    if (n + kappa <= 0) kappa <- 1e-6 - n + 1
  }
  if (n + kappa <= 0) stop("need n + kappa > 0")
  zs <- if (is.matrix(measurements)) {
    lapply(seq_len(nrow(measurements)), function(i) measurements[i, ])
  } else {
    measurements
  }
  states <- list(); covs <- list(); innovs <- list()
  for (z in zs) {
    z <- as.numeric(z)
    sp <- .ukf_sigma(x, P, kappa)
    pr <- .ukf_ut(sp$pts, sp$w, f)
    xp <- pr$mean
    Pp <- pr$cov + Q
    sp2 <- .ukf_sigma(xp, Pp, kappa)
    up <- .ukf_ut(sp2$pts, sp2$w, h)
    zp <- up$mean
    m <- length(zp)
    Pzz <- up$cov + R
    dx <- sweep(sp2$pts, 2, xp)
    dy <- sweep(up$ys, 2, zp)
    Pxz <- t(dx) %*% (dy * sp2$w)
    K <- Pxz %*% solve(Pzz)
    innov <- z - zp
    x <- as.numeric(xp + K %*% innov)
    P <- Pp - K %*% Pzz %*% t(K)
    P <- (P + t(P)) / 2
    states[[length(states) + 1]] <- x
    covs[[length(covs) + 1]] <- P
    innovs[[length(innovs) + 1]] <- innov
  }
  list(states = states, covariances = covs, innovations = innovs,
       kappa = kappa,
       method = "unscented Kalman filter (Julier & Uhlmann 1997)")
}
