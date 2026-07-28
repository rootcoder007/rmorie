# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors, part 9: nonparametric additive models.
# Mirrors morie.fn.hrzmir and morie.fn.hrzora.
#
# Collision scan: horowitz_native9.R and both exported names were free
# in both R trees.
#
# Spec: Horowitz, Sec. 3.1.1 (marginal integration, eqs. 3.5-3.9,
# Theorem 3.1) and Sec. 3.1.3 (two-step oracle-efficient estimation,
# eqs. 3.15-3.18; Horowitz and Mammen 2004).

#' Marginal integration for a nonparametric additive model
#'
#' For \eqn{E(Y|X = x) = \mu + m_1(x^1) + \dots + m_d(x^d)} (3.5) with
#' the location normalisation \eqn{E[m_j(X^j)] = 0} (3.6), which makes
#' \eqn{\mu = E(Y)} (3.7) and
#' \eqn{m_1(x^1) = \int E(Y|X = x)p_{-1}(x^{-1})dx^{-1} - \mu} (3.8).
#' Replacing the conditional mean by the kernel estimator (3.9) and
#' the outer integral by a sample average gives
#' \eqn{\hat m_1(x^1) = n^{-1}\sum_i \hat g(x^1, X_i^{(-1)}) - \hat\mu}.
#'
#' Hold the component of interest fixed, average the fitted surface
#' over the OTHERS at their observed values, subtract the mean. The
#' normalisation is what makes that meaningful: without
#' \eqn{E[m_j] = 0} each component could absorb a constant and
#' \eqn{\mu} shed one, so nothing would be identified.
#'
#' \strong{This estimator carries the curse of dimensionality}, which
#' is why the chapter goes on to develop others. \eqn{\hat g} smooths
#' the full covariate with a \eqn{(d-1)}-dimensional kernel, so
#' Theorem 3.1 needs the components \eqn{q} times differentiable for
#' some \eqn{q > d - 1} -- the requirement GROWS with d.
#' \code{smoothness_required} returns that q, so the cost is a number
#' rather than a caveat. Mirrors \code{morie.fn.hrzmir}.
#'
#' @param x numeric matrix of continuous covariates.
#' @param y numeric response.
#' @param bandwidth h1, or c(h1, h2); Silverman-based when NULL.
#' @param j which component to estimate (1-based in R).
#' @param grid evaluation points.
#' @return list: grid, m_hat, mu_hat, component, h1, h2,
#'   normalisation, mean_of_m_hat, smoothness_required,
#'   curse_of_dimensionality, n, d, method.
#' @references Horowitz, Sec. 3.1.1, eqs. (3.5)-(3.9), Theorem 3.1;
#'   Linton and Nielsen (1995), Linton and Hardle (1996).
#' @examples
#' x <- matrix(runif(400, -1, 1), ncol = 2)
#' y <- 2 + sin(pi * x[, 1]) + x[, 2]^2 - 1 / 3 + rnorm(200) * 0.2
#' morie_marginal_integration(x, y, j = 1)$smoothness_required
#' @export
morie_marginal_integration <- function(x, y, bandwidth = NULL, j = 1L,
                                       grid = NULL) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(X)
  d <- ncol(X)
  if (n < 20L) stop(sprintf("need at least 20 observations, got %d.", n),
                    call. = FALSE)
  if (d < 2L) {
    stop(sprintf("an additive model needs at least 2 components, got %d.", d),
         call. = FALSE)
  }
  jj <- as.integer(j)
  if (is.na(jj) || jj < 1L || jj > d) {
    stop(sprintf("j must lie in 1..%d, got %s.", d, j), call. = FALSE)
  }
  xj <- X[, jj]
  rest <- X[, -jj, drop = FALSE]
  if (is.null(bandwidth)) {
    h1 <- .hrz_silverman(xj)
    # the integrated-out directions are smoothed jointly, so their
    # bandwidth is inflated by the dimension they span
    h2 <- mean(vapply(seq_len(d - 1L), function(k) .hrz_silverman(rest[, k]),
                      numeric(1))) * n^(1 / 5 - 1 / (4 + d))
  } else {
    hb <- as.numeric(bandwidth)
    h1 <- hb[1L]
    h2 <- if (length(hb) == 1L) hb[1L] else hb[2L]
  }
  if (h1 <= 0 || h2 <= 0) {
    stop(sprintf("bandwidths must be positive, got (%g, %g).", h1, h2),
         call. = FALSE)
  }
  g <- if (is.null(grid)) {
    seq(stats::quantile(xj, 0.05), stats::quantile(xj, 0.95), length.out = 41L)
  } else as.numeric(grid)
  mu <- mean(yv)                                     # (3.7)

  # (3.9): K_1 on the held component, K_2 a product kernel on the rest
  k2 <- matrix(1, n, n)
  for (k in seq_len(d - 1L)) {
    k2 <- k2 * stats::dnorm(outer(rest[, k], rest[, k], "-") / h2)
  }
  m_hat <- vapply(g, function(v) {
    k1 <- stats::dnorm((v - xj) / h1)
    num <- k2 * matrix(k1 * yv, n, n, byrow = TRUE)
    den <- k2 * matrix(k1, n, n, byrow = TRUE)
    ds <- rowSums(den)
    ghat <- ifelse(ds > 0, rowSums(num) / pmax(ds, 1e-300), NA_real_)
    mean(ghat, na.rm = TRUE) - mu
  }, numeric(1))

  list(grid = g, m_hat = m_hat, mu_hat = mu, component = jj,
       h1 = h1, h2 = h2,
       normalisation = "E[m_j(X^j)] = 0 for every j, so mu = E(Y)",
       mean_of_m_hat = mean(m_hat, na.rm = TRUE),
       smoothness_required = d,   # Theorem 3.1(b): q > d - 1
       curse_of_dimensionality = TRUE, n = n, d = d,
       method = "Marginal integration (3.8)/(3.9); simple, but K_2 is (d-1)-dimensional")
}

#' Two-step oracle-efficient additive model estimator
#'
#' Horowitz and Mammen (2004). Stage one fits a SERIES approximation
#' by least squares with the additive structure imposed; stage two
#' runs a ONE-DIMENSIONAL regression of the partialled-out response on
#' the component of interest (3.18), by Nadaraya-Watson or local
#' linear.
#'
#' \strong{The point is what is not here.} No stage smooths in more
#' than one dimension: the first stage imposes additivity through the
#' basis so it never performs \eqn{d}-dimensional nonparametric
#' regression, and the second is a scalar smooth. The estimator is
#' \eqn{n^{-2/5}} consistent, asymptotically normal and ORACLE
#' EFFICIENT for any finite d -- each component estimated as
#' accurately as if the others were known -- so there is no curse of
#' dimensionality, unlike marginal integration.
#'
#' It is also NOT iterative, unlike backfitting, which is defined as
#' the limit of a sequence rather than by a formula and which
#' Opsomer and Ruppert found is not oracle efficient without strong
#' restrictions on the distribution of X.
#'
#' The basis is the full Fourier system minus the constant,
#' alternating \eqn{\sin(j\pi v)} and \eqn{\cos(j\pi v)}. A
#' cosine-only basis also satisfies (3.15) and (3.16), but every
#' \eqn{\cos(k\pi v)} is EVEN and so spans no odd function; it cannot
#' partial an odd component out, and the damage lands on the OTHER
#' components. Mirrors \code{morie.fn.hrzora}.
#'
#' @param x numeric matrix of covariates.
#' @param y numeric response.
#' @param bandwidth second-stage bandwidth, scalar or one per
#'   component; an \code{n^(-1/5)} rule when NULL.
#' @param kappa series terms per component; \code{ceiling(n^(1/5))}
#'   when NULL, at least 2.
#' @param local_linear use the local-linear second stage.
#' @param grid evaluation points on the rescaled [-1, 1] support.
#' @return list: grid, m_hat, mu_hat, theta, kappa, bandwidth,
#'   oracle_efficient, iterative, rate_exponent,
#'   max_smoothing_dimension, curse_of_dimensionality, n, d, method.
#' @references Horowitz, Sec. 3.1.3, eqs. (3.15)-(3.18);
#'   Horowitz and Mammen (2004).
#' @examples
#' x <- matrix(runif(800, -1, 1), ncol = 2)
#' y <- 2 + sin(pi * x[, 1]) + x[, 2]^2 - 1 / 3 + rnorm(400) * 0.2
#' morie_two_step_additive(x, y)$oracle_efficient
#' @export
morie_two_step_additive <- function(x, y, bandwidth = NULL, kappa = NULL,
                                    local_linear = TRUE, grid = NULL) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(X)
  d <- ncol(X)
  if (n < 30L) stop(sprintf("need at least 30 observations, got %d.", n),
                    call. = FALSE)
  if (d < 2L) {
    stop(sprintf("an additive model needs at least 2 components, got %d.", d),
         call. = FALSE)
  }
  lo <- apply(X, 2L, min)
  hi <- apply(X, 2L, max)
  span <- ifelse(hi > lo, hi - lo, 1)
  Z <- 2 * sweep(X, 2L, lo, "-") / rep(span, each = n) - 1

  kap <- if (is.null(kappa)) as.integer(ceiling(n^0.2)) else as.integer(kappa)
  if (is.na(kap) || kap < 2L) {
    stop(sprintf("kappa must be at least 2, got %s.", kappa), call. = FALSE)
  }
  basis <- function(v) {
    cols <- lapply(seq_len(kap), function(m) {
      k <- (m + 1L) %/% 2L
      if (m %% 2L == 1L) sin(pi * k * v) else cos(pi * k * v)
    })
    do.call(cbind, cols)
  }
  psi <- cbind(1, do.call(cbind, lapply(seq_len(d), function(jj) basis(Z[, jj]))))
  theta <- qr.coef(qr(psi), yv)
  theta[is.na(theta)] <- 0
  theta <- as.numeric(theta)
  mu_tilde <- theta[1L]
  series_component <- function(jj, zv) {
    as.numeric(basis(zv) %*% theta[(2L + (jj - 1L) * kap):(1L + jj * kap)])
  }
  hvec <- if (is.null(bandwidth)) {
    vapply(seq_len(d), function(k) stats::sd(Z[, k]) * n^-0.2, numeric(1))
  } else {
    hb <- as.numeric(bandwidth)
    if (length(hb) == 1L) rep(hb, d) else hb
  }
  if (length(hvec) != d) {
    stop(sprintf("bandwidth must be a scalar or %d values, got %d.",
                 d, length(hvec)), call. = FALSE)
  }
  if (any(hvec <= 0)) {
    stop(sprintf("bandwidths must be positive, got %s.",
                 paste(hvec, collapse = ", ")), call. = FALSE)
  }
  gz <- if (is.null(grid)) seq(-0.9, 0.9, length.out = 41L) else as.numeric(grid)
  m_hat <- matrix(NA_real_, d, length(gz))
  for (jj in seq_len(d)) {
    others <- rep(0, n)
    for (k in seq_len(d)) if (k != jj) others <- others + series_component(k, Z[, k])
    resid <- yv - mu_tilde - others
    zj <- Z[, jj]
    hh <- hvec[jj]
    for (tt in seq_along(gz)) {
      v <- gz[tt]
      w <- stats::dnorm((v - zj) / hh)
      sw <- sum(w)
      if (sw <= 0) next
      if (local_linear) {
        dx <- zj - v
        s0 <- sw; s1 <- sum(w * dx); s2 <- sum(w * dx^2)
        t0 <- sum(w * resid); t1 <- sum(w * resid * dx)
        det <- s0 * s2 - s1 * s1
        m_hat[jj, tt] <- if (det != 0) (s2 * t0 - s1 * t1) / det else t0 / s0
      } else {
        m_hat[jj, tt] <- sum(w * resid) / sw
      }
    }
    m_hat[jj, ] <- m_hat[jj, ] - mean(m_hat[jj, ], na.rm = TRUE)
  }
  list(grid = gz, m_hat = m_hat, mu_hat = mu_tilde, theta = theta,
       kappa = kap, bandwidth = if (d > 1L) hvec else hvec[1L],
       oracle_efficient = TRUE, iterative = FALSE, rate_exponent = -0.4,
       max_smoothing_dimension = 1L, curse_of_dimensionality = FALSE,
       n = n, d = d,
       method = "Horowitz-Mammen two-step (3.18); series first, scalar smooth second, no d-dimensional step")
}
