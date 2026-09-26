# SPDX-License-Identifier: AGPL-3.0-or-later
#' Marginal moments of Zeger's parameter-driven count model
#'
#' Schabenberger & Gotway (2005, p. 294), after Zeger (1988): with a latent
#' stationary Z1 (mean 1, covariance sigma2 rho1(h)) and counts with
#' conditional mean and variance mu(s) Z1(s), the marginal variance is
#' mu + sigma2 mu^2 and Corr[Z2(s), Z2(s + h)] = rho1 / sqrt((1 + 1 / (sigma2
#' mu(s))) (1 + 1 / (sigma2 mu(s + h)))); with sigma2 = mu = 1 it is rho1 / 2.
#'
#' @param mean_s,mean_sh Marginal means mu(s) and mu(s + h), positive.
#' @param sigma2 Latent variance, positive.
#' @param rho1 Latent correlation in [-1, 1].
#' @return Named list: mean_s, mean_sh, var_s, var_sh, cov, corr.
#' @references Zeger, S. L. (1988). Biometrika 75, 621-629. Schabenberger &
#'   Gotway (2005), p. 294.
#' @examples
#' zegcnt(1, 1, 1, 0.6)$corr
#' @export
zegcnt <- function(mean_s, mean_sh, sigma2, rho1) {
  if (!(mean_s > 0 && mean_sh > 0 && sigma2 > 0) || rho1 < -1 || rho1 > 1) {
    stop("means and `sigma2` must be positive and `rho1` in [-1, 1]", call. = FALSE)
  }
  vs <- mean_s + sigma2 * mean_s^2
  vh <- mean_sh + sigma2 * mean_sh^2
  cv <- sigma2 * rho1 * mean_s * mean_sh
  list(mean_s = mean_s, mean_sh = mean_sh, var_s = vs, var_sh = vh, cov = cv,
       corr = cv / sqrt(vs * vh))
}

#' Conditional negative definiteness of a semivariogram matrix
#'
#' A valid semivariogram satisfies sum_i sum_j a_i a_j gamma(s_i - s_j) <= 0
#' whenever sum a_i = 0 (Schabenberger & Gotway 2005, Problem 2.2). With
#' P = I - J/m, the condition at the given sites is that the largest
#' eigenvalue of P Gamma P on the sum-zero subspace is at most zero.
#'
#' @param gamma Symmetric m x m matrix of gamma(s_i - s_j).
#' @param tol Relative tolerance, scaled by max |gamma|.
#' @return Named list: max_eigenvalue, valid, m.
#' @references Schabenberger & Gotway (2005), Problem 2.2, p. 79.
#' @examples
#' d <- as.matrix(dist(cbind(1:4, c(0, 2, 1, 3))))
#' cndchk(d)$valid
#' @export
cndchk <- function(gamma, tol = 1e-10) {
  g <- as.matrix(gamma)
  m <- nrow(g)
  if (m < 2L || ncol(g) != m) stop("`gamma` must be a square matrix of at least 2 x 2", call. = FALSE)
  p <- g - outer(rowMeans(g), rep(1, m)) - outer(rep(1, m), colMeans(g)) + mean(g)
  p <- (p + t(p)) / 2
  ev <- sort(eigen(p, symmetric = TRUE, only.values = TRUE)$values)
  ev <- ev[-which.min(abs(ev))]
  top <- ev[length(ev)]
  scale <- max(abs(g))
  if (scale == 0) scale <- 1
  list(max_eigenvalue = top, valid = top <= tol * scale, m = m)
}

#' Non-negative least squares
#'
#' Lawson and Hanson's (1974, Ch. 23) active-set algorithm for min ||A x -
#' b|| subject to x >= 0, native.
#'
#' @param A Numeric matrix.
#' @param b Numeric vector, length nrow(A).
#' @param max_iter Outer iterations, default 3 ncol(A).
#' @return Named list: x, residual_norm, passive (1-based), iterations.
#' @references Lawson, C. L. & Hanson, R. J. (1974). Solving Least Squares
#'   Problems. Prentice-Hall, Ch. 23.
#' @examples
#' nnlsq(cbind(1, c(1, 2, 3)), c(1, 0, -1))$x
#' @export
nnlsq <- function(A, b, max_iter = NULL) {
  A <- as.matrix(A)
  b <- as.numeric(b)
  m <- nrow(A)
  n <- ncol(A)
  if (!m || length(b) != m) stop("`A` and `b` must have the same number of rows", call. = FALSE)
  anorm <- max(colSums(abs(A)))
  if (anorm == 0) anorm <- 1
  tol <- 10 * .Machine$double.eps * anorm * max(m, n)
  x <- numeric(n)
  passive <- integer(0)
  it <- 0L
  limit <- if (is.null(max_iter)) 3L * n else as.integer(max_iter)
  w <- drop(crossprod(A, b - A %*% x))
  while (it < limit) {
    active <- setdiff(seq_len(n), passive)
    if (!length(active) || max(w[active]) <= tol) break
    it <- it + 1L
    passive <- c(passive, active[which.max(w[active])])
    repeat {
      z <- numeric(n)
      z[passive] <- qr.solve(A[, passive, drop = FALSE], b)
      if (all(z[passive] > 0)) {
        x <- z
        break
      }
      neg <- passive[z[passive] <= 0]
      alpha <- min(x[neg] / (x[neg] - z[neg]))
      x <- x + alpha * (z - x)
      passive <- passive[x[passive] > tol]
      x[-passive] <- 0
      if (!length(passive)) break
    }
    w <- drop(crossprod(A, b - A %*% x))
  }
  list(x = x, residual_norm = sqrt(sum((b - A %*% x)^2)), passive = sort(passive),
       iterations = it)
}

.schab_npvg_omega <- function(d, x) {
  x <- as.numeric(x)
  if (d == 1) return(cos(x))
  if (d == 3) return(ifelse(x == 0, 1, sin(x) / ifelse(x == 0, 1, x)))
  if (d == 2) {
    if (!length(x)) return(numeric(0))
    return(.schab_bessel_j0(x, n_quad = max(200L, as.integer(1.5 * max(abs(x))) + 100L)))
  }
  stop("`d` must be 1, 2 or 3", call. = FALSE)
}

.schab_npvg_omega_integral <- function(d, h, lo, hi, n_per_panel = 20L) {
  if (hi <= lo) return(0)
  if (h == 0) return(hi - lo)
  panels <- max(1L, ceiling((hi - lo) * abs(h) / (pi / 2)))
  gl <- .schab_gauss_legendre(n_per_panel)
  step <- (hi - lo) / panels
  a <- lo + (seq_len(panels) - 1L) * step
  pts <- as.vector(outer(0.5 * step * (gl$nodes + 1), a, "+"))
  wts <- rep(0.5 * step * gl$weights, panels)
  sum(wts * .schab_npvg_omega(d, h * pts))
}

.schab_npvg_kernel_corr <- function(h, theta_l, theta_u, d, b) {
  width <- theta_u - theta_l
  g0 <- min(max(-theta_l / width, 0), 1)
  gb <- min(max((b - theta_l) / width, 0), 1)
  lo <- max(theta_l, 0)
  hi <- min(theta_u, b)
  vapply(h, function(hv) {
    g0 + (1 - gb) * .schab_npvg_omega(d, hv * b) +
      .schab_npvg_omega_integral(d, hv, lo, hi) / width
  }, numeric(1))
}

.schab_npvg_nelder_mead <- function(fn, x0, step = 0.35, max_iter = 4000L, tol = 1e-14) {
  n <- length(x0)
  sim <- rbind(x0, t(x0 + diag(step, n)))
  f <- apply(sim, 1L, fn)
  for (i in seq_len(max_iter)) {
    o <- order(f)
    sim <- sim[o, , drop = FALSE]
    f <- f[o]
    if (abs(f[n + 1L] - f[1L]) <= tol * (abs(f[1L]) + tol)) break
    cen <- colMeans(sim[seq_len(n), , drop = FALSE])
    xr <- 2 * cen - sim[n + 1L, ]
    fr <- fn(xr)
    if (fr < f[1L]) {
      xe <- 3 * cen - 2 * sim[n + 1L, ]
      fe <- fn(xe)
      if (fe < fr) {
        sim[n + 1L, ] <- xe
        f[n + 1L] <- fe
      } else {
        sim[n + 1L, ] <- xr
        f[n + 1L] <- fr
      }
    } else if (fr < f[n]) {
      sim[n + 1L, ] <- xr
      f[n + 1L] <- fr
    } else {
      xc <- 0.5 * (cen + sim[n + 1L, ])
      fc <- fn(xc)
      if (fc < f[n + 1L]) {
        sim[n + 1L, ] <- xc
        f[n + 1L] <- fc
      } else {
        for (k in 2:(n + 1L)) {
          sim[k, ] <- sim[1L, ] + 0.5 * (sim[k, ] - sim[1L, ])
          f[k] <- fn(sim[k, ])
        }
      }
    }
  }
  k <- which.min(f)
  list(par = unname(sim[k, ]), value = unname(f[k]))
}

#' Nonparametric (Shapiro-Botha) semivariogram
#'
#' Schabenberger & Gotway (2005) eqs (4.16), (4.46)-(4.47): with spectral
#' masses w_i >= 0 at nodes t_i, C(h) = sum w_i Omega_d(h t_i) and gamma(h) =
#' sum w_i (1 - Omega_d(h t_i)), Omega_1 = cos, Omega_2 = J0, Omega_3(t) =
#' sin(t)/t. Evaluate with `weights`, or fit the weights to `gamma_hat` by
#' non-negative least squares (Shapiro & Botha 1991), weighting by `npairs`.
#'
#' @param h Lags.
#' @param nodes Non-negative nodes t_i.
#' @param weights Masses w_i >= 0 (evaluate mode).
#' @param d Dimension, 1, 2 or 3.
#' @param gamma_hat Empirical semivariogram at `h` (fit mode).
#' @param npairs Optional pair counts for the fit.
#' @return Named list: gamma, covariance, weights, sill, nodes, d and, when
#'   fitting, rss.
#' @references Shapiro, A. & Botha, J. D. (1991). CSDA 11, 87-96.
#'   Schabenberger & Gotway (2005), eqs (4.16), (4.46), (4.47), pp. 147, 180.
#' @examples
#' spnpsv(1:5, c(0.2, 0.6), weights = c(1, 0.5))$gamma
#' @export
spnpsv <- function(h, nodes, weights = NULL, d = 2, gamma_hat = NULL, npairs = NULL) {
  h <- as.numeric(h)
  t <- as.numeric(nodes)
  if (!length(t) || any(t < 0)) stop("`nodes` must be non-negative and non-empty", call. = FALSE)
  basis <- matrix(.schab_npvg_omega(d, outer(h, t)), length(h), length(t))
  rss <- NULL
  if (!is.null(gamma_hat)) {
    if (length(gamma_hat) != length(h)) stop("`gamma_hat` must match `h`", call. = FALSE)
    sw <- if (is.null(npairs)) rep(1, length(h)) else sqrt(as.numeric(npairs))
    fit <- nnlsq(sw * (1 - basis), sw * as.numeric(gamma_hat))
    w <- fit$x
    rss <- fit$residual_norm^2
  } else if (!is.null(weights)) {
    w <- as.numeric(weights)
    if (length(w) != length(t) || any(w < 0)) {
      stop("`weights` must be non-negative, one per node", call. = FALSE)
    }
  } else {
    stop("give `weights` to evaluate or `gamma_hat` to fit", call. = FALSE)
  }
  cv <- drop(basis %*% w)
  sill <- sum(w)
  out <- list(gamma = ifelse(h == 0, 0, sill - cv), covariance = cv, weights = w,
              sill = sill, nodes = t, d = d)
  if (!is.null(rss)) out$rss <- rss
  out
}

#' Parametric kernel semivariogram
#'
#' Schabenberger & Gotway (2005) eqs (4.48)-(4.51): C(theta, h) = sigma2
#' integral_0^b Omega_d(h w) F(theta, dw) with F the U(theta_l, theta_u) cdf
#' on [0, b], zero below and one above (4.49); atoms at 0 and b are included
#' when the kernel extends past [0, b]. Evaluate with `sill`, `theta_l`,
#' `theta_u`, or fit to `gamma_hat` by the OLS criterion (4.51) with sigma2
#' profiled out and Nelder-Mead over (sqrt(theta_l), log(theta_u - theta_l)).
#' The fit keeps theta_l >= 0: below zero only theta_u and sigma2 theta_u /
#' (theta_u - theta_l) are identified.
#'
#' @param h Lags.
#' @param sill,theta_l,theta_u Model parameters (evaluate mode).
#' @param d Dimension, 1, 2 or 3.
#' @param b Upper bound of the spectral support.
#' @param gamma_hat Empirical semivariogram at `h` (fit mode).
#' @param start Starting (theta_l, theta_u - theta_l) for the fit.
#' @return Named list: gamma, covariance, sill, theta_l, sill_effective,
#'   theta_u, d, b and, when fitting, q.
#' @references Schabenberger & Gotway (2005), eqs (4.48)-(4.51), pp. 181-183.
#' @examples
#' spkrnv(c(0, 1, 5), sill = 1.5, theta_l = 0.1, theta_u = 0.3)$gamma
#' @export
spkrnv <- function(h, sill = NULL, theta_l = NULL, theta_u = NULL, d = 2, b = 1,
                   gamma_hat = NULL, start = c(0, 0.2)) {
  h <- as.numeric(h)
  q <- NULL
  if (!is.null(gamma_hat)) {
    g <- as.numeric(gamma_hat)
    if (length(g) != length(h)) stop("`gamma_hat` must match `h`", call. = FALSE)
    if (start[1L] < 0 || !(start[2L] > 0)) {
      stop("`start` needs theta_l >= 0 and a positive width", call. = FALSE)
    }
    profile <- function(p) {
      k <- .schab_npvg_kernel_corr(h, p[1L]^2, p[1L]^2 + exp(p[2L]), d, b)
      one <- ifelse(h == 0, 0, 1 - k)
      den <- sum(one^2)
      s2 <- if (den > 0) sum(g * one) / den else 0
      c(s2, sum((g - s2 * one)^2))
    }
    best <- .schab_npvg_nelder_mead(function(p) profile(p)[2L], c(sqrt(start[1L]), log(start[2L])))
    theta_l <- best$par[1L]^2
    theta_u <- theta_l + exp(best$par[2L])
    sill <- profile(best$par)[1L]
    q <- best$value
  }
  if (is.null(sill) || is.null(theta_l) || is.null(theta_u)) {
    stop("give `sill`, `theta_l`, `theta_u` to evaluate or `gamma_hat` to fit", call. = FALSE)
  }
  if (!(theta_u > theta_l)) stop("`theta_u` must exceed `theta_l`", call. = FALSE)
  cv <- sill * .schab_npvg_kernel_corr(h, theta_l, theta_u, d, b)
  out <- list(gamma = ifelse(h == 0, 0, sill - cv), covariance = cv, sill = sill,
              theta_l = theta_l, sill_effective = sill * theta_u / (theta_u - theta_l),
              theta_u = theta_u, d = d, b = b)
  if (!is.null(q)) out$q <- q
  out
}
