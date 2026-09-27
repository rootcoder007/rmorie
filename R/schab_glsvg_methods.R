# SPDX-License-Identifier: AGPL-3.0-or-later
.schab_glsvg_pairs <- function(coords, breaks) {
  coords <- as.matrix(coords)
  br <- as.numeric(breaks)
  if (length(br) < 2L || any(diff(br) <= 0)) {
    stop("`breaks` must be increasing, at least two values", call. = FALSE)
  }
  d <- as.matrix(stats::dist(coords))
  pr <- which(upper.tri(d), arr.ind = TRUE)
  pr <- pr[order(pr[, 1L], pr[, 2L]), , drop = FALSE]
  cls <- lapply(seq_len(length(br) - 1L), function(m) {
    pr[d[pr] > br[m] & d[pr] <= br[m + 1L], , drop = FALSE]
  })
  cls <- cls[vapply(cls, nrow, 1L) > 0L]
  list(d = d, classes = cls, lags = vapply(cls, function(p) mean(d[p]), 1))
}

.schab_glsvg_cov <- function(d, classes, nugget, sill, rng, model) {
  g <- matrix(.sp_semivariogram(as.vector(d), nugget, sill, rng, model), nrow(d))
  k <- length(classes)
  r <- matrix(0, k, k)
  for (a in seq_len(k)) {
    pa <- classes[[a]]
    for (b in a:k) {
      pb <- classes[[b]]
      cc <- g[pa[, 1L], pb[, 2L], drop = FALSE] + g[pa[, 2L], pb[, 1L], drop = FALSE] -
        g[pa[, 1L], pb[, 1L], drop = FALSE] - g[pa[, 2L], pb[, 2L], drop = FALSE]
      r[a, b] <- r[b, a] <- sum(cc^2) / (2 * nrow(pa) * nrow(pb))
    }
  }
  r
}

#' Semivariogram fit by generalized least squares (Cressie 1985)
#'
#' Schabenberger & Gotway (2005) eqs (4.30)-(4.32): minimise (gamma_hat -
#' gamma(h, theta))' R(theta)^-1 (gamma_hat - gamma(h, theta)) for the
#' Matheron estimator, with R(theta) exact for Gaussian data from
#' Cov(T_ij^2, T_kl^2) = 2 (gamma(s_i - s_l) + gamma(s_j - s_k) - gamma(s_i -
#' s_k) - gamma(s_j - s_l))^2 (Cressie 1993, eq 2.6.10; the printed (4.32)
#' has gamma(h_ij) for the first term). R is re-evaluated at each new
#' estimate until the estimates settle; Nelder-Mead on the log scale.
#'
#' @param coords Two-column matrix of site coordinates.
#' @param z Numeric responses.
#' @param breaks Lag-class boundaries; class m holds the lags h with breaks_m < h <= breaks_(m + 1).
#' @param model "exponential", "gaussian", "spherical", "wave", "tent" or
#'   "circular".
#' @param nugget Estimate a nugget (else zero).
#' @param start Optional starting c(nugget, sill, range).
#' @param max_reweight Maximum re-weighting iterations.
#' @param tol Change in the estimates, relative to each or to 1e-8 times the
#'   largest, that ends the re-weighting.
#' @return Named list: nugget, sill, range, lags, gamma_hat, fitted, npairs,
#'   R, criterion, iterations, model.
#' @references Cressie, N. (1985). Mathematical Geology 17, 563-586.
#'   Schabenberger & Gotway (2005), eqs (4.30)-(4.32), pp. 164-165.
#' @examples
#' xy <- cbind(rep(1:6, 6), rep(1:6, each = 6))
#' spglsv(xy, sin(xy[, 1]) + cos(xy[, 2]), c(0, 1.5, 2.5, 3.5, 4.5))$range
#' @export
spglsv <- function(coords, z, breaks, model = "exponential", nugget = TRUE, start = NULL,
                   max_reweight = 100L, tol = 1e-6) {
  z <- as.numeric(z)
  lp <- .schab_glsvg_pairs(coords, breaks)
  if (length(z) != nrow(lp$d)) stop("`coords` and `z` must have the same length", call. = FALSE)
  cls <- lp$classes
  lags <- lp$lags
  npar <- if (nugget) 3L else 2L
  if (length(cls) <= npar) stop("need more non-empty lag classes than parameters", call. = FALSE)
  gh <- vapply(cls, function(p) sum((z[p[, 1L]] - z[p[, 2L]])^2) / (2 * nrow(p)), 1)
  if (is.null(start)) start <- c(0.25 * gh[1L], max(gh), 0.5 * max(lags))
  theta <- as.numeric(start)
  if (!nugget) theta[1L] <- 0
  if (any(theta[2:3] <= 0) || theta[1L] < 0) stop("`start` values must be positive", call. = FALSE)
  unpack <- function(p) if (nugget) exp(p) else c(0, exp(p))
  crit <- NA_real_
  it <- 0L
  for (it in seq_len(max_reweight)) {
    lower <- t(chol(.schab_glsvg_cov(lp$d, cls, theta[1L], theta[2L], theta[3L], model)))
    q <- function(p) {
      th <- unpack(p)
      sum(forwardsolve(lower, gh - .sp_semivariogram(lags, th[1L], th[2L], th[3L], model))^2)
    }
    p0 <- log(pmax(if (nugget) theta else theta[2:3], 1e-12))
    best <- .schab_npvg_nelder_mead(q, p0)
    crit <- best$value
    new <- unpack(best$par)
    change <- max(abs(new - theta) / pmax(abs(theta), 1e-8 * max(abs(theta))))
    theta <- new
    if (change < tol) break
  }
  list(nugget = theta[1L], sill = theta[2L], range = theta[3L], lags = lags, gamma_hat = gh,
       fitted = .sp_semivariogram(lags, theta[1L], theta[2L], theta[3L], model),
       npairs = vapply(cls, nrow, 1L),
       R = .schab_glsvg_cov(lp$d, cls, theta[1L], theta[2L], theta[3L], model),
       criterion = crit, iterations = it, model = model)
}

#' Expected Matheron semivariogram under a linear drift
#'
#' Schabenberger & Gotway (2005) eq (5.35), after Cressie (1993, p. 165):
#' under Z(s) = X(s) beta + e(s), E((Z(s_i) - Z(s_j))^2) = 2 gamma(s_i - s_j) +
#' (sum_k beta_k (x_k(s_i) - x_k(s_j)))^2, so each lag class of the classical
#' estimator averages the semivariogram and a squared drift contrast.
#'
#' @param coords Two-column matrix of site coordinates.
#' @param X Covariate matrix, one row per site.
#' @param beta Coefficients, one per column of `X`.
#' @param breaks Lag-class boundaries.
#' @param nugget,sill,range Semivariogram parameters of the error process.
#' @param model Semivariogram model name.
#' @return Named list: lags, npairs, expected, semivariogram_part, drift_part.
#' @references Cressie (1993), p. 165. Schabenberger & Gotway (2005), eq
#'   (5.35), p. 255.
#' @examples
#' xy <- cbind(rep(1:5, 5), rep(1:5, each = 5))
#' vgdrift(xy, cbind(1, xy), c(0, 0.5, 0), c(0, 1.5, 3), 0, 1, 3)$drift_part
#' @export
vgdrift <- function(coords, X, beta, breaks, nugget, sill, range, model = "exponential") {
  lp <- .schab_glsvg_pairs(coords, breaks)
  X <- as.matrix(X)
  if (nrow(X) != nrow(lp$d) || ncol(X) != length(beta)) {
    stop("`X` needs one row per site and one column per `beta`", call. = FALSE)
  }
  mu <- drop(X %*% beta)
  semi <- vapply(lp$classes, function(p) mean(.sp_semivariogram(lp$d[p], nugget, sill, range, model)), 1)
  drift <- vapply(lp$classes, function(p) sum((mu[p[, 1L]] - mu[p[, 2L]])^2) / (2 * nrow(p)), 1)
  list(lags = lp$lags, npairs = vapply(lp$classes, nrow, 1L), expected = semi + drift,
       semivariogram_part = semi, drift_part = drift)
}
