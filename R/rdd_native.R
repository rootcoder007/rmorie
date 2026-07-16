# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native regression-discontinuity engines (feat/native-specializations,
# module 16). Replaces rdrobust (estimation, bandwidths, kink) and the
# archived rdd package (McCrary density test) with base-R
# implementations:
#
#   * Imbens-Kalyanaraman (2012) MSE-optimal plug-in bandwidth
#     (the three-step sigma^2/f/m'' algorithm with regularization).
#   * Local-polynomial RD estimator with the nearest-neighbor (J=3)
#     heteroskedasticity-robust variance of Calonico-Cattaneo-Titiunik.
#   * CCT bias-corrected inference via the higher-order local fit.
#   * McCrary (2008) log-density discontinuity test.

#' Internal helper: one-sided local polynomial fit at the cutoff
#'
#' Weighted least squares of y on ((x - c)^0 ... (x - c)^p) using
#' kernel weights K((x - c)/h), on one side of the cutoff. Returns the
#' full coefficient vector and the nearest-neighbor (J = 3)
#' heteroskedasticity-robust covariance (the "conventional" variance of
#' rdrobust with vce = "nn").
#'
#' @srrstats {G1.0} Calonico, Cattaneo & Titiunik (2014), Econometrica
#'   82(6); nearest-neighbor variance per Abadie & Imbens (2006).
#' @noRd
.morie_rdd_lp_nn <- function(x, y, cutoff, h, p = 1,
                             kernel = "triangular", nn_J = 3L) {
  K <- .morie_rdd_get_kernel(kernel)
  u <- (x - cutoff) / h
  w <- K(u)
  use <- which(w > 0)
  if (length(use) < (p + 2L)) {
    return(list(beta = rep(NA_real_, p + 1L),
                vcov = matrix(NA_real_, p + 1L, p + 1L),
                n = length(use)))
  }
  xs <- x[use]; ys <- y[use]; ws <- w[use]
  X <- outer(xs - cutoff, 0:p, "^")
  XtWX <- crossprod(X, ws * X)
  XtWX_inv <- tryCatch(solve(XtWX), error = function(e) .morie_ginv(XtWX))
  beta <- as.numeric(XtWX_inv %*% crossprod(X, ws * ys))
  # Nearest-neighbor residual variance sigma2_i = J/(J+1) (y_i - nnmean)^2
  # On the sorted running variable the J nearest neighbours live in a
  # +/- J window, so this is O(n J) instead of O(n^2).
  n_use <- length(use)
  J <- min(nn_J, n_use - 1L)
  ord <- order(xs)
  xo <- xs[ord]; yo <- ys[ord]
  # Vectorized +/-J window: distance and value matrices built from
  # shifted copies (Inf pads the ends), then the J nearest picked by
  # a running arg-min sweep — no per-observation allocation.
  offs <- c(-(J:1), 1:J)
  Dm <- matrix(Inf, n_use, length(offs))
  Ym <- matrix(0, n_use, length(offs))
  for (k in seq_along(offs)) {
    o <- offs[k]
    idx <- seq_len(n_use) + o
    okk <- idx >= 1L & idx <= n_use
    Dm[okk, k] <- abs(xo[idx[okk]] - xo[okk])
    Ym[okk, k] <- yo[idx[okk]]
  }
  nn_sum <- numeric(n_use)
  for (j in seq_len(J)) {
    pick <- max.col(-Dm, ties.method = "first")
    sel <- cbind(seq_len(n_use), pick)
    nn_sum <- nn_sum + Ym[sel]
    Dm[sel] <- Inf
  }
  sig2o <- (J / (J + 1)) * (yo - nn_sum / J)^2
  sig2 <- numeric(n_use)
  sig2[ord] <- sig2o
  meat <- crossprod(X, (ws^2 * sig2) * X)
  V <- XtWX_inv %*% meat %*% XtWX_inv
  list(beta = beta, vcov = V, n = n_use)
}

#' Internal helper: two-sided sharp RD estimate at the cutoff
#'
#' `deriv = 0` gives the level jump (sharp RD), `deriv = 1` the slope
#' jump (regression kink). Returns estimate, NN-robust SE, and side
#' fits.
#' @noRd
.morie_rdd_jump_native <- function(x, y, cutoff, h, p = 1,
                                   kernel = "triangular", deriv = 0L) {
  left <- x < cutoff
  fL <- .morie_rdd_lp_nn(x[left], y[left], cutoff, h, p, kernel)
  fR <- .morie_rdd_lp_nn(x[!left], y[!left], cutoff, h, p, kernel)
  k <- deriv + 1L
  scale <- factorial(deriv)
  est <- scale * (fR$beta[k] - fL$beta[k])
  se <- scale * sqrt(fR$vcov[k, k] + fL$vcov[k, k])
  list(estimate = est, se = se, n = fL$n + fR$n, left = fL, right = fR)
}

#' Internal helper: Imbens-Kalyanaraman (2012) plug-in bandwidth
#'
#' The three-step algorithm of IK (2012, REStud 79): (1) pilot density
#' and conditional variance at the cutoff; (2) second derivatives from
#' one-sided quadratic fits at pilot bandwidths driven by a global
#' cubic; (3) regularized MSE-optimal bandwidth with the
#' kernel-specific constant (3.4375 for the triangular kernel).
#'
#' @srrstats {G1.0} Imbens & Kalyanaraman (2012), Review of Economic
#'   Studies 79(3) 933-959.
#' @noRd
.morie_rdd_ik_native <- function(x, y, cutoff = 0,
                                 kernel = "triangular") {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  sd_x <- stats::sd(x)
  # Step 1: density + conditional variance at the cutoff
  h1 <- 1.84 * sd_x * n^(-1 / 5)
  il <- x >= cutoff - h1 & x < cutoff
  ir <- x >= cutoff & x <= cutoff + h1
  n_l <- sum(il); n_r <- sum(ir)
  if (n_l < 3L || n_r < 3L) {
    # Degenerate near-cutoff sample: fall back to the rule of thumb.
    return(list(bandwidth = 1.84 * sd_x * n^(-1 / 5),
                method = "ROT fallback (too few cutoff obs)"))
  }
  f_c <- (n_l + n_r) / (2 * n * h1)
  s2_c <- (sum((y[il] - mean(y[il]))^2) +
             sum((y[ir] - mean(y[ir]))^2)) / (n_l + n_r)
  # Step 2: curvature via global cubic -> pilot bandwidths -> quadratics
  d <- x - cutoff
  Tr <- as.numeric(d >= 0)
  g <- stats::lm(y ~ Tr + d + I(d^2) + I(d^3))
  m3 <- 6 * stats::coef(g)[["I(d^3)"]]
  if (!is.finite(m3) || m3 == 0) m3 <- 1e-8
  n_pos <- sum(d >= 0); n_neg <- sum(d < 0)
  h2_r <- 3.56 * (s2_c / (f_c * m3^2))^(1 / 7) * n_pos^(-1 / 7)
  h2_l <- 3.56 * (s2_c / (f_c * m3^2))^(1 / 7) * n_neg^(-1 / 7)
  fit2 <- function(mask, h2) {
    sel <- mask & abs(d) <= h2
    if (sum(sel) < 4L) return(list(m2 = 0, N = sum(sel)))
    q <- stats::lm(y[sel] ~ d[sel] + I(d[sel]^2))
    list(m2 = 2 * stats::coef(q)[[3L]], N = sum(sel))
  }
  qr_ <- fit2(d >= 0, h2_r)
  ql_ <- fit2(d < 0, h2_l)
  # Step 3: regularized MSE-optimal bandwidth
  r_r <- 2160 * s2_c / (max(qr_$N, 1L) * h2_r^4)
  r_l <- 2160 * s2_c / (max(ql_$N, 1L) * h2_l^4)
  CK <- switch(kernel, triangular = 3.4375, uniform = 5.40,
               epanechnikov = 4.497, 3.4375)
  denom <- (qr_$m2 - ql_$m2)^2 + r_r + r_l
  h_ik <- CK * (2 * s2_c / (f_c * denom))^(1 / 5) * n^(-1 / 5)
  list(bandwidth = as.numeric(h_ik), method = "IK 2012 plug-in",
       details = list(f_c = f_c, sigma2_c = s2_c,
                      m2_right = qr_$m2, m2_left = ql_$m2,
                      reg_right = r_r, reg_left = r_l))
}

#' Internal helper: McCrary (2008) density discontinuity test
#'
#' Histogram with the cutoff on a bin edge, then one-sided local
#' linear fits to the normalized bin counts; the statistic is the
#' log-difference of the density estimates at the cutoff with
#' McCrary's asymptotic standard error.
#'
#' @srrstats {G1.0} McCrary (2008), Journal of Econometrics 142(2)
#'   698-714.
#' @noRd
.morie_rdd_mccrary_native <- function(x, cutoff = 0, bin = NULL,
                                      bandwidth = NULL) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (is.null(bin)) bin <- 2 * stats::sd(x) * n^(-1 / 2)
  # Bin midpoints aligned so the cutoff is an edge
  l_edge <- floor((min(x) - cutoff) / bin) * bin + cutoff
  r_edge <- ceiling((max(x) - cutoff) / bin) * bin + cutoff
  edges <- seq(l_edge, r_edge, by = bin)
  mids <- edges[-length(edges)] + bin / 2
  counts <- as.numeric(table(cut(x, breaks = edges,
                                 include.lowest = TRUE)))
  dens <- counts / (n * bin)
  if (is.null(bandwidth)) {
    # McCrary's automatic bandwidth: global quartic on each side.
    auto_h <- function(side) {
      m <- mids[side]; f <- dens[side]
      if (length(m) < 6L) return(stats::sd(x))
      q <- stats::lm(f ~ m + I(m^2) + I(m^3) + I(m^4))
      cf <- stats::coef(q)
      m2 <- function(z) 2 * cf[3] + 6 * cf[4] * z + 12 * cf[5] * z^2
      kap <- mean(m2(m)^2)
      s2 <- stats::sd(stats::resid(q))^2
      rng <- max(m) - min(m)
      3.348 * (s2 * rng / max(kap, 1e-12))^(1 / 5) * n^(-1 / 5)
    }
    bandwidth <- mean(c(auto_h(mids < cutoff), auto_h(mids >= cutoff)))
  }
  side_fit <- function(side_mask) {
    m <- mids[side_mask]; f <- dens[side_mask]
    u <- (m - cutoff) / bandwidth
    w <- pmax(1 - abs(u), 0)
    use <- w > 0
    if (sum(use) < 3L) return(NA_real_)
    fit <- stats::lm(f[use] ~ I(m[use] - cutoff), weights = w[use])
    max(as.numeric(stats::coef(fit)[1L]), 1e-12)
  }
  f_l <- side_fit(mids < cutoff)
  f_r <- side_fit(mids >= cutoff)
  theta <- log(f_r) - log(f_l)
  se <- sqrt((1 / (n * bandwidth)) * (24 / 5) * (1 / f_r + 1 / f_l))
  z <- theta / se
  list(statistic = z, theta = theta, se = se,
       p_value = 2 * stats::pnorm(-abs(z)),
       f_left = f_l, f_right = f_r,
       bandwidth = bandwidth, binsize = bin)
}
