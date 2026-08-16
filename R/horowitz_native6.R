# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors, part 6: transformation models
# T(Y) = X'beta + U. Mirrors morie.fn._hrz_transform, hrzhot,
# hrztfap, hrzchet, hrzlam, hrzycp.
#
# Collision scan: horowitz_native6.R, all five exported names and the
# two internal helpers were free in both R trees.
#
# Spec: Horowitz, Ch. 6. Sec. 6.2.4 (estimating lambda_0, eq. 6.44),
# Sec. 6.3.1 (Horowitz's T_n and F_n, eqs. 6.57-6.66), Sec. 6.3.2
# (Theorems 6.4-6.5 and assumptions HT1-HT9), Sec. 6.3.3 (Chen's
# estimator, eq. 6.67, Theorem 6.6), Sec. 6.4 (prediction).
#
# The model is identified only up to location and scale. Both
# normalisations are the book's own (printed p. 215): |beta_1| = 1
# for scale and T(y0) = 0 for location. With the location
# normalisation there is NO centering assumption on U and NO
# intercept in X.

.MORIE_HRZ_SCALE_NOTE <- "|beta_1| = 1 (scale) and T(y0) = 0 (location); no intercept in X"

#' .morie_hrz_normalize_scale
#'
#' A step of the horowitz_native6 implementation. Called by \code{morie_chen_transform}, \code{morie_transform_prediction}, \code{morie_transform_T_F}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param beta See Usage.
#' @return A numeric value.
#' @export
.morie_hrz_normalize_scale <- function(beta) {
  b <- as.numeric(beta)
  if (length(b) == 0L) stop("beta must be non-empty.", call. = FALSE)
  if (b[1L] == 0) {
    stop(paste(
      "the first component of beta is zero, so |beta_1| = 1 cannot",
      "be imposed; reorder X so a component with a nonzero",
      "coefficient and a continuous conditional distribution comes",
      "first."
    ), call. = FALSE)
  }
  b / abs(b[1L])
}

# Sixth-order kernel for K_Z, required by assumption HT8. A
# second-order kernel will NOT do: G_nz is a functional of DERIVATIVES
# of K_Z, those converge relatively slowly, and the higher-order
# kernel is what restores fast enough convergence (printed p. 220-221).
#' Sixth-order kernel for K_Z, required by assumption HT8. A
#'
#' second-order kernel will NOT do: G_nz is a functional of DERIVATIVES
#' of K_Z, those converge relatively slowly, and the higher-order kernel
#' is what restores fast enough convergence (printed p. 220-221).
#'
#' @param u Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_hrz_kz6 <- function(u) (15 - 10 * u^2 + u^4) / 16 * stats::dnorm(u)

# K'(u) = phi(u)(-35u + 14u^3 - u^5)/16, by differentiating the above
# and using phi'(u) = -u phi(u).
#' K\'(u) = phi(u)(-35u + 14u^3 - u^5)/16, by differentiating the above
#'
#' and using phi\'(u) = -u phi(u).
#'
#' @param u Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_hrz_kz6_deriv <- function(u) {
  stats::dnorm(u) * (-35 * u + 14 * u^3 - u^5) / 16
}

#' Horowitz's nonparametric estimators of T and F
#'
#' For \eqn{T(Y) = X'\beta + U},
#' \eqn{T_n(y) = -\int_{y_0}^{y}\int_{S_w} w(z) G_{ny}(v|z)/G_{nz}(v|z)
#' dz dv} (6.60) and \eqn{F_n(u) = A_n(u)/B_n(u)} (6.66).
#'
#' The derivation is the point. Differentiating \eqn{G(y|z) =
#' F\[T(y) - z\]} gives \eqn{T'(y) = -G_y(y|z)/G_z(y|z)}, so T is an
#' INTEGRAL of a ratio of kernel estimators. Each converges more
#' slowly than \eqn{n^{-1/2}} and their ratio is not root-n
#' consistent for anything -- but integrating over v and z averages
#' the noise away, which is why the estimator is built on (6.59)
#' rather than the pointwise (6.57).
#'
#' F is NOT the empirical distribution function of
#' \eqn{U_n = T_n(Y) - X'b_n}. T is root-n estimable only on a
#' compact interval strictly inside the support of Y, since T may be
#' unbounded at the boundaries; outside that window the \eqn{U_{ni}}
#' behave like CENSORED observations, and (6.66) is what stays
#' consistent under that censoring. Mirrors \code{morie.fn.hrzhot}.
#'
#' @param x numeric matrix of covariates; no intercept.
#' @param y numeric response.
#' @param bandwidth h_ny, or c(h_ny, h_nz).
#' @param beta_hat root-n consistent beta, rescaled to |b_1| = 1.
#' @param y0 location-normalisation point; median of y when NULL.
#' @param y_grid points at which to return T_hat.
#' @param u_grid points at which to return F_hat.
#' @param y1,y2 trimming window y2 < y1 for (6.66); the 10th and 90th
#'   percentiles of y when NULL.
#' @return list: y_grid, T_hat, u_grid, F_hat, beta, y0, window,
#'   h_ny, h_nz, F_is_empirical_cdf, normalisation, n, d, method.
#' @references Horowitz, Sec. 6.3.1, eqs. (6.57)-(6.66);
#'   Horowitz (1996).
#' @examples
#' n <- 120
#' x <- cbind(rnorm(n), rnorm(n))
#' y <- exp(x %*% c(1, -0.5) + rlogis(n) * 0.6)
#' morie_transform_T_F(x, y, c(0.5, 0.5), c(1, -0.5))$F_is_empirical_cdf
#' @export
morie_transform_T_F <- function(x, y, bandwidth, beta_hat, y0 = NULL,
                                y_grid = NULL, u_grid = NULL,
                                y1 = NULL, y2 = NULL) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(X)
  d <- ncol(X)
  if (n < 20L) {
    stop(sprintf("need at least 20 observations, got %d.", n),
      call. = FALSE
    )
  }
  b <- .morie_hrz_normalize_scale(beta_hat)
  if (length(b) != d) {
    stop(sprintf("beta_hat has %d entries for %d covariates.", length(b), d),
      call. = FALSE
    )
  }
  hb <- as.numeric(bandwidth)
  h_ny <- hb[1L]
  h_nz <- if (length(hb) == 1L) hb[1L] else hb[2L]
  if (h_ny <= 0 || h_nz <= 0) {
    stop(sprintf("bandwidths must be positive, got (%g, %g).", h_ny, h_nz),
      call. = FALSE
    )
  }
  z_idx <- as.numeric(X %*% b)
  yy0 <- if (is.null(y0)) stats::median(yv) else as.numeric(y0)
  yg <- if (is.null(y_grid)) {
    seq(stats::quantile(yv, 0.05), stats::quantile(yv, 0.95), length.out = 41L)
  } else {
    as.numeric(y_grid)
  }

  # w is a weight on z with compact support S_w integrating to 1
  # (6.58); the interquartile range keeps S_w where p_Z is bounded
  # away from zero, which (6.58)(a) requires
  zq <- stats::quantile(z_idx, c(0.25, 0.75))
  zs <- seq(zq[1L], zq[2L], length.out = 25L)
  w <- if (zq[2L] > zq[1L]) rep(1 / (zq[2L] - zq[1L]), 25L) else rep(1, 25L)

  ratio <- function(v, z) {
    a <- (z_idx - z) / h_nz
    kz <- .morie_hrz_kz6(a)
    kzp <- .morie_hrz_kz6_deriv(a)
    ind <- as.numeric(yv <= v)
    dd <- sum(kz) / (n * h_nz)
    if (dd <= 0) {
      return(0)
    }
    nn_ <- sum(ind * kz) / (n * h_nz)
    d_dd <- -sum(kzp) / (n * h_nz^2)
    d_nn <- -sum(ind * kzp) / (n * h_nz^2)
    g_nz <- (d_nn * dd - nn_ * d_dd) / dd^2
    if (g_nz == 0) {
      return(0)
    }
    g_ny <- sum(stats::dnorm((yv - v) / h_ny) * kz) / (n * h_ny * h_nz * dd)
    g_ny / g_nz
  }

  lo <- min(min(yg), yy0)
  hi <- max(max(yg), yy0)
  vs <- seq(lo, hi, length.out = 61L)
  inner <- vapply(vs, function(v) {
    vals <- w * vapply(zs, function(zz) ratio(v, zz), numeric(1))
    sum(diff(zs) * (utils::head(vals, -1L) + utils::tail(vals, -1L)) / 2)
  }, numeric(1))
  cum <- c(0, cumsum(diff(vs) * (utils::head(inner, -1L) +
    utils::tail(inner, -1L)) / 2))
  base <- stats::approx(vs, cum, xout = yy0, rule = 2L)$y
  t_at <- function(q) -(stats::approx(vs, cum, xout = q, rule = 2L)$y - base)
  t_hat <- t_at(yg)

  q2 <- if (is.null(y2)) as.numeric(stats::quantile(yv, 0.10)) else as.numeric(y2)
  q1 <- if (is.null(y1)) as.numeric(stats::quantile(yv, 0.90)) else as.numeric(y1)
  if (q2 >= q1) {
    stop(sprintf("the trimming window needs y2 < y1, got (%g, %g).", q2, q1),
      call. = FALSE
    )
  }
  t_y2 <- t_at(q2)
  t_y1 <- t_at(q1)
  # T_n is a function of y alone, so it is interpolated off the same
  # integration grid rather than re-integrated per observation
  uni <- t_at(yv) - z_idx
  ug <- if (is.null(u_grid)) {
    seq(stats::quantile(uni, 0.1), stats::quantile(uni, 0.9), length.out = 41L)
  } else {
    as.numeric(u_grid)
  }
  f_hat <- vapply(ug, function(u) {
    inwin <- (t_y2 - u < z_idx) & (z_idx <= t_y1 - u)
    bn <- mean(inwin)
    if (bn > 0) mean((uni <= u) & inwin) / bn else NA_real_
  }, numeric(1))

  list(
    y_grid = yg, T_hat = t_hat, u_grid = ug, F_hat = f_hat,
    beta = b, y0 = yy0, window = c(q2, q1), h_ny = h_ny, h_nz = h_nz,
    F_is_empirical_cdf = FALSE, normalisation = .MORIE_HRZ_SCALE_NOTE,
    n = n, d = d,
    method = "Horowitz (1996) T_n (6.60) and F_n (6.66); F is not the EDF of U_n"
  )
}

#' Asymptotic properties of Horowitz's T_n and F_n
#'
#' Theorems 6.4 and 6.5: \eqn{T_n} and \eqn{F_n} are uniformly
#' consistent, and \eqn{n^{1/2}(T_n - T)} and \eqn{n^{1/2}(F_n - F)}
#' converge weakly to tight, mean-zero GAUSSIAN PROCESSES -- not to
#' normal distributions. \eqn{T_n} is a random FUNCTION; a pointwise
#' normal approximation is a consequence, not the theorem, and a
#' confidence band needs the covariance function (given in Horowitz
#' 1996, not reproduced in the book).
#'
#' Assumption HT9 constrains the two bandwidths differently, and that
#' asymmetry is the practical content: with \eqn{K_Y} of second order
#' and \eqn{K_Z} of SIXTH order, \eqn{h_{ny} \propto n^{-1/3}} and
#' \eqn{h_{nz} \propto n^{-1/10}}. \eqn{h_{nz}} shrinks far more
#' slowly than any density-estimation rule suggests, because
#' \eqn{G_{nz}} is a functional of derivatives of \eqn{K_Z}. One
#' bandwidth for both, or a second-order \eqn{K_Z}, breaks the
#' theorem rather than merely costing efficiency. Mirrors
#' \code{morie.fn.hrztfap}.
#'
#' @param x numeric matrix of covariates.
#' @param y numeric response.
#' @param bandwidth the (h_ny, h_nz) actually used.
#' @param n sample size to report at; from the data when NULL.
#' @return list: asymptotic_distribution, limit_is_process, rate,
#'   rate_exponent, h_ny_reference, h_nz_reference, h_ny, h_nz,
#'   bandwidths_consistent_with_HT9, Kz_order_required, uniform_over,
#'   n, d, method.
#' @references Horowitz, Sec. 6.3.2, HT1-HT9, Theorems 6.4-6.5.
#' @examples
#' x <- cbind(rnorm(50), rnorm(50))
#' morie_transform_asymptotics(x, rnorm(50), c(0.2, 0.6))$limit_is_process
#' @export
morie_transform_asymptotics <- function(x, y, bandwidth, n = NULL) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  nn <- if (is.null(n)) nrow(X) else as.integer(n)
  if (is.na(nn) || nn < 2L) {
    stop(sprintf("n must be at least 2, got %s.", nn), call. = FALSE)
  }
  hb <- as.numeric(bandwidth)
  h_ny <- hb[1L]
  h_nz <- if (length(hb) == 1L) hb[1L] else hb[2L]
  if (h_ny <= 0 || h_nz <= 0) {
    stop(sprintf("bandwidths must be positive, got (%g, %g).", h_ny, h_nz),
      call. = FALSE
    )
  }
  ref_y <- nn^(-1 / 3)
  ref_z <- nn^(-1 / 10)
  # HT9 fixes the RATES, not the constants
  ok <- (h_ny / ref_y >= 0.1) && (h_ny / ref_y <= 10) &&
    (h_nz / ref_z >= 0.1) && (h_nz / ref_z <= 10) && (h_nz > h_ny)
  list(
    asymptotic_distribution = "tight mean-zero Gaussian process",
    limit_is_process = TRUE, rate = nn^-0.5, rate_exponent = -0.5,
    h_ny_reference = ref_y, h_nz_reference = ref_z,
    h_ny = h_ny, h_nz = h_nz,
    bandwidths_consistent_with_HT9 = ok, Kz_order_required = 6L,
    uniform_over = "a compact interval strictly inside the support of Y",
    n = nn, d = ncol(X),
    method = "Theorems 6.4-6.5: uniform consistency and n^{1/2} weak convergence to a Gaussian PROCESS"
  )
}

#' Chen's (2002) rank estimator of T
#'
#' \eqn{T_n(y) = \arg\max_{t}\ \[n(n-1)\]^{-1}\sum_i\sum_{j \ne i}
#' (d_{iy} - d_{jy_0}) 1\{X_i'b_n - X_j'b_n \ge t\}} (6.67), with
#' \eqn{d_{iy} = 1\{Y_i \ge y\}}.
#'
#' The construction rests on a sign, not a smoothing:
#' \eqn{E(d_{iy} - d_{jy_0} | X_i, X_j) \ge 0} exactly when
#' \eqn{X_i'\beta - X_j'\beta \ge T(y)}, so maximising a pairwise sum
#' over t locates \eqn{T(y)}. It is a U-statistic of order two --
#' hence the \eqn{n(n-1)} normalisation, and hence the quadratic cost.
#'
#' \strong{This is not faster than Horowitz's estimator.} Both are
#' \eqn{n^{-1/2}} (Theorem 6.6). The book compares them and finds
#' neither dominates: Horowitz's has smaller mean-square error near
#' the centre of the range of y, Chen's further out, and no known
#' estimator is efficient uniformly over y. \code{bandwidth} is
#' accepted for interface symmetry and NOT used -- (6.67) contains no
#' kernel. Mirrors \code{morie.fn.hrzchet}.
#'
#' @param x numeric matrix of covariates.
#' @param y numeric response.
#' @param bandwidth ignored; (6.67) uses no kernel.
#' @param beta_hat beta, rescaled to |b_1| = 1; first canonical
#'   direction when NULL.
#' @param y0 location-normalisation point; median of y when NULL.
#' @param y_grid points at which to return T_hat.
#' @param t_grid the compact interval M searched over.
#' @return list: y_grid, T_hat, objective_max, y0, beta, t_grid,
#'   uses_kernel, rate_exponent, faster_than_horowitz, normalisation,
#'   n, d, method.
#' @references Horowitz, Sec. 6.3.3, eq. (6.67), CT1-CT6,
#'   Theorem 6.6; Chen (2002).
#' @examples
#' n <- 80
#' x <- cbind(rnorm(n), rnorm(n))
#' y <- exp(x %*% c(1, -0.5) + rlogis(n))
#' morie_chen_transform(x, y, beta_hat = c(1, -0.5))$uses_kernel
#' @export
morie_chen_transform <- function(x, y, bandwidth = NULL, beta_hat = NULL,
                                 y0 = NULL, y_grid = NULL, t_grid = NULL) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(X)
  d <- ncol(X)
  if (n < 10L) {
    stop(sprintf("need at least 10 observations, got %d.", n),
      call. = FALSE
    )
  }
  b <- if (is.null(beta_hat)) {
    c(1, numeric(d - 1L))
  } else {
    .morie_hrz_normalize_scale(beta_hat)
  }
  if (length(b) != d) {
    stop(sprintf("beta_hat has %d entries for %d covariates.", length(b), d),
      call. = FALSE
    )
  }
  z_idx <- as.numeric(X %*% b)
  yy0 <- if (is.null(y0)) stats::median(yv) else as.numeric(y0)
  yg <- if (is.null(y_grid)) {
    seq(stats::quantile(yv, 0.1), stats::quantile(yv, 0.9), length.out = 21L)
  } else {
    as.numeric(y_grid)
  }
  dmat <- outer(z_idx, z_idx, "-")
  diag(dmat) <- NA_real_
  dj0 <- as.numeric(yv >= yy0)
  tg <- if (is.null(t_grid)) {
    seq(min(dmat, na.rm = TRUE), max(dmat, na.rm = TRUE), length.out = 121L)
  } else {
    as.numeric(t_grid)
  }
  denom <- n * (n - 1L)
  t_hat <- numeric(length(yg))
  objmax <- numeric(length(yg))
  for (k in seq_along(yg)) {
    wgt <- outer(as.numeric(yv >= yg[k]), dj0, "-")
    vals <- vapply(tg, function(tt) {
      sum(wgt * (dmat >= tt), na.rm = TRUE) / denom
    }, numeric(1))
    j <- which.max(vals)
    t_hat[k] <- tg[j]
    objmax[k] <- vals[j]
  }
  list(
    y_grid = yg, T_hat = t_hat, objective_max = objmax, y0 = yy0,
    beta = b, t_grid = tg, uses_kernel = FALSE, rate_exponent = -0.5,
    faster_than_horowitz = FALSE, normalisation = .MORIE_HRZ_SCALE_NOTE,
    n = n, d = d,
    method = "Chen (2002) pairwise rank maximisation (6.67); same n^{-1/2} rate as Horowitz"
  )
}

#' Kernel-smoothed baseline hazard
#'
#' \eqn{\lambda_{n0}(y) = h_n^{-1}\int K((y - \xi)/h_n)
#' d\Lambda_{n0}(\xi)} (6.44), with \eqn{\Lambda_{n0}} the Breslow
#' estimator of the cumulative baseline hazard.
#'
#' The smoothing is not cosmetic. \eqn{\lambda_0 = d\Lambda_0/dy}, so
#' the obvious estimator is \eqn{d\Lambda_{n0}/dy} -- and that does
#' not work, because \eqn{\Lambda_{n0}} is a STEP function whose
#' derivative is zero almost everywhere and undefined at the jumps.
#' Same obstruction as differentiating an empirical distribution
#' function to get a density, same remedy: smooth first.
#'
#' The rate is no faster than \eqn{n^{-2/5}} for a twice
#' differentiable \eqn{\lambda_0} and a second-order kernel;
#' \eqn{n^{-1/2}} is NOT attainable. The leading bias is
#' \eqn{A_K h_n^2 \lambda_0''(y)/2} with
#' \eqn{A_K = \int \zeta^2 K(\zeta)d\zeta}, returned so the bias is
#' computable. Mirrors \code{morie.fn.hrzlam}.
#'
#' @param t observed durations, non-negative.
#' @param x numeric matrix of covariates.
#' @param event 1 for an event, 0 for right-censoring.
#' @param beta_hat regression coefficients.
#' @param bandwidth h_n; a spread-scaled \code{n^(-1/5)} when NULL.
#' @param grid evaluation points.
#' @return list: grid, lambda0_hat, cumhaz_times, cumhaz, bandwidth,
#'   A_K, rate_exponent, root_n_attainable, n_events, n, method.
#' @references Horowitz, Sec. 6.2.4, eqs. (6.44)-(6.48).
#' @examples
#' n <- 200
#' x <- cbind(rnorm(n), rnorm(n))
#' tt <- rexp(n) / exp(x %*% c(0.5, -0.3))
#' morie_baseline_hazard(tt, x, rep(1, n), c(0.5, -0.3))$root_n_attainable
#' @export
morie_baseline_hazard <- function(t, x, event, beta_hat, bandwidth = NULL,
                                  grid = NULL) {
  tv <- as.numeric(t)
  ev <- as.numeric(event)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(tv)) X <- t(X)
  if (nrow(X) != length(tv)) {
    stop("x must have one row per entry of t.", call. = FALSE)
  }
  if (length(ev) != length(tv)) {
    stop(sprintf(
      "event has %d entries for %d durations.",
      length(ev), length(tv)
    ), call. = FALSE)
  }
  if (!all(ev %in% c(0, 1))) stop("event must be binary 0/1.", call. = FALSE)
  if (any(tv < 0)) stop("durations must be non-negative.", call. = FALSE)
  n <- nrow(X)
  d <- ncol(X)
  if (n < 5L) {
    stop(sprintf("need at least 5 observations, got %d.", n),
      call. = FALSE
    )
  }
  b <- as.numeric(beta_hat)
  if (length(b) != d) {
    stop(sprintf("beta_hat has %d entries for %d covariates.", length(b), d),
      call. = FALSE
    )
  }
  risk <- exp(as.numeric(X %*% b))
  if (!all(is.finite(risk))) {
    stop("exp(X'beta) overflowed; rescale the covariates.", call. = FALSE)
  }
  ord <- order(tv)
  ts <- tv[ord]
  es <- ev[ord]
  rs <- risk[ord]
  at_risk <- rev(cumsum(rev(rs)))
  jump_t <- ts[es == 1]
  jump_w <- (1 / at_risk)[es == 1]
  if (length(jump_t) == 0L) {
    stop("no events: the baseline hazard is not identified.", call. = FALSE)
  }
  cumhaz <- cumsum(jump_w)
  spread <- max(jump_t) - min(jump_t)
  hh <- if (is.null(bandwidth)) {
    (if (spread > 0) spread else 1) * n^(-0.2)
  } else {
    as.numeric(bandwidth)
  }
  if (hh <= 0) {
    stop(sprintf("bandwidth must be positive, got %g.", hh),
      call. = FALSE
    )
  }
  g <- if (is.null(grid)) {
    seq(min(jump_t), max(jump_t), length.out = 50L)
  } else {
    as.numeric(grid)
  }
  # (6.44) as a Stieltjes sum: dLambda_n0 puts mass jump_w at each
  # event time
  lam <- rowSums(stats::dnorm(outer(g, jump_t, "-") / hh) *
    rep(jump_w, each = length(g))) / hh
  list(
    grid = g, lambda0_hat = lam, cumhaz_times = jump_t, cumhaz = cumhaz,
    bandwidth = hh, A_K = 1, rate_exponent = -0.4,
    root_n_attainable = FALSE, n_events = length(jump_t), n = n,
    method = "Breslow cumulative hazard smoothed by (6.44); differentiating the step function does not work"
  )
}

#' Prediction from a fitted transformation model
#'
#' \eqn{P(Y \le y | X = x) = F\[T(y) - x'\beta\]}, and the
#' gamma-quantile predictor \eqn{y_{n\gamma}(x) = \inf\{y : T_n(y) >
#' x'b_n + u_{n\gamma}\}} with
#' \eqn{u_{n\gamma} = \inf\{u : F_n(u) \ge \gamma\}}.
#'
#' The section's actual result is a NEGATIVE one, and it is why a
#' quantile appears at all: when T is nonparametric, \eqn{E(Y|X = x)}
#' CANNOT be estimated at rate \eqn{n^{-1/2}}. The root-n estimator
#' \eqn{n^{-1}\sum_i T_n^{-1}(U_{ni} + x'b_n)} needs T known up to a
#' finite-dimensional parameter, because it needs T at root-n
#' accuracy over the WHOLE support of Y, and Sec. 6.3.2 only delivers
#' that on a compact interval strictly inside the support. A
#' conditional quantile is usually root-n estimable, needing
#' \eqn{F_n} accurate only near \eqn{u_\gamma}. Mirrors
#' \code{morie.fn.hrzycp}.
#'
#' @param x covariate values, a vector or a matrix of rows.
#' @param y_threshold y at which P(Y <= y | X = x) is wanted.
#' @param T_hat T_n on y_grid, or a function of y.
#' @param F_hat F_n on u_grid, or a function of u.
#' @param beta_hat coefficients, rescaled to |b_1| = 1.
#' @param gamma quantile level in (0, 1); the median when NULL.
#' @param y_grid,u_grid required when T_hat / F_hat are vectors.
#' @return list: probability, quantile, gamma, u_gamma, index,
#'   mean_root_n_estimable, quantile_root_n_estimable, n_points,
#'   method.
#' @references Horowitz, Sec. 6.4; Cheng et al. (1997).
#' @examples
#' yg <- seq(0.5, 8, length.out = 60)
#' ug <- seq(-4, 4, length.out = 81)
#' morie_transform_prediction(c(0.3, 0.2), 2, log(yg), plogis(ug),
#'   c(1, -0.5),
#'   y_grid = yg, u_grid = ug
#' )$gamma
#' @export
morie_transform_prediction <- function(x, y_threshold, T_hat, F_hat, beta_hat,
                                       gamma = NULL, y_grid = NULL,
                                       u_grid = NULL) {
  b <- .morie_hrz_normalize_scale(beta_hat)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
  if (ncol(X) != length(b)) X <- t(X)
  if (ncol(X) != length(b)) {
    stop(sprintf("x must have %d columns to match beta_hat.", length(b)),
      call. = FALSE
    )
  }
  idx <- as.numeric(X %*% b)
  yq <- as.numeric(y_threshold)

  if (is.null(y_grid)) {
    stop("y_grid is required when T_hat is a vector.", call. = FALSE)
  }
  yg <- as.numeric(y_grid)
  tg <- if (is.function(T_hat)) {
    vapply(yg, T_hat, numeric(1))
  } else {
    as.numeric(T_hat)
  }
  if (length(tg) != length(yg)) {
    stop(sprintf(
      "T_hat has %d entries for %d grid points.",
      length(tg), length(yg)
    ), call. = FALSE)
  }
  if (any(diff(tg) < 0)) {
    stop(paste(
      "T_hat must be non-decreasing; assumption HT4 makes T",
      "strictly increasing."
    ), call. = FALSE)
  }
  if (is.null(u_grid)) {
    stop("u_grid is required when F_hat is a vector.", call. = FALSE)
  }
  ug <- as.numeric(u_grid)
  fg <- if (is.function(F_hat)) {
    vapply(ug, F_hat, numeric(1))
  } else {
    as.numeric(F_hat)
  }
  if (length(fg) != length(ug)) {
    stop(sprintf(
      "F_hat has %d entries for %d grid points.",
      length(fg), length(ug)
    ), call. = FALSE)
  }
  if (any(fg < 0) || any(fg > 1)) {
    stop("F_hat must lie in [0, 1].", call. = FALSE)
  }
  t_of <- function(v) stats::approx(yg, tg, xout = v, rule = 2L)$y
  f_of <- function(v) stats::approx(ug, fg, xout = v, rule = 2L)$y

  prob <- outer(idx, yq, function(zz, vv) {
    f_of(t_of(vv) - zz)
  })
  g <- if (is.null(gamma)) 0.5 else as.numeric(gamma)
  if (!(g > 0 && g < 1)) {
    stop(sprintf("gamma must lie in (0, 1), got %g.", g), call. = FALSE)
  }
  hit <- which(fg >= g)
  u_g <- if (length(hit)) ug[hit[1L]] else ug[length(ug)]
  quant <- vapply(idx, function(zz) {
    over <- which(tg > zz + u_g)
    if (length(over)) yg[over[1L]] else NA_real_
  }, numeric(1))

  prob_out <- if (length(idx) == 1L) {
    if (length(yq) == 1L) as.numeric(prob[1L, 1L]) else as.numeric(prob[1L, ])
  } else if (length(yq) == 1L) as.numeric(prob[, 1L]) else prob
  list(
    probability = prob_out,
    quantile = if (length(idx) > 1L) quant else quant[1L],
    gamma = g, u_gamma = u_g,
    index = if (length(idx) > 1L) idx else idx[1L],
    mean_root_n_estimable = FALSE, quantile_root_n_estimable = TRUE,
    n_points = length(idx),
    method = "P(Y<=y|x) = F[T(y) - x'b]; the conditional MEAN is not root-n estimable when T is nonparametric"
  )
}
