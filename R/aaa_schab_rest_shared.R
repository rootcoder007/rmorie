# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Moran moments (Ch 1), cross-K (Ch 3), periodogram (Ch 4) and
# non-stationary covariance (Ch 8) -- the last five modules of the
# Schabenberger & Gotway (2005) shelf. Twin of the Python arm's
# _schab_moran.py, the cross-K part of _schab_pp.py, _schab_spectral.py and
# _schab_nonstat.py: the same equations and the same arithmetic, so the two
# arms agree by construction rather than to a tolerance nobody chose.
#
# Sourcing notes carried over from the Python twins:
#
#   Sec. 1.3.2 / Problem 1.8: the printed E_r[I^2] is missing a bracket; the
#   grouping n[(n^2-3n+3)S1 - nS2 + 3w..^2] is the one the book's own
#   Example 1.7 confirms (sd 0.0732 vs the literal reading's 0.0740).
#
#   Sec. 3.4.4 eq (3.9): w(s_k, u_l) is the proportion of the circumference
#   of a circle centred at s_k with radius h_kl inside the window, computed
#   exactly by splitting the circle at critical angles, never by sampling.
#
#   Sec. 4.7.1 eq (4.57) carries 1/((2 pi)^2 r c); eq (4.59) -- periodogram
#   equals the Fourier transform of the sample covariance -- only closes
#   with that constant, and only away from the origin.
#
#   Sec. 8.2.1 eq (8.1): theta1 > 0, theta2, theta3 >= 0 are necessary but
#   NOT sufficient for positive semi-definiteness; the eigenvalues must be
#   examined. Sec. 8.3.1: local kriging keeps one global theta, Haas's
#   moving window re-estimates it per window; window rule is 35 sites, then
#   five at a time until every lag class holds a pair.
#
# Internal; `aaa_` collates it before its callers.

# --- Ch 1: Moran's I and Geary's c ------------------------------------------

#' .schab_moran_check_w
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_geary_c},
#' \code{.schab_moran_i}, \code{.schab_weight_sums}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w A matrix; passed to \code{nrow}.
#' @return The value of \code{w}, as built in the body.
#' @export
.schab_moran_check_w <- function(w) {
  w <- as.matrix(w)
  if (nrow(w) != ncol(w)) stop("weights must be a square matrix")
  if (any(!is.finite(w))) stop("weights must be finite")
  if (any(diag(w) != 0)) {
    stop("weights must have a zero diagonal; a site is not its own neighbour")
  }
  w
}

#' .schab_weight_sums
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moran_moments}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w A matrix; passed to \code{t}.
#' @return A list with \code{S0}, \code{S1}, \code{S2}.
#' @export
.schab_weight_sums <- function(w) {
  w <- .schab_moran_check_w(w)
  s0 <- sum(w)
  if (s0 <= 0) stop("total weight w.. must be positive")
  list(
    S0 = s0,
    S1 = 0.5 * sum((w + t(w))^2),
    S2 = sum((rowSums(w) + colSums(w))^2)
  )
}

#' .schab_moran_i
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moran_moments}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A vector; its length is taken.
#' @param w A matrix; passed to \code{nrow}.
#' @return A numeric value.
#' @export
.schab_moran_i <- function(z, w) {
  z <- as.numeric(z)
  w <- .schab_moran_check_w(w)
  n <- length(z)
  if (nrow(w) != n) stop("z and the weight matrix disagree on n")
  if (n < 4) stop("at least 4 sites are needed")
  d <- z - mean(z)
  denom <- sum(d * d)
  if (denom <= 0) stop("z is constant; Moran's I is undefined")
  n * as.numeric(t(d) %*% w %*% d) / (sum(w) * denom)
}

#' .schab_geary_c
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moran_moments}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A vector; its length is taken.
#' @param w A matrix; passed to \code{nrow}.
#' @return A numeric value.
#' @export
.schab_geary_c <- function(z, w) {
  z <- as.numeric(z)
  w <- .schab_moran_check_w(w)
  n <- length(z)
  if (nrow(w) != n) stop("z and the weight matrix disagree on n")
  d <- z - mean(z)
  denom <- sum(d * d)
  if (denom <= 0) stop("z is constant; Geary's c is undefined")
  diff2 <- outer(z, z, "-")^2
  (n - 1) * sum(w * diff2) / (2 * sum(w) * denom)
}

#' .schab_kurtosis_b
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moran_moments}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
#' @examples
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .schab_kurtosis_b(z = y)
#' res
.schab_kurtosis_b <- function(z) {
  d <- as.numeric(z) - mean(as.numeric(z))
  s2 <- sum(d * d)
  if (s2 <= 0) stop("z is constant; the kurtosis is undefined")
  length(d) * sum(d^4) / s2^2
}

#' .schab_moran_moments
#'
#' A step of the schab_rest_shared implementation. Called by \code{spmenv}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A vector; its length is taken.
#' @param w Passed to \code{.schab_weight_sums}.
#' @return A list with \code{I}, \code{expectation}, \code{variance_normal},
#' \code{variance_randomization}, \code{sd_normal}, \code{sd_randomization},
#' \code{z_normal}, \code{z_randomization}, \code{kurtosis_b}, \code{S0}, \code{S1},
#' \code{S2}, \code{n}, \code{geary_c}, \code{geary_expectation}.
#' @export
.schab_moran_moments <- function(z, w) {
  z <- as.numeric(z)
  s <- .schab_weight_sums(w)
  s0 <- s$S0
  s1 <- s$S1
  s2 <- s$S2
  n <- length(z)
  if (n < 4) stop("at least 4 sites are needed")
  i_obs <- .schab_moran_i(z, w)
  e_i <- -1 / (n - 1)
  var_norm <- (n * n * s1 - n * s2 + 3 * s0 * s0) /
    (s0 * s0 * (n * n - 1)) - e_i * e_i
  b <- .schab_kurtosis_b(z)
  first <- n * ((n * n - 3 * n + 3) * s1 - n * s2 + 3 * s0 * s0)
  second <- b * ((n * n - n) * s1 - 2 * n * s2 + 6 * s0 * s0)
  e_i2 <- (first - second) / ((n - 1) * (n - 2) * (n - 3) * s0 * s0)
  var_rand <- e_i2 - e_i * e_i
  zstat <- function(v) if (v > 0) (i_obs - e_i) / sqrt(v) else NA_real_
  list(
    I = i_obs, expectation = e_i,
    variance_normal = var_norm, variance_randomization = var_rand,
    sd_normal = if (var_norm > 0) sqrt(var_norm) else NA_real_,
    sd_randomization = if (var_rand > 0) sqrt(var_rand) else NA_real_,
    z_normal = zstat(var_norm), z_randomization = zstat(var_rand),
    kurtosis_b = b, S0 = s0, S1 = s1, S2 = s2, n = n,
    geary_c = .schab_geary_c(z, w), geary_expectation = 1
  )
}

# --- Ch 3: cross-K ----------------------------------------------------------

#' .schab_ripley_weights
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_cross_k}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param points Coerced to numeric by the body, with \code{as.numeric}.
#' @param region A vector; indexed elementwise.
#' @param radii Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{ifelse}.
#' @export
.schab_ripley_weights <- function(points, region, radii) {
  p <- matrix(as.numeric(points), ncol = 2)
  t_ <- as.numeric(radii)
  xmin <- region[1]
  ymin <- region[2]
  xmax <- region[3]
  ymax <- region[4]
  if (nrow(p) == 1L && length(t_) > 1L) {
    p <- p[rep(1L, length(t_)), , drop = FALSE]
  }
  n <- length(t_)
  dxm <- p[, 1] - xmin
  dxp <- xmax - p[, 1]
  dym <- p[, 2] - ymin
  dyp <- ymax - p[, 2]

  tt <- ifelse(t_ > 0, t_, 1) # avoid 0/0; fixed up below
  cos_b <- cbind(-dxm / tt, dxp / tt)
  sin_b <- cbind(-dym / tt, dyp / tt)

  cand <- matrix(0, n, 10)
  cand[, 2] <- 2 * pi
  ok_c <- abs(cos_b) <= 1
  ac <- acos(pmin(pmax(cos_b, -1), 1))
  cand[, 3:4] <- ifelse(ok_c, ac, 0)
  cand[, 5:6] <- ifelse(ok_c, 2 * pi - ac, 0)
  ok_s <- abs(sin_b) <= 1
  as_ <- asin(pmin(pmax(sin_b, -1), 1))
  cand[, 7:8] <- ifelse(ok_s, as_ %% (2 * pi), 0)
  cand[, 9:10] <- ifelse(ok_s, (pi - as_) %% (2 * pi), 0)

  cand <- t(apply(cand, 1L, sort))
  mid <- 0.5 * (cand[, -10, drop = FALSE] + cand[, -1, drop = FALSE])
  width <- cand[, -1, drop = FALSE] - cand[, -10, drop = FALSE]
  cx <- p[, 1] + t_ * cos(mid)
  cy <- p[, 2] + t_ * sin(mid)
  inside <- (cx >= xmin) & (cx <= xmax) & (cy >= ymin) & (cy <= ymax)
  w <- rowSums(width * inside) / (2 * pi)
  whole <- pmin(pmin(dxm, dxp), pmin(dym, dyp)) >= t_
  ifelse(whole | (t_ <= 0), 1, w)
}

#' .schab_cross_k
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_cross_k_combined}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p1 A matrix; indexed by row and column.
#' @param p2 A matrix; indexed by row and column.
#' @param region A vector; indexed elementwise.
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param correction One of \code{"none"}, \code{"ripley"}. Defaults to \code{"ripley"}.
#' @return A vector, from \code{vapply}.
#' @export
.schab_cross_k <- function(p1, p2, region, r, correction = "ripley") {
  p1 <- matrix(as.numeric(p1), ncol = 2)
  p2 <- matrix(as.numeric(p2), ncol = 2)
  r <- as.numeric(r)
  if (any(r < 0)) stop("`r` must be non-negative")
  if (!correction %in% c("ripley", "none")) {
    stop("`correction` must be 'ripley' or 'none'")
  }
  n1 <- nrow(p1)
  n2 <- nrow(p2)
  if (n1 == 0L || n2 == 0L) {
    stop("both patterns must contain at least one event")
  }
  area <- (region[3] - region[1]) * (region[4] - region[2])
  lam1 <- n1 / area
  lam2 <- n2 / area
  d <- sqrt(outer(p1[, 1], p2[, 1], "-")^2 + outer(p1[, 2], p2[, 2], "-")^2)
  if (correction == "none") {
    winv <- matrix(1, n1, n2)
  } else {
    pts <- p1[rep(seq_len(n1), each = n2), , drop = FALSE]
    w <- matrix(.schab_ripley_weights(pts, region, as.numeric(t(d))),
      n1, n2,
      byrow = TRUE
    )
    winv <- ifelse(w > 0, 1 / ifelse(w > 0, w, 1), 0)
  }
  vapply(r, function(h) sum(winv[d <= h]) / (lam1 * lam2 * area), numeric(1))
}

#' .schab_cross_k_combined
#'
#' A step of the schab_rest_shared implementation. Called by \code{spkcrs}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p1 A matrix; passed to \code{nrow}.
#' @param p2 A matrix; passed to \code{nrow}.
#' @param region A vector; indexed elementwise.
#' @param r Numeric; combined arithmetically in the body.
#' @param correction Passed to \code{.schab_cross_k}. Defaults to \code{"ripley"}.
#' @return A list with \code{K_star}, \code{K_12}, \code{K_21}, \code{L_star},
#' \code{L_minus_h}, \code{K_independence}, \code{r}, \code{lambda_1}, \code{lambda_2}.
#' @export
.schab_cross_k_combined <- function(p1, p2, region, r, correction = "ripley") {
  p1 <- matrix(as.numeric(p1), ncol = 2)
  p2 <- matrix(as.numeric(p2), ncol = 2)
  area <- (region[3] - region[1]) * (region[4] - region[2])
  lam1 <- nrow(p1) / area
  lam2 <- nrow(p2) / area
  k12 <- .schab_cross_k(p1, p2, region, r, correction)
  k21 <- .schab_cross_k(p2, p1, region, r, correction)
  kstar <- (lam2 * k12 + lam1 * k21) / (lam1 + lam2)
  lstar <- sqrt(pmax(kstar, 0) / pi)
  list(
    K_star = kstar, K_12 = k12, K_21 = k21, L_star = lstar,
    L_minus_h = lstar - r, K_independence = pi * r^2, r = r,
    lambda_1 = lam1, lambda_2 = lam2
  )
}

# Single-pattern K with the border ("reduced sample") correction, as in the
# Python arm's k_function; needed by Diggle-Chetwynd's D(h).
#' Single-pattern K with the border ("reduced sample") correction, as in
#' the
#'
#' Python arm\'s k_function; needed by Diggle-Chetwynd\'s D(h).
#'
#' @param p A matrix; indexed by row and column.
#' @param region A vector; indexed elementwise.
#' @param r Iterated over elementwise, with \code{vapply}.
#' @return A vector, from \code{vapply}.
#' @export
.schab_k_border <- function(p, region, r) {
  p <- matrix(as.numeric(p), ncol = 2)
  n <- nrow(p)
  area <- (region[3] - region[1]) * (region[4] - region[2])
  lam <- n / area
  d <- sqrt(outer(p[, 1], p[, 1], "-")^2 + outer(p[, 2], p[, 2], "-")^2)
  diag(d) <- Inf
  db <- pmin(
    pmin(p[, 1] - region[1], region[3] - p[, 1]),
    pmin(p[, 2] - region[2], region[4] - p[, 2])
  )
  vapply(r, function(h) {
    keep <- db > h
    m <- sum(keep)
    if (m == 0L) {
      return(NA_real_)
    }
    sum(d[keep, , drop = FALSE] <= h) / m / lam
  }, numeric(1))
}

#' .schab_dc_d
#'
#' A step of the schab_rest_shared implementation. Called by \code{spkcrs}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p1 Passed to \code{.schab_k_border}.
#' @param p2 Passed to \code{.schab_k_border}.
#' @param region Passed to \code{.schab_k_border}.
#' @param r Passed to \code{.schab_k_border}.
#' @return A list with \code{D}, \code{K_11}, \code{K_22}, \code{r}.
#' @export
.schab_dc_d <- function(p1, p2, region, r) {
  k11 <- .schab_k_border(p1, region, r)
  k22 <- .schab_k_border(p2, region, r)
  list(D = k11 - k22, K_11 = k11, K_22 = k22, r = r)
}

# --- Ch 4: periodogram ------------------------------------------------------

#' .schab_lattice_check
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_periodogram},
#' \code{.schab_periodogram_from_cov}, \code{.schab_sample_cov2d}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A matrix; passed to \code{nrow}.
#' @return The value of \code{z}, as built in the body.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .schab_lattice_check(z = X)
#' res
.schab_lattice_check <- function(z) {
  z <- as.matrix(z)
  if (nrow(z) < 2L || ncol(z) < 2L) stop("lattice must be at least 2x2")
  if (any(!is.finite(z))) stop("lattice contains non-finite values")
  z
}

#' .schab_fourier_freq
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_periodogram},
#' \code{.schab_periodogram_from_cov}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param r Numeric; combined arithmetically in the body.
#' @param c Numeric; combined arithmetically in the body.
#' @return A list with \code{w1}, \code{w2}, \code{j}, \code{k}.
#' @export
.schab_fourier_freq <- function(r, c) {
  j <- seq.int(-((r - 1) %/% 2), r %/% 2)
  k <- seq.int(-((c - 1) %/% 2), c %/% 2)
  list(w1 = 2 * pi * j / r, w2 = 2 * pi * k / c, j = j, k = k)
}

#' .schab_sample_cov2d
#'
#' A step of the schab_rest_shared implementation. Called by
#' \code{.schab_periodogram_from_cov}, \code{spperiod}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A matrix; passed to \code{nrow}.
#' @return A list with \code{cov}, \code{lags_j}, \code{lags_k}.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .schab_sample_cov2d(z = X)
#' res
.schab_sample_cov2d <- function(z) {
  z <- .schab_lattice_check(z)
  r <- nrow(z)
  c_ <- ncol(z)
  d <- z - mean(z)
  lags_j <- seq.int(-(r - 1), r - 1)
  lags_k <- seq.int(-(c_ - 1), c_ - 1)
  out <- matrix(0, length(lags_j), length(lags_k))
  for (a in seq_along(lags_j)) {
    jj <- lags_j[a]
    for (b in seq_along(lags_k)) {
      kk <- lags_k[b]
      u0 <- max(0L, -jj)
      u1 <- min(r, r - jj)
      v0 <- max(0L, -kk)
      v1 <- min(c_, c_ - kk)
      if (u1 <= u0 || v1 <= v0) next
      left <- d[(u0 + 1):u1, (v0 + 1):v1, drop = FALSE]
      right <- d[(u0 + jj + 1):(u1 + jj), (v0 + kk + 1):(v1 + kk),
        drop = FALSE
      ]
      out[a, b] <- sum(left * right) / (r * c_)
    }
  }
  list(cov = out, lags_j = lags_j, lags_k = lags_k)
}

#' .schab_periodogram
#'
#' A step of the schab_rest_shared implementation. Called by \code{spperiod}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A matrix; passed to \code{nrow}.
#' @param omit_zero_frequency A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{periodogram}, \code{omega1}, \code{omega2}, \code{j},
#' \code{k}, \code{zero_index}, \code{nonzero_mask}, \code{mean_invariant}, \code{r},
#' \code{c}.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .schab_periodogram(z = X)
#' res
.schab_periodogram <- function(z, omit_zero_frequency = TRUE) {
  z <- .schab_lattice_check(z)
  r <- nrow(z)
  c_ <- ncol(z)
  f <- .schab_fourier_freq(r, c_)
  u <- seq_len(r)
  v <- seq_len(c_)
  spec <- function(field) {
    eu <- exp(-1i * outer(f$w1, u))
    ev <- exp(-1i * outer(f$w2, v))
    amp <- eu %*% field %*% t(ev)
    Mod(amp)^2 / ((2 * pi)^2 * r * c_)
  }
  inten <- spec(z)
  inten_c <- spec(z - mean(z))
  zj <- which(f$j == 0L)
  zk <- which(f$k == 0L)
  mask <- matrix(TRUE, length(f$w1), length(f$w2))
  mask[zj, ] <- FALSE
  mask[, zk] <- FALSE
  invariant <- isTRUE(all.equal(inten[mask], inten_c[mask], tolerance = 1e-9))
  list(
    periodogram = if (omit_zero_frequency) inten_c else inten,
    omega1 = f$w1, omega2 = f$w2, j = f$j, k = f$k,
    zero_index = c(zj, zk), nonzero_mask = mask,
    mean_invariant = invariant, r = r, c = c_
  )
}

#' .schab_periodogram_from_cov
#'
#' A step of the schab_rest_shared implementation. Called by \code{spperiod}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A matrix; passed to \code{nrow}.
#' @return A list with \code{periodogram}, \code{omega1}, \code{omega2},
#' \code{covariance}, \code{lags_j}, \code{lags_k}.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .schab_periodogram_from_cov(z = X)
#' res
.schab_periodogram_from_cov <- function(z) {
  z <- .schab_lattice_check(z)
  f <- .schab_fourier_freq(nrow(z), ncol(z))
  sc <- .schab_sample_cov2d(z)
  out <- matrix(0, length(f$w1), length(f$w2))
  for (a in seq_along(f$w1)) {
    for (b in seq_along(f$w2)) {
      ang <- outer(f$w1[a] * sc$lags_j, f$w2[b] * sc$lags_k, "+")
      out[a, b] <- sum(sc$cov * cos(ang)) / (2 * pi)^2
    }
  }
  list(
    periodogram = out, omega1 = f$w1, omega2 = f$w2,
    covariance = sc$cov, lags_j = sc$lags_j, lags_k = sc$lags_k
  )
}

# --- Ch 8: point source + moving windows ------------------------------------

#' .schab_point_source_corr
#'
#' A step of the schab_rest_shared implementation. Called by \code{spnst}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param source Coerced to numeric by the body, with \code{as.numeric}.
#' @param theta1 Numeric; combined arithmetically in the body.
#' @param theta2 Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param theta3 Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param anisotropy Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param source_anisotropy Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_point_source_corr <- function(coords, source, theta1, theta2 = 0,
                                     theta3 = 0, anisotropy = NULL,
                                     source_anisotropy = NULL) {
  s <- as.matrix(coords)
  cvec <- as.numeric(source)
  if (length(cvec) != ncol(s)) {
    stop("source dimension does not match the coordinates")
  }
  if (theta1 <= 0) stop("theta1 must be positive (Sec. 8.2.1)")
  if (theta2 < 0 || theta3 < 0) {
    stop("theta2 and theta3 must be non-negative (Sec. 8.2.1)")
  }
  if (is.null(anisotropy)) {
    h <- as.matrix(stats::dist(s))
  } else {
    h <- as.matrix(stats::dist(s %*% t(as.matrix(anisotropy))))
  }
  dc <- sweep(s, 2L, cvec)
  if (!is.null(source_anisotropy)) {
    dc <- dc %*% t(as.matrix(source_anisotropy))
  }
  ci <- sqrt(rowSums(dc^2))
  inflate <- exp(theta2 * abs(outer(ci, ci, "-")) +
    theta3 * outer(ci, ci, pmin))
  corr <- exp(-theta1 * h * inflate)
  diag(corr) <- 1
  eig <- min(eigen(0.5 * (corr + t(corr)),
    symmetric = TRUE,
    only.values = TRUE
  )$values)
  out <- list(
    correlation = corr, source_distance = ci, separation = h,
    min_eigenvalue = eig, valid = eig >= -1e-10,
    theta = c(theta1, theta2, theta3)
  )
  if (!out$valid) {
    out$warning <- paste0(
      "the correlation matrix is not positive semi-definite (minimum ",
      sprintf("eigenvalue %.3e", eig), "). Sec. 8.2.1 notes that the ",
      "parameter constraints are necessary but not sufficient"
    )
  }
  out
}

#' .schab_practical_range
#'
#' A step of the schab_rest_shared implementation. Called by \code{spnst}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param theta1 Numeric; combined arithmetically in the body.
#' @param theta2 Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param theta3 Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param ci Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param cj Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.schab_practical_range <- function(theta1, theta2 = 0, theta3 = 0,
                                   ci = NULL, cj = NULL) {
  if (theta1 <= 0) stop("theta1 must be positive")
  if (is.null(ci) && is.null(cj)) {
    return(3 / theta1)
  }
  3 * exp(-theta2 * abs(ci - cj) - theta3 * pmin(ci, cj)) / theta1
}

#' .schab_haas_window
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param target Coerced to numeric by the body, with \code{as.numeric}.
#' @param min_sites Coerced to integer by the body, with \code{as.integer}. Defaults to \code{35L}.
#' @param step Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.
#' @param lag_classes Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param max_sites Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return The value of \code{repeat}.
#' @export
.schab_haas_window <- function(coords, target, min_sites = 35L, step = 5L,
                               lag_classes = NULL, max_sites = NULL) {
  s <- as.matrix(coords)
  n <- nrow(s)
  if (min_sites < 2L) stop("min_sites must be at least 2")
  d <- sqrt(rowSums(sweep(s, 2L, as.numeric(target))^2))
  ord <- order(d)
  k <- min(as.integer(min_sites), n)
  cap <- if (is.null(max_sites)) n else min(n, as.integer(max_sites))
  repeat {
    idx <- ord[seq_len(k)]
    radius <- max(d[idx])
    pd <- as.matrix(stats::dist(s[idx, , drop = FALSE]))
    h <- pd[upper.tri(pd)]
    nlag <- if (is.null(lag_classes)) {
      max(1L, as.integer(floor(sqrt(length(h)))))
    } else {
      as.integer(lag_classes)
    }
    if (length(h) && max(h) > 0) {
      edges <- seq(0, max(h), length.out = nlag + 1L)
      counts <- tabulate(findInterval(h, edges, rightmost.closed = TRUE),
        nbins = nlag
      )
      filled <- all(counts > 0)
    } else {
      counts <- integer(nlag)
      filled <- FALSE
    }
    if (filled || k >= cap) {
      return(list(
        index = idx, n_sites = k, radius = radius,
        lag_counts = counts, all_lag_classes_filled = filled,
        reached_cap = (k >= cap && !filled)
      ))
    }
    k <- min(cap, k + as.integer(step))
  }
}

#' .schab_empirical_variogram
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param z A vector; indexed elementwise.
#' @param n_lags A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.
#' @return A list with \code{h}, \code{gamma}, \code{counts}.
#' @export
.schab_empirical_variogram <- function(coords, z, n_lags = 10L) {
  s <- as.matrix(coords)
  z <- as.numeric(z)
  pd <- as.matrix(stats::dist(s))
  ut <- upper.tri(pd)
  h <- pd[ut]
  ij <- which(ut, arr.ind = TRUE)
  sq <- 0.5 * (z[ij[, 1]] - z[ij[, 2]])^2
  if (!length(h) || max(h) <= 0) {
    return(list(h = numeric(0), gamma = numeric(0), counts = numeric(0)))
  }
  n_lags <- as.integer(n_lags)
  edges <- seq(0, max(h), length.out = n_lags + 1L)
  which_bin <- pmin(pmax(findInterval(h, edges), 1L), n_lags)
  hbar <- numeric(n_lags)
  gbar <- rep(NA_real_, n_lags)
  cnt <- numeric(n_lags)
  for (b in seq_len(n_lags)) {
    m <- which_bin == b
    cnt[b] <- sum(m)
    if (cnt[b]) {
      hbar[b] <- mean(h[m])
      gbar[b] <- mean(sq[m])
    }
  }
  list(h = hbar, gamma = gbar, counts = cnt)
}

#' .schab_variogram_wls
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param h A vector; indexed elementwise.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}.
#' @param counts Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{sill}, \code{range}, \code{converged}, \code{wls}.
#' @export
.schab_variogram_wls <- function(h, gamma, counts) {
  h <- as.numeric(h)
  g <- as.numeric(gamma)
  w <- as.numeric(counts)
  ok <- is.finite(g) & (w > 0) & (h > 0)
  if (sum(ok) < 2L) {
    return(list(sill = NA_real_, range = NA_real_, converged = FALSE))
  }
  h <- h[ok]
  g <- g[ok]
  w <- w[ok]
  grid <- exp(seq(log(max(min(h), 1e-9)), log(max(h) * 3), length.out = 60L))
  best <- NULL
  loss <- Inf
  for (rng in grid) {
    basis <- 1 - exp(-3 * h / rng)
    denom <- sum(w * basis * basis)
    if (denom <= 0) next
    sill <- sum(w * basis * g) / denom
    if (sill <= 0) next
    resid <- g - sill * basis
    val <- sum(w * resid * resid)
    if (val < loss) {
      loss <- val
      best <- c(sill, rng)
    }
  }
  if (is.null(best)) {
    return(list(sill = NA_real_, range = NA_real_, converged = FALSE))
  }
  list(sill = best[1], range = best[2], converged = TRUE, wls = loss)
}

#' .schab_krige_at
#'
#' A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param z Numeric; combined arithmetically in the body.
#' @param target Coerced to numeric by the body, with \code{as.numeric}.
#' @param sill Numeric; combined arithmetically in the body.
#' @param rng Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.schab_krige_at <- function(coords, z, target, sill, rng, mu) {
  s <- as.matrix(coords)
  z <- as.numeric(z)
  d <- as.matrix(stats::dist(s))
  covm <- sill * exp(-3 * d / rng)
  c0 <- sill * exp(-3 * sqrt(rowSums(sweep(s, 2L, as.numeric(target))^2)) / rng)
  sol <- tryCatch(solve(covm + 1e-10 * diag(nrow(s)), z - mu),
    error = function(e) qr.solve(covm, z - mu)
  )
  mu + sum(c0 * sol)
}

#' .schab_moving_window_krige
#'
#' A step of the schab_rest_shared implementation. Called by \code{spmwst}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @param z A vector; its length is taken and its elements indexed.
#' @param targets A matrix; passed to \code{as.matrix}.
#' @param min_sites Passed to \code{.schab_haas_window}. Defaults to \code{35L}.
#' @param step Passed to \code{.schab_haas_window}. Defaults to \code{5L}.
#' @param n_lags Passed to \code{.schab_empirical_variogram}. Defaults to \code{10L}.
#' @param local_mean A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param local_variogram A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{prediction}, \code{local_sill}, \code{local_range},
#' \code{window_sizes}, \code{converged}, \code{theta_is_global}, \code{global_sill},
#' \code{global_range}, \code{caveats}.
#' @export
.schab_moving_window_krige <- function(coords, z, targets, min_sites = 35L,
                                       step = 5L, n_lags = 10L,
                                       local_mean = FALSE,
                                       local_variogram = TRUE) {
  s <- as.matrix(coords)
  z <- as.numeric(z)
  tg <- as.matrix(targets)
  if (nrow(s) != length(z)) stop("coordinates and observations disagree on n")
  gv <- .schab_empirical_variogram(s, z, n_lags)
  global_fit <- .schab_variogram_wls(gv$h, gv$gamma, gv$counts)
  m <- nrow(tg)
  preds <- sills <- ranges <- sizes <- numeric(m)
  conv <- logical(m)
  for (i in seq_len(m)) {
    win <- .schab_haas_window(s, tg[i, ], min_sites = min_sites, step = step)
    idx <- win$index
    if (local_variogram) {
      lv <- .schab_empirical_variogram(s[idx, , drop = FALSE], z[idx], n_lags)
      fit <- .schab_variogram_wls(lv$h, lv$gamma, lv$counts)
      if (!fit$converged) fit <- global_fit
    } else {
      fit <- global_fit
    }
    mu <- if (local_mean) mean(z[idx]) else mean(z)
    preds[i] <- .schab_krige_at(
      s[idx, , drop = FALSE], z[idx], tg[i, ],
      fit$sill, fit$range, mu
    )
    sills[i] <- fit$sill
    ranges[i] <- fit$range
    sizes[i] <- win$n_sites
    conv[i] <- isTRUE(fit$converged)
  }
  list(
    prediction = preds, local_sill = sills, local_range = ranges,
    window_sizes = sizes, converged = conv,
    theta_is_global = !local_variogram,
    global_sill = global_fit$sill, global_range = global_fit$range,
    caveats = paste0(
      "a predictor that excludes observed sites is no longer best; ",
      "windows that change with prediction location can introduce ",
      "spurious discontinuities (Sec. 8.3.1)"
    )
  )
}
