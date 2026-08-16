# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Spatio-temporal covariance, semivariogram and point-process primitives.
# Schabenberger & Gotway (2005), Chapter 9. Twin of the Python arm's
# src/morie/fn/_schab_st.py -- one implementation per book equation, and the
# SAME arithmetic in both languages rather than merely the same answer to
# plotting accuracy.
#
# That is why the Bessel functions below are written out instead of calling
# base R's besselJ() and besselK(). Those are correct, but they are a
# different algorithm from the quadrature the Python arm uses, so the two
# sides would agree only to a tolerance nobody had chosen. Cross-language
# verification of a simulation needs the arms to run the same steps.
#
# Sec. 9.1 is explicit that treating spatio-temporal data as a field in
# R^{d+1} "is not encouraged": eq (9.2) shows the naive isotropic exponential
# in R^3 is a valid correlation function that makes no practical sense,
# because the spatial range, the temporal range and the units are not
# comparable. So nothing here concatenates t onto the coordinate vector --
# ||h|| and |k| are carried separately through every function.
#
# Internal; `aaa_` collates it before its callers.

#' .schab_st_as_lags
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @return A list with \code{h}, \code{k}.
#' @export
.schab_st_as_lags <- function(h, k) {
  h <- as.numeric(h)
  k <- abs(as.numeric(k))
  if (any(h < 0)) stop("spatial lag `h` must be non-negative", call. = FALSE)
  n <- max(length(h), length(k))
  list(h = rep_len(h, n), k = rep_len(k, n))
}

#' .schab_st_lag_matrices
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param coords See Usage.
#' @param times See Usage.
#' @return A list with \code{d}, \code{k}.
#' @export
.schab_st_lag_matrices <- function(coords, times) {
  coords <- as.matrix(coords)
  times <- as.numeric(times)
  if (nrow(coords) != length(times)) {
    stop("`coords` and `times` must have the same length", call. = FALSE)
  }
  d <- as.matrix(stats::dist(coords))
  k <- abs(outer(times, times, "-"))
  list(d = d, k = k)
}

# --- Sec. 9.2, separable covariance functions ------------------------------

#' .schab_st_separable_covariance
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param cov_spatial See Usage.
#' @param cov_temporal See Usage.
#' @param form Defaults to \code{"product"}.
#' @return The value of \code{switch}.
#' @export
.schab_st_separable_covariance <- function(h, k, cov_spatial, cov_temporal,
                                           form = "product") {
  lg <- .schab_st_as_lags(h, k)
  cs <- as.numeric(cov_spatial(lg$h))
  ct <- as.numeric(cov_temporal(lg$k))
  switch(form,
    product = cs * ct,
    sum = cs + ct,
    # De Cesare, Myers and Posa (2001); the text calls it generally
    # nonseparable even though it appears in the separable section
    product_sum = cs * ct + cs + ct,
    stop("`form` must be 'product', 'sum' or 'product_sum'", call. = FALSE)
  )
}

#' .schab_st_is_separable
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param form See Usage.
#' @return Nothing; this branch always raises.
#' @export
.schab_st_is_separable <- function(form) {
  if (form %in% c("product", "sum")) {
    return(TRUE)
  }
  if (identical(form, "product_sum")) {
    return(FALSE)
  }
  stop(sprintf("unknown form '%s'", form), call. = FALSE)
}

#' .schab_st_anisotropic_correlation
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param theta_s See Usage.
#' @param theta_t See Usage.
#' @param corr_fn See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_st_anisotropic_correlation <- function(h, k, theta_s, theta_t, corr_fn) {
  lg <- .schab_st_as_lags(h, k)
  if (theta_s <= 0 || theta_t <= 0) {
    stop("anisotropy parameters must be positive", call. = FALSE)
  }
  as.numeric(corr_fn(theta_s * lg$h^2 + theta_t * lg$k^2)) # eq (9.3)
}

#' .schab_st_exponential_separable
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param theta_s See Usage.
#' @param theta_t See Usage.
#' @return A numeric value.
#' @export
.schab_st_exponential_separable <- function(h, k, theta_s, theta_t) {
  lg <- .schab_st_as_lags(h, k) # eq (9.4)
  if (theta_s <= 0 || theta_t <= 0) {
    stop("`theta_s` and `theta_t` must be positive", call. = FALSE)
  }
  exp(-theta_s * lg$h) * exp(-theta_t * lg$k)
}

# --- Sec. 9.3.1, Gneiting's monotone construction --------------------------

#' .schab_st_gneiting
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param sigma2 Defaults to \code{1}.
#' @param a Defaults to \code{1}.
#' @param c Defaults to \code{1}.
#' @param alpha Defaults to \code{1}.
#' @param beta Defaults to \code{1}.
#' @param gamma Defaults to \code{1}.
#' @param d Defaults to \code{2}.
#' @return A numeric value.
#' @export
.schab_st_gneiting <- function(h, k, sigma2 = 1, a = 1, c = 1, alpha = 1,
                               beta = 1, gamma = 1, d = 2) {
  lg <- .schab_st_as_lags(h, k)
  if (a <= 0 || c <= 0) stop("`a` and `c` must be positive", call. = FALSE)
  if (!(gamma > 0 && gamma <= 1)) stop("`gamma` must lie in (0, 1]", call. = FALSE)
  if (!(alpha > 0 && alpha <= 1)) stop("`alpha` must lie in (0, 1]", call. = FALSE)
  if (!(beta >= 0 && beta <= 1)) stop("`beta` must lie in [0, 1]", call. = FALSE)
  if (sigma2 < 0) stop("`sigma2` must be non-negative", call. = FALSE)
  psi <- a * lg$k^(2 * alpha) + 1 # eq (9.8)
  (sigma2 / psi^(beta * d / 2)) * exp(-c * lg$h^(2 * gamma) / psi^(beta * gamma))
}

#' .schab_st_gneiting_with_temporal
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param sigma2 Defaults to \code{1}.
#' @param a Defaults to \code{1}.
#' @param c Defaults to \code{1}.
#' @param alpha Defaults to \code{1}.
#' @param beta Defaults to \code{1}.
#' @param beta_t Defaults to \code{1}.
#' @param gamma Defaults to \code{1}.
#' @param d Defaults to \code{2}.
#' @return A numeric value.
#' @export
.schab_st_gneiting_with_temporal <- function(h, k, sigma2 = 1, a = 1, c = 1,
                                             alpha = 1, beta = 1, beta_t = 1,
                                             gamma = 1, d = 2) {
  lg <- .schab_st_as_lags(h, k)
  if (beta_t < 0) stop("`beta_t` must be non-negative", call. = FALSE)
  if (a <= 0 || c <= 0) stop("`a` and `c` must be positive", call. = FALSE)
  if (!(gamma > 0 && gamma <= 1) || !(alpha > 0 && alpha <= 1) ||
    !(beta >= 0 && beta <= 1)) {
    stop("Gneiting parameter bounds violated", call. = FALSE)
  }
  psi <- a * lg$k^(2 * alpha) + 1 # eq (9.9)
  (sigma2 / psi^(beta_t + beta * d / 2)) *
    exp(-c * lg$h^(2 * gamma) / psi^(beta * gamma))
}

#' Sec. 6.2.3 states the rule outright: on the boundary of the parameter
#'
#' space the statistic is a mixture of a degenerate distribution at zero
#' and a chi-square, so "simply divide the p-value ... by 2" (Self and
#' Liang, 1987; Littell, Milliken, Stroup and Wolfinger, 1996).
#'
#' @param neg2_unrestricted See Usage.
#' @param neg2_separable See Usage.
#' @return A list with \code{statistic}, \code{p_value}, \code{p_value_naive_chi2_1}, \code{reference}.
#' @export
.schab_st_separability_test <- function(neg2_unrestricted, neg2_separable) {
  # Sec. 6.2.3 states the rule outright: on the boundary of the parameter
  # space the statistic is a mixture of a degenerate distribution at zero and
  # a chi-square, so "simply divide the p-value ... by 2" (Self and Liang,
  # 1987; Littell, Milliken, Stroup and Wolfinger, 1996).
  stat <- max(0, as.numeric(neg2_separable) - as.numeric(neg2_unrestricted))
  p_naive <- .schab_st_chi2_sf(stat, 1)
  list(
    statistic = stat, p_value = 0.5 * p_naive,
    p_value_naive_chi2_1 = p_naive,
    reference = "0.5 chi^2_0 + 0.5 chi^2_1 (Self and Liang, 1987)"
  )
}

# --- Sec. 9.3.3, Ma's mixtures ---------------------------------------------

#' .schab_st_power_mixture
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param rs See Usage.
#' @param rt See Usage.
#' @param distribution Defaults to \code{"poisson"}.
#' @param ... Passed through.
#' @return Nothing; this branch always raises.
#' @export
.schab_st_power_mixture <- function(rs, rt, distribution = "poisson", ...) {
  rs <- as.numeric(rs)
  rt <- as.numeric(rt)
  p <- list(...)
  if (any(abs(rs) > 1 + 1e-12) || any(abs(rt) > 1 + 1e-12)) {
    stop("`rs` and `rt` must be correlations in [-1, 1]", call. = FALSE)
  }
  w <- rs * rt # eq (9.14): the pgf evaluated at w = Rs(h) Rt(k)
  if (identical(distribution, "poisson")) {
    lam <- if (is.null(p$lam)) 1 else as.numeric(p$lam)
    if (lam <= 0) stop("`lam` must be positive", call. = FALSE)
    return(exp(lam * (w - 1)))
  }
  if (identical(distribution, "binomial")) {
    n <- if (is.null(p$n)) 1L else as.integer(p$n)
    pi_ <- if (is.null(p$pi)) 0.5 else as.numeric(p$pi)
    if (n < 1 || pi_ < 0 || pi_ > 1) {
      stop("`n` >= 1 and `pi` in [0, 1] required", call. = FALSE)
    }
    return((pi_ * (w - 1) + 1)^n)
  }
  stop("`distribution` must be 'poisson' or 'binomial'", call. = FALSE)
}

#' .schab_st_bivariate_power_mixture
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param rs See Usage.
#' @param rt See Usage.
#' @param pmf See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_st_bivariate_power_mixture <- function(rs, rt, pmf) {
  rs <- as.numeric(rs)
  rt <- as.numeric(rt)
  pmf <- as.matrix(pmf)
  if (any(pmf < 0)) stop("`pmf` must be non-negative", call. = FALSE)
  if (!isTRUE(all.equal(sum(pmf), 1, tolerance = 1e-8))) {
    stop(sprintf("`pmf` must sum to 1 (got %.12g)", sum(pmf)), call. = FALSE)
  }
  n <- max(length(rs), length(rt))
  rs <- rep_len(rs, n)
  rt <- rep_len(rt, n)
  out <- numeric(n) # eq (9.13)
  for (i in seq_len(nrow(pmf))) {
    for (j in seq_len(ncol(pmf))) {
      if (pmf[i, j] == 0) next
      out <- out + rs^(i - 1) * rt^(j - 1) * pmf[i, j]
    }
  }
  out
}

#' .schab_st_scale_mixture
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param cov_spatial See Usage.
#' @param cov_temporal See Usage.
#' @param nodes See Usage.
#' @param weights See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_st_scale_mixture <- function(h, k, cov_spatial, cov_temporal, nodes,
                                    weights) {
  lg <- .schab_st_as_lags(h, k)
  u <- as.numeric(nodes)
  w <- as.numeric(weights)
  if (length(u) != length(w)) {
    stop("`nodes` and `weights` must have the same length", call. = FALSE)
  }
  if (any(w < 0)) stop("`weights` must be non-negative (F is a d.f.)", call. = FALSE)
  if (!isTRUE(all.equal(sum(w), 1, tolerance = 1e-8))) {
    stop(sprintf("`weights` must sum to 1 (got %.12g)", sum(w)), call. = FALSE)
  }
  if (any(u < 0)) stop("scale `nodes` must be non-negative", call. = FALSE)
  out <- numeric(length(lg$h)) # eq (9.16)
  for (i in seq_along(u)) {
    out <- out + w[i] * as.numeric(cov_spatial(lg$h * u[i])) *
      as.numeric(cov_temporal(lg$k * u[i]))
  }
  out
}

# --- Sec. 9.3.4, the differential equation approach ------------------------

#' Golub and Welsch (1969), Math. Comp. 23(106):221-230 -- NOT a
#'
#' Schabenberger & Gotway result. Nodes are the eigenvalues of the
#' Jacobi matrix; weights are mu_0 times the squared first eigenvector
#' components, with mu_0 the ZEROTH MOMENT of the weight function -- 2
#' for Legendre on [-1, 1]. The Hermite rule elsewhere carries no such
#' factor because there the Gaussian weight integrates to 1.
#'
#' @param n See Usage.
#' @return A list with \code{nodes}, \code{weights}.
#' @export
.schab_gauss_legendre <- function(n) {
  # Golub and Welsch (1969), Math. Comp. 23(106):221-230 -- NOT a
  # Schabenberger & Gotway result. Nodes are the eigenvalues of the Jacobi
  # matrix; weights are mu_0 times the squared first eigenvector components,
  # with mu_0 the ZEROTH MOMENT of the weight function -- 2 for Legendre on
  # [-1, 1]. The Hermite rule elsewhere carries no such factor because there
  # the Gaussian weight integrates to 1.
  n <- as.integer(n)
  if (n < 1L) stop("`n` must be positive", call. = FALSE)
  if (n == 1L) {
    return(list(nodes = 0, weights = 2))
  }
  k <- seq_len(n - 1L)
  off <- k / sqrt(4 * k * k - 1)
  jac <- matrix(0, n, n)
  jac[cbind(k, k + 1L)] <- off
  jac[cbind(k + 1L, k)] <- off
  e <- eigen(jac, symmetric = TRUE)
  ord <- order(e$values)
  list(nodes = e$values[ord], weights = 2 * (e$vectors[1, ord])^2)
}

#' J_0(x) = (1/pi) integral_0^pi cos(x sin theta) dtheta. The integrand
#' is
#'
#' smooth and periodic so the trapezoid rule converges geometrically;
#' base R\'s besselJ() is deliberately not used, see the file header.
#'
#' @param x See Usage.
#' @param n_quad Defaults to \code{200L}.
#' @return A vector, from \code{vapply}.
#' @export
.schab_bessel_j0 <- function(x, n_quad = 200L) {
  # J_0(x) = (1/pi) integral_0^pi cos(x sin theta) dtheta. The integrand is
  # smooth and periodic so the trapezoid rule converges geometrically; base
  # R's besselJ() is deliberately not used, see the file header.
  x <- as.numeric(x)
  theta <- seq(0, pi, length.out = as.integer(n_quad) + 1L)
  wt <- rep(1, length(theta))
  wt[1] <- 0.5
  wt[length(wt)] <- 0.5
  step <- pi / as.integer(n_quad)
  vapply(x, function(xi) sum(wt * cos(xi * sin(theta))) * step / pi, numeric(1))
}

#' K_1(z) = integral_0^inf exp{-z cosh u} cosh u du
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param z See Usage.
#' @param upper Defaults to \code{40}.
#' @param n_quad Defaults to \code{400L}.
#' @return A vector, from \code{vapply}.
#' @export
.schab_bessel_k1 <- function(z, upper = 40, n_quad = 400L) {
  # K_1(z) = integral_0^inf exp{-z cosh u} cosh u du
  gl <- .schab_gauss_legendre(n_quad)
  u <- 0.5 * upper * (gl$nodes + 1)
  wu <- 0.5 * upper * gl$weights
  ch <- cosh(u)
  vapply(as.numeric(z), function(zi) sum(wu * exp(-zi * ch) * ch), numeric(1))
}

#' .schab_whittle_covariance
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param sigma2 Defaults to \code{1}.
#' @param theta Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
.schab_whittle_covariance <- function(h, sigma2 = 1, theta = 1) {
  h <- as.numeric(h) # Whittle (1954)
  if (any(h < 0)) stop("lag `h` must be non-negative", call. = FALSE)
  if (theta <= 0) stop("`theta` must be positive", call. = FALSE)
  z <- theta * h
  out <- rep(sigma2, length(z))
  pos <- z > 0
  if (any(pos)) out[pos] <- sigma2 * z[pos] * .schab_bessel_k1(z[pos])
  out
}

#' .schab_st_tail_bound_j0
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param h See Usage.
#' @param p See Usage.
#' @return A numeric value.
#' @export
.schab_st_tail_bound_j0 <- function(t, h, p) {
  if (t <= 0) {
    return(Inf)
  }
  if (h > 0 && p > 1.25) {
    return(sqrt(2 / (pi * h)) * t^(2.5 - 2 * p) / (2 * p - 2.5))
  }
  if (p > 1) {
    return(t^(2 - 2 * p) / (2 * p - 2))
  }
  Inf
}

#' .schab_st_hankel_panels
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param hval See Usage.
#' @param kval See Usage.
#' @param theta See Usage.
#' @param c See Usage.
#' @param p See Usage.
#' @param n_quad Defaults to \code{40L}.
#' @param rtol Defaults to \code{1e-10}.
#' @param max_panels Defaults to \code{20000L}.
#' @param quiet_runs Defaults to \code{4L}.
#' @return A list with \code{value}, \code{upper}, \code{last_rel}, \code{tail_bound}.
#' @export
.schab_st_hankel_panels <- function(hval, kval, theta, c, p, n_quad = 40L,
                                    rtol = 1e-10, max_panels = 20000L,
                                    quiet_runs = 4L) {
  # Panels sized to the J_0 oscillation period, run outward until several
  # consecutive panels contribute nothing. Stopping on the analytic tail
  # bound instead would never terminate: at k = 0 the integrand decays only
  # algebraically, so the bound is far too pessimistic to be met.
  gl <- .schab_gauss_legendre(n_quad)
  panel <- if (hval > 0) min(pi / hval, max(1, 2 * theta)) else max(1, 2 * theta)
  panel <- max(panel, 1e-3)
  acc <- 0
  t <- 0
  quiet <- 0L
  panels <- 0L
  last_rel <- Inf
  while (panels < max_panels) {
    a0 <- t
    b0 <- t + panel
    tau <- 0.5 * (b0 - a0) * (gl$nodes + 1) + a0
    wt <- 0.5 * (b0 - a0) * gl$weights
    q <- tau^2 + theta^2
    core <- tau * exp(-(kval / c) * q^p) / q^p
    contrib <- sum(wt * core * .schab_bessel_j0(tau * hval))
    acc <- acc + contrib
    t <- b0
    panels <- panels + 1L
    last_rel <- abs(contrib) / max(abs(acc), 1e-300)
    quiet <- if (last_rel < rtol) quiet + 1L else 0L
    if (quiet >= quiet_runs) break
  }
  list(
    value = acc, upper = t, last_rel = last_rel,
    tail_bound = .schab_st_tail_bound_j0(t, hval, p)
  )
}

#' .schab_st_jones_zhang
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param sigma2 Defaults to \code{1}.
#' @param theta Defaults to \code{1}.
#' @param c Defaults to \code{1}.
#' @param p Defaults to \code{1.5}.
#' @param d Defaults to \code{2}.
#' @param n_quad Defaults to \code{40L}.
#' @return A list with \code{covariance}, \code{quadrature}.
#' @export
.schab_st_jones_zhang <- function(h, k, sigma2 = 1, theta = 1, c = 1, p = 1.5,
                                  d = 2, n_quad = 40L) {
  lg <- .schab_st_as_lags(h, k) # eq (9.17)
  if (theta <= 0 || c <= 0 || sigma2 < 0) {
    stop("`theta` and `c` must be positive, `sigma2` >= 0", call. = FALSE)
  }
  if (p <= max(1, d / 2)) {
    stop(sprintf(
      paste0(
        "`p` must exceed max(1, d/2) = %g for the integral in ",
        "eq (9.17) to converge (got p = %g)"
      ),
      max(1, d / 2), p
    ), call. = FALSE)
  }
  vals <- numeric(length(lg$h))
  reach <- rels <- bnds <- numeric(length(lg$h))
  for (i in seq_along(lg$h)) {
    r <- .schab_st_hankel_panels(lg$h[i], lg$k[i], theta, c, p, n_quad = n_quad)
    vals[i] <- r$value
    reach[i] <- r$upper
    rels[i] <- r$last_rel
    bnds[i] <- r$tail_bound
  }
  scale <- sigma2 / (4 * c * pi)
  list(
    covariance = vals * scale,
    quadrature = list(
      upper_reached = max(reach), n_quad = as.integer(n_quad),
      last_panel_rel = max(rels),
      tail_bound = max(bnds) * scale
    )
  )
}

# --- eq (9.5), validity ----------------------------------------------------

#' .schab_st_covariance_matrix
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param coords See Usage.
#' @param times See Usage.
#' @param cov_fn See Usage.
#' @return A matrix, from \code{matrix}.
#' @export
.schab_st_covariance_matrix <- function(coords, times, cov_fn) {
  lm_ <- .schab_st_lag_matrices(coords, times)
  matrix(as.numeric(cov_fn(lm_$d, lm_$k)), nrow = nrow(lm_$d))
}

#' Eq (9.5) is positive semi-definiteness. Checked by
#' eigendecomposition,
#'
#' not by sampling random coefficient vectors: the minimum eigenvalue IS
#' the minimum of the quadratic form, so one eigensolve settles what no
#' finite number of random draws can. Sec. 9.3 records that Gneiting
#' (2002) found published covariance functions in Cressie and Huang
#' (1999) to be invalid, so construction alone is not proof.
#'
#' @param coords See Usage.
#' @param times See Usage.
#' @param cov_fn See Usage.
#' @param tol Defaults to \code{NULL}.
#' @return A list with \code{valid}, \code{min_eigenvalue}, \code{max_eigenvalue}, \code{tolerance}, \code{reason}.
#' @export
.schab_st_is_valid_covariance <- function(coords, times, cov_fn, tol = NULL) {
  # eq (9.5) is positive semi-definiteness. Checked by eigendecomposition,
  # not by sampling random coefficient vectors: the minimum eigenvalue IS the
  # minimum of the quadratic form, so one eigensolve settles what no finite
  # number of random draws can. Sec. 9.3 records that Gneiting (2002) found
  # published covariance functions in Cressie and Huang (1999) to be invalid,
  # so construction alone is not proof.
  sigma <- .schab_st_covariance_matrix(coords, times, cov_fn)
  if (!isTRUE(all.equal(sigma, t(sigma), tolerance = 1e-10))) {
    return(list(
      valid = FALSE, min_eigenvalue = NA_real_,
      reason = "covariance matrix is not symmetric"
    ))
  }
  vals <- eigen(sigma, symmetric = TRUE, only.values = TRUE)$values
  lo <- min(vals)
  scale <- max(abs(vals))
  if (is.null(tol)) tol <- -1e-10 * max(scale, 1)
  list(
    valid = lo >= tol, min_eigenvalue = lo, max_eigenvalue = max(vals),
    tolerance = tol,
    reason = if (lo >= tol) "" else "minimum eigenvalue is negative"
  )
}

# --- Sec. 9.4, the spatio-temporal semivariogram ---------------------------

#' .schab_st_semivariogram_from_cov
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param h See Usage.
#' @param k See Usage.
#' @param cov_fn See Usage.
#' @return A numeric value.
#' @export
.schab_st_semivariogram_from_cov <- function(h, k, cov_fn) {
  lg <- .schab_st_as_lags(h, k) # gamma = C(0,0) - C(h,k)
  c0 <- as.numeric(cov_fn(0, 0))[1]
  c0 - as.numeric(cov_fn(lg$h, lg$k))
}

#' .schab_st_empirical_semivariogram
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param coords See Usage.
#' @param times See Usage.
#' @param z See Usage.
#' @param n_space_bins Defaults to \code{10L}.
#' @param n_time_bins Defaults to \code{5L}.
#' @param max_dist Defaults to \code{NULL}.
#' @param max_time Defaults to \code{NULL}.
#' @return A list with \code{gamma}, \code{counts}, \code{space_lags}, \code{time_lags}, \code{space_edges}, \code{time_edges}.
#' @export
.schab_st_empirical_semivariogram <- function(coords, times, z, n_space_bins = 10L,
                                              n_time_bins = 5L, max_dist = NULL,
                                              max_time = NULL) {
  coords <- as.matrix(coords)
  times <- as.numeric(times)
  z <- as.numeric(z)
  n <- length(z)
  if (nrow(coords) != n || length(times) != n) {
    stop("`coords`, `times` and `z` must have the same length", call. = FALSE)
  }
  if (n < 2L) stop("need at least two observations", call. = FALSE)
  idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  i <- idx[, 1]
  j <- idx[, 2]
  d <- sqrt(rowSums((coords[i, , drop = FALSE] - coords[j, , drop = FALSE])^2))
  u <- abs(times[i] - times[j])
  sq <- (z[i] - z[j])^2
  if (is.null(max_dist)) max_dist <- max(d) / 2
  if (is.null(max_time)) max_time <- max(u) / 2
  if (max_dist <= 0 || max_time <= 0) {
    stop("`max_dist` and `max_time` must be positive", call. = FALSE)
  }
  keep <- d <= max_dist & u <= max_time
  d <- d[keep]
  u <- u[keep]
  sq <- sq[keep]

  ns <- as.integer(n_space_bins)
  nt <- as.integer(n_time_bins)
  d_edges <- seq(0, max_dist, length.out = ns + 1L)
  u_edges <- seq(0, max_time, length.out = nt + 1L)
  di <- pmin(pmax(findInterval(d, d_edges, rightmost.closed = FALSE), 1L), ns)
  ui <- pmin(pmax(findInterval(u, u_edges, rightmost.closed = FALSE), 1L), nt)

  counts <- matrix(0L, ns, nt)
  total <- matrix(0, ns, nt)
  for (m in seq_along(d)) {
    counts[di[m], ui[m]] <- counts[di[m], ui[m]] + 1L
    total[di[m], ui[m]] <- total[di[m], ui[m]] + sq[m]
  }
  gamma <- matrix(NA_real_, ns, nt) # eq (9.18); empty cells stay NA
  nz <- counts > 0L
  gamma[nz] <- total[nz] / (2 * counts[nz])
  list(
    gamma = gamma, counts = counts,
    space_lags = 0.5 * (d_edges[-1] + d_edges[-length(d_edges)]),
    time_lags = 0.5 * (u_edges[-1] + u_edges[-length(u_edges)]),
    space_edges = d_edges, time_edges = u_edges
  )
}

#' .schab_st_conditional_semivariogram
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param coords See Usage.
#' @param times See Usage.
#' @param z See Usage.
#' @param at_time See Usage.
#' @param n_bins Defaults to \code{10L}.
#' @param max_dist Defaults to \code{NULL}.
#' @param tol Defaults to \code{0}.
#' @return A list with \code{gamma}, \code{counts}, \code{n_at_time}, \code{lags}, \code{edges}.
#' @export
.schab_st_conditional_semivariogram <- function(coords, times, z, at_time,
                                                n_bins = 10L, max_dist = NULL,
                                                tol = 0) {
  coords <- as.matrix(coords)
  times <- as.numeric(times)
  z <- as.numeric(z)
  sel <- abs(times - as.numeric(at_time)) <= as.numeric(tol)
  if (sum(sel) < 2L) {
    stop(sprintf("fewer than two observations at time %g", at_time), call. = FALSE)
  }
  cc <- coords[sel, , drop = FALSE]
  y <- z[sel]
  m <- length(y)
  idx <- which(upper.tri(matrix(0, m, m)), arr.ind = TRUE)
  i <- idx[, 1]
  j <- idx[, 2]
  d <- sqrt(rowSums((cc[i, , drop = FALSE] - cc[j, , drop = FALSE])^2))
  sq <- (y[i] - y[j])^2 # eq (9.19)
  if (is.null(max_dist)) max_dist <- max(d) / 2
  keep <- d <= max_dist
  d <- d[keep]
  sq <- sq[keep]
  nb <- as.integer(n_bins)
  edges <- seq(0, max_dist, length.out = nb + 1L)
  bi <- pmin(pmax(findInterval(d, edges, rightmost.closed = FALSE), 1L), nb)
  counts <- integer(nb)
  total <- numeric(nb)
  for (q in seq_along(d)) {
    counts[bi[q]] <- counts[bi[q]] + 1L
    total[bi[q]] <- total[bi[q]] + sq[q]
  }
  gamma <- rep(NA_real_, nb)
  nz <- counts > 0L
  gamma[nz] <- total[nz] / (2 * counts[nz])
  list(
    gamma = gamma, counts = counts, n_at_time = sum(sel),
    lags = 0.5 * (edges[-1] + edges[-length(edges)]), edges = edges
  )
}

#' .schab_st_wls_objective
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param emp See Usage.
#' @param model_fn See Usage.
#' @return A numeric value.
#' @export
.schab_st_wls_objective <- function(emp, model_fn) {
  gamma_hat <- emp$gamma
  counts <- emp$counts
  hh <- outer(emp$space_lags, rep(1, length(emp$time_lags)))
  kk <- outer(rep(1, length(emp$space_lags)), emp$time_lags)
  model <- matrix(as.numeric(model_fn(as.numeric(hh), as.numeric(kk))),
    nrow = nrow(gamma_hat)
  )
  ok <- counts > 0L & is.finite(gamma_hat) & is.finite(model) & model > 0
  if (!any(ok)) {
    return(Inf)
  }
  resid <- gamma_hat[ok] - model[ok]
  sum(counts[ok] / (2 * model[ok]^2) * resid^2)
}

# --- Sec. 9.5, spatio-temporal point processes -----------------------------

#' .schab_st_region_box
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param region See Usage.
#' @return The value of \code{r}, as built in the body.
#' @export
.schab_st_region_box <- function(region) {
  r <- as.numeric(region)
  if (length(r) != 4L) stop("`region` must be (xmin, xmax, ymin, ymax)", call. = FALSE)
  if (r[2] <= r[1] || r[4] <= r[3]) {
    stop("`region` must have positive extent", call. = FALSE)
  }
  r
}

#' .schab_st_intensity
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param points See Usage.
#' @param times See Usage.
#' @param region See Usage.
#' @param time_interval See Usage.
#' @return A list with \code{intensity}, \code{n}, \code{area}, \code{duration}, \code{volume}.
#' @export
.schab_st_intensity <- function(points, times, region, time_interval) {
  pts <- as.matrix(points)
  t <- as.numeric(times)
  if (nrow(pts) != length(t)) {
    stop("`points` and `times` must have the same length", call. = FALSE)
  }
  r <- .schab_st_region_box(region)
  area <- (r[2] - r[1]) * (r[4] - r[3])
  span <- as.numeric(time_interval[2]) - as.numeric(time_interval[1])
  if (span <= 0) stop("`time_interval` must have positive length", call. = FALSE)
  list(
    intensity = length(t) / (area * span), n = length(t), # eq (9.20)
    area = area, duration = span, volume = area * span
  )
}

#' .schab_st_marginal_intensities
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param points See Usage.
#' @param times See Usage.
#' @param region See Usage.
#' @param time_interval See Usage.
#' @param n_space_bins Defaults to \code{4L}.
#' @param n_time_bins Defaults to \code{4L}.
#' @return A list with \code{marginal_spatial}, \code{marginal_temporal}, \code{cell_area}, \code{bin_width}, \code{x_edges}, \code{y_edges}, \code{t_edges}.
#' @export
.schab_st_marginal_intensities <- function(points, times, region, time_interval,
                                           n_space_bins = 4L, n_time_bins = 4L) {
  pts <- as.matrix(points)
  t <- as.numeric(times)
  r <- .schab_st_region_box(region)
  t0 <- as.numeric(time_interval[1])
  t1 <- as.numeric(time_interval[2])
  ns <- as.integer(n_space_bins)
  nt <- as.integer(n_time_bins)
  xe <- seq(r[1], r[2], length.out = ns + 1L)
  ye <- seq(r[3], r[4], length.out = ns + 1L)
  te <- seq(t0, t1, length.out = nt + 1L)
  cell_area <- (xe[2] - xe[1]) * (ye[2] - ye[1])
  bin_width <- te[2] - te[1]
  xi <- pmin(pmax(findInterval(pts[, 1], xe), 1L), ns)
  yi <- pmin(pmax(findInterval(pts[, 2], ye), 1L), ns)
  ti <- pmin(pmax(findInterval(t, te), 1L), nt)
  spatial <- matrix(0, ns, ns)
  temporal <- numeric(nt)
  for (m in seq_len(nrow(pts))) {
    spatial[xi[m], yi[m]] <- spatial[xi[m], yi[m]] + 1
    temporal[ti[m]] <- temporal[ti[m]] + 1
  }
  list(
    marginal_spatial = spatial / cell_area, # eqs (9.21), (9.22)
    marginal_temporal = temporal / bin_width,
    cell_area = cell_area, bin_width = bin_width,
    x_edges = xe, y_edges = ye, t_edges = te
  )
}

#' .schab_cstr_reference
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param area See Usage.
#' @param duration See Usage.
#' @param lam See Usage.
#' @return A list with \code{expected_count}, \code{variance}, \code{intensity}, \code{second_order_intensity}, \code{volume}.
#' @export
.schab_cstr_reference <- function(area, duration, lam) {
  area <- as.numeric(area)
  duration <- as.numeric(duration)
  lam <- as.numeric(lam)
  if (area <= 0 || duration <= 0 || lam < 0) {
    stop("`area`, `duration` must be positive and `lam` >= 0", call. = FALSE)
  }
  mean_ <- lam * area * duration # N(A,T) ~ Poisson(lambda |A x T|)
  list(
    expected_count = mean_, variance = mean_, intensity = lam,
    second_order_intensity = lam^2, volume = area * duration
  )
}

#' .schab_cstr_test
#'
#' Part of the schab_st_shared implementation; see the file header for
#' the source it follows.
#'
#' @param points See Usage.
#' @param times See Usage.
#' @param region See Usage.
#' @param time_interval See Usage.
#' @param n_space_bins Defaults to \code{3L}.
#' @param n_time_bins Defaults to \code{3L}.
#' @return A list with \code{index_of_dispersion}, \code{df}, \code{p_value}, \code{counts}, \code{mean_count}, \code{var_count}.
#' @export
.schab_cstr_test <- function(points, times, region, time_interval,
                             n_space_bins = 3L, n_time_bins = 3L) {
  # Chapter 9 defines the CSTR benchmark but gives no test. This is the
  # book's own quadrat statistic for CSR, Sec. 3.3 eq (3.3), in the form
  # X^2 = (rc-1) s^2 / nbar with s^2 the SAMPLE variance, reference
  # chi^2_{rc-1}; extended from quadrats in D to cells in D x T by the
  # analogy Sec. 9.5.3 itself draws.
  pts <- as.matrix(points)
  t <- as.numeric(times)
  r <- .schab_st_region_box(region)
  t0 <- as.numeric(time_interval[1])
  t1 <- as.numeric(time_interval[2])
  ns <- as.integer(n_space_bins)
  nt <- as.integer(n_time_bins)
  xe <- seq(r[1], r[2], length.out = ns + 1L)
  ye <- seq(r[3], r[4], length.out = ns + 1L)
  te <- seq(t0, t1, length.out = nt + 1L)
  xi <- pmin(pmax(findInterval(pts[, 1], xe), 1L), ns)
  yi <- pmin(pmax(findInterval(pts[, 2], ye), 1L), ns)
  ti <- pmin(pmax(findInterval(t, te), 1L), nt)
  counts <- array(0, dim = c(ns, ns, nt))
  for (m in seq_len(nrow(pts))) {
    counts[xi[m], yi[m], ti[m]] <- counts[xi[m], yi[m], ti[m]] + 1
  }
  flat <- as.numeric(counts)
  m <- length(flat)
  mean_ <- mean(flat)
  if (mean_ <= 0) {
    return(list(
      index_of_dispersion = NA_real_, df = m - 1L, p_value = NA_real_,
      counts = counts, mean_count = mean_
    ))
  }
  v <- var(flat) # sample variance, ddof = 1
  idx <- (m - 1) * v / mean_
  list(
    index_of_dispersion = idx, df = m - 1L,
    p_value = .schab_st_chi2_sf(idx, m - 1L),
    counts = counts, mean_count = mean_, var_count = v
  )
}

#' P(chi^2_df > x) = Q(df/2, x/2). The HALVING of x is the whole content
#' of
#'
#' the mapping and is easy to drop: Q(df/2, x) is a perfectly
#' well-behaved number, just not this one.
#'
#' @param x See Usage.
#' @param df See Usage.
#' @return A numeric value.
#' @export
.schab_st_chi2_sf <- function(x, df) {
  # P(chi^2_df > x) = Q(df/2, x/2). The HALVING of x is the whole content of
  # the mapping and is easy to drop: Q(df/2, x) is a perfectly well-behaved
  # number, just not this one.
  x <- 0.5 * as.numeric(x)
  a <- 0.5 * as.numeric(df)
  if (x <= 0) {
    return(1)
  }
  if (x < a + 1) { # series for P(a, x)
    term <- 1 / a
    total <- term
    n <- 0L
    while (n < 10000L) {
      n <- n + 1L
      term <- term * x / (a + n)
      total <- total + term
      if (abs(term) < abs(total) * 1e-16) break
    }
    return(1 - total * exp(-x + a * log(x) - lgamma(a)))
  }
  tiny <- 1e-300 # Lentz, continued fraction
  b <- x + 1 - a
  cc <- 1 / tiny
  d <- 1 / b
  hh <- d
  for (i in seq_len(10000L)) {
    an <- -i * (i - a)
    b <- b + 2
    d <- an * d + b
    if (abs(d) < tiny) d <- tiny
    cc <- b + an / cc
    if (abs(cc) < tiny) cc <- tiny
    d <- 1 / d
    delta <- d * cc
    hh <- hh * delta
    if (abs(delta - 1) < 1e-16) break
  }
  exp(-x + a * log(x) - lgamma(a)) * hh
}
