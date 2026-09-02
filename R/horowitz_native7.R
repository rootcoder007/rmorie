# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors, part 7: panel-data deconvolution for
# Y_jt = X_jt'beta + U_j + eps_jt. Mirrors morie.fn._hrz_paneldec,
# hrzpanel, hrzfnu, hrzfneps, hrzfpt.
#
# Collision scan: horowitz_native7.R, all four exported names and the
# internal helpers were free in both R trees.
#
# Spec: Horowitz, Sec. 5.2, eqs. (5.20)-(5.26), assumptions P1-P4 and
# Theorem 5.4.
#
# The identification trick and its precondition:
#   (5.21) W = Y - b'X estimates U + eps, so psi_W = psi_U psi_eps.
#   (5.22) eta = (Y_t - Y_1) - b'(X_t - X_1) removes U_j entirely and
#          estimates a difference of two independent eps, so
#          psi_eta = |psi_eps|^2.
#   Hence psi_eps = psi_eta^(1/2) and psi_U = psi_W / psi_eta^(1/2).
#
# That square root is legitimate ONLY because eps is assumed
# symmetric about zero (making psi_eps real) and nonvanishing (making
# it positive). Without symmetry the sign of the root is not
# identified.

# psi_zeta: a bounded real characteristic function supported on
# [-1, 1] -- the book's example is the fourfold convolution of the
# uniform density with itself, whose characteristic function is
# sinc^4. Compact support in tau is the point: it stops the integrand
# being evaluated where the denominator has died.
#' Psi_zeta: a bounded real characteristic function supported on
#'
#' \[-1, 1\] -- the book\'s example is the fourfold convolution of the
#' uniform density with itself, whose characteristic function is sinc^4.
#' Compact support in tau is the point: it stops the integrand being
#' evaluated where the denominator has died.
#'
#' @param u Numeric; passed to \code{abs}.
#' @return The value of \code{ifelse}.
#' @export
.morie_hrz_smoothing_cf <- function(u) {
  s <- ifelse(u == 0, 1, (sin(u / 4) / (u / 4))^4)
  ifelse(abs(u) <= 1, s, 0)
}

#' .morie_hrz_panel_residuals
#'
#' A step of the horowitz_native7 implementation. Called by \code{morie_panel_deconvolution}, \code{morie_panel_densities}, \code{morie_smoothed_fU}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A matrix; the body checks with \code{is.matrix}.
#' @param x A matrix; indexed by row and column.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{W}, \code{eta}, \code{n}, \code{T}.
#' @export
.morie_hrz_panel_residuals <- function(y, x, beta) {
  Y <- if (is.matrix(y)) y else matrix(as.numeric(y), nrow = 1L)
  b <- as.numeric(beta)
  n <- nrow(Y)
  tt <- ncol(Y)
  if (tt < 2L) {
    stop(sprintf("need at least 2 periods, got %d.", tt),
      call. = FALSE
    )
  }
  if (is.matrix(x)) {
    if (nrow(x) == n * tt && ncol(x) == length(b)) {
      X <- array(0, dim = c(n, tt, length(b)))
      for (j in seq_along(b)) X[, , j] <- matrix(x[, j], nrow = n, byrow = TRUE)
    } else if (nrow(x) == n && ncol(x) == tt && length(b) == 1L) {
      X <- array(x, dim = c(n, tt, 1L))
    } else {
      stop(sprintf(
        "x has %d x %d entries, cannot match %d x %d.",
        nrow(x), ncol(x), n, tt
      ), call. = FALSE)
    }
  } else if (length(dim(x)) == 3L) {
    X <- x
  } else {
    stop("x must be (n, T, d) or (n*T, d).", call. = FALSE)
  }
  if (dim(X)[1L] != n || dim(X)[2L] != tt) {
    stop(sprintf(
      "x has %d x %d panel dimensions, expected %d x %d.",
      dim(X)[1L], dim(X)[2L], n, tt
    ), call. = FALSE)
  }
  if (dim(X)[3L] != length(b)) {
    stop(sprintf(
      "beta has %d entries for %d covariates.",
      length(b), dim(X)[3L]
    ), call. = FALSE)
  }
  xb <- matrix(0, n, tt)
  for (j in seq_along(b)) xb <- xb + X[, , j] * b[j]
  w <- Y - xb
  eta <- (Y[, -1L, drop = FALSE] - Y[, 1L]) -
    (xb[, -1L, drop = FALSE] - xb[, 1L])
  list(W = as.numeric(w), eta = as.numeric(eta), n = n, T = tt)
}

#' .morie_hrz_deconvolve_pair
#'
#' A step of the horowitz_native7 implementation. Called by \code{morie_panel_deconvolution}, \code{morie_panel_densities}, \code{morie_smoothed_fU}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w Passed to \code{outer}.
#' @param eta Passed to \code{outer}.
#' @param grid_u Iterated over elementwise, with \code{vapply}.
#' @param grid_z Iterated over elementwise, with \code{vapply}.
#' @param nu_U Numeric; combined arithmetically in the body.
#' @param nu_eps Numeric; combined arithmetically in the body.
#' @param n_tau Passed to \code{seq}. Defaults to \code{2001L}.
#' @return A list with \code{f_U}, \code{f_eps}.
#' @export
.morie_hrz_deconvolve_pair <- function(w, eta, grid_u, grid_z, nu_U, nu_eps,
                                       n_tau = 2001L) {
  if (nu_U <= 0 || nu_eps <= 0) {
    stop(sprintf("bandwidths must be positive, got (%g, %g).", nu_U, nu_eps),
      call. = FALSE
    )
  }
  trapz <- function(xx, yy) {
    sum(diff(xx) * (utils::head(yy, -1L) +
      utils::tail(yy, -1L)) / 2)
  }
  tau_e <- seq(-1 / nu_eps, 1 / nu_eps, length.out = n_tau)
  psi_eta_e <- rowMeans(exp(1i * outer(tau_e, eta)))
  integ_e <- sqrt(Mod(psi_eta_e)) * .morie_hrz_smoothing_cf(nu_eps * tau_e)
  f_eps <- vapply(grid_z, function(z) {
    Re(trapz(tau_e, integ_e * exp(-1i * tau_e * z))) / (2 * pi)
  }, numeric(1))

  tau_u <- seq(-1 / nu_U, 1 / nu_U, length.out = n_tau)
  psi_w_u <- rowMeans(exp(1i * outer(tau_u, w)))
  psi_eta_u <- rowMeans(exp(1i * outer(tau_u, eta)))
  root <- sqrt(Mod(psi_eta_u))
  weight <- .morie_hrz_smoothing_cf(nu_U * tau_u)
  integ_u <- ifelse(weight > 0, psi_w_u * weight / pmax(root, 1e-300), 0)
  f_u <- vapply(grid_u, function(u) {
    Re(trapz(tau_u, integ_u * exp(-1i * tau_u * u))) / (2 * pi)
  }, numeric(1))
  list(f_U = f_u, f_eps = f_eps)
}

#' Panel-data deconvolution of the individual and idiosyncratic densities
#'
#' For \eqn{Y_{jt} = X_{jt}'\beta + U_j + \varepsilon_{jt}}, recovers
#' \eqn{f_U} and \eqn{f_\varepsilon} nonparametrically via (5.21)
#' through (5.26).
#'
#' The levels residual estimates \eqn{U + \varepsilon}, so
#' \eqn{\psi_W = \psi_U\psi_\varepsilon}; the within-individual
#' difference removes \eqn{U_j} completely and estimates a difference
#' of two independent copies of \eqn{\varepsilon}, so
#' \eqn{\psi_\eta = |\psi_\varepsilon|^2}. Hence
#' \eqn{\psi_\varepsilon = \psi_\eta^{1/2}}.
#'
#' \strong{The square root is what the symmetry assumption is for.}
#' \eqn{\psi_\eta} determines \eqn{\psi_\varepsilon} only up to sign;
#' symmetry about zero makes it real and non-vanishing makes it
#' positive, which picks the root. This is a HARDER problem than
#' Sec. 5.1, where the contaminating distribution was known, and the
#' rates are just as slow -- for normal \eqn{\varepsilon} the fastest
#' possible is \eqn{(\log n)^{-1}}. Asymptotics are in n with T
#' FIXED. Mirrors \code{morie.fn.hrzpanel}.
#'
#' @param y numeric matrix (n, T) of responses.
#' @param x numeric array (n, T, d) or matrix (n*T, d).
#' @param beta root-n consistent coefficients.
#' @param nu_U,nu_eps smoothing bandwidths; \code{(log n)^(-1/2)}
#'   when NULL.
#' @param grid_u,grid_z evaluation points.
#' @return list: grid_u, f_U, grid_z, f_eps, psi_eps_from_root,
#'   symmetry_required, nu_U, nu_eps, fastest_possible_rate,
#'   asymptotics_in, n, T, d, method.
#' @references Horowitz, Sec. 5.2.1-5.2.2, eqs. (5.21)-(5.26),
#'   Theorem 5.4; Horowitz and Markatou (1996).
#' @examples
#' n <- 60
#' tt <- 3
#' x <- array(rnorm(n * tt * 2), dim = c(n, tt, 2))
#' y <- matrix(rnorm(n * tt), n, tt)
#' morie_panel_deconvolution(y, x, c(1, -0.5))$symmetry_required
#' @export
morie_panel_deconvolution <- function(y, x, beta, nu_U = NULL, nu_eps = NULL,
                                      grid_u = NULL, grid_z = NULL) {
  r <- .morie_hrz_panel_residuals(y, x, beta)
  if (r$n < 10L) {
    stop(sprintf("need at least 10 individuals, got %d.", r$n), call. = FALSE)
  }
  default <- log(r$n)^-0.5
  nu1 <- if (is.null(nu_U)) default else as.numeric(nu_U)
  nu2 <- if (is.null(nu_eps)) default else as.numeric(nu_eps)
  gu <- if (is.null(grid_u)) {
    seq(stats::quantile(r$W, 0.05), stats::quantile(r$W, 0.95),
      length.out = 61L
    )
  } else {
    as.numeric(grid_u)
  }
  gz <- if (is.null(grid_z)) {
    seq(stats::quantile(r$eta, 0.05), stats::quantile(r$eta, 0.95),
      length.out = 61L
    )
  } else {
    as.numeric(grid_z)
  }
  d <- .morie_hrz_deconvolve_pair(r$W, r$eta, gu, gz, nu1, nu2)
  list(
    grid_u = gu, f_U = d$f_U, grid_z = gz, f_eps = d$f_eps,
    psi_eps_from_root = TRUE, symmetry_required = TRUE,
    nu_U = nu1, nu_eps = nu2,
    fastest_possible_rate = "(log n)^{-1} for normal eps",
    asymptotics_in = "n with T fixed",
    n = r$n, T = r$T, d = length(as.numeric(beta)),
    method = "Panel deconvolution (5.21)-(5.26); psi_eps = psi_eta^{1/2} needs eps symmetric"
  )
}

#' Smoothed deconvolution estimator of f_U
#'
#' \eqn{f_{nU}(u) = (2\pi)^{-1}\int e^{-i\tau u}
#' \psi_{nW}(\tau)\psi_\zeta(\nu_{nU}\tau)/|\psi_{n\eta}(\tau)|^{1/2}
#' d\tau} (5.26).
#'
#' Substituting the empirical characteristic functions straight into
#' the inversion formula does NOT work -- the integral does not exist
#' in general. \eqn{\psi_\zeta} is the regularisation: a
#' characteristic function supported on \eqn{\[-1, 1\]}, so the
#' integrand is identically zero past \eqn{1/\nu_{nU}} and the ratio
#' is never formed where the denominator has died. It is the
#' Fourier-transform analogue of kernel smoothing, and it is
#' mandatory rather than a refinement. Mirrors
#' \code{morie.fn.hrzfnu}.
#'
#' @param y numeric matrix (n, T) of responses.
#' @param x numeric array (n, T, d) or matrix (n*T, d).
#' @param beta root-n consistent coefficients.
#' @param nu_U smoothing bandwidth; \code{(log n)^(-1/2)} when NULL.
#' @param grid evaluation points.
#' @return list: grid, f_U, nu_U, cutoff, regularisation_required,
#'   n, T, method.
#' @references Horowitz, Sec. 5.2.1, eq. (5.26).
#' @examples
#' n <- 60
#' tt <- 3
#' x <- array(rnorm(n * tt * 2), dim = c(n, tt, 2))
#' morie_smoothed_fU(matrix(rnorm(n * tt), n, tt), x, c(1, -0.5))$cutoff
#' @export
morie_smoothed_fU <- function(y, x, beta, nu_U = NULL, grid = NULL) {
  r <- .morie_hrz_panel_residuals(y, x, beta)
  if (r$n < 10L) {
    stop(sprintf("need at least 10 individuals, got %d.", r$n), call. = FALSE)
  }
  nu1 <- if (is.null(nu_U)) log(r$n)^-0.5 else as.numeric(nu_U)
  if (nu1 <= 0) {
    stop(sprintf("nu_U must be positive, got %g.", nu1),
      call. = FALSE
    )
  }
  g <- if (is.null(grid)) {
    seq(stats::quantile(r$W, 0.05), stats::quantile(r$W, 0.95),
      length.out = 61L
    )
  } else {
    as.numeric(grid)
  }
  d <- .morie_hrz_deconvolve_pair(r$W, r$eta, g, g[1L], nu1, nu1)
  list(
    grid = g, f_U = d$f_U, nu_U = nu1, cutoff = 1 / nu1,
    regularisation_required = TRUE, n = r$n, T = r$T,
    method = "(5.26): psi_zeta compactly supported, so the ratio is never formed past the cut-off"
  )
}

#' Both panel-deconvolution density estimators
#'
#' \eqn{f_{n\varepsilon}} (5.25) and \eqn{f_{nU}} (5.26) together.
#' The pair is returned together because the two bandwidths are NOT
#' interchangeable: Theorem 5.4 treats them separately, and
#' \eqn{f_{n\varepsilon}} needs no division at all -- it is built
#' from \eqn{|\psi_{n\eta}|^{1/2}} directly -- while \eqn{f_{nU}}
#' divides by it. That asymmetry is why they take different
#' bandwidths in practice. Mirrors \code{morie.fn.hrzfneps}.
#'
#' @param y numeric matrix (n, T) of responses.
#' @param x numeric array (n, T, d) or matrix (n*T, d).
#' @param beta root-n consistent coefficients.
#' @param nu_U,nu_eps the two bandwidths.
#' @param grid_u,grid_z evaluation points.
#' @return list: grid_u, f_U, grid_z, f_eps, nu_U, nu_eps,
#'   f_eps_requires_division, f_U_requires_division,
#'   bandwidths_independent, n, T, method.
#' @references Horowitz, Sec. 5.2.1-5.2.2, eqs. (5.25)-(5.26),
#'   P1-P4, Theorem 5.4.
#' @examples
#' n <- 60
#' tt <- 3
#' x <- array(rnorm(n * tt * 2), dim = c(n, tt, 2))
#' y <- matrix(rnorm(n * tt), n, tt)
#' morie_panel_densities(y, x, c(1, -0.5))$f_U_requires_division
#' @export
morie_panel_densities <- function(y, x, beta, nu_U = NULL, nu_eps = NULL,
                                  grid_u = NULL, grid_z = NULL) {
  r <- .morie_hrz_panel_residuals(y, x, beta)
  if (r$n < 10L) {
    stop(sprintf("need at least 10 individuals, got %d.", r$n), call. = FALSE)
  }
  default <- log(r$n)^-0.5
  nu1 <- if (is.null(nu_U)) default else as.numeric(nu_U)
  nu2 <- if (is.null(nu_eps)) default else as.numeric(nu_eps)
  gu <- if (is.null(grid_u)) {
    seq(stats::quantile(r$W, 0.05), stats::quantile(r$W, 0.95),
      length.out = 61L
    )
  } else {
    as.numeric(grid_u)
  }
  gz <- if (is.null(grid_z)) {
    seq(stats::quantile(r$eta, 0.05), stats::quantile(r$eta, 0.95),
      length.out = 61L
    )
  } else {
    as.numeric(grid_z)
  }
  d <- .morie_hrz_deconvolve_pair(r$W, r$eta, gu, gz, nu1, nu2)
  list(
    grid_u = gu, f_U = d$f_U, grid_z = gz, f_eps = d$f_eps,
    nu_U = nu1, nu_eps = nu2,
    f_eps_requires_division = FALSE, f_U_requires_division = TRUE,
    bandwidths_independent = TRUE, n = r$n, T = r$T,
    method = "(5.25) needs no division; (5.26) does, so the two carry separate bandwidths"
  )
}

#' First-passage-time probability in a panel model
#'
#' \eqn{P(\theta|y_1, y^*, x) = f_W(y_1 - \beta'x_1)^{-1}\int
#' f_\varepsilon(y_1 - \beta'x_1 - u)\[\prod_{k=2}^{\theta}
#' F_\varepsilon(y^* - \beta'x_k - u)\] f_U(u) du} (5.20).
#'
#' This is why Sec. 5.2 estimates \eqn{f_U} and \eqn{f_\varepsilon}
#' at all: they are rarely interesting in themselves, but the
#' first-passage distribution cannot be written without BOTH.
#'
#' The integral over u is what handles the individual effect
#' correctly. Given \eqn{U_j = u} the periods are independent, so
#' their probabilities multiply -- the product in the integrand --
#' but unconditionally they are DEPENDENT, because they share
#' \eqn{U_j}. Integrating the product against \eqn{f_U} rather than
#' multiplying unconditional probabilities is the whole difference.
#' Mirrors \code{morie.fn.hrzfpt}.
#'
#' @param theta horizon, at least 2.
#' @param y1 initial value.
#' @param y_star threshold.
#' @param x covariates for periods 1..theta.
#' @param beta coefficients.
#' @param f_U,grid_u density of U on its grid.
#' @param f_eps,grid_z density of eps on its grid.
#' @return list: probability, theta, f_W_at_initial,
#'   periods_conditionally_independent,
#'   periods_marginally_independent, method.
#' @references Horowitz, Sec. 5.2.3, eq. (5.20).
#' @examples
#' gu <- seq(-5, 5, length.out = 201)
#' morie_first_passage_time(
#'   3, 0, 1, matrix(0, 3, 2), c(1, -0.5),
#'   dnorm(gu), gu, dnorm(gu, sd = 0.5), gu
#' )$probability
#' @export
morie_first_passage_time <- function(theta, y1, y_star, x, beta, f_U, grid_u,
                                     f_eps, grid_z) {
  th <- as.integer(theta)
  if (is.na(th) || th < 2L) {
    stop(sprintf("theta must be at least 2, got %s.", theta), call. = FALSE)
  }
  b <- as.numeric(beta)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = length(b))
  if (ncol(X) != length(b)) X <- t(X)
  if (ncol(X) != length(b)) {
    stop(sprintf("x must have %d columns to match beta.", length(b)),
      call. = FALSE
    )
  }
  if (nrow(X) < th) {
    stop(sprintf(
      "x has %d periods but theta = %d needs %d.",
      nrow(X), th, th
    ), call. = FALSE)
  }
  gu <- as.numeric(grid_u)
  fu <- as.numeric(f_U)
  gz <- as.numeric(grid_z)
  fe <- as.numeric(f_eps)
  if (length(fu) != length(gu)) {
    stop(sprintf(
      "f_U has %d entries for %d grid points.",
      length(fu), length(gu)
    ), call. = FALSE)
  }
  if (length(fe) != length(gz)) {
    stop(sprintf(
      "f_eps has %d entries for %d grid points.",
      length(fe), length(gz)
    ), call. = FALSE)
  }
  if (any(fu < 0) || any(fe < 0)) {
    stop("densities must be non-negative.", call. = FALSE)
  }
  trapz <- function(xx, yy) {
    sum(diff(xx) * (utils::head(yy, -1L) +
      utils::tail(yy, -1L)) / 2)
  }
  f_cum <- c(0, cumsum(diff(gz) * (utils::head(fe, -1L) +
    utils::tail(fe, -1L)) / 2))
  total <- f_cum[length(f_cum)]
  if (total > 0) f_cum <- f_cum / total
  f_eps_cdf <- function(v) {
    pmin(pmax(stats::approx(gz, f_cum,
      xout = v,
      rule = 2L
    )$y, 0), 1)
  }
  f_eps_at <- function(v) {
    pmax(stats::approx(gz, fe,
      xout = v, yleft = 0,
      yright = 0, rule = 1L
    )$y, 0)
  }
  idx1 <- as.numeric(y1 - X[1L, ] %*% b)
  prod_v <- rep(1, length(gu))
  for (k in 2:th) {
    prod_v <- prod_v * f_eps_cdf(y_star - as.numeric(X[k, ] %*% b) - gu)
  }
  numer <- trapz(gu, f_eps_at(idx1 - gu) * prod_v * fu)
  f_w <- trapz(gu, f_eps_at(idx1 - gu) * fu)
  if (f_w <= 0) {
    stop(paste(
      "f_W vanishes at the initial residual; the conditioning event",
      "has zero estimated density there."
    ), call. = FALSE)
  }
  list(
    probability = min(max(numer / f_w, 0), 1), theta = th,
    f_W_at_initial = f_w,
    periods_conditionally_independent = TRUE,
    periods_marginally_independent = FALSE,
    method = "(5.20): the product is integrated against f_U, since periods share U_j"
  )
}
