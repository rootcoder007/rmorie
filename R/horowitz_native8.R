# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors, part 8: single-index estimators.
# Mirrors morie.fn.hrzrank, hrzwfun, hrzasym, hrzdiscd.
#
# Collision scan: horowitz_native8.R and all four exported names were
# free in both R trees. .hrz_silverman and .hrz_nw_loo come from
# R/aaa_helpers_horowitz.R; the grid-scan constants match
# morie.fn._horowitz.
#
# Spec: Horowitz, Sec. 2.5.2 (choosing the weight function, eq. 2.32),
# Sec. 2.5.5 (rank estimators), Sec. 2.6.3 (discrete covariates,
# eqs. 2.45-2.51), Sec. 2.6.4 (one-step efficiency, eqs. 2.52-2.55).

.MORIE_HRZ_GRID_HALF <- 10
.MORIE_HRZ_GRID_N <- 2001L

# Numerical derivative matching numpy.gradient with edge_order = 1.
#
# The interior formula must account for UNEQUAL spacing -- the naive
# (f[i+1] - f[i-1]) / (x[i+1] - x[i-1]) is only second-order accurate on
# a uniform grid, and the index here is sorted data, which is anything
# but. Using the naive form put every quantity downstream of G' about
# 1e-4 away from morie.fn, which is small but is not parity.
.morie_hrz_gradient <- function(fv, xv) {
  n <- length(fv)
  g <- numeric(n)
  if (n < 2L) return(g)
  g[1L] <- (fv[2L] - fv[1L]) / (xv[2L] - xv[1L])
  g[n] <- (fv[n] - fv[n - 1L]) / (xv[n] - xv[n - 1L])
  if (n > 2L) {
    i <- 2:(n - 1L)
    hd <- xv[i] - xv[i - 1L]
    hs <- xv[i + 1L] - xv[i]
    g[i] <- (-hs / (hd * (hd + hs))) * fv[i - 1L] +
      ((hs - hd) / (hs * hd)) * fv[i] +
      (hd / (hs * (hd + hs))) * fv[i + 1L]
  }
  g
}

# Nadaraya-Watson fitted values on a supplied grid.
.morie_hrz_nw <- function(x, y, grid, h) {
  w <- exp(-0.5 * (outer(grid, x, "-") / h)^2)
  den <- rowSums(w)
  ifelse(den > 0, as.numeric(w %*% y) / pmax(den, 1e-300), NA_real_)
}

#' Semiparametric rank estimators of a single-index model
#'
#' Maximum rank correlation (Han 1987),
#' \eqn{b_{MRC} = \arg\max_b \[n(n-1)\]^{-1}\sum_i\sum_{j\ne i}
#' 1\{Y_i > Y_j\}1\{X_i'b > X_j'b\}}, and the Cavanagh-Sherman
#' variant, which replaces \eqn{1\{Y_i > Y_j\}} with an increasing
#' \eqn{M(Y_i)}.
#'
#' The identifying observation is an ordering, not a moment: if G is
#' nondecreasing and \eqn{Y - G(X'\beta)} is independent of X, then
#' \eqn{X_i'\beta > X_j'\beta} implies \eqn{P(Y_i > Y_j) > P(Y_j >
#' Y_i)}. G never appears. That is the appeal and the cost:
#'
#' \itemize{
#'   \item \strong{no bandwidth} -- no kernel, no smoothing
#'     parameter, no nonparametric pre-estimate of G;
#'   \item \strong{not asymptotically efficient}, as the book states
#'     plainly. Root-n consistent and asymptotically normal, but it
#'     does not attain \eqn{\Omega_{SI}};
#'   \item \strong{inference is awkward} -- the asymptotic covariance
#'     is hard to implement, so the practical route is the bootstrap
#'     (Subbotin 2008). No analytic standard error is returned,
#'     because a plausible-looking one would be the wrong thing to
#'     trust.
#' }
#'
#' At d = 2 the objective is PIECEWISE CONSTANT in the single free
#' coefficient t, flipping exactly at
#' \eqn{t = -(x_{1i} - x_{1j})/(x_{2i} - x_{2j})}. Sorting those
#' \eqn{n^2} breakpoints and accumulating costs \eqn{O(n^2\log n)}
#' ONCE, against \eqn{O(n^2)} per grid point for a naive scan -- at
#' n = 300 the difference between minutes and milliseconds. Mirrors
#' \code{morie.fn.hrzrank}.
#'
#' @param x numeric matrix of covariates.
#' @param y response; only its ordering is used.
#' @param variant "mrc" or "cs".
#' @param M increasing function for the CS variant; ranks when NULL.
#' @return list: beta, objective, variant, requires_bandwidth,
#'   asymptotically_efficient, rate_exponent, inference, se, n, d,
#'   method.
#' @references Horowitz, Sec. 2.5.5; Han (1987), Sherman (1993),
#'   Cavanagh and Sherman (1998), Subbotin (2008).
#' @examples
#' x <- cbind(rnorm(80), rnorm(80))
#' morie_rank_index(x, tanh(x %*% c(1, -0.6)) + rnorm(80) * 0.3)$beta
#' @export
morie_rank_index <- function(x, y, variant = "mrc", M = NULL) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  if (!variant %in% c("mrc", "cs")) {
    stop("variant must be 'mrc' or 'cs'.", call. = FALSE)
  }
  n <- nrow(X)
  d <- ncol(X)
  if (n < 10L) stop(sprintf("need at least 10 observations, got %d.", n),
                    call. = FALSE)
  if (d < 2L) stop(sprintf("need at least 2 covariates, got %d.", d),
                   call. = FALSE)
  denom <- n * (n - 1L)
  if (variant == "mrc") {
    wm <- outer(yv, yv, ">") * 1
  } else {
    mv <- if (is.null(M)) as.numeric(rank(yv, ties.method = "first")) else
      vapply(yv, M, numeric(1))
    if (any(diff(mv[order(yv)]) < 0)) {
      stop("M must be increasing in y.", call. = FALSE)
    }
    wm <- matrix(mv, n, n)
  }
  diag(wm) <- 0
  if (d == 2L) {
    a <- as.numeric(outer(X[, 1L], X[, 1L], "-"))
    cc <- as.numeric(outer(X[, 2L], X[, 2L], "-"))
    ww <- as.numeric(wm)
    keep <- ww != 0
    a <- a[keep]; cc <- cc[keep]; ww <- ww[keep]
    # value as t -> -inf: a + t c -> +inf exactly when c < 0
    base <- sum(ww[cc < 0 | (cc == 0 & a > 0)])
    moving <- cc != 0
    thr <- -a[moving] / cc[moving]
    sgn <- ifelse(cc[moving] > 0, 1, -1) * ww[moving]
    o <- order(thr)
    thr_s <- thr[o]
    cum <- base + c(0, cumsum(sgn[o]))
    grid <- seq(-.MORIE_HRZ_GRID_HALF, .MORIE_HRZ_GRID_HALF,
                length.out = .MORIE_HRZ_GRID_N)
    vals <- cum[findInterval(grid, thr_s) + 1L] / denom
    k <- which.max(vals)
    beta <- c(1, grid[k])
    obj <- vals[k]
  } else {
    stop("only d = 2 is implemented in the R mirror; use morie.fn.hrzrank for higher d.",
         call. = FALSE)
  }
  list(beta = beta, objective = obj, variant = variant,
       requires_bandwidth = FALSE, asymptotically_efficient = FALSE,
       rate_exponent = -0.5, inference = "bootstrap", se = NULL,
       n = n, d = d,
       method = "Rank correlation over orderings; no bandwidth, but not efficient and no analytic SE")
}

#' Efficient weight function for semiparametric weighted NLS
#'
#' \eqn{W(x) = 1/\sigma^2(x)} attains the single-index efficiency
#' bound \eqn{\Omega_{SI} = \{E\[1(X \in A_x)\sigma^{-2}(X)\,\partial
#' G\,\partial G'\]\}^{-1}} (2.32).
#'
#' \strong{Not knowing sigma^2 costs nothing.} It can be replaced by
#' a consistent estimate and the bound is still attained, by the
#' two-step procedure the section gives: fit with \eqn{W = 1}, then
#' regress the SQUARED residuals nonparametrically on X and refit
#' with \eqn{W = 1/s_n^2}.
#'
#' \strong{Not knowing G does cost something.} Except in special
#' cases \eqn{\Omega_{SI}} exceeds the bound achievable with G known.
#' The price of a nonparametric link is a loss of EFFICIENCY, not of
#' rate, and the two are returned separately because conflating them
#' is the usual error.
#'
#' The covariance is the sandwich \eqn{C_n^{-1}D_nC_n^{-1}} with
#' \eqn{D_n} carrying \eqn{W^2}, not \eqn{W}: only \eqn{W^2} makes
#' the sandwich collapse to \eqn{\Omega_{SI}} at \eqn{W = 1/\sigma^2},
#' which is what the book asserts the efficient weight achieves.
#' Mirrors \code{morie.fn.hrzwfun}.
#'
#' @param x numeric matrix of covariates.
#' @param y numeric response.
#' @param bandwidth bandwidth; Silverman's rule when NULL.
#' @param weights supplied weights; efficient ones estimated when NULL.
#' @param beta_hat index direction; first canonical direction when NULL.
#' @return list: beta, weights, sigma2_hat, omega, omega_SI, C, D,
#'   max_weight, efficient_weight_used,
#'   efficiency_loss_from_unknown_G, rate_loss_from_unknown_G,
#'   bandwidth, n, d, method.
#' @references Horowitz, Sec. 2.5.2, eq. (2.32); Ichimura (1993),
#'   Chamberlain (1986), Newey and Stoker (1993).
#' @examples
#' x <- cbind(rnorm(100), rnorm(100))
#' y <- tanh(x %*% c(1, -0.6)) + rnorm(100) * 0.3
#' morie_nls_weight_function(x, y, beta_hat = c(1, -0.6))$efficient_weight_used
#' @export
morie_nls_weight_function <- function(x, y, bandwidth = NULL, weights = NULL,
                                      beta_hat = NULL) {
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
  if (d < 2L) stop(sprintf("need at least 2 covariates, got %d.", d),
                   call. = FALSE)
  b <- if (is.null(beta_hat)) c(1, numeric(d - 1L)) else as.numeric(beta_hat)
  if (length(b) != d) {
    stop(sprintf("beta_hat has %d entries for %d.", length(b), d),
         call. = FALSE)
  }
  if (b[1L] == 0) {
    stop("the scale normalisation needs a nonzero first coefficient.",
         call. = FALSE)
  }
  b <- b / abs(b[1L])
  z <- as.numeric(X %*% b)
  hh <- if (is.null(bandwidth)) .hrz_silverman(z) else as.numeric(bandwidth)
  if (hh <= 0) stop(sprintf("bandwidth must be positive, got %g.", hh),
                    call. = FALSE)
  ghat <- .morie_hrz_nw(z, yv, z, hh)
  resid <- yv - ghat
  # sigma^2 is a property of the MODEL, so it is estimated whatever
  # weight is used -- Omega_SI is defined by it, not by the caller's
  # choice of weight
  s2 <- pmax(.morie_hrz_nw(z, resid^2, z, hh), 1e-12)
  if (is.null(weights)) {
    w <- 1 / s2
    efficient <- TRUE
  } else {
    w <- as.numeric(weights)
    if (length(w) != n) {
      stop(sprintf("weights has %d entries for %d rows.", length(w), n),
           call. = FALSE)
    }
    if (any(w < 0)) stop("weights must be non-negative.", call. = FALSE)
    efficient <- FALSE
  }
  o <- order(z)
  gp <- numeric(n)
  gs <- ghat[o]
  zs <- z[o]
  gp[o] <- .morie_hrz_gradient(gs, zs)
  xt <- X[, -1L, drop = FALSE]
  xbar <- vapply(seq_len(d - 1L), function(j)
    .morie_hrz_nw(z, xt[, j], z, hh), numeric(n))
  dg <- gp * (xt - xbar)
  cmat <- 2 * crossprod(dg * w, dg) / n
  dmat <- 4 * crossprod(dg * (w^2 * resid^2), dg) / n
  cinv <- solve(cmat)
  omega <- cinv %*% dmat %*% cinv
  omega_si <- solve(crossprod(dg / s2, dg) / n)
  list(beta = b, weights = w, sigma2_hat = s2, omega = omega,
       omega_SI = omega_si, C = cmat, D = dmat, max_weight = max(w),
       efficient_weight_used = efficient,
       efficiency_loss_from_unknown_G = TRUE,
       rate_loss_from_unknown_G = FALSE,
       bandwidth = hh, n = n, d = d,
       method = "W = 1/sigma^2 attains Omega_SI; a two-step estimate of sigma^2 loses nothing")
}

#' One Newton step from any root-n start to asymptotic efficiency
#'
#' \eqn{\tilde b_n = \tilde b_n^{*} - \[\partial^2 S_n/\partial
#' \tilde b\partial\tilde b'\]^{-1}\partial S_n/\partial\tilde b}
#' (2.52), where \eqn{S_n} is the weighted-NLS objective with
#' \eqn{W = 1/s_n^2} and \eqn{\tilde b_n^{*}} is ANY root-n
#' consistent estimator. The result attains \eqn{\Omega_{SI}}.
#'
#' The word doing the work is \strong{one}. This is not an iterative
#' optimiser stopped early: (2.53)-(2.55) show the single step
#' already removes the leading term, so iterating to convergence buys
#' nothing asymptotically. That matters practically, because the
#' direct estimators of Sec. 2.6.1-2.6.3 are fast while minimising
#' \eqn{S_n} is slow and possibly multimodal -- a cheap direct start
#' plus one step beats a full optimisation. Mirrors
#' \code{morie.fn.hrzasym}.
#'
#' @param x numeric matrix of covariates.
#' @param y numeric response.
#' @param bandwidth bandwidth; Silverman's rule when NULL.
#' @param initial_estimator the root-n consistent start.
#' @param n_steps Newton steps; the theory needs one.
#' @return list: beta, beta_initial, step, omega, se,
#'   attains_omega_SI, theory_requires_steps, n_steps, bandwidth,
#'   n, d, method.
#' @references Horowitz, Sec. 2.6.4, eqs. (2.52)-(2.55).
#' @examples
#' x <- cbind(rnorm(100), rnorm(100))
#' y <- tanh(x %*% c(1, -0.6)) + rnorm(100) * 0.3
#' morie_one_step_efficient(x, y, initial_estimator = c(1, -0.2))$n_steps
#' @export
morie_one_step_efficient <- function(x, y, bandwidth = NULL,
                                     initial_estimator = NULL, n_steps = 1L) {
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
  if (d < 2L) stop(sprintf("need at least 2 covariates, got %d.", d),
                   call. = FALSE)
  steps <- as.integer(n_steps)
  if (is.na(steps) || steps < 1L) {
    stop(sprintf("n_steps must be at least 1, got %s.", n_steps),
         call. = FALSE)
  }
  if (is.null(initial_estimator)) {
    stop("initial_estimator is required in the R mirror; morie.fn.hrzasym defaults to its average-derivative estimate.",
         call. = FALSE)
  }
  b0 <- as.numeric(initial_estimator)
  if (length(b0) != d) {
    stop(sprintf("initial_estimator has %d entries for %d.", length(b0), d),
         call. = FALSE)
  }
  if (b0[1L] == 0) {
    stop("the scale normalisation needs a nonzero first coefficient in the initial estimator.",
         call. = FALSE)
  }
  b0 <- b0 / abs(b0[1L])
  b_init <- b0
  hh <- if (is.null(bandwidth)) .hrz_silverman(as.numeric(X %*% b0)) else
    as.numeric(bandwidth)
  if (hh <= 0) stop(sprintf("bandwidth must be positive, got %g.", hh),
                    call. = FALSE)
  xt <- X[, -1L, drop = FALSE]
  b <- b0
  hess <- NULL
  for (s in seq_len(steps)) {
    z <- as.numeric(X %*% b)
    ghat <- .morie_hrz_nw(z, yv, z, hh)
    resid <- yv - ghat
    s2 <- pmax(.morie_hrz_nw(z, resid^2, z, hh), 1e-12)
    w <- 1 / s2
    o <- order(z)
    gs <- ghat[o]
    zs <- z[o]
    gp <- numeric(n)
    gp[o] <- .morie_hrz_gradient(gs, zs)
    xbar <- vapply(seq_len(d - 1L), function(j)
      .morie_hrz_nw(z, xt[, j], z, hh), numeric(n))
    dg <- gp * (xt - xbar)
    grad <- -2 * colSums(dg * (w * resid)) / n
    hess <- 2 * crossprod(dg * w, dg) / n
    b <- c(1, b[-1L] - as.numeric(solve(hess, grad)))
  }
  omega <- solve(hess)
  list(beta = b, beta_initial = b_init, step = b[-1L] - b_init[-1L],
       omega = omega, se = sqrt(pmax(diag(omega), 0) / n),
       attains_omega_SI = TRUE, theory_requires_steps = 1L,
       n_steps = steps, bandwidth = hh, n = n, d = d,
       method = "(2.52): ONE Newton step from any root-n start attains Omega_SI; iterating adds nothing")
}

#' Direct estimation with discrete covariates
#'
#' \eqn{E(Y|X = x, Z = z) = G(x'\beta + z'\alpha)} (2.45) with X
#' continuous and Z discrete.
#'
#' Average-derivative methods CANNOT estimate \eqn{\alpha}:
#' derivatives with respect to the discrete components do not exist,
#' so no amount of averaging produces them. That absence, not a
#' technical inconvenience, is why the section exists.
#'
#' The construction goes around it. \eqn{\beta} is estimated stratum
#' by stratum and combined by (2.46), a weighted average renormalised
#' by its first component. \eqn{\alpha} then comes from a LINEAR
#' system: under the weak monotonicity of Assumption G the truncated
#' integral \eqn{J(z)} satisfies \eqn{J\[z^{(i)}\] - J\[z^{(1)}\] =
#' (c_1 - c_0)(z^{(i)} - z^{(1)})'\alpha} (2.47), solved by
#' \eqn{\alpha = (c_1 - c_0)(W'W)^{-1}W'\Delta J} (2.48). A shift in
#' the discrete covariate shows up as a horizontal SHIFT of G, and
#' integrating the truncated G recovers its size. Mirrors
#' \code{morie.fn.hrzdiscd}.
#'
#' @param x continuous covariates.
#' @param y numeric response.
#' @param z discrete covariates; omit for the plain index case.
#' @param beta index direction (the R mirror does not re-estimate it).
#' @param c0,c1 truncation levels of Assumption G, c0 < c1.
#' @param bandwidth bandwidth for the within-stratum regressions.
#' @param n_grid points for the integral defining J.
#' @return list: beta, alpha, support_z, J, c0, c1, identified,
#'   average_derivative_can_estimate_alpha, n, d, dz, method.
#' @references Horowitz, Sec. 2.6.3, eqs. (2.45)-(2.51),
#'   Assumption G, Theorem 2.5; Horowitz and Hardle (1996).
#' @examples
#' n <- 300
#' x <- cbind(rnorm(n), rnorm(n))
#' z <- as.numeric(sample(0:2, n, TRUE))
#' y <- tanh(x %*% c(1, -0.6) + z * 0.8) + rnorm(n) * 0.2
#' morie_direct_discrete(x, y, z, beta = c(1, -0.6))$identified
#' @export
morie_direct_discrete <- function(x, y, z = NULL, beta = NULL, c0 = NULL,
                                  c1 = NULL, bandwidth = NULL, n_grid = 60L) {
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
  if (is.null(z)) {
    b <- if (is.null(beta)) {
      dl <- morie_average_derivative(X, yv)$delta
      dl / abs(dl[1L])
    } else as.numeric(beta) / abs(as.numeric(beta)[1L])
    return(list(beta = b, alpha = NULL, support_z = NULL, J = NULL,
                c0 = NULL, c1 = NULL, identified = TRUE,
                average_derivative_can_estimate_alpha = FALSE,
                n = n, d = d, dz = 0L,
                method = "No discrete covariates: the index estimate alone"))
  }
  Z <- if (is.matrix(z)) z else matrix(as.numeric(z), ncol = 1L)
  if (nrow(Z) != n) Z <- t(Z)
  if (nrow(Z) != n) stop("z must have one row per entry of y.", call. = FALSE)
  dz <- ncol(Z)
  key <- apply(Z, 1L, paste, collapse = "\r")
  ukey <- unique(key)
  m_lev <- length(ukey)
  if (m_lev < 2L) {
    stop("z takes a single value; alpha is not identified.", call. = FALSE)
  }
  support <- do.call(rbind, lapply(ukey, function(k) Z[match(k, key), ]))
  # Sort the support lexicographically. numpy's unique() returns sorted
  # rows while R's unique() preserves first appearance, so without this
  # the two languages number the strata differently and every
  # stratum-indexed output -- J, the weights, the delta matrix -- comes
  # out permuted.
  ord <- do.call(order, as.data.frame(support))
  support <- support[ord, , drop = FALSE]
  ukey <- ukey[ord]

  # (2.46): stratum-wise average derivatives, combined by sample-share
  # weights and renormalised by the first component. beta is estimated
  # here rather than supplied, matching morie.fn.hrzdiscd -- the whole
  # point of the section is that alpha cannot be got this way, not that
  # beta cannot.
  if (is.null(beta)) {
    deltas <- t(vapply(ukey, function(k) {
      sel <- key == k
      if (sum(sel) < 10L) {
        stop(sprintf("stratum %s has %d observations, too few for an average-derivative estimate.",
                     k, sum(sel)), call. = FALSE)
      }
      as.numeric(morie_average_derivative(X[sel, , drop = FALSE], yv[sel])$delta)
    }, numeric(d)))
    wn <- vapply(ukey, function(k) sum(key == k) / n, numeric(1))
    den <- sum(wn * deltas[, 1L])
    if (den == 0) {
      stop("the weighted first component of the stratum average derivatives is zero; beta is not normalisable.",
           call. = FALSE)
    }
    b <- as.numeric(colSums(wn * deltas) / den)
  } else {
    b <- as.numeric(beta) / abs(as.numeric(beta)[1L])
    deltas <- NULL
    wn <- NULL
  }
  beta_source <- if (is.null(beta)) "stratum-wise (2.46)" else "supplied"
  v <- as.numeric(X %*% b)
  hh <- if (is.null(bandwidth)) .hrz_silverman(v) else as.numeric(bandwidth)
  if (hh <= 0) stop(sprintf("bandwidth must be positive, got %g.", hh),
                    call. = FALSE)
  fitted <- .morie_hrz_nw(v, yv, v, hh)
  cc0 <- if (is.null(c0)) as.numeric(stats::quantile(fitted, 0.2, na.rm = TRUE)) else as.numeric(c0)
  cc1 <- if (is.null(c1)) as.numeric(stats::quantile(fitted, 0.8, na.rm = TRUE)) else as.numeric(c1)
  if (cc0 >= cc1) {
    stop(sprintf("need c0 < c1, got (%g, %g).", cc0, cc1), call. = FALSE)
  }
  rng <- stats::quantile(v, c(0.1, 0.9))
  grid <- seq(rng[1L], rng[2L], length.out = as.integer(n_grid))
  jj <- vapply(ukey, function(k) {
    sel <- key == k
    gm <- .morie_hrz_nw(v[sel], yv[sel], grid, hh)
    trunc <- ifelse(gm < cc0, cc0, ifelse(gm > cc1, cc1, gm))
    sum(diff(grid) * (utils::head(trunc, -1L) + utils::tail(trunc, -1L)) / 2)
  }, numeric(1))
  wmat <- support[-1L, , drop = FALSE] -
    matrix(support[1L, ], m_lev - 1L, dz, byrow = TRUE)
  dj <- jj[-1L] - jj[1L]
  wtw <- crossprod(wmat)
  identified <- qr(wtw)$rank == dz
  alpha <- if (identified) as.numeric(solve(wtw, crossprod(wmat, dj))) /
    (cc1 - cc0) else NULL
  list(beta = b, alpha = alpha, support_z = support,
       delta_by_stratum = deltas, weights = wn, J = as.numeric(jj),
       beta_source = beta_source,
       c0 = cc0, c1 = cc1, identified = identified,
       average_derivative_can_estimate_alpha = FALSE,
       n = n, d = d, dz = dz,
       method = "(2.46) for beta; (2.48) for alpha, since derivatives in z do not exist")
}
