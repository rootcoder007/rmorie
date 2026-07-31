# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Shared machinery for fitting a semivariogram model to data.
#
# Schabenberger & Gotway (2005), Statistical Methods for Spatial Data
# Analysis, Sec. 4.5.1 (least squares) and Sec. 4.5.2 (likelihood).
#
# Internal; the public entry points are spols(), spwls() and spreml().
# The `aaa_` prefix keeps these collated before their callers -- R never
# sources a file whose name begins with a non-alphanumeric character.

.schab_as_empirical_variogram <- function(ev) {
  # Accept the list the empirical estimator returns, or a plain matrix.
  # Counts default to 1 so an unweighted table still fits; that makes the WLS
  # weights degenerate to 1/(2 gamma^2), the right limiting form rather than
  # a silent failure.
  if (is.list(ev) && !is.null(ev$gamma)) {
    # `.sp_empirical_variogram` names these lag / gamma / n_pairs; accept the
    # lags / counts spelling too so a hand-built list also works.
    lags <- as.numeric(if (!is.null(ev$lag)) ev$lag else ev$lags)
    gamma <- as.numeric(ev$gamma)
    counts <- if (!is.null(ev$n_pairs)) as.numeric(ev$n_pairs)
              else if (!is.null(ev$counts)) as.numeric(ev$counts)
              else rep(1, length(lags))
    return(list(lags = lags, gamma = gamma, counts = counts))
  }
  if (is.list(ev) && is.null(names(ev)) && length(ev) %in% c(2L, 3L)) {
    lags <- as.numeric(ev[[1]])
    gamma <- as.numeric(ev[[2]])
    counts <- if (length(ev) == 3L) as.numeric(ev[[3]]) else rep(1, length(lags))
    return(list(lags = lags, gamma = gamma, counts = counts))
  }
  arr <- as.matrix(ev)
  if (ncol(arr) < 2L) {
    stop("`empirical_variogram` needs at least lag and gamma columns")
  }
  counts <- if (ncol(arr) > 2L) arr[, 3] else rep(1, nrow(arr))
  list(lags = arr[, 1], gamma = arr[, 2], counts = counts)
}

.schab_start_and_bounds <- function(lags, ghat) {
  # The bounds ARE the parameter space of Sec. 4.3: a nugget and a partial
  # sill are variances so they are non-negative, and a range is a distance so
  # it is strictly positive. Nothing narrower -- a fit that cannot reach
  # sill = 0 cannot report "no spatial structure", and one that cannot reach
  # nugget = 0 cannot report a continuous field.
  finite <- is.finite(ghat)
  if (!any(finite)) stop("empirical semivariogram is entirely non-finite")
  gmax <- max(ghat[finite])
  hmax <- max(lags[finite])
  list(start = c(0.1 * gmax, 0.9 * gmax, 0.5 * hmax),
       lo = c(0, 0, 1e-8 * hmax + 1e-12),
       hi = c(10 * gmax + 1, 10 * gmax + 1, 10 * hmax))
}

.schab_objective <- function(kind, lags, ghat, counts, model) {
  ok <- is.finite(ghat) & is.finite(lags) & counts > 0
  h <- lags[ok]; g <- ghat[ok]; n <- counts[ok]
  function(theta) {
    nugget <- theta[1]; sill <- theta[2]; rng <- theta[3]
    if (nugget < 0 || sill < 0 || rng <= 0) return(Inf)
    fitted <- .sp_semivariogram(h, nugget, sill, rng, model)
    resid <- g - fitted
    if (identical(kind, "ols")) {
      # R = phi * I: the OLS simplification named in the text after eq (4.34),
      # which ignores both the correlation and the unequal dispersion among
      # the gamma-hat(h_m).
      return(sum(resid * resid))
    }
    # eq (4.34): sum_m |N(h_m)| / (2 gamma(h_m,theta)^2) * resid_m^2. The
    # weights are functions of theta, which is what makes this a re-weighted
    # rather than a plain weighted fit.
    denom <- 2 * fitted^2
    good <- denom > 0
    if (!any(good)) return(Inf)
    sum(n[good] * resid[good]^2 / denom[good])
  }
}

.schab_fit_semivariogram <- function(lags, ghat, counts, model = "exponential",
                                     kind = "wls") {
  if (!kind %in% c("ols", "wls")) stop("`kind` must be 'ols' or 'wls'")
  ok <- is.finite(ghat) & is.finite(lags) & counts > 0
  if (sum(ok) < 3L) {
    stop("need at least 3 usable lag classes to fit 3 parameters")
  }
  f <- .schab_objective(kind, lags, ghat, counts, model)
  sb <- .schab_start_and_bounds(lags[ok], ghat[ok])
  start <- sb$start; lo <- sb$lo; hi <- sb$hi

  # Bounds enforced inside the objective, not by the solver: optim() ignores
  # bounds under Nelder-Mead, and pushing the box into the function is also
  # what lets this arm run the identical search to the Python one rather than
  # two solvers that merely agree in intent.
  bounded <- function(theta) {
    if (any(!is.finite(theta)) || any(theta < lo) || any(theta > hi)) return(Inf)
    f(theta)
  }

  # Several starts: these objectives have a flat ridge along (nugget + sill)
  # and a simplex launched onto it stalls. The starts span the nugget
  # fraction, which is the direction the ridge runs in.
  best_x <- start
  best_f <- bounded(start)
  for (frac in c(0.0, 0.1, 0.3, 0.6)) {
    for (rscale in c(0.25, 0.5, 1.0)) {
      x0 <- pmin(pmax(c(frac * start[2], start[2], rscale * 2 * start[3]), lo), hi)
      res <- stats::optim(x0, bounded, method = "Nelder-Mead",
                          control = list(maxit = 4000, reltol = 1e-12))
      if (is.finite(res$value) && res$value < best_f) {
        best_x <- res$par
        best_f <- res$value
      }
    }
  }
  converged <- (best_f < bounded(start)) || isTRUE(all.equal(best_x, start))
  list(nugget = best_x[1], partial_sill = best_x[2], range = best_x[3],
       objective = best_f, converged = converged)
}

.schab_covariance_matrix <- function(coords, nugget, sill, rng, model) {
  # Sigma(theta) for the model of Sec. 4.3. C(0) = c0 + sigma0^2 and
  # C(h) = sigma0^2 R(h) for h > 0, so the nugget enters only on the
  # diagonal -- it is a discontinuity at the origin, not a term added
  # everywhere.
  coords <- as.matrix(coords)
  d <- as.matrix(stats::dist(coords))
  # .sp_correlogram works on a plain vector, so rebuild the matrix shape
  # rather than relying on R preserving dim through it.
  cr <- .sp_correlogram(as.numeric(d), rng, model)
  sigma <- matrix(sill * cr, nrow = nrow(d), ncol = ncol(d))
  diag(sigma) <- nugget + sill
  sigma
}

.schab_error_contrasts <- function(X) {
  # A matrix K of error contrasts: full row rank, K X = 0. Sec. 4.5.2 builds
  # K explicitly for the intercept-only case and notes, citing Harville
  # (1974), that the choice of K does not matter for estimation. Taking the
  # orthogonal complement of the column space of X gives one such K for any
  # linear mean structure.
  X <- as.matrix(X)
  n <- nrow(X); p <- ncol(X)
  s <- svd(X, nu = n, nv = p)
  tol <- max(n, p) * .Machine$double.eps * (if (length(s$d)) s$d[1] else 1)
  rank <- sum(s$d > tol)
  if (rank >= n) stop("design matrix leaves no error contrasts")
  t(s$u[, (rank + 1L):n, drop = FALSE])
}
