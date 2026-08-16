# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geographically weighted regression primitives.
# Twin of the Python arm's src/morie/fn/_schab_gwr.py -- the same equations
# and the same arithmetic, so the two arms agree by construction rather than
# to a tolerance nobody chose.
#
# Schabenberger, O. & Gotway, C. A. (2005), Statistical Methods for Spatial
# Data Analysis, Sec. 6.1.3.1, pp. 316-317: the model (6.9), the weighted
# least squares estimator, the hat matrix L whose ith row is
# x(s_i)'{X'W(s_i)X}^{-1}X'W(s_i), and Cressie's residual variance
# (Z - Zhat)'(Z - Zhat) / tr{(I-L)(I-L)'}. For the kernels the book refers
# back to Sec. 5.3.2, pp. 240-241 (Epanechnikov and Gaussian density forms,
# the tri-cube attributed to Cleveland 1979).
#
# The book gives NO bandwidth-selection criterion; it defers to Fotheringham,
# Brunsdon & Charlton (2002), Geographically Weighted Regression: The Analysis
# of Spatially Varying Relationships (Wiley), which has now been read
# directly. Checked against it: eq (2.24) the Gaussian kernel; eq (2.25) the
# bi-square, zero unless d_ij < b (strict); eq (2.31) the CV score with the
# observations for point i omitted from the calibration; eq (2.33) = eq (4.21)
# the AICc; eq (4.22) the AIC; eq (4.23) fixing sigma^2 = RSS/n for both;
# eqs (2.17)-(2.18) v1 = tr(S) and v2 = tr(S'S) with 2v1 - v2 the effective
# number of parameters; eq (2.16) the inference variance with denominator
# n - 2v1 + v2; eqs (2.14)-(2.15) Var[beta] = CC' sigma^2; eq (2.20) the
# hat-matrix row. Golden section is the book's own named search method
# (p. 60, after Greig 1980).
#
# Corroborated against material by the same authors and their own reference
# implementations:
#
#   Charlton, M., Geographically Weighted Regression -- White Paper, pp. 6-8:
#   the Gaussian and bisquare kernels, the effective parameter count
#   2 tr(S) - tr(S'S), and, citing Hurvich, Simonoff & Tsai (1998),
#     AICc = 2 n log(sigma) + n log(2 pi) + n (n + tr S) / (n - 2 - tr S).
#
#   spgwr (CRAN), R/gwr.cv.R: gwr.aic.f fixes sigma^2 as the ML estimate
#   y'(I-S)'(I-S)y / n, and gwr.cv.f fixes the CV score as leave-one-out --
#   the local fit at i is taken with w_ii forced to zero.
#
#   GWmodel (CRAN), R/gw.weight.r: the four kernels this shelf names --
#   boxcar, gaussian, bisquare, tricube -- citing Fotheringham et al. (2002)
#   pp. 56-57. The only source consulted that names the boxcar.
#
#   mgwr (Oshan, Li, Kang, Wolf & Fotheringham), mgwr/search.py multi_bw:
#   the MGWR backfitting algorithm.
#
#   Fotheringham, Yang & Kang (2017), Ann. Amer. Assoc. Geogr.
#   107(6):1247-1265, read directly: eq (9) SOC-RSS, eq (10) SOC-f, the
#   back-fitting algorithm of Figure 1, GWR estimates as the initialisation,
#   and SOC-f below 1e-5 as the termination criterion.
#
#   Fotheringham, Oshan & Li (2024), Multiscale Geographically Weighted
#   Regression: Theory and Practice, 1st ed., CRC Press,
#   doi:10.1201/9781003435464. Sec 2.3.2 eqs (2.38)-(2.39) restate the SOC;
#   Sec 2.3.3.2 and Sec 6.3 require standardization before the
#   covariate-specific bandwidths can be compared. Its eqs (2.40)-(2.45) are
#   NOT implemented here.
#
# Two printed things that could not be transcribed as written are recorded
# in the Python twin's module docstring: the stub's AICc, which reproduces
# no published number, and Sec. 5.3.2's density-form Gaussian, which differs
# from the GWR literature's by a constant that weighted least squares cannot
# see.
#
# Internal; `aaa_` collates it before its callers.

.schab_gwr_kernels <- c("gaussian", "bisquare", "tricube", "boxcar")

#' .schab_pairwise_distances
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_mgwr_backfit}, \code{.schab_select_bandwidth}, \code{spgwrb}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coords A matrix; passed to \code{ncol}.
#' @return A matrix, from \code{as.matrix}.
#' @export
.schab_pairwise_distances <- function(coords) {
  coords <- as.matrix(coords)
  if (ncol(coords) < 1L) stop("coords must have at least one column")
  as.matrix(stats::dist(coords, method = "euclidean"))
}

#' .schab_reshape_like
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_kernel_weights}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param values See Usage.
#' @param template A matrix; passed to \code{dim}.
#' @return The value of \code{array}.
#' @export
.schab_reshape_like <- function(values, template) {
  d <- dim(template)
  if (is.null(d)) {
    return(as.numeric(values))
  }
  array(values, dim = d)
}

#' .schab_kernel_weights
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_local_weights}, \code{spgwrk}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param distance Passed to \code{.schab_reshape_like}.
#' @param bandwidth See Usage.
#' @param kernel Compared against \code{"gaussian"}. Defaults to \code{"gaussian"}.
#' @param normalized A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{.schab_reshape_like}.
#' @export
.schab_kernel_weights <- function(distance, bandwidth, kernel = "gaussian",
                                  normalized = FALSE) {
  d <- as.numeric(distance)
  h <- as.numeric(bandwidth)[1L]
  if (!is.finite(h) || h <= 0) {
    stop("bandwidth must be a positive finite number")
  }
  if (!kernel %in% .schab_gwr_kernels) {
    stop(sprintf("unknown kernel '%s'", kernel))
  }
  if (any(d < 0)) stop("distances must be non-negative")
  z <- d / h
  if (kernel == "gaussian") {
    w <- exp(-0.5 * z * z)
    if (normalized) w <- w / (h * sqrt(2 * pi))
    return(.schab_reshape_like(w, distance))
  }
  if (normalized) stop("normalized applies to the Gaussian kernel only")
  inside <- z < 1
  w <- switch(kernel,
    bisquare = ifelse(inside, (1 - z * z)^2, 0),
    tricube  = ifelse(inside, (1 - z^3)^3, 0),
    boxcar   = ifelse(inside, 1, 0)
  )
  .schab_reshape_like(w, distance)
}

# mgwr/kernels.py: the n_neighbours-th order statistic, nudged by eps so the
# neighbour itself falls strictly inside a truncated kernel's support. The
# regression point counts as its own first neighbour.
#' Mgwr/kernels.py: the n_neighbours-th order statistic, nudged by eps
#' so the
#'
#' neighbour itself falls strictly inside a truncated kernel\'s support.
#' The regression point counts as its own first neighbour.
#'
#' @param distance_row See Usage.
#' @param n_neighbours See Usage.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{1.0000001}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schab_adaptive_bandwidth <- function(distance_row, n_neighbours,
                                      eps = 1.0000001) {
  d <- sort(as.numeric(distance_row))
  k <- as.integer(n_neighbours)
  if (is.na(k) || k < 1L || k > length(d)) {
    stop(sprintf("n_neighbours must be in 1 to %d", length(d)))
  }
  as.numeric(d[k] * eps)
}

#' .schab_local_weights
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_cv_score}, \code{.schab_gwr_fit}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param d_row Passed to \code{.schab_adaptive_bandwidth}.
#' @param bandwidth Passed to \code{.schab_adaptive_bandwidth}.
#' @param kernel Passed to \code{.schab_kernel_weights}.
#' @param adaptive A flag; the body branches on it.
#' @return The value of \code{.schab_kernel_weights}.
#' @export
.schab_local_weights <- function(d_row, bandwidth, kernel, adaptive) {
  h <- if (adaptive) .schab_adaptive_bandwidth(d_row, bandwidth) else bandwidth
  .schab_kernel_weights(d_row, h, kernel)
}

# Weighted least squares through the SVD of the square-root-weighted design,
# rather than by inverting X'WX -- and ALWAYS through the SVD, never by
# branching on whether an inversion raised. A truncated kernel can leave a
# local fit with fewer non-zero weights than parameters, at which point X'WX
# is rank deficient. R's solve() raises there and numpy's inv() does not
# (it returned a garbage inverse at a condition number of 6e15), which put
# the two arms 1.5e+02 apart on a bisquare CV score. Deciding rank
# explicitly, with the same cutoff in both arms, removes the divergence and
# the silent garbage together. Rank-deficient fits get the minimum-norm
# solution; the caller is told how many there were.
#
# The cutoff is numpy.linalg.pinv's default: max(dim) * eps * largest
# singular value. Written out rather than taken from MASS::ginv because MASS
# is only in Suggests.
#' The cutoff is numpy.linalg.pinv\'s default: max(dim) * eps * largest
#'
#' singular value. Written out rather than taken from MASS::ginv because
#' MASS is only in Suggests.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param w Numeric; passed to \code{sqrt}.
#' @return The value of \code{op}, as built in the body.
#' @export
.schab_wls_operator <- function(X, w) {
  sw <- sqrt(w)
  Xw <- X * sw
  sv <- svd(Xw)
  cutoff <- max(dim(Xw)) * .Machine$double.eps * max(sv$d)
  keep <- sv$d > cutoff
  rank <- sum(keep)
  if (rank == 0L) {
    op <- matrix(0, ncol(X), nrow(X))
  } else {
    pinv_Xw <- sv$v[, keep, drop = FALSE] %*%
      (t(sv$u[, keep, drop = FALSE]) / sv$d[keep])
    op <- sweep(pinv_Xw, 2L, sw, "*")
  }
  attr(op, "rank") <- rank
  op
}

#' .schab_gwr_fit
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_gwr_criterion}, \code{.schab_mgwr_backfit}, \code{spgwrb}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A matrix; passed to \code{t}.
#' @param X A matrix; indexed by row and column.
#' @param distances A matrix; passed to \code{as.matrix}.
#' @param bandwidth Passed to \code{.schab_local_weights}.
#' @param kernel Passed to \code{.schab_local_weights}. Defaults to \code{"gaussian"}.
#' @param adaptive Passed to \code{.schab_local_weights}. Defaults to \code{FALSE}.
#' @return A list with \code{se_params}, \code{sigma2_gwr}, \code{edf_resid}, \code{v1}, \code{v2}, \code{params}, \code{fitted}, \code{resid}, \code{S}, \code{tr_S}, \code{tr_STS}, \code{effective_parameters}, \code{rss}, \code{sigma2}, \code{sigma2_cressie}, \code{n}, \code{p}, \code{bandwidth}, \code{kernel}, \code{adaptive}, \code{n_rank_deficient}.
#' @export
.schab_gwr_fit <- function(y, X, distances, bandwidth, kernel = "gaussian",
                           adaptive = FALSE) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  D <- as.matrix(distances)
  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n || !identical(dim(D), c(n, n))) {
    stop("y, X and the distance matrix disagree on n")
  }
  params <- matrix(0, n, p)
  S <- matrix(0, n, n)
  ccT <- matrix(0, n, p)
  n_deficient <- 0L
  for (i in seq_len(n)) {
    w <- .schab_local_weights(D[i, ], bandwidth, kernel, adaptive)
    op <- .schab_wls_operator(X, w)
    if (attr(op, "rank") < p) n_deficient <- n_deficient + 1L
    params[i, ] <- as.numeric(op %*% y)
    S[i, ] <- as.numeric(X[i, , drop = FALSE] %*% op)
    # eqs (2.14)-(2.15): C = (X'WX)^-1 X'W, Var[beta(u_i,v_i)] = C C' sigma^2
    ccT[i, ] <- rowSums(op * op)
  }
  fitted <- as.numeric(S %*% y)
  resid <- y - fitted
  ImS <- diag(n) - S
  B <- crossprod(ImS, ImS)
  rss <- as.numeric(t(y) %*% B %*% y)
  tr_S <- sum(diag(S))
  tr_STS <- sum(diag(crossprod(S, S)))
  trace_B <- sum(diag(B))
  # eq (2.16): the book's own residual variance for INFERENCE, denominator
  # n - 2v1 + v2, the effective residual degrees of freedom. Distinct from
  # eq (4.23)'s ML estimate, which is what the AIC and AICc take (the book
  # says so outright), and from Schabenberger's Cressie estimate with
  # tr{(I-L)(I-L)'}. Three denominators for three jobs.
  edf_resid <- n - 2 * tr_S + tr_STS
  sigma2_gwr <- if (edf_resid > 0) rss / edf_resid else NA_real_
  se_params <- sqrt(pmax(ccT * sigma2_gwr, 0))
  list(
    se_params = se_params, sigma2_gwr = sigma2_gwr, edf_resid = edf_resid,
    v1 = tr_S, v2 = tr_STS,
    params = params, fitted = fitted, resid = resid, S = S,
    tr_S = tr_S, tr_STS = tr_STS,
    effective_parameters = 2 * tr_S - tr_STS,
    rss = rss, sigma2 = rss / n,
    sigma2_cressie = if (trace_B > 0) rss / trace_B else NA_real_,
    n = n, p = p, bandwidth = bandwidth, kernel = kernel,
    adaptive = adaptive,
    # Local fits with fewer estimable directions than parameters -- a
    # bandwidth too narrow for the design, not a numerical accident.
    n_rank_deficient = n_deficient
  )
}

# Charlton white paper p. 8; spgwr::gwr.aic.f; mgwr.diagnostics.get_AICc.
# Fotheringham et al. (2002) p. 61 eq (2.33) / p. 96 eq (4.21).
#' Charlton white paper p. 8; spgwr::gwr.aic.f;
#' mgwr.diagnostics.get_AICc
#'
#' Fotheringham et al. (2002) p. 61 eq (2.33) / p. 96 eq (4.21).
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param sigma2 Numeric; passed to \code{sqrt}.
#' @param tr_S Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.schab_aicc_from_parts <- function(n, sigma2, tr_S) {
  n <- as.numeric(n)
  tr_S <- as.numeric(tr_S)
  denom <- n - 2 - tr_S
  if (denom <= 0) {
    return(Inf)
  }
  2 * n * log(sqrt(sigma2)) + n * log(2 * pi) + n * (n + tr_S) / denom
}

# Fotheringham et al. (2002) p. 96 eq (4.22).
#' Fotheringham et al. (2002) p. 96 eq (4.22)
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_gwr_criterion}, \code{spgwrb}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param sigma2 Numeric; passed to \code{sqrt}.
#' @param tr_S See Usage.
#' @return A numeric value.
#' @export
.schab_aic_from_parts <- function(n, sigma2, tr_S) {
  n <- as.numeric(n)
  2 * n * log(sqrt(sigma2)) + n * log(2 * pi) + n + as.numeric(tr_S)
}

# spgwr::gwr.cv.f -- leave-one-out; y_i never predicts itself.
#' Spgwr::gwr.cv.f -- leave-one-out; y_i never predicts itself
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_gwr_criterion}, \code{spgwrb}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A matrix; passed to \code{\%*\%}.
#' @param X A matrix; indexed by row and column.
#' @param distances A matrix; passed to \code{as.matrix}.
#' @param bandwidth Passed to \code{.schab_local_weights}.
#' @param kernel Passed to \code{.schab_local_weights}. Defaults to \code{"gaussian"}.
#' @param adaptive Passed to \code{.schab_local_weights}. Defaults to \code{FALSE}.
#' @return The value of \code{total}, as built in the body.
#' @export
.schab_cv_score <- function(y, X, distances, bandwidth, kernel = "gaussian",
                            adaptive = FALSE) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  D <- as.matrix(distances)
  n <- nrow(X)
  total <- 0
  for (i in seq_len(n)) {
    w <- .schab_local_weights(D[i, ], bandwidth, kernel, adaptive)
    w[i] <- 0
    if (!any(w > 0)) {
      return(Inf)
    }
    beta_i <- as.numeric(.schab_wls_operator(X, w) %*% y)
    r <- y[i] - sum(X[i, ] * beta_i)
    total <- total + r * r
  }
  total
}

#' .schab_gwr_criterion
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_select_bandwidth}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.schab_cv_score}.
#' @param X Passed to \code{.schab_cv_score}.
#' @param distances Passed to \code{.schab_cv_score}.
#' @param bandwidth Passed to \code{.schab_cv_score}.
#' @param kernel Passed to \code{.schab_cv_score}. Defaults to \code{"gaussian"}.
#' @param adaptive Passed to \code{.schab_cv_score}. Defaults to \code{FALSE}.
#' @param criterion One of \code{"aic"}, \code{"aicc"}, \code{"cv"}. Defaults to \code{"cv"}.
#' @return One of two values, depending on the branch taken.
#' @export
.schab_gwr_criterion <- function(y, X, distances, bandwidth,
                                 kernel = "gaussian", adaptive = FALSE,
                                 criterion = "cv") {
  if (criterion == "cv") {
    return(.schab_cv_score(y, X, distances, bandwidth, kernel, adaptive))
  }
  if (!criterion %in% c("aicc", "aic")) {
    stop(sprintf("unknown criterion '%s'", criterion))
  }
  fit <- .schab_gwr_fit(y, X, distances, bandwidth, kernel, adaptive)
  if (fit$sigma2 <= 0) {
    return(Inf)
  }
  if (criterion == "aicc") {
    .schab_aicc_from_parts(fit$n, fit$sigma2, fit$tr_S)
  } else {
    .schab_aic_from_parts(fit$n, fit$sigma2, fit$tr_S)
  }
}

# Golden section rather than R's optimize (Brent): deterministic and with no
# parabolic-interpolation step whose tie-breaking the Python arm would have
# to match bit for bit.
#' Golden section rather than R\'s optimize (Brent): deterministic and
#' with no
#'
#' parabolic-interpolation step whose tie-breaking the Python arm would
#' have to match bit for bit.
#'
#' @param func See Usage.
#' @param lower See Usage.
#' @param upper See Usage.
#' @param tol Defaults to \code{1e-04}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.
#' @return A list with \code{x}, \code{value}.
#' @export
.schab_golden_section <- function(func, lower, upper, tol = 1e-4,
                                  max_iter = 200L) {
  invphi <- (sqrt(5) - 1) / 2
  a <- as.numeric(lower)
  b <- as.numeric(upper)
  if (!(b > a)) stop("upper must exceed lower")
  cc <- b - invphi * (b - a)
  dd <- a + invphi * (b - a)
  fc <- func(cc)
  fd <- func(dd)
  for (i in seq_len(max_iter)) {
    if (abs(b - a) < tol) break
    if (fc < fd) {
      b <- dd
      dd <- cc
      fd <- fc
      cc <- b - invphi * (b - a)
      fc <- func(cc)
    } else {
      a <- cc
      cc <- dd
      fc <- fd
      dd <- a + invphi * (b - a)
      fd <- func(dd)
    }
  }
  x <- 0.5 * (a + b)
  list(x = x, value = func(x))
}

# spgwr::gwr.sel's search interval: the bounding-box diagonal and a
# thousandth of it.
#' Spgwr::gwr.sel\'s search interval: the bounding-box diagonal and a
#'
#' thousandth of it.
#'
#' @param coords A matrix; passed to \code{as.matrix}.
#' @return A vector, from \code{c}.
#' @export
.schab_default_bounds <- function(coords) {
  coords <- as.matrix(coords)
  span <- apply(coords, 2, max) - apply(coords, 2, min)
  diag_len <- sqrt(sum(span^2))
  if (diag_len <= 0) {
    stop("coordinates are degenerate; cannot set a bandwidth range")
  }
  c(diag_len / 1000, diag_len)
}

#' .schab_select_bandwidth
#'
#' A step of the schab_gwr_shared implementation. Called by \code{.schab_mgwr_backfit}, \code{spgwrb}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.schab_gwr_criterion}.
#' @param X Passed to \code{.schab_gwr_criterion}.
#' @param coords Passed to \code{.schab_pairwise_distances}.
#' @param kernel Passed to \code{.schab_gwr_criterion}. Defaults to \code{"gaussian"}.
#' @param criterion Passed to \code{.schab_gwr_criterion}. Defaults to \code{"cv"}.
#' @param adaptive A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param bounds Defaults to \code{NULL}.
#' @param tol Passed to \code{.schab_golden_section}. Defaults to \code{1e-04}.
#' @return A list with \code{bandwidth}, \code{score}, \code{criterion}, \code{bounds}, \code{adaptive}.
#' @export
.schab_select_bandwidth <- function(y, X, coords, kernel = "gaussian",
                                    criterion = "cv", adaptive = FALSE,
                                    bounds = NULL, tol = 1e-4) {
  D <- .schab_pairwise_distances(coords)
  if (adaptive) {
    n <- nrow(D)
    rng <- if (is.null(bounds)) c(2, n) else bounds
    grid <- seq.int(ceiling(rng[1]), floor(rng[2]))
    scores <- vapply(grid, function(k) {
      .schab_gwr_criterion(y, X, D, k, kernel, TRUE, criterion)
    }, numeric(1))
    best <- grid[which.min(scores)]
    return(list(
      bandwidth = as.integer(best), score = min(scores),
      criterion = criterion,
      bounds = c(grid[1], grid[length(grid)]),
      adaptive = TRUE, grid = grid, scores = scores
    ))
  }
  rng <- if (is.null(bounds)) .schab_default_bounds(coords) else bounds
  opt <- .schab_golden_section(
    function(h) .schab_gwr_criterion(y, X, D, h, kernel, FALSE, criterion),
    rng[1], rng[2],
    tol = tol
  )
  list(
    bandwidth = opt$x, score = opt$value, criterion = criterion,
    bounds = rng, adaptive = FALSE
  )
}

# mgwr/search.py multi_bw. Each covariate j is smoothed against the partial
# residual XB[, j] + err by a univariate GWR with its own bandwidth; err is
# threaded through the inner loop so later covariates in the same sweep see
# the earlier updates.
#
# `standardize` is on by default, which is not a stylistic choice.
# Fotheringham, Oshan & Li (2024) Sec. 2.3.3.2: "in order to effectively
# compare the values of the estimated bandwidth to each other, it is
# necessary to first standardize the input data so that y and each column of
# X have a mean of zero and variance of one before using the data in the
# MGWR calibration routine". Sec. 6.3 adds that in the authors' own software
# this "is one of the default settings and has to be actively turned off",
# because "without data standardization, the optimized covariate-specific
# bandwidths will be, in part, a function of the variability of each
# covariate". Constant columns are left alone -- an intercept has no
# variance to normalise.
#
# NOT implemented, named rather than left silently missing: the
# covariate-specific hat matrices R_k of Fotheringham, Oshan & Li (2024)
# eqs (2.40)-(2.42), the per-covariate effective parameter counts
# ENP_k = tr(R_k) of eq (2.43), and the covariate-specific adjusted alpha of
# eq (2.45), all after Yu et al. (2020). This returns bandwidths and
# coefficients, not MGWR inference.
#
# A caution the SOC makes necessary: both scores measure how much the fit
# MOVED, not how good it is. When the initial single-bandwidth GWR already
# sits at the wide end of the interval, the first sweep can leave every
# covariate there, the score is tiny, and the loop stops after two or three
# sweeps having found no scale separation at all. Measured over eight seeds
# of a fixture with two genuinely different scales this happened twice. It
# is a property of the criterion, not of this port -- the reference uses the
# same score and the same default tolerance. `at_search_boundary` flags it.
#' A caution the SOC makes necessary: both scores measure how much the
#' fit
#'
#' MOVED, not how good it is. When the initial single-bandwidth GWR
#' already sits at the wide end of the interval, the first sweep can
#' leave every covariate there, the score is tiny, and the loop stops
#' after two or three sweeps having found no scale separation at all.
#' Measured over eight seeds of a fixture with two genuinely different
#' scales this happened twice. It is a property of the criterion, not of
#' this port -- the reference uses the same score and the same default
#' tolerance. `at_search_boundary` flags it.
#'
#' @param y A matrix; passed to \code{nrow}.
#' @param X A matrix; indexed by row and column.
#' @param coords Passed to \code{.schab_pairwise_distances}.
#' @param kernel Passed to \code{.schab_gwr_fit}. Defaults to \code{"gaussian"}.
#' @param criterion Passed to \code{.schab_select_bandwidth}. Defaults to \code{"aicc"}.
#' @param adaptive A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param tol Defaults to \code{1e-05}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.
#' @param rss_score A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param bws_same_times Defaults to \code{5L}.
#' @param init_bandwidth Defaults to \code{NULL}.
#' @param standardize A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{bandwidths}, \code{at_search_boundary}, \code{standardized}, \code{y_centre}, \code{y_scale}, \code{x_centre}, \code{x_scale}, \code{params}, \code{fitted}, \code{resid}, \code{bandwidth_gwr}, \code{bandwidth_history}, \code{score_history}, \code{n_iter}, \code{converged}, \code{criterion}, \code{kernel}.
#' @export
.schab_mgwr_backfit <- function(y, X, coords, kernel = "gaussian",
                                criterion = "aicc", adaptive = FALSE,
                                tol = 1e-5, max_iter = 200L,
                                rss_score = FALSE, bws_same_times = 5L,
                                init_bandwidth = NULL, standardize = TRUE) {
  y <- matrix(as.numeric(y), ncol = 1L)
  X <- as.matrix(X)
  n <- nrow(X)
  k <- ncol(X)
  if (nrow(y) != n) stop("y and X disagree on n")
  D <- .schab_pairwise_distances(coords)

  y_raw <- as.numeric(y)
  y_centre <- 0
  y_scale <- 1
  x_centre <- rep(0, k)
  x_scale <- rep(1, k)
  sd_pop <- function(v) sqrt(mean((v - mean(v))^2))
  if (standardize) {
    y_centre <- mean(as.numeric(y))
    y_scale <- sd_pop(as.numeric(y))
    if (y_scale == 0) y_scale <- 1
    y <- matrix((as.numeric(y) - y_centre) / y_scale, ncol = 1L)
    for (j in seq_len(k)) {
      sdj <- sd_pop(X[, j])
      if (sdj > 0) { # leave a constant column alone
        x_centre[j] <- mean(X[, j])
        x_scale[j] <- sdj
      }
    }
    X <- sweep(sweep(X, 2L, x_centre, "-"), 2L, x_scale, "/")
  }

  fit_sub <- function(resp, design, bw) {
    .schab_gwr_fit(as.numeric(resp), design, D, bw, kernel, adaptive)
  }
  select_sub <- function(resp, design) {
    .schab_select_bandwidth(as.numeric(resp), design, coords,
      kernel = kernel,
      criterion = criterion, adaptive = adaptive
    )$bandwidth
  }

  bw_gwr <- if (is.null(init_bandwidth)) select_sub(y, X) else init_bandwidth
  optim <- fit_sub(y, X, bw_gwr)
  err <- matrix(optim$resid, ncol = 1L)
  XB <- X * optim$params

  rss <- sum(err^2)
  bws <- numeric(k)
  bw_history <- list()
  score_history <- numeric(0)
  stable <- 0L
  converged <- FALSE
  params <- matrix(0, n, k)

  for (sweep in seq_len(max_iter)) {
    new_XB <- matrix(0, n, k)
    params <- matrix(0, n, k)
    for (j in seq_len(k)) {
      temp_y <- XB[, j, drop = FALSE] + err
      temp_X <- X[, j, drop = FALSE]
      bw <- if (stable >= bws_same_times) bws[j] else select_sub(temp_y, temp_X)
      sub <- fit_sub(temp_y, temp_X, bw)
      err <- matrix(sub$resid, ncol = 1L)
      new_XB[, j] <- sub$fitted
      params[, j] <- as.numeric(sub$params)
      bws[j] <- bw
    }

    if (length(bw_history) > 0 && all(bw_history[[length(bw_history)]] == bws)) {
      stable <- stable + 1L
    } else {
      stable <- 0L
    }

    num <- sum((new_XB - XB)^2) / n
    den <- sum(rowSums(new_XB)^2)
    score <- if (den > 0) sqrt(num / den) else Inf
    XB <- new_XB

    if (rss_score) {
      predy <- rowSums(params * X)
      new_rss <- sum((as.numeric(y) - predy)^2)
      score <- if (new_rss > 0) abs((new_rss - rss) / new_rss) else 0
      rss <- new_rss
    }

    score_history <- c(score_history, score)
    bw_history[[length(bw_history) + 1L]] <- bws
    if (score < tol) {
      converged <- TRUE
      break
    }
  }

  # Back to the original units, so `fitted`/`resid` mean what their names
  # say regardless of `standardize`. Coefficients stay on the standardized
  # scale -- the scale on which the source says they are comparable across
  # covariates -- with the centres and scales returned so a caller can undo it.
  fitted <- rowSums(params * X) * y_scale + y_centre
  at_boundary <- if (adaptive) {
    FALSE
  } else {
    all(bws > 0.95 * .schab_default_bounds(coords)[2])
  }
  list(
    bandwidths = bws, at_search_boundary = at_boundary,
    standardized = isTRUE(standardize),
    y_centre = y_centre, y_scale = y_scale,
    x_centre = x_centre, x_scale = x_scale,
    params = params, fitted = fitted,
    resid = y_raw - fitted, bandwidth_gwr = bw_gwr,
    bandwidth_history = bw_history, score_history = score_history,
    n_iter = length(score_history), converged = converged,
    criterion = criterion, kernel = kernel
  )
}
