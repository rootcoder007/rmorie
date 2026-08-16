# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors, part 4: extensions of the maximum-score and
# smoothed maximum-score estimators. Mirrors morie.fn.hrzcbsm,
# hrzpanms, hrzormsc, hrzsmsrc.
#
# Collision scan: horowitz_native4.R, all five exported names and
# .morie_optimize_scale_normalized were free in both R trees.
#
# Spec: Horowitz, J. L., Semiparametric and Nonparametric Methods in
# Econometrics, Springer. Sec. 4.3.3 (smoothed maximum score, the
# rate), Sec. 4.4.1 (choice-based samples, eqs. 4.33/4.35),
# Sec. 4.4.2 (panel data, eqs. 4.39/4.40), Sec. 4.4.3
# (ordered-response models, eq. 4.43). Section numbers checked
# against the printed table of contents.

# Minimise fn over b with b[1] fixed at 1. Every maximum-score variant
# is identified only up to scale.
#
# These objectives are STEP FUNCTIONS of b, which rules out gradient
# methods and makes simplex methods start-dependent -- they stall on
# the first flat region they land in. For d = 2 (one free coordinate,
# and the usual case) an exhaustive grid scan is used instead: it is
# the right method for a piecewise-constant objective AND exactly
# reproducible, so this agrees with morie.fn to the last digit rather
# than to whatever the two languages' simplex routines happen to do.
# Grid spans +/-10 at a resolution of 0.01, matching
# _horowitz.GRID_SCAN_HALF_WIDTH / GRID_SCAN_POINTS.
#
# Callers maximising a score pass its negation.
.MORIE_GRID_SCAN_HALF_WIDTH <- 10
.MORIE_GRID_SCAN_POINTS <- 2001L

#' .morie_optimize_scale_normalized
#'
#' Part of the horowitz_native4 implementation; see the file header for
#' the source it follows.
#'
#' @param fn See Usage.
#' @param d See Usage.
#' @param n_restarts Defaults to \code{8L}.
#' @param seed Defaults to \code{0L}.
#' @param x0 Defaults to \code{NULL}.
#' @return A list with \code{beta}, \code{value}.
#' @export
.morie_optimize_scale_normalized <- function(fn, d, n_restarts = 8L, seed = 0L,
                                             x0 = NULL) {
  d <- as.integer(d)
  if (d < 2L) {
    stop(sprintf("need at least 2 coefficients, got %d.", d),
      call. = FALSE
    )
  }
  if (d == 2L) {
    grid <- seq(-.MORIE_GRID_SCAN_HALF_WIDTH, .MORIE_GRID_SCAN_HALF_WIDTH,
      length.out = .MORIE_GRID_SCAN_POINTS
    )
    vals <- vapply(grid, function(g) fn(c(1, g)), numeric(1))
    k <- which.min(vals)
    return(list(beta = c(1, grid[k]), value = vals[k]))
  }
  set.seed(seed)
  starts <- list(if (is.null(x0)) numeric(d - 1L) else utils::tail(as.numeric(x0), d - 1L))
  for (i in seq_len(as.integer(n_restarts))) starts[[i + 1L]] <- stats::rnorm(d - 1L)
  best <- NULL
  best_val <- Inf
  for (st in starts) {
    r <- stats::optim(st, function(z) fn(c(1, z)),
      method = "Nelder-Mead",
      control = list(maxit = 3000L, reltol = 1e-12)
    )
    if (r$value < best_val) {
      best_val <- r$value
      best <- r$par
    }
  }
  list(beta = c(1, best), value = best_val)
}

#' Variance-minimising shares for a choice-based sample
#'
#' \eqn{q_0 = \sqrt{\pi_0}/(\sqrt{\pi_0} + \sqrt{\pi_1})} and
#' \eqn{q_1 = \sqrt{\pi_1}/(\sqrt{\pi_0} + \sqrt{\pi_1})}. The
#' designer of a choice-based sample picks the stratum sizes, and the
#' asymptotic distribution depends on them only through
#' \eqn{\pi_1/q_1 + \pi_0/q_0}. Minimising that factor gives the
#' square-root rule: the optimum is NOT \eqn{q_j = \pi_j} (a random
#' sample) and not an even split either, except at \eqn{\pi_1 = 1/2}
#' where the two coincide. Mirrors \code{morie.fn.hrzcbsm}.
#'
#' @param pi1 population share with Y = 1, in (0, 1).
#' @return list: q1, q0, factor, factor_at_random_sample, method.
#' @references Horowitz, Sec. 4.4.1, printed p. 123.
#' @examples
#' morie_choice_based_shares(0.1)$q1
#' @export
morie_choice_based_shares <- function(pi1) {
  p1 <- as.numeric(pi1)
  if (!(p1 > 0 && p1 < 1)) {
    stop(sprintf("pi1 must lie in (0, 1), got %g.", p1), call. = FALSE)
  }
  p0 <- 1 - p1
  denom <- sqrt(p0) + sqrt(p1)
  q0 <- sqrt(p0) / denom
  q1 <- sqrt(p1) / denom
  list(
    q1 = q1, q0 = q0, factor = p1 / q1 + p0 / q0,
    factor_at_random_sample = 2,
    method = "q_j proportional to sqrt(pi_j); minimises pi_1/q_1 + pi_0/q_0"
  )
}

#' Maximum-score estimator for a choice-based sample
#'
#' \eqn{S_{n,CB}(b) = (\pi_1/n_1)\sum_i Y_i 1\{X_i'b \ge 0\} -
#' (\pi_0/n_0)\sum_i (1 - Y_i) 1\{X_i'b \ge 0\}} (4.33), maximised
#' subject to \eqn{|b_1| = 1}; the smoothed form (4.35) replaces each
#' indicator with \eqn{K(X_i'b/h_n)}.
#'
#' A choice-based sample is stratified ON Y: the fraction with
#' \eqn{Y = 1} is fixed by design and X is drawn conditional on Y.
#' Estimators built for random samples are INCONSISTENT here except in
#' special cases. The reweighting repairs that -- the two terms are
#' divided by the realised counts and multiplied by the POPULATION
#' shares, assumed known from an external source such as a census.
#' Passing sample shares instead silently reproduces the uncorrected
#' estimator. Mirrors \code{morie.fn.hrzcbsm}.
#'
#' @param x numeric matrix of covariates, one row per observation.
#' @param y binary 0/1 response.
#' @param sampling_weights population share pi1, or c(pi0, pi1).
#' @param smoothed use (4.35) rather than the discontinuous (4.33).
#' @param h bandwidth for the smoothed form; \code{n^(-1/5)} when NULL.
#' @param n_restarts restarts, since (4.33) is a step function.
#' @param seed RNG seed for the restarts.
#' @return list: beta, score, pi1, pi0, n1, n0, smoothed, bandwidth,
#'   rate_exponent, standard_errors_valid, n, d, method.
#' @references Horowitz, Sec. 4.4.1, eqs. (4.33)-(4.35),
#'   Theorems 4.7-4.8.
#' @examples
#' x <- matrix(rnorm(400), ncol = 2)
#' y <- as.numeric(x %*% c(1, -0.8) + rnorm(200) > 0)
#' morie_choice_based_max_score(x, y, 0.5)$beta
#' @export
morie_choice_based_max_score <- function(x, y, sampling_weights,
                                         smoothed = TRUE, h = NULL,
                                         n_restarts = 8L, seed = 0L) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  if (!all(yv %in% c(0, 1))) stop("y must be binary 0/1.", call. = FALSE)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) {
    stop(sprintf("need at least 2 covariates, got %d.", d),
      call. = FALSE
    )
  }
  w <- as.numeric(sampling_weights)
  if (length(w) == 1L) {
    pi1 <- w
    pi0 <- 1 - pi1
  } else if (length(w) == 2L) {
    pi0 <- w[1L]
    pi1 <- w[2L]
  } else {
    stop("sampling_weights must be pi1 or c(pi0, pi1).", call. = FALSE)
  }
  if (!(pi1 > 0 && pi1 < 1 && pi0 > 0 && pi0 < 1)) {
    stop(sprintf("aggregate shares must lie in (0, 1), got (%g, %g).", pi0, pi1),
      call. = FALSE
    )
  }
  n1 <- sum(yv == 1)
  n0 <- n - n1
  if (n1 == 0L || n0 == 0L) {
    stop("need both response categories present.", call. = FALSE)
  }
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  if (hh <= 0) {
    stop(sprintf("bandwidth must be positive, got %g.", hh),
      call. = FALSE
    )
  }
  score <- function(b) {
    v <- as.numeric(X %*% b)
    # (4.35)'s K is the INTEGRAL of a kernel -- a smooth CDF standing
    # in for the indicator, not a density
    ind <- if (smoothed) stats::pnorm(v / hh) else as.numeric(v >= 0)
    (pi1 / n1) * sum(yv * ind) - (pi0 / n0) * sum((1 - yv) * ind)
  }
  fit <- .morie_optimize_scale_normalized(function(b) -score(b), d,
    n_restarts = n_restarts, seed = seed
  )
  list(
    beta = fit$beta, score = -fit$value, pi1 = pi1, pi0 = pi0,
    n1 = n1, n0 = n0, smoothed = isTRUE(smoothed),
    bandwidth = if (smoothed) hh else NULL,
    rate_exponent = if (smoothed) -0.4 else -1 / 3,
    standard_errors_valid = isTRUE(smoothed), n = n, d = d,
    method = "Choice-based max score (4.33)/(4.35); population shares reweight the strata"
  )
}

#' Maximum-score estimator for panel binary response
#'
#' \eqn{S_{ms,pan}(b) = n^{-1}\sum_i\sum_{t>r}(Y_{it} - Y_{ir})
#' 1\{W_{itr}'b \ge 0\}} with \eqn{W_{itr} = X_{it} - X_{ir}} (4.39),
#' maximised subject to \eqn{|b_1| = 1}; the smoothed form (4.40)
#' replaces the indicator with \eqn{K(W_{itr}'b/h_n)}.
#'
#' Differencing removes the individual effect \eqn{U_i} entirely: no
#' distributional assumption on it is needed and it may be correlated
#' with the regressors. The same differencing costs what a linear
#' fixed-effects model gives up -- an intercept is not identified,
#' regressors constant within an individual are not identified, and
#' only pairs with \eqn{Y_{it} \ne Y_{ir}} contribute. Constant
#' columns are detected and named rather than silently estimated.
#' Mirrors \code{morie.fn.hrzpanms}.
#'
#' @param x numeric array (n, T, d) or matrix (n*T, d) with each
#'   individual's T rows contiguous.
#' @param y binary responses shaped to match x.
#' @param n_periods number of periods T, at least 2.
#' @param smoothed use (4.40) rather than the discontinuous (4.39).
#' @param h bandwidth for the smoothed form; \code{n^(-1/5)} when NULL.
#' @param n_restarts restarts, since (4.39) is a step function.
#' @param seed RNG seed for the restarts.
#' @return list: beta, score, n_pairs, n_discordant_pairs,
#'   unidentified_columns, intercept_identified, smoothed, bandwidth,
#'   rate_exponent, n, T, d, method.
#' @references Horowitz, Sec. 4.4.2, eqs. (4.37)-(4.40),
#'   Theorems 4.9-4.10; Manski (1987).
#' @examples
#' x <- array(rnorm(800), dim = c(200, 2, 2))
#' y <- matrix(as.numeric(rnorm(400) > 0), nrow = 200)
#' morie_panel_max_score(x, y, 2, n_restarts = 2)$intercept_identified
#' @export
morie_panel_max_score <- function(x, y, n_periods, smoothed = TRUE, h = NULL,
                                  n_restarts = 8L, seed = 0L) {
  tt <- as.integer(n_periods)
  if (is.na(tt) || tt < 2L) {
    stop(sprintf("need at least 2 periods, got %s.", n_periods), call. = FALSE)
  }
  if (is.matrix(x)) {
    if (nrow(x) %% tt != 0L) {
      stop(sprintf(
        "x has %d rows, not a multiple of n_periods=%d.",
        nrow(x), tt
      ), call. = FALSE)
    }
    d <- ncol(x)
    n <- nrow(x) %/% tt
    X <- array(0, dim = c(n, tt, d))
    for (j in seq_len(d)) X[, , j] <- matrix(x[, j], nrow = n, byrow = TRUE)
  } else if (length(dim(x)) == 3L) {
    X <- x
    n <- dim(X)[1L]
    d <- dim(X)[3L]
    if (dim(X)[2L] != tt) {
      stop(sprintf("x has %d periods, expected %d.", dim(X)[2L], tt),
        call. = FALSE
      )
    }
  } else {
    stop("x must be (n, T, d) or (n*T, d).", call. = FALSE)
  }
  Y <- if (is.matrix(y)) y else matrix(as.numeric(y), ncol = tt, byrow = TRUE)
  if (!identical(dim(Y), c(n, tt))) {
    stop(sprintf(
      "y has %d x %d entries, expected %d x %d.",
      nrow(Y), ncol(Y), n, tt
    ), call. = FALSE)
  }
  if (!all(Y %in% c(0, 1))) stop("y must be binary 0/1.", call. = FALSE)
  if (d < 2L) {
    stop(sprintf("need at least 2 covariates, got %d.", d),
      call. = FALSE
    )
  }

  wl <- list()
  dyl <- list()
  k <- 0L
  for (t in 2:tt) {
    for (r in seq_len(t - 1L)) {
      k <- k + 1L
      wl[[k]] <- matrix(X[, t, ] - X[, r, ], nrow = n)
      dyl[[k]] <- Y[, t] - Y[, r]
    }
  }
  W <- do.call(rbind, wl)
  dY <- unlist(dyl, use.names = FALSE)

  # a covariate constant within every individual differences to zero
  const_cols <- which(vapply(seq_len(d), function(j) {
    isTRUE(all.equal(as.numeric(X[, , j] - X[, 1L, j]),
      numeric(n * tt),
      tolerance = 1e-12
    ))
  }, logical(1)))

  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  if (hh <= 0) {
    stop(sprintf("bandwidth must be positive, got %g.", hh),
      call. = FALSE
    )
  }
  score <- function(b) {
    v <- as.numeric(W %*% b)
    ind <- if (smoothed) stats::pnorm(v / hh) else as.numeric(v >= 0)
    sum(dY * ind) / n
  }
  fit <- .morie_optimize_scale_normalized(function(b) -score(b), d,
    n_restarts = n_restarts, seed = seed
  )
  list(
    beta = fit$beta, score = -fit$value, n_pairs = length(dY),
    n_discordant_pairs = sum(dY != 0),
    unidentified_columns = as.integer(const_cols),
    intercept_identified = FALSE, smoothed = isTRUE(smoothed),
    bandwidth = if (smoothed) hh else NULL,
    rate_exponent = if (smoothed) -0.4 else -1 / 3,
    n = n, T = tt, d = d,
    method = "Panel max score (4.39)/(4.40); differencing kills U_i and the intercept with it"
  )
}

#' Maximum-score estimator for an ordered-response model
#'
#' \eqn{S_{n,OR}(b) = n^{-1}\sum_i |W_i - \sum_{m} 1\{X_i'b >
#' \alpha_m\}|} (4.43), with \eqn{W = \sum_m 1\{Y^* > \alpha_m\}}
#' observable from Y.
#'
#' \strong{This objective is MINIMISED.} The book prints "maximize"
#' above (4.43), but the next sentence calls the result a
#' median-regression estimator, and a median regression minimises
#' absolute deviations. Measured on a simulated ordered model with
#' \eqn{\beta = (1, -0.7)} and n = 4000, minimising over a grid
#' returns -0.75 while maximising runs to the boundary of the search
#' region and stops at 3.0. The printed "maximize" is treated as an
#' erratum, and \code{sense} records the direction taken.
#'
#' Unlike the binary-response case this model needs NO scale
#' normalisation; the normalisation used is Lee's (1992), fixing the
#' first coefficient at 1 and \eqn{\alpha_1 = 0}. Mirrors
#' \code{morie.fn.hrzormsc}.
#'
#' @param x numeric matrix of covariates.
#' @param y integer category labels 0..M-1, at least 3 categories.
#' @param thresholds known cut-points; estimated when NULL.
#' @param smoothed use the Melenberg-van Soest objective, which IS
#'   maximised.
#' @param h bandwidth for the smoothed form; \code{n^(-1/5)} when NULL.
#' @param n_restarts restarts, since (4.43) is a step function.
#' @param seed RNG seed for the restarts.
#' @return list: beta, thresholds, objective, sense,
#'   thresholds_estimated, scale_normalisation_required, smoothed,
#'   bandwidth, M, n, d, method.
#' @references Horowitz, Sec. 4.4.3, eqs. (4.41)-(4.43),
#'   Theorem 4.11; Lee (1992), Melenberg and van Soest (1996).
#' @examples
#' x <- matrix(rnorm(600), ncol = 2)
#' ys <- x %*% c(1, -0.7) + rnorm(300)
#' y <- findInterval(ys, c(-1, 0, 1.2))
#' morie_ordered_max_score(x, y, thresholds = c(-1, 0, 1.2))$sense
#' @export
morie_ordered_max_score <- function(x, y, thresholds = NULL, smoothed = FALSE,
                                    h = NULL, n_restarts = 8L, seed = 0L) {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  yi <- as.integer(round(yv))
  if (!isTRUE(all.equal(as.numeric(yi), yv))) {
    stop("y must hold integer category labels.", call. = FALSE)
  }
  if (min(yi) < 0L) stop("y categories must start at 0.", call. = FALSE)
  n <- nrow(X)
  d <- ncol(X)
  m_cat <- max(yi) + 1L
  if (m_cat < 3L) {
    stop(sprintf(
      "an ordered-response model needs at least 3 categories, got %d.",
      m_cat
    ), call. = FALSE)
  }
  if (d < 2L) {
    stop(sprintf("need at least 2 covariates, got %d.", d),
      call. = FALSE
    )
  }
  # W = 1 + (number of finite cut-points Y* passed)
  W <- 1 + as.numeric(yi)
  known <- !is.null(thresholds)
  if (known) {
    a0 <- as.numeric(thresholds)
    if (length(a0) != m_cat - 1L) {
      stop(sprintf(
        "thresholds must have M-1 = %d entries, got %d.",
        m_cat - 1L, length(a0)
      ), call. = FALSE)
    }
    if (any(diff(a0) <= 0)) {
      stop("thresholds must be strictly increasing.", call. = FALSE)
    }
  }
  hh <- if (is.null(h)) n^(-0.2) else as.numeric(h)
  if (hh <= 0) {
    stop(sprintf("bandwidth must be positive, got %g.", hh),
      call. = FALSE
    )
  }
  unpack <- function(z) {
    b <- c(1, z[seq_len(d - 1L)])
    if (known) {
      return(list(b = b, a = a0))
    }
    gaps <- abs(z[-seq_len(d - 1L)])
    list(b = b, a = if (m_cat > 2L) c(0, cumsum(gaps)) else 0)
  }
  objective <- function(z) {
    pk <- unpack(z)
    v <- as.numeric(X %*% pk$b)
    if (smoothed) {
      # (2 W_im - 1) K((X_i'b - a_m)/h): a genuine max-score
      # objective, so it is MAXIMISED and negated here
      wim <- outer(yi, seq_len(m_cat - 1L) - 1L, ">") * 1
      -sum((2 * wim - 1) *
        stats::pnorm(outer(v, pk$a, "-") / hh)) / n
    } else {
      mean(abs(W - (1 + rowSums(outer(v, pk$a, ">")))))
    }
  }
  if (known) {
    # only beta is free, so the shared scale-normalised routine
    # applies -- and for d = 2 it scans the grid exhaustively rather
    # than trusting a simplex on a step function
    fit <- .morie_optimize_scale_normalized(function(b) objective(b[-1L]), d,
      n_restarts = n_restarts, seed = seed
    )
    return(list(
      beta = fit$beta, thresholds = a0,
      objective = if (smoothed) -fit$value else fit$value,
      sense = if (smoothed) "maximised" else "minimised",
      thresholds_estimated = FALSE, scale_normalisation_required = FALSE,
      smoothed = isTRUE(smoothed), bandwidth = if (smoothed) hh else NULL,
      M = m_cat, n = n, d = d,
      method = "Ordered max score (4.43); a median regression, so absolute deviations are MINIMISED"
    ))
  }
  k <- (d - 1L) + max(m_cat - 2L, 0L)
  set.seed(seed)
  starts <- list(numeric(k))
  for (i in seq_len(as.integer(n_restarts))) starts[[i + 1L]] <- stats::rnorm(k)
  best <- NULL
  best_val <- Inf
  for (st in starts) {
    r <- stats::optim(st, objective,
      method = "Nelder-Mead",
      control = list(maxit = 5000L, reltol = 1e-12)
    )
    if (r$value < best_val) {
      best_val <- r$value
      best <- r$par
    }
  }
  pk <- unpack(best)
  list(
    beta = pk$b, thresholds = pk$a,
    objective = if (smoothed) -best_val else best_val,
    sense = if (smoothed) "maximised" else "minimised",
    thresholds_estimated = !known,
    scale_normalisation_required = FALSE, smoothed = isTRUE(smoothed),
    bandwidth = if (smoothed) hh else NULL,
    M = m_cat, n = n, d = d,
    method = "Ordered max score (4.43); a median regression, so absolute deviations are MINIMISED"
  )
}

#' Rate of convergence of the smoothed maximum-score estimator
#'
#' \eqn{\|\hat\beta - \beta\| = O_p(n^{-s/(2s+1)})} for kernel
#' smoothness \eqn{s \ge 2}. The exponent is derived from the
#' theorem's own normalisation rather than asserted: Theorem 4.8 has
#' \eqn{(nh_n)^{1/2}} converging in distribution under
#' \eqn{nh_n^{2s+1} \to \lambda}, so \eqn{h_n \propto n^{-1/(2s+1)}}
#' and \eqn{(nh_n)^{1/2} = n^{s/(2s+1)}}.
#'
#' Smoothing BUYS a rate -- the unsmoothed estimator converges at
#' \eqn{n^{-1/3}} with a non-normal Chernoff limit, the smoothed one
#' at \eqn{n^{-2/5}} already at s = 2 -- but never ATTAINS
#' \eqn{n^{-1/2}}, which is a supremum over smoothness rather than a
#' value any finite s reaches. Mirrors \code{morie.fn.hrzsmsrc}.
#'
#' @param n sample size, at least 2.
#' @param smoothness_order the order s, at least 2.
#' @return list: rate, exponent, bandwidth_exponent, unsmoothed_rate,
#'   unsmoothed_exponent, ratio_to_unsmoothed, attains_root_n,
#'   smoothness_order, n, method.
#' @references Horowitz, Sec. 4.3.3 and Theorem 4.8; Horowitz (1992).
#' @examples
#' morie_sms_rate(10000, 2)$exponent
#' @export
morie_sms_rate <- function(n, smoothness_order = 2L) {
  nn <- as.integer(n)
  if (is.na(nn) || nn < 2L) {
    stop(sprintf("n must be at least 2, got %s.", n), call. = FALSE)
  }
  s <- as.integer(smoothness_order)
  if (is.na(s) || s < 2L) {
    stop(sprintf(
      "the theorem requires a smoothness order of at least 2, got %s.",
      smoothness_order
    ), call. = FALSE)
  }
  expo <- -s / (2 * s + 1)
  list(
    rate = nn^expo, exponent = expo,
    bandwidth_exponent = -1 / (2 * s + 1),
    unsmoothed_rate = nn^(-1 / 3), unsmoothed_exponent = -1 / 3,
    ratio_to_unsmoothed = nn^expo / nn^(-1 / 3),
    attains_root_n = FALSE, smoothness_order = s, n = nn,
    method = "n^{-s/(2s+1)} from (n h_n)^{1/2} with n h_n^{2s+1} -> lambda"
  )
}
