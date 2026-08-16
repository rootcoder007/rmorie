# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of tipsne -- delta-adjusted tipping-point sensitivity analysis
# for missing outcomes. Mirrors src/morie/fn/tipsne.py operation for
# operation, including the generator: .ghc_rng is the R side of
# _array_core._SplitMix64, so the imputations are the same draws.
#
# A trial analysed under MAR gives one number. A tipping-point analysis
# answers a different question: how much worse would the unseen outcomes
# of the people who dropped out have to be before the conclusion stops
# holding? The answer is a quantity -- the delta at which the result tips
# -- reported so a clinician can judge whether a departure that large is
# plausible, which is a judgement no model makes.
#
# The procedure, which is the one regulators ask for:
#
#   1. Impute the missing outcomes under MAR, m times.
#   2. Add a shift delta to the imputed values -- AFTER imputation and
#      BEFORE analysis. That is the whole point: the imputation model
#      stays MAR and the MNAR departure sits on top of it, so the size of
#      the departure is explicit rather than buried in a model. The shift
#      is per arm, so the grid is two-dimensional.
#   3. Analyse each completed data set (ANCOVA: outcome on arm plus any
#      covariates) and pool with Rubin's rules.
#   4. Sweep delta and find where the p-value crosses alpha, by linear
#      interpolation between the two grid points that bracket it.
#
# Routes, all selectable
#
#   mi = "proper"     Rubin's proper multiple imputation: per data set,
#                     draw sigma^2 from its scaled inverse chi-square
#                     posterior and beta from N(betahat, sigma^2
#                     (X'X)^-1), then draw the missing values. The
#                     parameter draw is what makes the between-imputation
#                     variance an honest estimate of uncertainty in the
#                     imputation model rather than only of residual noise.
#   mi = "improper"   Impute from the fitted mean plus residual noise at
#                     the point estimates, no parameter draw. Understates
#                     the variance; kept alongside so the size of that
#                     understatement is visible rather than assumed.
#   mi = "deterministic"
#                     One imputation at the fitted mean, no noise. Not
#                     multiple imputation and not defensible as
#                     inference, but the only route whose answer does not
#                     move with the seed, so the anchors use it to pin
#                     the arithmetic.
#
#   pooling = "rubin1987"      df = (m-1) (1 + Ubar / ((1 + 1/m) B))^2
#   pooling = "barnard_rubin"  the small-sample correction, which matters
#                              here because a trial has a finite
#                              complete-data df and Rubin's original
#                              formula can return a df larger than the
#                              complete-data one, which is nonsense.
#
# Everything is written out in exact arithmetic -- compensated sums, an
# explicit Cholesky, explicit triangular solves -- because lm(), solve()
# and %*% make no promise the Python arm can be held to.
#
# References
#   Rubin, D.B. (1987) "Multiple Imputation for Nonresponse in Surveys."
#     Wiley. Chapter 3: the combining rules.
#   Barnard, J. and Rubin, D.B. (1999) "Small-sample degrees of freedom
#     with multiple imputation." Biometrika 86(4), 948-955.
#   Yan, X., Lee, S., Ling, N. and Lin, J. (2021) tipping-point
#     sensitivity analysis for MNAR departures in clinical trials; the
#     delta-adjustment procedure here follows the now-standard regulatory
#     form (impute under MAR, shift imputed values by delta per arm,
#     re-analyse, locate the crossing).

.TIPSNE_MI <- c("proper", "improper", "deterministic")
.TIPSNE_POOL <- c("rubin1987", "barnard_rubin")

# Neumaier-compensated sum. Written out rather than left to sum():
# CPython 3.12+ compensates a run of floats and R's sum() accumulates in
# long double, so the two built-ins are different functions and a
# comparison at the twelfth digit finds the difference.
#' Neumaier-compensated sum. Written out rather than left to sum():
#'
#' CPython 3.12+ compensates a run of floats and R\'s sum() accumulates
#' in long double, so the two built-ins are different functions and a
#' comparison at the twelfth digit finds the difference.
#'
#' @param v See Usage.
#' @return A numeric value.
#' @export
.tipsne_csum <- function(v) {
  s <- 0; cc <- 0
  for (t in v) {
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t) else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

# Compensated dot product. Not sum(a * b), same reason.
#' Compensated dot product. Not sum(a * b), same reason
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.tipsne_dot <- function(a, b) {
  s <- 0; cc <- 0
  n <- length(a)
  if (n == 0L) return(0)
  for (i in seq_len(n)) {
    t <- a[i] * b[i]
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t) else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

# Cholesky factor L with A = L L', lower triangular. Explicit rather than
# chol() so the Python arm can match it element by element.
#' Cholesky factor L with A = L L\', lower triangular. Explicit rather
#' than
#'
#' chol() so the Python arm can match it element by element.
#'
#' @param a See Usage.
#' @return The value of \code{lo}, as built in the body.
#' @export
.tipsne_chol <- function(a) {
  p <- nrow(a)
  lo <- matrix(0, p, p)
  for (i in seq_len(p)) {
    for (j in seq_len(i)) {
      s <- a[i, j] - if (j > 1L) .tipsne_dot(lo[i, seq_len(j - 1L)],
                                             lo[j, seq_len(j - 1L)]) else 0
      if (i == j) {
        if (s <= 0) stop("design matrix is not full rank")
        lo[i, j] <- sqrt(s)
      } else lo[i, j] <- s / lo[j, j]
    }
  }
  lo
}

# Solve L L' x = b by forward then back substitution.
#' Solve L L\' x = b by forward then back substitution
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param lo See Usage.
#' @param b See Usage.
#' @return The value of \code{x}, as built in the body.
#' @export
.tipsne_solve_chol <- function(lo, b) {
  p <- nrow(lo)
  z <- numeric(p)
  for (i in seq_len(p)) {
    acc <- if (i > 1L) .tipsne_dot(lo[i, seq_len(i - 1L)], z[seq_len(i - 1L)]) else 0
    z[i] <- (b[i] - acc) / lo[i, i]
  }
  x <- numeric(p)
  for (i in seq(p, 1L)) {
    acc <- if (i < p) .tipsne_csum(vapply((i + 1L):p, function(k) lo[k, i] * x[k],
                                          numeric(1))) else 0
    x[i] <- (z[i] - acc) / lo[i, i]
  }
  x
}

# (L L')^-1, formed column by column from the factor.
#' (L L\')^-1, formed column by column from the factor
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param lo See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.tipsne_inv_from_chol <- function(lo) {
  p <- nrow(lo)
  cols <- lapply(seq_len(p), function(j) {
    e <- numeric(p); e[j] <- 1
    .tipsne_solve_chol(lo, e)
  })
  out <- matrix(0, p, p)
  for (i in seq_len(p)) for (j in seq_len(p)) out[i, j] <- cols[[j]][i]
  out
}

#' Least squares of y on a design matrix, via the normal equations
#'
#' @param y Numeric outcome vector.
#' @param design Numeric matrix, one row per observation, including the
#'   intercept column.
#' @return A list with beta, the residual sum of squares, the residual
#'   degrees of freedom, sigma2, the unscaled covariance and the fitted
#'   values.
#' @keywords internal
morie_tipsne_ancova <- function(y, design) {
  n <- length(y)
  p <- ncol(design)
  xtx <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p))
    xtx[a, b] <- .tipsne_csum(design[, a] * design[, b])
  xty <- vapply(seq_len(p), function(a) .tipsne_csum(design[, a] * y), numeric(1))
  lo <- .tipsne_chol(xtx)
  beta <- .tipsne_solve_chol(lo, xty)
  fitted <- vapply(seq_len(n), function(i) .tipsne_dot(design[i, ], beta), numeric(1))
  rss <- .tipsne_csum((y - fitted) * (y - fitted))
  df <- n - p
  if (df < 1L) stop("no residual degrees of freedom")
  list(beta = beta, rss = rss, df = df, sigma2 = rss / df,
       xtx_inv = .tipsne_inv_from_chol(lo), fitted = fitted, chol = lo)
}

# Intercept, arm indicator, then any covariates.
#' Intercept, arm indicator, then any covariates
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param arm See Usage.
#' @param X See Usage.
#' @param n See Usage.
#' @return A matrix, from \code{matrix}.
#' @export
.tipsne_design <- function(arm, X, n) {
  m <- cbind(rep(1, n), as.numeric(arm))
  if (!is.null(X)) m <- cbind(m, as.matrix(X))
  matrix(as.numeric(m), nrow = n)
}

# beta* ~ N(betahat, sigma2 (X'X)^-1) via the Cholesky of the covariance.
# The draw is coordinate by coordinate so the stream position matches the
# Python arm term for term.
#' Beta* ~ N(betahat, sigma2 (X\'X)^-1) via the Cholesky of the
#' covariance
#'
#' The draw is coordinate by coordinate so the stream position matches
#' the Python arm term for term.
#'
#' @param e See Usage.
#' @param beta See Usage.
#' @param xtx_inv See Usage.
#' @param sigma2_draw See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.tipsne_draw_beta <- function(e, beta, xtx_inv, sigma2_draw) {
  p <- length(beta)
  cov <- sigma2_draw * xtx_inv
  lo <- .tipsne_chol(cov)
  z <- vapply(seq_len(p), function(i) .ghc_norm(e, 1L), numeric(1))
  vapply(seq_len(p), function(i)
    beta[i] + .tipsne_dot(lo[i, seq_len(i)], z[seq_len(i)]), numeric(1))
}

#' One completed outcome vector under the chosen imputation route
#'
#' @param e A generator environment from .ghc_rng.
#' @param y Outcome vector, NA where missing.
#' @param arm Arm indicator.
#' @param X Covariates or NULL.
#' @param miss Integer missingness indicator.
#' @param fit The complete-case ANCOVA fit.
#' @param mi Imputation route.
#' @return A completed numeric outcome vector.
#' @keywords internal
morie_tipsne_impute <- function(e, y, arm, X, miss, fit, mi) {
  n <- length(y)
  des <- .tipsne_design(arm, X, n)
  if (mi == "deterministic") {
    beta <- fit$beta; sd <- 0
  } else if (mi == "improper") {
    beta <- fit$beta; sd <- sqrt(fit$sigma2)
  } else {
    # sigma2* = rss / chi2_df, the scaled inverse chi-square posterior
    # draw; chi2 is Gamma(df/2, 2), the shape the matched generator
    # provides in both arms.
    g <- .ghc_gamma1(e, fit$df / 2, 2)
    sigma2 <- fit$rss / g
    beta <- .tipsne_draw_beta(e, fit$beta, fit$xtx_inv, sigma2)
    sd <- sqrt(sigma2)
  }
  out <- y
  for (i in seq_len(n)) {
    if (miss[i] == 1L) {
      mu <- .tipsne_dot(des[i, ], beta)
      out[i] <- mu + if (sd > 0) sd * .ghc_norm(e, 1L) else 0
    }
  }
  out
}

# Lanczos log-gamma, written out so both arms use the same one.
.TIPSNE_LG <- c(76.18009172947146, -86.50532032941677, 24.01409824083091,
                -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5)

#' .tipsne_lgamma
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param z See Usage.
#' @return A numeric value.
#' @export
.tipsne_lgamma <- function(z) {
  x <- z
  tmp <- x + 5.5
  tmp <- tmp - (x + 0.5) * log(tmp)
  ser <- 1.000000000190015
  for (j in 1:6) {
    x <- x + 1
    ser <- ser + .TIPSNE_LG[j] / x
  }
  -tmp + log(2.5066282746310005 * ser / z)
}

# Lentz's algorithm for the beta continued fraction.
#' Lentz\'s algorithm for the beta continued fraction
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param x See Usage.
#' @return The value of \code{h}, as built in the body.
#' @export
.tipsne_betacf <- function(a, b, x) {
  tiny <- 1e-30
  qab <- a + b; qap <- a + 1; qam <- a - 1
  cc <- 1
  d <- 1 - qab * x / qap
  if (abs(d) < tiny) d <- tiny
  d <- 1 / d
  h <- d
  for (m in 1:300) {
    m2 <- 2 * m
    aa <- m * (b - m) * x / ((qam + m2) * (a + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    cc <- 1 + aa / cc
    if (abs(cc) < tiny) cc <- tiny
    d <- 1 / d
    # h * (d * cc), NOT (h * d) * cc: the Python arm writes `h *= d * c`
    # and the two associations differ in the last bit, which a long
    # continued fraction then carries into the p-value.
    h <- h * (d * cc)
    aa <- -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    cc <- 1 + aa / cc
    if (abs(cc) < tiny) cc <- tiny
    d <- 1 / d
    de <- d * cc
    h <- h * de
    if (abs(de - 1) < 3e-16) break
  }
  h
}

# Regularised incomplete beta I_x(a, b) by the continued fraction.
#' Regularised incomplete beta I_x(a, b) by the continued fraction
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param x See Usage.
#' @return A numeric value.
#' @export
.tipsne_betainc <- function(a, b, x) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  lbeta <- .tipsne_lgamma(a) + .tipsne_lgamma(b) - .tipsne_lgamma(a + b)
  front <- exp(a * log(x) + b * log(1 - x) - lbeta)
  if (x < (a + 1) / (a + b + 2))
    return(front * .tipsne_betacf(a, b, x) / a)
  1 - exp(b * log(1 - x) + a * log(x) - lbeta) * .tipsne_betacf(b, a, 1 - x) / b
}

# Upper tail of Student's t, from the regularised incomplete beta.
# Written out because pt() and the Python arm are separate
# implementations and would disagree in the last digits.
#' Upper tail of Student\'s t, from the regularised incomplete beta
#'
#' Written out because pt() and the Python arm are separate
#' implementations and would disagree in the last digits.
#'
#' @param t See Usage.
#' @param df See Usage.
#' @return A numeric value.
#' @export
.tipsne_t_sf <- function(t, df) 0.5 * .tipsne_betainc(df / 2, 0.5, df / (df + t * t))

#' Combine per-imputation estimates and variances by Rubin's rules
#'
#' @param ests Point estimates, one per imputation.
#' @param vars Squared standard errors, one per imputation.
#' @param pooling "rubin1987" or "barnard_rubin".
#' @param df_complete Complete-data residual degrees of freedom;
#'   required by the Barnard-Rubin route.
#' @return A list with the pooled estimate, standard error, degrees of
#'   freedom, t statistic, p-value and the variance decomposition.
#' @keywords internal
morie_tipsne_pool <- function(ests, vars, pooling = "rubin1987",
                              df_complete = NULL) {
  m <- length(ests)
  qbar <- .tipsne_csum(ests) / m
  ubar <- .tipsne_csum(vars) / m
  b <- if (m > 1L) .tipsne_csum((ests - qbar) * (ests - qbar)) / (m - 1) else 0
  total <- ubar + (1 + 1 / m) * b
  if (b <= 0 || m < 2L) {
    # No between-imputation variance: the imputation added nothing, so
    # the complete-data df is the honest answer and Rubin's formula
    # would divide by zero.
    df <- if (!is.null(df_complete)) df_complete else 1e6
  } else {
    r <- (1 + 1 / m) * b / ubar
    df <- (m - 1) * (1 + 1 / r) * (1 + 1 / r)
    if (pooling == "barnard_rubin") {
      if (is.null(df_complete)) stop("barnard_rubin needs df_complete")
      gamma <- (1 + 1 / m) * b / total
      dfo <- (df_complete + 1) / (df_complete + 3) * df_complete * (1 - gamma)
      df <- 1 / (1 / df + 1 / dfo)
    }
  }
  se <- sqrt(total)
  tt <- if (se > 0) qbar / se else 0
  list(estimate = qbar, se = se, df = df, t = tt,
       p = 2 * .tipsne_t_sf(abs(tt), df), within = ubar, between = b,
       total = total, fmi = if (total > 0) (1 + 1 / m) * b / total else 0)
}

#' .tipsne_sd
#'
#' Part of the tipsne_native implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @return A numeric value.
#' @export
.tipsne_sd <- function(v) {
  n <- length(v)
  mu <- .tipsne_csum(v) / n
  sqrt(.tipsne_csum((v - mu) * (v - mu)) / (n - 1))
}

#' Delta-adjusted tipping-point sensitivity analysis
#'
#' @param y Outcome; NA where missing.
#' @param D Arm indicator, 0 control and 1 treatment.
#' @param missing_indicator 1 where the outcome is missing. Derived from
#'   y when omitted; a disagreement between the two raises rather than
#'   silently picking one.
#' @param X Covariates for the ANCOVA, complete for every unit.
#' @param delta_treat Shifts applied to imputed values in the treated
#'   arm. The default sweeps 0 down to -2.5 pooled standard deviations in
#'   eleven steps.
#' @param delta_control Shifts applied in the control arm; 0 by default,
#'   which is the one-way analysis. Pass both for a two-way grid.
#' @param n_imputations m. Ignored by the deterministic route.
#' @param seed Seed for the shared generator. Every grid cell is imputed
#'   from the SAME seed, so a difference between cells is the delta and
#'   not the draws.
#' @param alpha Significance level the crossing is measured against.
#' @param mi "proper", "improper" or "deterministic".
#' @param pooling "rubin1987" or "barnard_rubin".
#' @param standardise Report the deltas in pooled standard deviations as
#'   well as the outcome's own units.
#' @return A list with the MAR analysis, the grid, the tipping point for
#'   each control-arm delta, and whether the result tipped at all.
#' @export
morie_tipsne <- function(y, D, missing_indicator = NULL, X = NULL,
                         delta_treat = NULL, delta_control = NULL,
                         n_imputations = 20L, seed = 1, alpha = 0.05,
                         mi = "proper", pooling = "rubin1987",
                         standardise = TRUE) {
  if (!(mi %in% .TIPSNE_MI))
    stop("mi must be one of ", paste(.TIPSNE_MI, collapse = ", "))
  if (!(pooling %in% .TIPSNE_POOL))
    stop("pooling must be one of ", paste(.TIPSNE_POOL, collapse = ", "))
  yv <- as.numeric(y)
  n <- length(yv)
  arm <- as.numeric(D)
  derived <- as.integer(is.na(yv))
  if (is.null(missing_indicator)) {
    miss <- derived
  } else {
    miss <- as.integer(as.logical(missing_indicator))
    if (any(miss == 0L & derived == 1L))
      stop("missing_indicator says observed where y is missing")
  }
  if (!is.null(X)) X <- matrix(as.numeric(as.matrix(X)), nrow = n)
  obs <- which(miss == 0L)
  if (length(obs) < 3L) stop("fewer than three observed outcomes")

  des_all <- .tipsne_design(arm, X, n)
  fit <- morie_tipsne_ancova(yv[obs], des_all[obs, , drop = FALSE])
  pooled_sd <- .tipsne_sd(yv[obs])
  df_complete <- n - ncol(des_all)

  if (is.null(delta_treat)) {
    step <- 2.5 * pooled_sd / 10
    delta_treat <- vapply(0:10, function(k) -step * k, numeric(1))
  } else delta_treat <- as.numeric(delta_treat)
  delta_control <- if (is.null(delta_control)) 0 else as.numeric(delta_control)

  m <- if (mi == "deterministic") 1L else as.integer(n_imputations)

  cell <- function(dc, dt) {
    e <- .ghc_rng(seed)
    ests <- numeric(m); vars <- numeric(m)
    for (r in seq_len(m)) {
      comp <- morie_tipsne_impute(e, yv, arm, X, miss, fit, mi)
      for (i in seq_len(n))
        if (miss[i] == 1L)
          comp[i] <- comp[i] + if (arm[i] == 1) dt else dc
      f <- morie_tipsne_ancova(comp, des_all)
      ests[r] <- f$beta[2]
      vars[r] <- f$sigma2 * f$xtx_inv[2, 2]
    }
    morie_tipsne_pool(ests, vars, pooling, df_complete)
  }

  mar <- cell(0, 0)

  grid <- list()
  tips <- list()
  for (dc in delta_control) {
    row <- lapply(delta_treat, function(dt) cell(dc, dt))
    for (k in seq_along(delta_treat)) {
      rr <- row[[k]]
      grid[[length(grid) + 1L]] <- list(
        delta_control = dc, delta_treat = delta_treat[k],
        estimate = rr$estimate, se = rr$se, df = rr$df, p = rr$p,
        significant = rr$p < alpha)
    }
    # The crossing, by linear interpolation between the two grid points
    # that bracket it. NULL when the row never crosses -- an
    # extrapolated tipping point outside the grid would be a number the
    # data does not support.
    tp <- NULL
    if (length(delta_treat) > 1L)
      for (k in 2:length(delta_treat)) {
        p0 <- row[[k - 1L]]$p; p1 <- row[[k]]$p
        if (((p0 < alpha) != (p1 < alpha)) && p1 != p0) {
          w <- (alpha - p0) / (p1 - p0)
          tp <- delta_treat[k - 1L] + w * (delta_treat[k] - delta_treat[k - 1L])
          break
        }
      }
    tips[[length(tips) + 1L]] <- list(
      delta_control = dc, tipping_point = tp,
      tipping_point_sd = if (is.null(tp) || pooled_sd == 0) NULL else tp / pooled_sd)
  }

  list(estimate = mar$estimate, se = mar$se, p = mar$p, df = mar$df,
       mar = mar, grid = grid, tipping_points = tips,
       tipped = any(vapply(tips, function(t) !is.null(t$tipping_point), logical(1))),
       n = n, n_missing = sum(miss),
       n_missing_treat = sum(miss == 1L & arm == 1),
       n_missing_control = sum(miss == 1L & arm == 0),
       pooled_sd = if (standardise) pooled_sd else NULL,
       m = m, mi = mi, pooling = pooling, alpha = as.numeric(alpha),
       seed = as.integer(seed),
       method = "delta-adjusted tipping-point sensitivity analysis")
}

#' One-line summary of the tipsne module
#'
#' @return A character scalar.
#' @export
morie_tipsne_cheatsheet <- function()
  paste0("tipsne: delta-adjusted tipping-point sensitivity analysis for ",
         "MNAR missingness. mi routes ", paste(.TIPSNE_MI, collapse = ", "),
         "; pooling ", paste(.TIPSNE_POOL, collapse = ", "))
