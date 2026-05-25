# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Regression Discontinuity Design (RDD) estimators for morie.
# Ports the public API of `src/morie/rdd.py` (~1851 LOC) to R.
#
# Strategy: prefer CRAN wrappers.  Sharp / fuzzy RDD, bias-corrected
# inference, optimal bandwidth selection (CCT/IK), and standard plot
# helpers dispatch to `rdrobust::rdrobust` (the reference implementation
# of Calonico, Cattaneo, Titiunik & co-authors).  The McCrary density
# manipulation test dispatches to `rddensity::rddensity` (or
# `rdd::DCdensity` as a fallback).  When neither is available the
# package falls back to base-R local-polynomial code that mirrors
# the Python module.
#
# Internal mathematical helpers that merely replicate `rdrobust`
# internals (full MSE-optimal AMSE plug-in derivative bandwidth,
# Cattaneo-Jansson-Ma local-polynomial density) are stubbed with
# informative TODOs.
#
# Public R names mirror the Python module under the `morie_rdd_*` prefix.

#' @importFrom stats lm coef vcov pnorm pt pf pchisq qnorm qt sd var
#'   model.matrix predict quantile complete.cases approx
NULL


# ---------------------------------------------------------------------------
# Shared @param block for the morie_rdd_* family. Functions inherit via
# @inheritParams morie_rdd_params (a roxygen-only stub).
# ---------------------------------------------------------------------------

#' Shared parameters for morie_rdd_* estimators and diagnostics
#'
#' Roxygen-only stub holding the @param entries shared across the
#' RDD family (sharp / fuzzy / bias-corrected, McCrary / Cattaneo
#' density, bandwidth selectors, covariate balance, placebo cutoffs,
#' kink, donut, geographic, local randomisation, power, etc.).
#' Functions reference these via `@inheritParams morie_rdd_params` so
#' each `@param` is documented once and the Rd files stay consistent.
#'
#' @param x Numeric vector of running-variable values (used by
#'   bandwidth selectors + density tests that don't take a
#'   `data.frame`).
#' @param y Numeric vector of outcome values aligned with `x`.
#' @param data A `data.frame` holding the outcome, running variable,
#'   treatment, and any covariates referenced by name.
#' @param outcome Character; column name of the response variable in
#'   `data`.
#' @param running Character; column name of the running (forcing)
#'   variable in `data`.
#' @param treatment Character; column name of the treatment-receipt
#'   variable (fuzzy designs).
#' @param cutoff Numeric scalar; the threshold on `running`. Default
#'   `0` (the canonical normalisation).
#' @param bandwidth Numeric; the local-polynomial bandwidth on each
#'   side of the cutoff. `NULL` invokes the data-driven CCT selector.
#' @param p Integer; local-polynomial order (default 1 for local-
#'   linear). 2 picks up quadratic curvature for bias correction.
#' @param kernel One of `"triangular"` (default), `"epanechnikov"`,
#'   `"uniform"`, or `"gaussian"`.
#' @param alpha Significance level (default `0.05`).
#' @param rho Bandwidth ratio for bias correction (Calonico, Cattaneo
#'   & Titiunik 2014); default `1` (same bandwidth).
#' @param donut Numeric; symmetric window around the cutoff to drop
#'   in a donut-RDD robustness check (default `0`).
#' @param window Numeric; half-width of the local randomisation
#'   window.
#' @param n_permutations Integer; permutation count for the
#'   randomisation-based inference.
#' @param seed Integer; RNG seed for permutation / bootstrap routines.
#' @param distance_to_boundary Character; column name of the signed
#'   distance to the geographic boundary in `data`.
#' @param side Character; column name encoding the treatment side
#'   (e.g. `"left"`/`"right"`).
#' @param covariates Character vector of column names whose balance
#'   at the cutoff is checked.
#' @param true_cutoff Numeric; the actual policy cutoff (placebo
#'   robustness re-runs the analysis at `placebo_cutoffs`).
#' @param placebo_cutoffs Numeric vector of false cutoffs to test.
#' @param bandwidth_range Numeric vector of candidate bandwidths used
#'   by the sensitivity analysis.
#' @param n_bins Integer; bin count for histogram-based density tests
#'   and binned-plot reductions.
#' @param p_global Integer; polynomial order for the global
#'   component of `morie_rdd_plot_data`.
#' @param p_local Integer; polynomial order for the local component
#'   of `morie_rdd_plot_data`.
#' @param n Integer; sample-size argument to `morie_rdd_power`.
#' @param tau Numeric; the treatment-effect size used by power /
#'   sample-size calculators.
#' @param sigma Numeric; outcome standard deviation.
#' @param cutoff_density Numeric; running-variable density at the
#'   cutoff.
#' @param power Numeric in `(0, 1)`; target statistical power.
#' @keywords internal
#' @name morie_rdd_params
NULL


# ---------------------------------------------------------------------------
# Kernel functions (vectorised, support [-1, 1])
# ---------------------------------------------------------------------------

#' RDD kernel functions
#'
#' Vectorised kernel functions on the support |u| <= 1 (Gaussian is on
#' the real line). Used by RDD local-linear estimators and friends for
#' kernel weighting around the cutoff.
#'
#' \itemize{
#'   \item \code{morie_rdd_kernel_triangular}: \eqn{K(u) = \max(1 - |u|, 0)}{K(u) = max(1 - |u|, 0)}
#'   \item \code{morie_rdd_kernel_epanechnikov}: \eqn{K(u) = (3/4)(1 - u^2)}{K(u) = 0.75 (1 - u^2)} on |u| <= 1
#'   \item \code{morie_rdd_kernel_uniform}: \eqn{K(u) = 1/2}{K(u) = 0.5} on |u| <= 1
#'   \item \code{morie_rdd_kernel_gaussian}: \eqn{K(u) = \phi(u)}{K(u) = phi(u)}, the standard normal density
#' }
#'
#' @param u Numeric vector of standardised distances from the cutoff
#'   (i.e. \eqn{(x - c)/h}{(x - c) / h}).
#' @return Numeric vector of kernel weights, same length as `u`.
#' @name morie_rdd_kernels
#' @rdname morie_rdd_kernels
#' @export
morie_rdd_kernel_triangular  <- function(u) pmax(1 - abs(u), 0)
#' @rdname morie_rdd_kernels
#' @export
morie_rdd_kernel_epanechnikov <- function(u)
  ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
#' @rdname morie_rdd_kernels
#' @export
morie_rdd_kernel_uniform <- function(u) ifelse(abs(u) <= 1, 0.5, 0)
#' @rdname morie_rdd_kernels
#' @export
morie_rdd_kernel_gaussian <- function(u) stats::dnorm(u)

.morie_rdd_kernels <- list(
  triangular   = morie_rdd_kernel_triangular,
  epanechnikov = morie_rdd_kernel_epanechnikov,
  uniform      = morie_rdd_kernel_uniform,
  gaussian     = morie_rdd_kernel_gaussian
)

.morie_rdd_get_kernel <- function(name) {
  fn <- .morie_rdd_kernels[[name]]
  if (is.null(fn))
    stop("Unknown kernel '", name,
         "'. Choose from: ", paste(names(.morie_rdd_kernels), collapse = ", "))
  fn
}

.morie_rdd_have_rdrobust  <- function() requireNamespace("rdrobust",  quietly = TRUE)
.morie_rdd_have_rddensity <- function() requireNamespace("rddensity", quietly = TRUE)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @keywords internal
.morie_rdd_local_poly_fit <- function(x, y, x0, h, p = 1,
                                      kernel = "triangular") {
  K   <- .morie_rdd_get_kernel(kernel)
  u   <- (x - x0) / h
  w   <- K(u)
  use <- w > 0
  if (sum(use) < (p + 2)) return(list(beta = rep(NA, p + 1),
                                      se = rep(NA, p + 1),
                                      n = sum(use)))
  X <- sapply(0:p, function(j) (x[use] - x0)^j)
  W <- diag(w[use], nrow = sum(use))
  XtWX <- crossprod(X, W %*% X)
  XtWy <- crossprod(X, W %*% y[use])
  beta <- as.numeric(solve(XtWX, XtWy))
  resid <- y[use] - X %*% beta
  s2 <- sum(w[use] * resid^2) / sum(w[use])
  vcov_ <- s2 * solve(XtWX)
  list(beta = beta, se = sqrt(diag(vcov_)), n = sum(use),
       fit_value = beta[1])
}


# ---------------------------------------------------------------------------
# Local polynomial regression
# ---------------------------------------------------------------------------

#' Local polynomial regression at user-supplied evaluation points
#'
#' @param x Running variable (numeric).
#' @param y Outcome (numeric).
#' @param eval_points Points at which to evaluate the fit.
#' @param h Bandwidth.
#' @param p Polynomial order (default 1, i.e. local linear).
#' @param kernel One of \code{"triangular"} (default), \code{"epanechnikov"},
#'   \code{"uniform"}, or \code{"gaussian"}.
#' @return A data frame of fitted values and standard errors.
#' @export
morie_rdd_local_polynomial <- function(x, y, eval_points, h, p = 1,
                                       kernel = "triangular") {
  rows <- lapply(eval_points, function(x0) {
    f <- .morie_rdd_local_poly_fit(x, y, x0, h, p, kernel)
    data.frame(eval_point = x0, fit = f$fit_value,
               se = f$se[1], n_effective = f$n)
  })
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# Bandwidth selectors
# ---------------------------------------------------------------------------

.morie_rdd_bw_result <- function(h, method, details = list())
  list(bandwidth = as.numeric(h), method = method, details = details)

#' Imbens-Kalyanaraman (IK) MSE-optimal bandwidth
#'
#' Dispatches to \code{rdrobust::rdbwselect(bwselect = "mserd")} which
#' implements the modern IK-equivalent CCT MSE-optimal rule.
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_bandwidth_ik <- function(x, y, cutoff = 0,
                                   kernel = "triangular") {
  if (.morie_rdd_have_rdrobust()) {
    bw <- rdrobust::rdbwselect(y = y, x = x, c = cutoff,
                               bwselect = "mserd", kernel = kernel)
    h <- bw$bws[1, 1]
    return(.morie_rdd_bw_result(h, "IK (rdrobust mserd)",
                                details = list(fit = bw)))
  }
  # TODO: native IK 2012 first-/second-derivative plug-in;
  # ROT fallback below
  morie_rdd_bandwidth_rot(x, y, cutoff)
}

#' Rule-of-thumb (ROT) bandwidth -- Silverman-style on running variable
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_bandwidth_rot <- function(x, y, cutoff = 0) {
  sd_x  <- stats::sd(x)
  n     <- length(x)
  h     <- 1.84 * sd_x * n^(-1 / 5)
  .morie_rdd_bw_result(h, "Rule of thumb (Silverman)")
}

#' Calonico-Cattaneo-Titiunik (CCT) MSE-optimal bandwidth
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_bandwidth_cct <- function(x, y, cutoff = 0,
                                    kernel = "triangular", p = 1) {
  if (.morie_rdd_have_rdrobust()) {
    bw <- rdrobust::rdbwselect(y = y, x = x, c = cutoff,
                               bwselect = "mserd", kernel = kernel, p = p)
    h <- bw$bws[1, 1]
    return(.morie_rdd_bw_result(h, "CCT MSE-optimal (rdrobust)",
                                details = list(fit = bw)))
  }
  morie_rdd_bandwidth_rot(x, y, cutoff)
}


# ---------------------------------------------------------------------------
# Sharp / fuzzy / bias-corrected RDD
# ---------------------------------------------------------------------------

.morie_rdd_result <- function(estimate, se, n, method, alpha = 0.05,
                              details = list()) {
  t   <- estimate / se
  p   <- 2 * stats::pnorm(-abs(t))
  cv  <- stats::qnorm(1 - alpha / 2)
  list(
    estimate = estimate,
    std_error = se,
    t_stat = t,
    p_value = p,
    ci_lower = estimate - cv * se,
    ci_upper = estimate + cv * se,
    n_obs = n,
    method = method,
    details = details
  )
}

#' Sharp RDD treatment effect at the cutoff
#'
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param running Running variable column.
#' @param cutoff Threshold (default 0).
#' @param bandwidth Optional bandwidth; if \code{NULL}, CCT MSE-optimal.
#' @param p Local-polynomial order.
#' @param kernel Kernel name.
#' @param cluster Optional cluster column.
#' @param covariates Optional character vector of covariate names.
#' @param alpha Significance level.
#' @export
morie_rdd_sharp <- function(data, outcome, running, cutoff = 0,
                            bandwidth = NULL, p = 1, kernel = "triangular",
                            cluster = NULL, covariates = NULL, alpha = 0.05) {
  if (.morie_rdd_have_rdrobust()) {
    cov_mat <- if (length(covariates))
      as.matrix(data[, covariates, drop = FALSE]) else NULL
    cl_vec  <- if (!is.null(cluster)) data[[cluster]] else NULL
    fit <- rdrobust::rdrobust(
      y = data[[outcome]], x = data[[running]], c = cutoff,
      kernel = kernel, p = p,
      h = if (!is.null(bandwidth)) bandwidth else NULL,
      covs = cov_mat, cluster = cl_vec
    )
    est <- fit$coef["Conventional", 1]
    se  <- fit$se["Conventional", 1]
    n   <- sum(fit$N_h)
    return(.morie_rdd_result(est, se, n,
                             method = "sharp RDD (rdrobust)",
                             alpha = alpha,
                             details = list(fit = fit)))
  }
  # Base-R fallback: separate local-polynomial fits left/right of cutoff
  if (is.null(bandwidth))
    bandwidth <- morie_rdd_bandwidth_rot(data[[running]],
                                         data[[outcome]], cutoff)$bandwidth
  x <- data[[running]]
  y <- data[[outcome]]
  fL <- .morie_rdd_local_poly_fit(x[x <  cutoff], y[x <  cutoff],
                                  cutoff, bandwidth, p, kernel)
  fR <- .morie_rdd_local_poly_fit(x[x >= cutoff], y[x >= cutoff],
                                  cutoff, bandwidth, p, kernel)
  est <- fR$beta[1] - fL$beta[1]
  se  <- sqrt(fR$se[1]^2 + fL$se[1]^2)
  .morie_rdd_result(est, se, fL$n + fR$n,
                    method = "sharp RDD (base-R local poly)",
                    alpha = alpha,
                    details = list(left = fL, right = fR,
                                   bandwidth = bandwidth))
}

#' Fuzzy RDD treatment effect via instrumented Wald ratio
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_fuzzy <- function(data, outcome, running, treatment,
                            cutoff = 0, bandwidth = NULL, p = 1,
                            kernel = "triangular", alpha = 0.05) {
  if (.morie_rdd_have_rdrobust()) {
    fit <- rdrobust::rdrobust(
      y = data[[outcome]], x = data[[running]],
      fuzzy = data[[treatment]],
      c = cutoff, kernel = kernel, p = p,
      h = if (!is.null(bandwidth)) bandwidth else NULL
    )
    return(.morie_rdd_result(fit$coef["Conventional", 1],
                             fit$se["Conventional", 1],
                             sum(fit$N_h),
                             method = "fuzzy RDD (rdrobust)",
                             alpha = alpha, details = list(fit = fit)))
  }
  num <- morie_rdd_sharp(data, outcome, running, cutoff, bandwidth,
                         p, kernel, alpha = alpha)
  den <- morie_rdd_sharp(data, treatment, running, cutoff, bandwidth,
                         p, kernel, alpha = alpha)
  est <- num$estimate / den$estimate
  se  <- sqrt((num$std_error / den$estimate)^2 +
              (num$estimate * den$std_error / den$estimate^2)^2)
  .morie_rdd_result(est, se, num$n_obs,
                    method = "fuzzy RDD (Wald ratio)",
                    alpha = alpha,
                    details = list(numerator = num, denominator = den))
}

#' CCT bias-corrected, robust-SE RDD inference
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_bias_corrected <- function(data, outcome, running, cutoff = 0,
                                     bandwidth = NULL, rho = 1, p = 1,
                                     kernel = "triangular", alpha = 0.05) {
  if (.morie_rdd_have_rdrobust()) {
    fit <- rdrobust::rdrobust(
      y = data[[outcome]], x = data[[running]], c = cutoff,
      kernel = kernel, p = p,
      h = if (!is.null(bandwidth)) bandwidth else NULL,
      rho = rho
    )
    est <- fit$coef["Bias-Corrected", 1]
    se  <- fit$se["Robust", 1]
    return(.morie_rdd_result(est, se, sum(fit$N_h),
                             method = "CCT bias-corrected RDD",
                             alpha = alpha, details = list(fit = fit)))
  }
  res <- morie_rdd_sharp(data, outcome, running, cutoff, bandwidth, p,
                         kernel, alpha = alpha)
  res$method <- "bias-corrected (sharp fallback \u2014 install rdrobust)"
  res
}


# ---------------------------------------------------------------------------
# Density / manipulation tests
# ---------------------------------------------------------------------------

#' McCrary (2008) density manipulation test
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_mccrary <- function(x, cutoff = 0, n_bins = 50,
                              bandwidth = NULL) {
  if (.morie_rdd_have_rddensity()) {
    fit <- rddensity::rddensity(x, c = cutoff)
    return(list(statistic = fit$test$t_jk,
                p_value   = fit$test$p_jk,
                name = "McCrary (rddensity)", details = list(fit = fit)))
  }
  # The `rdd` package was a documented fallback but CRAN archived
  # it in 2024 and pak can no longer resolve it. rddensity is the
  # primary backend and is on CRAN (declared in Suggests). The
  # native McCrary histogram density test is still TODO.
  list(statistic = NA, p_value = NA,
       name = "McCrary (requires rddensity)")
}

#' Cattaneo-Jansson-Ma (2020) local-polynomial density test
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_cattaneo_density <- function(x, cutoff = 0, p = 2,
                                       kernel = "triangular",
                                       bandwidth = NULL) {
  if (.morie_rdd_have_rddensity()) {
    fit <- rddensity::rddensity(x, c = cutoff, p = p, kernel = kernel,
                                h = bandwidth)
    return(list(statistic = fit$test$t_jk,
                p_value   = fit$test$p_jk,
                name = "Cattaneo-Jansson-Ma (rddensity)",
                details = list(fit = fit)))
  }
  morie_rdd_mccrary(x, cutoff, bandwidth = bandwidth)
}


# ---------------------------------------------------------------------------
# Validity diagnostics
# ---------------------------------------------------------------------------

#' Covariate balance at the cutoff
#'
#' Runs a sharp-RDD null test on each covariate.
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_covariate_balance <- function(data, running, covariates,
                                        cutoff = 0, bandwidth = NULL,
                                        kernel = "triangular", alpha = 0.05) {
  rows <- lapply(covariates, function(c) {
    res <- morie_rdd_sharp(data, c, running, cutoff, bandwidth,
                           kernel = kernel, alpha = alpha)
    data.frame(covariate = c, estimate = res$estimate,
               std_error = res$std_error, t_stat = res$t_stat,
               p_value = res$p_value, balanced = res$p_value > alpha)
  })
  do.call(rbind, rows)
}

#' Placebo cutoff falsification test
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_placebo_cutoff <- function(data, outcome, running, true_cutoff,
                                     placebo_cutoffs, bandwidth = NULL,
                                     p = 1, kernel = "triangular",
                                     alpha = 0.05) {
  rows <- lapply(placebo_cutoffs, function(c) {
    if (isTRUE(all.equal(c, true_cutoff))) return(NULL)
    sub <- data[data[[running]] < true_cutoff, , drop = FALSE]
    if (c > true_cutoff)
      sub <- data[data[[running]] >= true_cutoff, , drop = FALSE]
    res <- morie_rdd_sharp(sub, outcome, running, cutoff = c,
                           bandwidth = bandwidth, p = p, kernel = kernel,
                           alpha = alpha)
    data.frame(placebo_cutoff = c, estimate = res$estimate,
               std_error = res$std_error, p_value = res$p_value,
               significant = res$p_value < alpha)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

#' Donut-hole RDD
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_donut <- function(data, outcome, running, cutoff = 0, donut = 0,
                            bandwidth = NULL, p = 1, kernel = "triangular",
                            alpha = 0.05) {
  keep <- abs(data[[running]] - cutoff) > donut
  res  <- morie_rdd_sharp(data[keep, , drop = FALSE],
                          outcome, running, cutoff, bandwidth, p, kernel,
                          alpha = alpha)
  res$method <- paste0(res$method, " (donut=", donut, ")")
  res$details$donut <- donut
  res
}

#' RDD with discrete running variable
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_discrete <- function(data, outcome, running, cutoff = 0,
                               bandwidth = NULL, p = 0, alpha = 0.05) {
  res <- morie_rdd_sharp(data, outcome, running, cutoff, bandwidth,
                         p = p, kernel = "uniform", alpha = alpha)
  res$method <- paste0(res$method, " (discrete running var)")
  res
}


# ---------------------------------------------------------------------------
# Plot / sensitivity / kink / randomisation / geographic
# ---------------------------------------------------------------------------

#' Binned scatter + global-polynomial data for an RD plot
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_plot_data <- function(data, outcome, running, cutoff = 0,
                                n_bins = 20, p_global = 4, p_local = 1,
                                bandwidth = NULL, kernel = "triangular") {
  if (.morie_rdd_have_rdrobust()) {
    plot <- rdrobust::rdplot(y = data[[outcome]], x = data[[running]],
                             c = cutoff, nbins = n_bins, p = p_global,
                             hide = TRUE)
    return(list(bins = plot$vars_bins, poly = plot$vars_poly,
                fit = plot))
  }
  x <- data[[running]]
  y <- data[[outcome]]
  breaks <- stats::quantile(x, probs = seq(0, 1, length.out = n_bins + 1),
                            na.rm = TRUE)
  bin <- cut(x, breaks = unique(breaks), include.lowest = TRUE)
  bins <- aggregate(list(mean_y = y, mean_x = x), by = list(bin = bin),
                    FUN = mean)
  poly_fit <- stats::lm(y ~ poly(x, p_global))
  poly <- data.frame(x = sort(x), fitted = stats::predict(poly_fit,
                                            newdata = data.frame(x = sort(x))))
  list(bins = bins, poly = poly)
}

#' Bandwidth sensitivity sweep
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_bandwidth_sensitivity <- function(data, outcome, running,
                                            cutoff = 0,
                                            bandwidth_range = NULL,
                                            p = 1, kernel = "triangular",
                                            alpha = 0.05) {
  if (is.null(bandwidth_range)) {
    base_h <- morie_rdd_bandwidth_rot(data[[running]],
                                      data[[outcome]],
                                      cutoff)$bandwidth
    bandwidth_range <- seq(0.5 * base_h, 2 * base_h, length.out = 10)
  }
  rows <- lapply(bandwidth_range, function(h) {
    res <- morie_rdd_sharp(data, outcome, running, cutoff,
                           bandwidth = h, p = p, kernel = kernel,
                           alpha = alpha)
    data.frame(bandwidth = h, estimate = res$estimate,
               std_error = res$std_error, p_value = res$p_value,
               ci_lower = res$ci_lower, ci_upper = res$ci_upper)
  })
  do.call(rbind, rows)
}

#' Regression kink design -- slope discontinuity at the cutoff
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_kink <- function(data, outcome, running, cutoff = 0,
                           bandwidth = NULL, kernel = "triangular",
                           alpha = 0.05) {
  if (.morie_rdd_have_rdrobust()) {
    fit <- rdrobust::rdrobust(y = data[[outcome]], x = data[[running]],
                              c = cutoff, deriv = 1, kernel = kernel,
                              h = if (!is.null(bandwidth)) bandwidth else NULL)
    return(.morie_rdd_result(fit$coef["Conventional", 1],
                             fit$se["Conventional", 1],
                             sum(fit$N_h),
                             method = "kink RDD (rdrobust deriv=1)",
                             alpha = alpha, details = list(fit = fit)))
  }
  # TODO: native local-quadratic derivative jump estimator
  res <- morie_rdd_sharp(data, outcome, running, cutoff, bandwidth, p = 2,
                         kernel = kernel, alpha = alpha)
  res$method <- "kink (sharp fallback \u2014 install rdrobust)"
  res
}

#' Local-randomisation RDD via permutation in a fixed window
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_local_randomisation <- function(data, outcome, running, cutoff = 0,
                                          window = 1, n_permutations = 1000,
                                          seed = 42, alpha = 0.05) {
  set.seed(seed)
  in_w <- abs(data[[running]] - cutoff) <= window
  sub  <- data[in_w, , drop = FALSE]
  z    <- as.integer(sub[[running]] >= cutoff)
  y    <- sub[[outcome]]
  obs  <- mean(y[z == 1]) - mean(y[z == 0])
  permstats <- replicate(n_permutations, {
    z_perm <- sample(z)
    mean(y[z_perm == 1]) - mean(y[z_perm == 0])
  })
  p <- mean(abs(permstats) >= abs(obs))
  list(estimate = obs,
       std_error = stats::sd(permstats),
       p_value   = p,
       ci_lower  = stats::quantile(permstats, alpha / 2),
       ci_upper  = stats::quantile(permstats, 1 - alpha / 2),
       n_obs     = nrow(sub),
       method    = "local-randomisation (permutation)",
       details   = list(window = window, n_permutations = n_permutations))
}

#' Geographic / boundary RDD on a signed distance
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_geographic <- function(data, outcome, distance_to_boundary, side,
                                 bandwidth = NULL, p = 1,
                                 kernel = "triangular", alpha = 0.05) {
  signed <- ifelse(data[[side]] == 1, abs(data[[distance_to_boundary]]),
                                       -abs(data[[distance_to_boundary]]))
  data$.signed_dist_ <- signed
  res <- morie_rdd_sharp(data, outcome, ".signed_dist_", cutoff = 0,
                         bandwidth, p, kernel, alpha = alpha)
  res$method <- "geographic RDD (signed distance)"
  res
}


# ---------------------------------------------------------------------------
# Power / sample-size analytics
# ---------------------------------------------------------------------------

#' RDD power calculation
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_power <- function(n, tau, sigma, cutoff_density = 1,
                            bandwidth = NULL, kernel = "triangular",
                            alpha = 0.05) {
  if (is.null(bandwidth)) bandwidth <- n^(-1 / 5)
  K <- .morie_rdd_get_kernel(kernel)
  k2 <- stats::integrate(function(u) K(u)^2, -1, 1)$value
  ne <- n * bandwidth * cutoff_density
  se <- sqrt(2 * k2 * sigma^2 / ne)
  z_alpha <- stats::qnorm(1 - alpha / 2)
  power <- 1 - stats::pnorm(z_alpha - tau / se) +
                stats::pnorm(-z_alpha - tau / se)
  list(power = power, std_error = se, effective_n = ne,
       tau = tau, sigma = sigma, alpha = alpha)
}

#' RDD sample-size determination
#' @inheritParams morie_rdd_params
#' @export
morie_rdd_sample_size <- function(tau, sigma, cutoff_density = 1,
                                  bandwidth = 1, power = 0.8,
                                  kernel = "triangular", alpha = 0.05) {
  K  <- .morie_rdd_get_kernel(kernel)
  k2 <- stats::integrate(function(u) K(u)^2, -1, 1)$value
  z_a <- stats::qnorm(1 - alpha / 2)
  z_b <- stats::qnorm(power)
  ne  <- 2 * k2 * sigma^2 * (z_a + z_b)^2 / tau^2
  as.integer(ceiling(ne / (bandwidth * cutoff_density)))
}
