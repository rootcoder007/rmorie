# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Regression Discontinuity Design (RDD) estimators for morie.
# Ports the public API of `src/morie/rdd.py` (~1851 LOC) to R.
#
# Module 16 (feat/native-specializations): the RDD family is native.
# Sharp / fuzzy / bias-corrected estimation, the IK (2012) MSE-optimal
# plug-in bandwidth, the kink (deriv = 1) estimator, and the McCrary
# density test run on the engines in R/rdd_native.R (local-polynomial
# WLS with nearest-neighbor robust variance). rdrobust is no longer
# used. rddensity remains ONLY as an extender backend for the
# Cattaneo-Jansson-Ma local-polynomial density test (tests/cross
# validates the native estimators against rdrobust where installed).
#
# Public R names mirror the Python module under the `morie_rdd_*` prefix.

#' @importFrom stats lm coef vcov pnorm pt pf pchisq qnorm qt sd var model.matrix predict quantile complete.cases approx
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
#' @return A \code{function} of one argument returning Epanechnikov kernel weights.
#' @examples
#' morie_rdd_kernel_epanechnikov(seq(-1, 1, by = 0.5))
#' @export
morie_rdd_kernel_epanechnikov <- function(u)
  ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
#' @rdname morie_rdd_kernels
#' @return A \code{function} of one argument returning uniform kernel weights.
#' @examples
#' morie_rdd_kernel_uniform(seq(-2, 2, by = 1))
#' @export
morie_rdd_kernel_uniform <- function(u) ifelse(abs(u) <= 1, 0.5, 0)
#' @rdname morie_rdd_kernels
#' @return A \code{function} of one argument returning Gaussian kernel weights.
#' @examples
#' morie_rdd_kernel_gaussian(seq(-1, 1, by = 0.5))
#' @export
morie_rdd_kernel_gaussian <- function(u) stats::dnorm(u)

.morie_rdd_kernels <- list(
  triangular   = morie_rdd_kernel_triangular,
  epanechnikov = morie_rdd_kernel_epanechnikov,
  uniform      = morie_rdd_kernel_uniform,
  gaussian     = morie_rdd_kernel_gaussian
)

#' Internal helper: Morie Rdd Get Kernel
#' @noRd
.morie_rdd_get_kernel <- function(name) {
  fn <- .morie_rdd_kernels[[name]]
  if (is.null(fn))
    stop("Unknown kernel '", name,
         "'. Choose from: ", paste(names(.morie_rdd_kernels), collapse = ", "))
  fn
}

#' Internal helper: Morie Rdd Have Rdrobust
#' @noRd
.morie_rdd_have_rdrobust  <- function() requireNamespace("rdrobust",  quietly = TRUE)
#' Internal helper: Morie Rdd Have Rddensity
#' @noRd
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
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' ep <- c(-0.5, 0, 0.5)
#' out <- morie_rdd_local_polynomial(x, y, ep, h = 0.5)
#' out
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

#' Internal helper: Morie Rdd Bw Result
#' @noRd
.morie_rdd_bw_result <- function(h, method, details = list())
  list(bandwidth = as.numeric(h), method = method, details = details)

#' Imbens-Kalyanaraman (IK) MSE-optimal bandwidth
#'
#' Native implementation of the Imbens & Kalyanaraman (2012) three-step
#' plug-in rule: pilot density and conditional variance at the cutoff,
#' one-sided curvature estimates, and the regularized MSE-optimal
#' bandwidth with the kernel constant.
#' @inheritParams morie_rdd_params
#' @return A named list with elements \code{bandwidth}, \code{method}, \code{details}.
#' @examples
#' set.seed(40)
#' x <- runif(1000, -1, 1)
#' y <- 0.5 + 0.8 * x - 0.4 * x^2 + 1.2 * (x >= 0) + rnorm(1000, 0, 0.4)
#' bw <- morie_rdd_bandwidth_ik(x, y)
#' bw$bandwidth
#' @export
morie_rdd_bandwidth_ik <- function(x, y, cutoff = 0,
                                   kernel = "triangular") {
  ik <- .morie_rdd_ik_native(x, y, cutoff, kernel)
  .morie_rdd_bw_result(ik$bandwidth, "IK 2012 plug-in (rmorie native)",
                       details = ik$details %||% list())
}

#' Rule-of-thumb (ROT) bandwidth -- Silverman-style on running variable
#' @inheritParams morie_rdd_params
#' @return A named list with elements \code{bandwidth}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' bw <- morie_rdd_bandwidth_rot(x, y)
#' bw$bandwidth
#' @export
morie_rdd_bandwidth_rot <- function(x, y, cutoff = 0) {
  n     <- length(x)
  # np.std: population sd, divisor n, not stats::sd's n - 1.
  sd_x  <- sqrt(sum((x - mean(x))^2) / n)
  # np.percentile's default interpolation is R's quantile type 7.
  qs    <- stats::quantile(x, c(0.25, 0.75), names = FALSE, type = 7)
  iqr_x <- qs[2] - qs[1]
  # Silverman's rule: the robust scale keeps a heavy tail from
  # inflating the window.
  h     <- 0.9 * min(sd_x, iqr_x / 1.349) * n^(-1 / 5)
  .morie_rdd_bw_result(h, "ROT",
                       list(sd_x = sd_x, iqr_x = iqr_x))
}

#' Internal: weighted local polynomial fit at a point
#'
#' Kernel-weighted least squares of \code{y} on powers of
#' \code{x - x0}, with the sandwich variance the CCT bandwidth needs.
#' Mirrors the Python arm's \code{_local_poly_fit} term for term, so
#' the two produce the same coefficients and the same \code{V}.
#'
#' @param x,y The running variable and the outcome, one side of the
#'   cutoff.
#' @param x0 The point to fit at -- the cutoff.
#' @param h The bandwidth.
#' @param p Polynomial order.
#' @param kernel One of the names in \code{.morie_rdd_kernels}.
#' @return A list with \code{beta} (coefficients, intercept first) and
#'   \code{V} (heteroskedasticity-robust covariance).
#' @keywords internal
#' @noRd
.morie_rdd_local_poly <- function(x, y, x0, h, p = 1L,
                                  kernel = "triangular") {
  kf <- .morie_rdd_kernels[[kernel]]
  if (is.null(kf)) {
    stop("unknown kernel '", kernel, "'; choose from ",
         paste(names(.morie_rdd_kernels), collapse = ", "), ".",
         call. = FALSE)
  }
  p <- as.integer(p)
  u <- (x - x0) / h
  kw <- kf(u) / h
  X <- vapply(0:p, function(j) (x - x0)^j, numeric(length(x)))
  if (is.null(dim(X))) X <- matrix(X, nrow = length(x))
  XtWX <- crossprod(X, kw * X)
  XtWX_inv <- tryCatch(solve(XtWX),
                       error = function(e) .morie_ginv(XtWX))
  beta <- as.numeric(XtWX_inv %*% crossprod(X, kw * y))
  resid <- as.numeric(y - X %*% beta)
  # The sandwich the Python arm forms: kw^2 * resid^2 in the meat, and
  # sigma2 scaling it, with the degrees of freedom counted over the
  # points the kernel actually gives weight to.
  df <- max(sum(kw > 0) - (p + 1L), 1L)
  sigma2 <- sum(kw * resid^2) / df
  meat <- crossprod(X, (kw^2 * resid^2) * X)
  V <- sigma2 * (XtWX_inv %*% meat %*% XtWX_inv)
  list(beta = beta, V = V)
}

#' Calonico-Cattaneo-Titiunik MSE-optimal bandwidth
#'
#' The IK bandwidth is used as the pilot; the curvature difference
#' across the cutoff gives the squared bias and the two one-sided
#' intercept variances give the variance, and the MSE-optimal rule
#' balances them. The polynomial order \code{p} enters everywhere --
#' the curvature is read off the order \code{p + 1} fit and the rate is
#' \eqn{n^{-1/(2p+3)}} -- which is why the previous implementation,
#' delegating to IK and discarding \code{p}, could not answer for any
#' order but the one IK assumes.
#'
#' @param x,y The running variable and the outcome.
#' @param cutoff The threshold.
#' @param kernel One of \code{"triangular"}, \code{"epanechnikov"},
#'   \code{"uniform"}, \code{"gaussian"}.
#' @param p Polynomial order for the local fit.
#' @return A named list with \code{bandwidth}, \code{method} and
#'   \code{details} carrying \code{h_mse}, \code{h_cer},
#'   \code{bias_sq} and \code{variance}.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' bw <- morie_rdd_bandwidth_cct(x, y)
#' bw$bandwidth
morie_rdd_bandwidth_cct <- function(x, y, cutoff = 0,
                                    kernel = "triangular", p = 1) {
  x <- as.numeric(x); y <- as.numeric(y)
  n <- length(x)
  p <- as.integer(p)
  ik <- .morie_rdd_ik_native(x, y, cutoff, kernel)
  h_pilot <- ik$bandwidth

  left <- x < cutoff
  right <- x >= cutoff

  curvature <- function(xs, ys, h) {
    if (length(xs) < p + 3L) return(0)
    b <- .morie_rdd_local_poly(xs, ys, cutoff, h, p = p + 1L,
                               kernel = kernel)$beta
    if (length(b) > p + 1L) (p + 1) * b[p + 2L] else 0
  }
  var_side <- function(xs, ys, h) {
    if (length(xs) < p + 2L)
      return(if (length(ys)) sum((ys - mean(ys))^2) / length(ys) else 1)
    .morie_rdd_local_poly(xs, ys, cutoff, h, p = p, kernel = kernel)$V[1, 1]
  }

  b_left  <- curvature(x[left], y[left], h_pilot)
  b_right <- curvature(x[right], y[right], h_pilot)
  bias_sq <- (b_right - b_left)^2
  variance <- var_side(x[left], y[left], h_pilot) +
    var_side(x[right], y[right], h_pilot)

  h_mse <- if (bias_sq > 0) {
    (variance / (2 * (p + 1) * bias_sq))^(1 / (2 * p + 3)) *
      n^(-1 / (2 * p + 3))
  } else {
    h_pilot
  }
  x_range <- max(x) - min(x)
  h_mse <- min(max(h_mse, x_range * 0.01), x_range * 0.5)
  h_cer <- h_mse * n^(-p / (3 * (2 * p + 3)))

  .morie_rdd_bw_result(h_mse, "CCT",
                       list(h_mse = h_mse, h_cer = h_cer,
                            bias_sq = bias_sq, variance = variance))
}


# ---------------------------------------------------------------------------
# Sharp / fuzzy / bias-corrected RDD
# ---------------------------------------------------------------------------

#' Internal helper: Morie Rdd Result
#' @noRd
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
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(33)
#' n <- 1500
#' x <- runif(n, -1, 1)
#' y <- 0.5 * x + 0.3 * x^2 + 1.5 * (x >= 0) + rnorm(n, sd = 0.5)
#' d <- data.frame(y, x)
#' res <- morie_rdd_sharp(d, outcome = "y", running = "x", cutoff = 0)
#' res$estimate
#' @export
morie_rdd_sharp <- function(data, outcome, running, cutoff = 0,
                            bandwidth = NULL, p = 1, kernel = "triangular",
                            cluster = NULL, covariates = NULL, alpha = 0.05) {
  x <- data[[running]]
  y <- data[[outcome]]
  if (length(covariates)) {
    # Covariate adjustment: partial the covariates out of the outcome
    # (common-coefficient adjustment of Calonico et al. 2019).
    Xc <- as.matrix(data[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    y <- as.numeric(stats::lm.fit(cbind(1, Xc), y)$residuals) + mean(y)
  }
  if (is.null(bandwidth))
    bandwidth <- .morie_rdd_ik_native(x, y, cutoff, kernel)$bandwidth
  fit <- .morie_rdd_jump_native(x, y, cutoff, bandwidth, p, kernel)
  .morie_rdd_result(fit$estimate, fit$se, fit$n,
                    method = "sharp RDD (rmorie native)",
                    alpha = alpha,
                    details = list(left = fit$left, right = fit$right,
                                   bandwidth = bandwidth))
}

#' Fuzzy RDD treatment effect via instrumented Wald ratio
#' @inheritParams morie_rdd_params
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(34)
#' n <- 1000
#' x <- runif(n, -1, 1)
#' tr <- as.integer(xor(x >= 0, rbinom(n, 1, 0.1) == 1))
#' y <- 0.3 * x + 1.5 * tr + rnorm(n, sd = 0.5)
#' d <- data.frame(y, x, tr)
#' res <- morie_rdd_fuzzy(d, outcome = "y", running = "x",
#'                        treatment = "tr", cutoff = 0)
#' res$estimate
#' @export
morie_rdd_fuzzy <- function(data, outcome, running, treatment,
                            cutoff = 0, bandwidth = NULL, p = 1,
                            kernel = "triangular", alpha = 0.05) {
  if (is.null(bandwidth))
    bandwidth <- .morie_rdd_ik_native(data[[running]], data[[outcome]],
                                      cutoff, kernel)$bandwidth
  num <- morie_rdd_sharp(data, outcome, running, cutoff, bandwidth,
                         p, kernel, alpha = alpha)
  den <- morie_rdd_sharp(data, treatment, running, cutoff, bandwidth,
                         p, kernel, alpha = alpha)
  est <- num$estimate / den$estimate
  se  <- sqrt((num$std_error / den$estimate)^2 +
              (num$estimate * den$std_error / den$estimate^2)^2)
  .morie_rdd_result(est, se, num$n_obs,
                    method = "fuzzy RDD (rmorie native Wald ratio)",
                    alpha = alpha,
                    details = list(numerator = num, denominator = den))
}

#' CCT bias-corrected, robust-SE RDD inference
#' @inheritParams morie_rdd_params
#' @return A numeric value.
#' @examples
#' set.seed(44)
#' x <- runif(1000, -1, 1)
#' y <- 0.5 + 0.8 * x - 0.4 * x^2 + 1.2 * (x >= 0) + rnorm(1000, 0, 0.4)
#' d <- data.frame(x, y)
#' bc <- morie_rdd_bias_corrected(d, "y", "x", bandwidth = 0.5, rho = 1)
#' bc$estimate
#' @export
morie_rdd_bias_corrected <- function(data, outcome, running, cutoff = 0,
                                     bandwidth = NULL, rho = 1, p = 1,
                                     kernel = "triangular", alpha = 0.05) {
  x <- data[[running]]
  y <- data[[outcome]]
  if (is.null(bandwidth))
    bandwidth <- .morie_rdd_ik_native(x, y, cutoff, kernel)$bandwidth
  # CCT with b = h/rho: when rho = 1 the bias-corrected point estimate
  # equals the order-(p+1) local fit at h, and the robust variance is
  # that fit's NN variance (Calonico-Cattaneo-Titiunik 2014, Remark 7).
  b <- bandwidth / rho
  fit_q <- .morie_rdd_jump_native(x, y, cutoff, b, p + 1L, kernel)
  .morie_rdd_result(fit_q$estimate, fit_q$se, fit_q$n,
                    method = "CCT bias-corrected RDD (rmorie native)",
                    alpha = alpha,
                    details = list(fit = fit_q, h = bandwidth, b = b,
                                   rho = rho))
}


# ---------------------------------------------------------------------------
# Density / manipulation tests
# ---------------------------------------------------------------------------

#' McCrary (2008) density manipulation test
#' @inheritParams morie_rdd_params
#' @return A named \code{list} (see Details).
#' @examples
#' set.seed(35)
#' x <- runif(2000, -1, 1)
#' out <- morie_rdd_mccrary(x, cutoff = 0)
#' out$p_value
#' @export
morie_rdd_mccrary <- function(x, cutoff = 0, n_bins = 50,
                              bandwidth = NULL) {
  fit <- .morie_rdd_mccrary_native(x, cutoff, bandwidth = bandwidth)
  list(statistic = fit$statistic,
       p_value   = fit$p_value,
       theta     = fit$theta,
       name = "McCrary (rmorie native)", details = fit)
}

#' Cattaneo-Jansson-Ma (2020) local-polynomial density test
#' @inheritParams morie_rdd_params
#' @return A named \code{list} (see Details).
#' @examples
#' set.seed(1)
#' x <- runif(300, -1, 1)
#' res <- morie_rdd_cattaneo_density(x)
#' res$name
#' @export
morie_rdd_cattaneo_density <- function(x, cutoff = 0, p = 2,
                                       kernel = "triangular",
                                       bandwidth = NULL) {
  if (.morie_rdd_have_rddensity()) {
    fit <- rddensity::rddensity(x, c = cutoff, p = p, kernel = kernel,
                                h = bandwidth)
    return(list(statistic = fit$test$t_jk,
                p_value   = fit$test$p_jk,
                name = "Cattaneo-Jansson-Ma (rddensity extender)",
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
#' @return A \code{data.frame} of covariate-balance statistics, one row per covariate.
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' d <- data.frame(x, y, c1 = rnorm(400), c2 = rnorm(400))
#' out <- morie_rdd_covariate_balance(d, "x", c("c1", "c2"))
#' out
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
#' @return A logical scalar.
#' @examples
#' set.seed(1)
#' x <- runif(600, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(600, sd = 0.3)
#' d <- data.frame(x, y)
#' out <- morie_rdd_placebo_cutoff(d, "y", "x", true_cutoff = 0,
#'                                 placebo_cutoffs = c(-0.5, 0, 0.5))
#' out
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
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' x <- runif(800, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(800, sd = 0.3)
#' d <- data.frame(x, y)
#' res <- morie_rdd_donut(d, "y", "x", donut = 0.05)
#' res$estimate
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
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' d <- data.frame(x, y)
#' res <- morie_rdd_discrete(d, "y", "x")
#' res$method
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
#' @return A named \code{list} (see Details).
#' @examples
#' set.seed(1)
#' x <- runif(300, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(300, sd = 0.3)
#' d <- data.frame(x, y)
#' out <- morie_rdd_plot_data(d, "y", "x", n_bins = 10L)
#' head(out$bins)
#' @export
morie_rdd_plot_data <- function(data, outcome, running, cutoff = 0,
                                n_bins = 20, p_global = 4, p_local = 1,
                                bandwidth = NULL, kernel = "triangular") {
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
#' @return A \code{data.frame} of RDD estimates across bandwidths, one row per bandwidth.
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' d <- data.frame(x, y)
#' out <- morie_rdd_bandwidth_sensitivity(d, "y", "x")
#' out
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
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(45)
#' n <- 2000
#' x <- runif(n, -1, 1)
#' y <- 1 + 0.5 * x + 1.5 * pmax(x, 0) + rnorm(n, 0, 0.2)
#' df <- data.frame(x, y)
#' res <- morie_rdd_kink(df, "y", "x", bandwidth = 0.6)
#' res$estimate
#' @export
morie_rdd_kink <- function(data, outcome, running, cutoff = 0,
                           bandwidth = NULL, kernel = "triangular",
                           alpha = 0.05) {
  x <- data[[running]]
  y <- data[[outcome]]
  if (is.null(bandwidth))
    bandwidth <- .morie_rdd_ik_native(x, y, cutoff, kernel)$bandwidth
  # Slope discontinuity: local-quadratic one-sided fits, first
  # derivative jump (deriv = 1) with NN-robust variance.
  fit <- .morie_rdd_jump_native(x, y, cutoff, bandwidth, p = 2L,
                                kernel = kernel, deriv = 1L)
  .morie_rdd_result(fit$estimate, fit$se, fit$n,
                    method = "kink RDD (rmorie native deriv=1)",
                    alpha = alpha,
                    details = list(fit = fit, bandwidth = bandwidth))
}

#' Local-randomisation RDD via permutation in a fixed window
#' @inheritParams morie_rdd_params
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' x <- runif(400, -1, 1)
#' y <- 0.3 * x + 1.0 * (x >= 0) + rnorm(400, sd = 0.3)
#' d <- data.frame(x, y)
#' res <- morie_rdd_local_randomisation(d, "y", "x", window = 0.3,
#'                                      n_permutations = 200L, seed = 1)
#' res$p_value
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
#' @return A named list with elements \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
#' @examples
#' set.seed(1)
#' n <- 400
#' side <- rbinom(n, 1, 0.5)
#' dist_to_boundary <- runif(n, 0, 1)
#' signed <- ifelse(side == 1, dist_to_boundary, -dist_to_boundary)
#' y <- 0.5 * (signed >= 0) + rnorm(n, sd = 0.3)
#' d <- data.frame(y, dist_to_boundary, side)
#' res <- morie_rdd_geographic(d, "y", "dist_to_boundary", "side")
#' res$estimate
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
#' @return A named list with elements \code{power}, \code{std_error}, \code{effective_n}, \code{tau}, \code{sigma}, \code{alpha}.
#' @examples
#' res <- morie_rdd_power(n = 500, tau = 0.5, sigma = 1, kernel = "triangular")
#' res$power
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
#' @return An integer scalar: the required sample size.
#' @examples
#' ss <- morie_rdd_sample_size(tau = 0.5, sigma = 1, power = 0.8)
#' ss
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
