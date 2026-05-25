# SPDX-License-Identifier: AGPL-3.0-or-later
#' Semiparametric kernel primitives (R port)
#'
#' R port of \code{morie.semipar_bridge}. Provides the kernel-based
#' building blocks used by morie's nuisance estimation pipelines
#' (TMLE, AIPW, DML): kernel evaluation, Nadaraya-Watson regression,
#' local linear regression, kernel density estimation, and bandwidth
#' selection.
#'
#' The Python module loads a C shared library (\code{semipar_kernels.dylib}
#' / \code{.so}) and falls back to NumPy. The R port is pure R: it
#' implements the same algorithms in vectorised form and additionally
#' wraps \code{mgcv::gam} for a high-quality penalised-spline smoother
#' as an alternative to manual bandwidth selection.
#'
#' Functions
#' ---------
#' \itemize{
#'   \item \code{\link{kernel_eval}}: evaluate a kernel function.
#'   \item \code{\link{nw_regression}}: Nadaraya-Watson kernel regression.
#'   \item \code{\link{local_linear}}: local linear kernel regression.
#'   \item \code{\link{kde}}: kernel density estimation.
#'   \item \code{\link{silverman_bandwidth}}: rule-of-thumb bandwidth.
#'   \item \code{\link{loocv_bandwidth}}: leave-one-out CV bandwidth
#'     for NW regression.
#'   \item \code{\link{kernel_cond_moments}}: kernel-weighted mean
#'     and variance.
#'   \item \code{\link{gam_smoother}}: \code{mgcv::gam} thin-plate
#'     smoother fit + predict.
#'   \item \code{\link{SemiparKernels}}: object-style wrapper.
#' }
#'
#' @name morie_semipar_bridge
NULL


# ---------------------------------------------------------------------------
# Kernel type integer codes (mirror the Python enum)
# ---------------------------------------------------------------------------

#' Kernel type integer codes
#'
#' Integer codes used by morie's C++ semiparametric bridge to select
#' the kernel function for local-polynomial smoothing. Mirror the
#' Python `morie.semipar.KernelType` enum so an R caller can pass these
#' constants directly to any C++ kernel routine.
#'
#' \itemize{
#'   \item \code{KERNEL_GAUSSIAN}: \eqn{K(u) = (1/\sqrt{2\pi}) \exp(-u^2/2)}{K(u) = (1/sqrt(2 pi)) exp(-u^2 / 2)}
#'   \item \code{KERNEL_EPANECHNIKOV}: \eqn{K(u) = (3/4)(1-u^2)}{K(u) = 0.75 (1 - u^2)} on |u| <= 1
#'   \item \code{KERNEL_UNIFORM}: \eqn{K(u) = 1/2}{K(u) = 0.5} on |u| <= 1
#'   \item \code{KERNEL_TRIANGULAR}: \eqn{K(u) = 1 - |u|}{K(u) = 1 - |u|} on |u| <= 1
#'   \item \code{KERNEL_BIWEIGHT}: \eqn{K(u) = (15/16)(1-u^2)^2}{K(u) = (15/16) (1 - u^2)^2} on |u| <= 1
#' }
#'
#' @format Integer scalars (0L, 1L, 2L, 3L, 4L).
#' @name kernel-codes
NULL

#' @rdname kernel-codes
#' @export
KERNEL_GAUSSIAN <- 0L
#' @rdname kernel-codes
#' @export
KERNEL_EPANECHNIKOV <- 1L
#' @rdname kernel-codes
#' @export
KERNEL_UNIFORM <- 2L
#' @rdname kernel-codes
#' @export
KERNEL_TRIANGULAR <- 3L
#' @rdname kernel-codes
#' @export
KERNEL_BIWEIGHT <- 4L

.kernel_name_map <- list(
  gaussian = KERNEL_GAUSSIAN,
  epanechnikov = KERNEL_EPANECHNIKOV,
  uniform = KERNEL_UNIFORM,
  triangular = KERNEL_TRIANGULAR,
  biweight = KERNEL_BIWEIGHT
)


# ---------------------------------------------------------------------------
# Internal kernel functions
# ---------------------------------------------------------------------------

.kernel_gaussian <- function(u) {
  (1.0 / sqrt(2.0 * pi)) * exp(-0.5 * u * u)
}

.kernel_epanechnikov <- function(u) {
  ifelse(abs(u) <= 1.0, 0.75 * (1.0 - u * u), 0.0)
}

.kernel_uniform <- function(u) {
  ifelse(abs(u) <= 1.0, 0.5, 0.0)
}

.kernel_triangular <- function(u) {
  ifelse(abs(u) <= 1.0, 1.0 - abs(u), 0.0)
}

.kernel_biweight <- function(u) {
  ifelse(abs(u) <= 1.0, (15.0 / 16.0) * (1.0 - u * u) ^ 2, 0.0)
}

.kernel_fn <- function(kernel_type) {
  switch(kernel_type + 1L,
         .kernel_gaussian,
         .kernel_epanechnikov,
         .kernel_uniform,
         .kernel_triangular,
         .kernel_biweight,
         .kernel_gaussian)
}

.resolve_kernel <- function(kernel) {
  if (is.numeric(kernel)) return(as.integer(kernel))
  if (is.character(kernel)) {
    code <- .kernel_name_map[[tolower(kernel)]]
    if (is.null(code)) {
      stop(sprintf("Unknown kernel '%s'. Choose from: %s",
                   kernel, paste(names(.kernel_name_map), collapse = ", ")))
    }
    return(code)
  }
  stop("kernel must be character or integer")
}


# ---------------------------------------------------------------------------
# Kernel evaluation
# ---------------------------------------------------------------------------

#' Evaluate a kernel function at point u
#'
#' @param u Numeric evaluation point (scaled by bandwidth).
#' @param kernel_type Integer code or kernel name. One of
#'   \code{KERNEL_GAUSSIAN} (0), \code{KERNEL_EPANECHNIKOV} (1),
#'   \code{KERNEL_UNIFORM} (2), \code{KERNEL_TRIANGULAR} (3),
#'   \code{KERNEL_BIWEIGHT} (4), or the matching string.
#' @return Kernel density value K(u).
#' @export
kernel_eval <- function(u, kernel_type = KERNEL_GAUSSIAN) {
  k <- .resolve_kernel(kernel_type)
  fn <- .kernel_fn(k)
  as.numeric(fn(u))
}


# ---------------------------------------------------------------------------
# Nadaraya-Watson regression
# ---------------------------------------------------------------------------

#' Nadaraya-Watson kernel regression
#'
#' Computes the kernel-weighted local mean estimator
#' m-hat(x) = sum K_h(x - X_i) Y_i divided by sum K_h(x - X_i),
#' with a Gaussian kernel.
#'
#' @param x Numeric vector of observed covariate values, length n.
#' @param y Numeric vector of observed outcomes, length n.
#' @param x_eval Numeric vector of evaluation points.
#' @param bandwidth Positive bandwidth h.
#' @return Numeric vector of fitted values at \code{x_eval}.
#' @references Nadaraya, E. A. (1964). On Estimating Regression.
#'   \emph{Theory of Probability and Its Applications}, 9(1), 141-142.
#' @export
nw_regression <- function(x, y, x_eval, bandwidth) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  x_eval <- as.numeric(x_eval)
  if (length(x) != length(y)) stop("x and y must have equal length")
  if (!is.finite(bandwidth) || bandwidth <= 0) stop("bandwidth must be > 0")
  inv_h <- 1.0 / bandwidth
  y_hat <- numeric(length(x_eval))
  for (j in seq_along(x_eval)) {
    u <- (x_eval[j] - x) * inv_h
    w <- .kernel_gaussian(u)
    den <- sum(w)
    y_hat[j] <- if (den > 1e-300) sum(w * y) / den else 0.0
  }
  y_hat
}


# ---------------------------------------------------------------------------
# Local linear regression
# ---------------------------------------------------------------------------

#' Local linear kernel regression
#'
#' Avoids the boundary bias of Nadaraya-Watson by fitting a local
#' linear model at each evaluation point.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric outcome vector.
#' @param x_eval Evaluation grid.
#' @param bandwidth Positive bandwidth.
#' @param return_slope Logical; if TRUE, also return local slopes.
#' @return If \code{return_slope = FALSE}, a numeric vector of fitted
#'   values; otherwise a list with \code{y_hat} and \code{beta_hat}.
#' @references Fan, J. and Gijbels, I. (1996). Local Polynomial
#'   Modelling and Its Applications. Chapman and Hall.
#' @export
local_linear <- function(x, y, x_eval, bandwidth, return_slope = FALSE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  x_eval <- as.numeric(x_eval)
  if (length(x) != length(y)) stop("x and y must have equal length")
  if (!is.finite(bandwidth) || bandwidth <= 0) stop("bandwidth must be > 0")
  inv_h <- 1.0 / bandwidth
  n_eval <- length(x_eval)
  y_hat <- numeric(n_eval)
  beta_hat <- if (return_slope) numeric(n_eval) else NULL

  for (j in seq_len(n_eval)) {
    x0 <- x_eval[j]
    u <- (x0 - x) * inv_h
    w <- .kernel_gaussian(u)
    dx <- x - x0
    s0 <- sum(w)
    s1 <- sum(w * dx)
    s2 <- sum(w * dx * dx)
    t0 <- sum(w * y)
    t1 <- sum(w * y * dx)
    det <- s0 * s2 - s1 * s1
    if (abs(det) < 1e-300) {
      y_hat[j] <- if (s0 > 1e-300) t0 / s0 else 0.0
      if (return_slope) beta_hat[j] <- 0.0
    } else {
      inv_det <- 1.0 / det
      y_hat[j] <- (s2 * t0 - s1 * t1) * inv_det
      if (return_slope) beta_hat[j] <- (s0 * t1 - s1 * t0) * inv_det
    }
  }
  if (return_slope) list(y_hat = y_hat, beta_hat = beta_hat) else y_hat
}


# ---------------------------------------------------------------------------
# Kernel density estimation
# ---------------------------------------------------------------------------

#' Kernel density estimation
#'
#' Computes f-hat(x) equal to one over n times h times the sum over i
#' of K of (x minus X_i) divided by h.
#'
#' @param x Numeric data vector.
#' @param x_eval Evaluation grid.
#' @param bandwidth Positive bandwidth.
#' @param kernel_type Integer code or kernel name.
#' @return Numeric vector of estimated densities.
#' @references Silverman, B. W. (1986). Density Estimation for
#'   Statistics and Data Analysis. Chapman and Hall.
#' @export
kde <- function(x, x_eval, bandwidth, kernel_type = KERNEL_GAUSSIAN) {
  x <- as.numeric(x)
  x_eval <- as.numeric(x_eval)
  if (!is.finite(bandwidth) || bandwidth <= 0) stop("bandwidth must be > 0")
  k <- .resolve_kernel(kernel_type)
  fn <- .kernel_fn(k)
  n <- length(x)
  inv_h <- 1.0 / bandwidth
  scale <- 1.0 / (n * bandwidth)
  density <- numeric(length(x_eval))
  for (j in seq_along(x_eval)) {
    u <- (x_eval[j] - x) * inv_h
    density[j] <- sum(fn(u)) * scale
  }
  density
}


# ---------------------------------------------------------------------------
# Bandwidth selection
# ---------------------------------------------------------------------------

#' Silverman rule-of-thumb bandwidth
#'
#' Returns h equal to 0.9 times min of sigma-hat and IQR over 1.34
#' times n to the negative one-fifth.
#'
#' @param x Numeric data vector.
#' @return Bandwidth (numeric scalar).
#' @references Silverman, B. W. (1986), p. 48.
#' @export
silverman_bandwidth <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) return(1.0)
  sd_hat <- stats::sd(x)
  iqr <- stats::quantile(x, 0.75, names = FALSE) -
         stats::quantile(x, 0.25, names = FALSE)
  spread <- if (iqr > 0) min(sd_hat, iqr / 1.34) else sd_hat
  if (!is.finite(spread) || spread <= 0) return(1.0)
  0.9 * spread * n ^ (-0.2)
}


#' Leave-one-out cross-validation bandwidth for NW regression
#'
#' Minimises CV(h) equal to one over n times sum over i of
#' (Y_i minus m-hat-h-minus-i of X_i) squared on a grid spanning
#' \code{bw_min} to \code{bw_max}.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric outcome vector.
#' @param bw_min Minimum candidate bandwidth (defaults to
#'   0.1 times the Silverman bandwidth).
#' @param bw_max Maximum candidate bandwidth (defaults to
#'   2.0 times the Silverman bandwidth).
#' @param n_grid Number of candidate values.
#' @return Optimal bandwidth (numeric scalar).
#' @references Hardle, W. (1990). Applied Nonparametric Regression.
#'   Cambridge.
#' @export
loocv_bandwidth <- function(x, y, bw_min = NULL, bw_max = NULL,
                             n_grid = 30L) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must have equal length")
  n <- length(x)
  h_rot <- silverman_bandwidth(x)
  if (is.null(bw_min)) bw_min <- 0.1 * h_rot
  if (is.null(bw_max)) bw_max <- 2.0 * h_rot
  if (bw_min <= 0) bw_min <- 0.01
  if (bw_max <= bw_min) bw_max <- bw_min + 1.0

  grid <- seq(bw_min, bw_max, length.out = as.integer(n_grid))
  best_bw <- grid[1L]
  best_cv <- Inf

  for (h in grid) {
    inv_h <- 1.0 / h
    cv <- 0.0
    for (i in seq_len(n)) {
      mask <- seq_len(n) != i
      u <- (x[i] - x[mask]) * inv_h
      w <- .kernel_gaussian(u)
      den <- sum(w)
      y_hat_i <- if (den > 1e-300) sum(w * y[mask]) / den else 0.0
      cv <- cv + (y[i] - y_hat_i) ^ 2
    }
    cv <- cv / n
    if (cv < best_cv) {
      best_cv <- cv
      best_bw <- h
    }
  }
  best_bw
}


# ---------------------------------------------------------------------------
# Conditional moments
# ---------------------------------------------------------------------------

#' Kernel-weighted conditional mean and variance
#'
#' Useful for the conditional outcome stage of TMLE / AIPW.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric outcome vector.
#' @param x_eval Evaluation grid.
#' @param bandwidth Positive bandwidth.
#' @param return_variance Logical; if FALSE, only the mean is returned.
#' @return Either a numeric vector (mean only) or a list with
#'   \code{mean} and \code{variance}.
#' @export
kernel_cond_moments <- function(x, y, x_eval, bandwidth,
                                 return_variance = TRUE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  x_eval <- as.numeric(x_eval)
  if (length(x) != length(y)) stop("x and y must have equal length")
  if (!is.finite(bandwidth) || bandwidth <= 0) stop("bandwidth must be > 0")
  inv_h <- 1.0 / bandwidth
  n_eval <- length(x_eval)
  mean_out <- numeric(n_eval)
  var_out <- if (return_variance) numeric(n_eval) else NULL

  for (j in seq_len(n_eval)) {
    u <- (x_eval[j] - x) * inv_h
    w <- .kernel_gaussian(u)
    w_sum <- sum(w)
    if (w_sum > 1e-300) {
      m <- sum(w * y) / w_sum
      mean_out[j] <- m
      if (return_variance) {
        ey2 <- sum(w * y * y) / w_sum
        var_out[j] <- max(ey2 - m * m, 0.0)
      }
    } else {
      mean_out[j] <- 0.0
      if (return_variance) var_out[j] <- 0.0
    }
  }
  if (return_variance) list(mean = mean_out, variance = var_out) else mean_out
}


# ---------------------------------------------------------------------------
# mgcv::gam smoother helper
# ---------------------------------------------------------------------------

#' Thin-plate spline smoother via mgcv::gam
#'
#' A penalised-spline alternative to the kernel methods above. Fits
#' \code{y ~ s(x, k = k)} and returns fitted values at \code{x_eval}.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric outcome vector.
#' @param x_eval Evaluation grid (defaults to \code{x}).
#' @param k Basis dimension for the smoother (default 10).
#' @param family GLM family for \code{mgcv::gam} (default
#'   \code{gaussian()}).
#' @return A list with \code{fit} (the fitted gam object),
#'   \code{x_eval}, \code{y_hat} (predictions), and \code{edf}
#'   (effective degrees of freedom).
#' @export
gam_smoother <- function(x, y, x_eval = NULL, k = 10, family = stats::gaussian()) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("gam_smoother requires the mgcv package")
  }
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must have equal length")
  if (is.null(x_eval)) x_eval <- x
  x_eval <- as.numeric(x_eval)

  df <- data.frame(x = x, y = y)
  fit <- mgcv::gam(y ~ s(x, k = k), data = df, family = family)
  newdat <- data.frame(x = x_eval)
  y_hat <- as.numeric(stats::predict(fit, newdata = newdat))
  edf <- sum(fit$edf)
  list(fit = fit, x_eval = x_eval, y_hat = y_hat, edf = edf, k = k)
}


# ---------------------------------------------------------------------------
# Object-style wrapper
# ---------------------------------------------------------------------------

#' Object-style wrapper for the semiparametric kernel toolkit
#'
#' Returns a list of closures bound to the same backend (always pure R
#' in this port; the Python module additionally supports a C backend).
#'
#' @return A list with class \code{morie_semipar_kernels} carrying
#'   methods \code{nw_regression}, \code{local_linear}, \code{kde},
#'   \code{silverman_bandwidth}, \code{loocv_bandwidth},
#'   \code{kernel_cond_moments}, plus a \code{backend} string.
#' @export
SemiparKernels <- function() {
  obj <- list(
    backend = "r",
    available = TRUE,
    nw_regression = function(x, y, x_eval, bandwidth) {
      nw_regression(x, y, x_eval, bandwidth)
    },
    local_linear = function(x, y, x_eval, bandwidth) {
      local_linear(x, y, x_eval, bandwidth, return_slope = TRUE)
    },
    kde = function(x, x_eval, bandwidth, kernel = "gaussian") {
      kde(x, x_eval, bandwidth, kernel_type = .resolve_kernel(kernel))
    },
    silverman_bandwidth = function(x) silverman_bandwidth(x),
    loocv_bandwidth = function(x, y, bw_min = NULL, bw_max = NULL,
                                n_grid = 30L) {
      loocv_bandwidth(x, y, bw_min, bw_max, n_grid)
    },
    kernel_cond_moments = function(x, y, x_eval, bandwidth) {
      kernel_cond_moments(x, y, x_eval, bandwidth, return_variance = TRUE)
    }
  )
  class(obj) <- c("morie_semipar_kernels", "list")
  obj
}


#' @export
print.morie_semipar_kernels <- function(x, ...) {
  cat("morie SemiparKernels\
")
  cat(sprintf("  backend   : %s\
", x$backend))
  cat(sprintf("  available : %s\
", x$available))
  cat("  methods   : nw_regression, local_linear, kde, silverman_bandwidth,\
")
  cat("              loocv_bandwidth, kernel_cond_moments\
")
  invisible(x)
}
