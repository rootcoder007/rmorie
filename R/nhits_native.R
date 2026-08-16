# morie.fn -- function file (rootcoder007/morie)
# N-HiTS: hierarchical interpolation and multi-rate sampling.
#
# N-BEATS spends the same capacity on every frequency in the signal, and
# emits a forecast point for every horizon step from every block. Over a
# long horizon that is both expensive and badly conditioned: the number
# of output parameters grows with the horizon, and high-frequency blocks
# waste them predicting detail that is unpredictable far out.
#
# Two changes, and they are the same idea applied at both ends.
#
# Multi-rate signal sampling. Each block max-pools its input by a
# kernel k_l before reading it. A large kernel leaves only the
# slow components, so that block sees -- and can only fit -- low
# frequencies. Pooling is what makes the block frequency-specific.
#
# Hierarchical interpolation. Each block predicts only
# ceil(r_l H) points, at an expressiveness ratio r_l <= 1, and those
# are interpolated up to the full horizon H. A block with r=1/8 emits
# an eighth as many numbers and stretches them across the horizon,
# which is exactly the right parameterisation for a low-frequency
# component.
#
# The ratios and the pooling must move together, and that is the whole
# design. Pair a large pooling kernel with a small expressiveness ratio
# and the block sees a smooth signal and predicts it with few points --
# coherent. Pair a large kernel with r=1 and the block has full
# output resolution for a signal that has none, which is the waste
# N-HiTS removes. The anchor checks that a large-kernel block really does
# lose the high-frequency component, and that interpolation from a
# handful of knots reconstructs a smooth series but not a jagged one.
#
# Interpolation must be exact at its knots. Whatever the ratio, the
# interpolated output has to pass through the predicted points, or the
# block is not predicting what it appears to be. Checked as an identity.
#
# References
# ----------
# Challu, C., Olivares, K. G., Oreshkin, B. N., Garza, F.,
# Mergenthaler-Canseco, M. & Dubrawski, A. (2023) "NHITS: Neural
# Hierarchical Interpolation for Time Series Forecasting", *Proceedings
# of the AAAI Conference on Artificial Intelligence* 37(6), 6989-6997,
# doi:10.1609/aaai.v37i6.25854, arXiv:2201.12886. Sec. 3: multi-rate
# sampling, hierarchical interpolation, and the expressiveness ratios.
#
# Oreshkin, B. N., Carpov, D., Chapados, N. & Bengio, Y. (2020)
# "N-BEATS: Neural basis expansion analysis for interpretable time series
# forecasting", *International Conference on Learning Representations*,
# arXiv:1905.10437. The doubly residual stack N-HiTS inherits.

#' .nhits_vec
#'
#' A step of the nhits_native implementation. Called by \code{morie_nhits}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
.nhits_vec <- function(y) {
  as.numeric(y)
}

#' .nhits_lstsq
#'
#' A step of the nhits_native implementation. Called by \code{.nhits_nhits_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{crossprod}.
#' @param y A matrix; passed to \code{crossprod}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @return A vector, from \code{as.numeric}.
#' @export
.nhits_lstsq <- function(X, y, ridge = 1e-8) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  XtX <- crossprod(X)
  diag(XtX) <- diag(XtX) + ridge
  Xty <- crossprod(X, y)
  theta <- solve(XtX, Xty)
  as.numeric(theta)
}

#' .nhits_max_pool
#'
#' A step of the nhits_native implementation. Called by \code{.nhits_nhits_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param kernel Coerced to integer by the body, with \code{as.integer}.
#' @param stride Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A vector, from \code{vapply}.
#' @export
.nhits_max_pool <- function(x, kernel, stride = NULL) {
  xv <- as.numeric(x)
  kk <- as.integer(kernel)
  if (kk < 1L) stop(sprintf("nhits: the kernel must be at least 1, got %d", kk))
  st <- if (is.null(stride)) kk else as.integer(stride)
  if (st < 1L) stop("nhits: the stride must be at least 1")
  if (kk > length(xv)) {
    stop(sprintf("nhits: kernel %d exceeds the input length %d", kk, length(xv)))
  }
  starts <- seq(1L, length(xv) - kk + 1L, by = st)
  vapply(starts, function(s) max(xv[s:(s + kk - 1L)]), numeric(1L))
}

#' .nhits_expressiveness_knots
#'
#' A step of the nhits_native implementation. Called by \code{.nhits_nhits_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param horizon Coerced to integer by the body, with \code{as.integer}.
#' @param ratio Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.nhits_expressiveness_knots <- function(horizon, ratio) {
  r <- as.numeric(ratio)
  if (r <= 0 || r > 1) {
    stop(sprintf("nhits: the ratio must be in (0, 1], got %s", format(ratio)))
  }
  H <- as.integer(horizon)
  max(2L, as.integer(ceiling(r * H)))
}

#' .nhits_linear_interpolate
#'
#' A step of the nhits_native implementation. Called by \code{.nhits_nhits_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param knots Coerced to numeric by the body, with \code{as.numeric}.
#' @param horizon Coerced to integer by the body, with \code{as.integer}.
#' @return A numeric value.
#' @export
.nhits_linear_interpolate <- function(knots, horizon) {
  kv <- as.numeric(knots)
  n <- length(kv)
  H <- as.integer(horizon)
  if (n < 2L) stop(sprintf("nhits: need at least 2 knots, got %d", n))
  if (H < 1L) stop("nhits: the horizon must be at least 1")
  if (H == 1L) return(kv[1L])
  h_vals <- 0:(H - 1L)
  pos <- h_vals * (n - 1L) / as.numeric(H - 1L)
  lo <- floor(pos)
  hi <- pmin(lo + 1L, n - 1L)
  w <- pos - lo
  (1 - w) * kv[lo + 1L] + w * kv[hi + 1L]
}

#' .nhits_nhits_block
#'
#' A step of the nhits_native implementation. Called by \code{.nhits_nhits_stack}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param window Coerced to numeric by the body, with \code{as.numeric}.
#' @param horizon Coerced to integer by the body, with \code{as.integer}.
#' @param kernel Passed to \code{.nhits_max_pool}. Defaults to \code{1L}.
#' @param ratio Passed to \code{.nhits_expressiveness_knots}. Defaults to \code{1}.
#' @param degree Numeric; combined arithmetically in the body. Defaults to \code{2L}.
#' @param ridge Passed to \code{.nhits_lstsq}. Defaults to \code{1e-08}.
#' @return A list with \code{backcast}, \code{forecast}, \code{knots}, \code{pooled}.
#' @export
.nhits_nhits_block <- function(window, horizon, kernel = 1L, ratio = 1.0,
                                degree = 2L, ridge = 1e-8) {
  w <- as.numeric(window)
  L <- length(w)
  H <- as.integer(horizon)
  pooled <- .nhits_max_pool(w, kernel)
  Lp <- length(pooled)
  if (Lp < degree + 1L) {
    stop(sprintf("nhits: pooling by %d leaves %d points, too few for degree %d",
                 kernel, Lp, degree))
  }
  t_vals <- 0:(Lp - 1L)
  bb <- lapply(0:as.integer(degree),
               function(p) (t_vals / max(Lp - 1L, 1L))^p)
  X_bb <- do.call(cbind, bb)
  theta <- .nhits_lstsq(X_bb, pooled, ridge)
  back_p <- as.numeric(X_bb %*% theta)
  backcast <- if (Lp >= 2L) {
    .nhits_linear_interpolate(back_p, L)
  } else {
    rep(back_p[1L], L)
  }
  n_knots <- .nhits_expressiveness_knots(H, ratio)
  j_vals <- 0:(n_knots - 1L)
  inner <- (Lp - 1L + (j_vals + 1L) * (Lp - 1L) / max(n_knots, 1L)) /
    max(Lp - 1L, 1L)
  fb <- lapply(0:as.integer(degree), function(p) inner^p)
  X_fb <- do.call(cbind, fb)
  knots <- as.numeric(X_fb %*% theta)
  forecast <- .nhits_linear_interpolate(knots, H)
  list(backcast = backcast, forecast = forecast,
       knots = knots, pooled = pooled)
}

#' .nhits_nhits_stack
#'
#' A step of the nhits_native implementation. Called by \code{morie_nhits}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param window Coerced to numeric by the body, with \code{as.numeric}.
#' @param horizon Passed to \code{.nhits_nhits_block}.
#' @param blocks A vector; its length is taken and its elements indexed.
#' @param ridge Passed to \code{.nhits_nhits_block}. Defaults to \code{1e-08}.
#' @return A list with \code{total}, \code{resid}, \code{trace}.
#' @export
.nhits_nhits_stack <- function(window, horizon, blocks, ridge = 1e-8) {
  resid <- as.numeric(window)
  total <- rep(0.0, as.integer(horizon))
  trace <- list()
  for (i in seq_along(blocks)) {
    kern <- blocks[[i]][1L]
    ratio <- blocks[[i]][2L]
    deg <- blocks[[i]][3L]
    res <- .nhits_nhits_block(resid, horizon, kernel = kern, ratio = ratio,
                              degree = deg, ridge = ridge)
    resid <- resid - res$backcast
    total <- total + res$forecast
    trace[[length(trace) + 1L]] <- list(
      kernel = kern, ratio = ratio,
      n_knots = length(res$knots), knots = res$knots,
      pooled_length = length(res$pooled),
      backcast = res$backcast, forecast = res$forecast,
      residual_norm = sqrt(sum(resid * resid))
    )
  }
  list(total = total, resid = resid, trace = trace)
}

#' morie_nhits
#'
#' A step of the nhits_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.nhits_vec}.
#' @param horizon Coerced to integer by the body, with \code{as.integer}.
#' @param lookback Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param blocks Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param ridge Passed to \code{.nhits_nhits_stack}. Defaults to \code{1e-08}.
#' @return A list with \code{estimate}, \code{forecast}, \code{residual}, \code{blocks}, \code{lookback}, \code{horizon}, \code{n}, \code{total_knots}, \code{dense_parameters}, \code{residual_norm}, \code{n_blocks}, \code{method}.
#' @export
morie_nhits <- function(y, horizon, lookback = NULL, blocks = NULL,
                        ridge = 1e-8) {
  yv <- .nhits_vec(y)
  n <- length(yv)
  H <- as.integer(horizon)
  lb <- if (is.null(lookback)) {
    min(n, max(16L, 4L * H))
  } else {
    min(n, as.integer(lookback))
  }
  if (lb < 8L) {
    stop(sprintf("nhits: lookback of %d is too short", lb))
  }
  blk <- if (is.null(blocks)) {
    list(c(4, 0.25, 2), c(2, 0.5, 2), c(1, 1.0, 2))
  } else {
    blocks
  }
  window <- yv[(n - lb + 1L):n]
  res <- .nhits_nhits_stack(window, H, blk, ridge = ridge)
  list(
    estimate = res$total,
    forecast = res$total,
    residual = res$resid,
    blocks = res$trace,
    lookback = lb,
    horizon = H,
    n = n,
    total_knots = sum(sapply(res$trace, function(b) b$n_knots)),
    dense_parameters = H * length(blk),
    residual_norm = sqrt(sum(res$resid * res$resid)),
    n_blocks = length(blk),
    method = "N-HiTS multi-rate sampling and hierarchical interpolation, Challu et al. (2023)"
  )
}
