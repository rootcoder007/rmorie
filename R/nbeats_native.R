# nbeats -- doubly residual stacking for forecasting
# Reference: Oreshkin et al. (2020) "N-BEATS" arXiv:1905.10437
# Base R only.

nbeats_trend_basis <- function(length, degree, offset = 0, scale = NULL) {
  if (degree < 0L) stop(sprintf("nbeats: degree must be non-negative, got %d", degree))
  sc <- if (is.null(scale)) as.numeric(length) else as.numeric(scale)
  t_seq <- (seq_len(length) - 1L)
  # rows are basis functions, columns are time -- P x L, matching the
  # Python arm and nbeats_seasonality_basis. sapply() alone returns the
  # transpose, which left the two bases in this file disagreeing and
  # made the seasonality block non-conformable.
  t(sapply(seq_len(degree + 1L) - 1L,
           function(p) ((offset + t_seq) / sc)^p))
}

nbeats_seasonality_basis <- function(length, harmonics, offset = 0, period = NULL) {
  if (harmonics < 1L) stop(sprintf("nbeats: need at least 1 harmonic, got %d", harmonics))
  per <- if (is.null(period)) as.numeric(length) else as.numeric(period)
  t_seq <- (seq_len(length) - 1L)
  rows <- list()
  for (h in seq_len(harmonics)) {
    rows[[length(rows) + 1L]] <- cos(2 * pi * h * (offset + t_seq) / per)
    rows[[length(rows) + 1L]] <- sin(2 * pi * h * (offset + t_seq) / per)
  }
  do.call(rbind, rows)
}

nbeats_lstsq <- function(X, y, ridge = 1e-8) {
  # X is L x P, y is length L; return theta
  p <- ncol(X)
  XtX <- crossprod(X)
  if (ridge > 0) diag(XtX) <- diag(XtX) + ridge
  Xty <- crossprod(X, y)
  as.numeric(solve(XtX, Xty))
}

nbeats_block <- function(window, horizon, kind = "generic", degree = 2,
                         harmonics = 3, ridge = 1e-8) {
  if (!(kind %in% c("generic", "trend", "seasonality"))) {
    stop(sprintf("nbeats: kind must be generic, trend or seasonality, got %s", kind))
  }
  L <- length(window)
  H <- as.integer(horizon)
  if (H < 1L) stop(sprintf("nbeats: horizon must be at least 1, got %d", H))
  if (kind == "trend") {
    bb <- nbeats_trend_basis(L, degree, scale = L)
    fb <- nbeats_trend_basis(H, degree, offset = L, scale = L)
  } else if (kind == "seasonality") {
    bb <- nbeats_seasonality_basis(L, harmonics, period = L)
    fb <- nbeats_seasonality_basis(H, harmonics, offset = L, period = L)
  } else {
    bb <- diag(1, nrow = L, ncol = L)
    fb <- matrix(1 / max(L, 1), nrow = L, ncol = H)
  }
  # bases are P x L, so the design matrix is t(bb) (L x P) and both
  # casts are evaluated as t(basis) %*% theta
  theta <- nbeats_lstsq(t(bb), window, ridge)
  backcast <- as.numeric(t(bb) %*% theta)
  forecast <- as.numeric(t(fb) %*% theta)
  list(backcast = backcast, forecast = forecast, theta = theta)
}

nbeats_stack <- function(window, horizon, blocks, ridge = 1e-8) {
  resid <- as.numeric(window)
  total <- rep(0, as.integer(horizon))
  trace <- list()
  for (b in blocks) {
    kind <- b[[1]]; deg <- as.integer(b[[2]]); harm <- as.integer(b[[3]])
    res <- nbeats_block(resid, horizon, kind = kind, degree = deg,
                        harmonics = harm, ridge = ridge)
    resid <- resid - res$backcast
    total <- total + res$forecast
    trace[[length(trace) + 1L]] <- list(kind = kind, backcast = res$backcast,
                                        forecast = res$forecast, theta = res$theta,
                                        residual_norm = sqrt(sum(resid * resid)))
  }
  list(forecast = total, residual = resid, trace = trace)
}

nbeats_forecast <- function(y, horizon, lookback = NULL, blocks = NULL, ridge = 1e-8) {
  yv <- as.numeric(y)
  n <- length(yv)
  H <- as.integer(horizon)
  lb <- min(n, if (is.null(lookback)) min(n, max(8L, 3L * H)) else as.integer(lookback))
  if (lb < 4L) stop(sprintf("nbeats: lookback of %d is too short", lb))
  if (n < lb) stop(sprintf("nbeats: %d observations for a lookback of %d", n, lb))
  blk <- if (is.null(blocks)) list(c("trend", 2, 3), c("seasonality", 2, 3), c("trend", 1, 3)) else blocks
  window <- yv[(n - lb + 1L):n]
  res <- nbeats_stack(window, H, blk, ridge = ridge)
  explained <- window - res$residual
  list(estimate = res$forecast, forecast = res$forecast,
       residual = res$residual, backcast = explained, blocks = res$trace,
       lookback = lb, horizon = H, n = n,
       residual_norm = sqrt(sum(res$residual^2)),
       window_norm = sqrt(sum(window^2)),
       n_blocks = length(blk),
       method = "N-BEATS doubly residual stacking, Oreshkin, Carpov, Chapados & Bengio (2020)")
}

nbeats_cheatsheet <- function() {
  paste("nbeats: each block emits a BACKCAST and a forecast from one theta. Residual in: x_l = x_{l-1} - xhat_{l-1}; forecasts out: yhat = sum_l yhat_l. The residual telescopes exactly, so block l only ever sees what its predecessors could not explain -- skip the subtraction and every block re-fits the same trend. Trend and seasonality blocks CONSTRAIN the basis (polynomial, Fourier); that is where interpretability comes from.")
}

# house entry point: the package exports one morie_<module>
morie_nbeats <- nbeats_forecast
