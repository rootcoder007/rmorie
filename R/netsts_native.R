# morie.fn -- function file (rootcoder007/morie)
# LSTM time-series forecasting, and what the gates are for.
#
# A plain recurrent network multiplies by the same weight matrix at every
# step, so a gradient travelling back T steps is scaled by that matrix
# T times: it vanishes or explodes. The LSTM's answer is a cell state
# updated **additively**,
#
#   f_t = sigmoid(W_f [h_{t-1}, x_t] + b_f)
#   i_t = sigmoid(W_i [.] + b_i)
#   o_t = sigmoid(W_o [.] + b_o)
#   c_t = f_t * c_{t-1} + i_t * tanh(W_c [.] + b_c)
#   h_t = o_t * tanh(c_t)
#
# so the path from c_{t-1} to c_t is multiplication by f_t alone. With
# the forget gate open the gradient passes essentially unchanged -- the
# constant error carousel. The anchor measures that directly: it
# propagates a signal through 200 steps with the gate open and with it
# closed, and compares.
#
# The forget-gate bias should start positive, and it is not a detail.
# At zero initialisation f ~ 0.5, so the cell halves at every step and
# memory is gone in a dozen steps before training begins. Initialising
# b_f to 1 or 2 opens the gate, and the anchor measures the retention
# horizon under each.
#
# Forecasting recursively compounds error, and that is the honest
# comparison. Feeding a one-step model its own prediction accumulates
# error at every step; a direct model trained per horizon does not, at
# the cost of one model per step. Both are here, and the anchor measures
# the gap growing with horizon rather than stating a preference.
#
# Scaling matters more than architecture at this size. tanh saturates
# outside roughly [-2, 2], so an unscaled series with values in the
# hundreds puts every gate hard against its rails and the network stops
# learning. Standardisation is applied and inverted around the forecast.
#
# References
# ----------
# Hochreiter, S. & Schmidhuber, J. (1997) "Long Short-Term Memory",
# Neural Computation 9(8), 1735-1780, doi:10.1162/neco.1997.9.8.1735.
# The cell, and the constant error carousel argument.
#
# Gers, F. A., Schmidhuber, J. & Cummins, F. (2000) "Learning to Forget:
# Continual Prediction with LSTM", Neural Computation 12(10),
# 2451-2471, doi:10.1162/089976600300015015. The forget gate itself.
#
# Jozefowicz, R., Zaremba, W. & Sutskever, I. (2015) "An Empirical
# Exploration of Recurrent Network Architectures", Proceedings of the
# 32nd International Conference on Machine Learning, PMLR 37,
# 2342-2350. The positive forget-gate bias initialisation.
#
# Hewamalage, H., Bergmeir, C. & Bandara, K. (2021) "Recurrent Neural
# Networks for Time Series Forecasting: Current status and future
# directions", International Journal of Forecasting 37(1), 388-427,
# doi:10.1016/j.ijforecast.2020.06.008. Recursive versus direct
# strategies, and preprocessing.

#' .netsts_sigmoid
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
.netsts_sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

#' .netsts_sd
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @return A numeric value.
#' @export
.netsts_sd <- function(y) {
  n <- length(y)
  if (n < 2) return(0)
  m <- mean(y)
  sqrt(sum((y - m)^2) / (n - 1))
}

#' .netsts_vec
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.netsts_vec <- function(y) {
  as.numeric(y)
}

#' .netsts_lstsq
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param ridge See Usage.
#' @return The value of \code{beta}, as built in the body.
#' @export
.netsts_lstsq <- function(X, y, ridge) {
  X <- if (is.list(X)) do.call(rbind, X) else as.matrix(X)
  y <- as.numeric(y)
  p <- ncol(X)
  XtX <- crossprod(X)
  diag(XtX) <- diag(XtX) + ridge
  Xty <- crossprod(X, y)
  beta <- solve(XtX, Xty)
  beta
}

#' .netsts_lstm_cell
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param c See Usage.
#' @param W See Usage.
#' @param b See Usage.
#' @param forget_bias Defaults to \code{0}.
#' @return A list with \code{h}, \code{c}, \code{gates}.
#' @export
.netsts_lstm_cell <- function(x, h, c, W, b, forget_bias = 0.0) {
  d <- length(h)
  if (length(c) != d) stop("netsts: hidden and cell sizes differ")
  inp <- c(as.numeric(x), h)
  if (nrow(W) != length(inp))
    stop("netsts: W has wrong number of rows for the given input")
  if (length(b) != 4 * d)
    stop("netsts: bias needs 4*hidden entries")
  z <- numeric(4 * d)
  for (j in seq_len(4 * d)) {
    z[j] <- sum(inp * W[, j]) + b[j]
  }
  i_g <- vapply(seq_len(d), function(j) .netsts_sigmoid(z[j]), numeric(1))
  f_g <- vapply(seq_len(d), function(j)
    .netsts_sigmoid(z[d + j] + forget_bias), numeric(1))
  g_g <- vapply(seq_len(d), function(j) tanh(z[2 * d + j]), numeric(1))
  o_g <- vapply(seq_len(d), function(j) .netsts_sigmoid(z[3 * d + j]), numeric(1))
  cn <- f_g * c + i_g * g_g
  hn <- o_g * tanh(cn)
  list(h = hn, c = cn, gates = list(i = i_g, f = f_g, g = g_g, o = o_g))
}

#' .netsts_lstm_run
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param W See Usage.
#' @param b See Usage.
#' @param hidden See Usage.
#' @param forget_bias Defaults to \code{0}.
#' @return A list with \code{hs}, \code{cs}, \code{gates}.
#' @export
.netsts_lstm_run <- function(X, W, b, hidden, forget_bias = 0.0) {
  d <- as.integer(hidden)
  h <- rep(0.0, d)
  c <- rep(0.0, d)
  n_steps <- length(X)
  hs <- vector("list", n_steps)
  cs <- vector("list", n_steps)
  gates <- vector("list", n_steps)
  for (i in seq_len(n_steps)) {
    result <- .netsts_lstm_cell(X[[i]], h, c, W, b, forget_bias)
    h <- result$h
    c <- result$c
    hs[[i]] <- h
    cs[[i]] <- c
    gates[[i]] <- result$gates
  }
  list(hs = hs, cs = cs, gates = gates)
}

#' .netsts_gradient_retention
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param forget_value See Usage.
#' @param steps See Usage.
#' @return A numeric value.
#' @export
.netsts_gradient_retention <- function(forget_value, steps) {
  f <- as.numeric(forget_value)
  if (f < 0 || f > 1)
    stop("netsts: the forget value must be in [0, 1]")
  f ^ as.integer(steps)
}

#' .netsts_standardize
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @return A list with \code{z}, \code{mu}, \code{sd}.
#' @export
.netsts_standardize <- function(y) {
  yv <- as.numeric(y)
  mu <- mean(yv)
  sd <- .netsts_sd(yv)
  if (sd < 1e-12) sd <- 1.0
  list(z = (yv - mu) / sd, mu = mu, sd = sd)
}

#' Standard normals via Box-Muller on pairs of uniforms from the
#'
#' glibc LCG-backed helper. Two uniforms per sample.
#'
#' @param rng See Usage.
#' @param n See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.netsts_standard_normal <- function(rng, n) {
  # Standard normals via Box-Muller on pairs of uniforms from the
  # glibc LCG-backed helper. Two uniforms per sample.
  u <- .ghc_unif(rng, 2 * n)
  out <- numeric(n)
  for (i in seq_len(n)) {
    u1 <- u[2 * i - 1]
    u2 <- u[2 * i]
    if (u1 < 1e-12) u1 <- 1e-12
    if (u1 > 1 - 1e-12) u1 <- 1 - 1e-12
    out[i] <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  }
  out
}

#' morie_netsts
#'
#' Part of the netsts_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param horizon See Usage.
#' @param hidden Defaults to \code{8}.
#' @param n_lags Defaults to \code{4}.
#' @param strategy Defaults to \code{"recursive"}.
#' @param forget_bias Defaults to \code{1}.
#' @param seed Defaults to \code{0}.
#' @param ridge Defaults to \code{1e-06}.
#' @return A list with \code{estimate}, \code{forecast}, \code{strategy}, \code{hidden}, \code{n_lags}, \code{forget_bias}, \code{mean}, \code{sd}, \code{n_models}, \code{retention_10}, \code{method}.
#' @export
morie_netsts <- function(y, horizon, hidden = 8, n_lags = 4,
                         strategy = "recursive", forget_bias = 1.0,
                         seed = 0, ridge = 1e-6) {
  if (!(strategy %in% c("recursive", "direct")))
    stop("netsts: strategy must be recursive or direct")

  yv <- .netsts_vec(y)
  n <- length(yv)
  H <- as.integer(horizon)
  p <- as.integer(n_lags)
  if (n < p + H + 4)
    stop(sprintf("netsts: %d observations is too few for %d lags and a horizon of %d",
                 n, p, H))

  std <- .netsts_standardize(yv)
  zs <- std$z
  mu <- std$mu
  sd <- std$sd
  d <- as.integer(hidden)

  rng <- .ghc_rng(seed)
  n_W <- (1 + d) * 4 * d
  w_vec <- .netsts_standard_normal(rng, n_W) * 0.3
  W <- matrix(w_vec, nrow = 1 + d, ncol = 4 * d, byrow = TRUE)
  b <- rep(0.0, 4 * d)

  .netsts_features <- function(seq) {
    X <- as.list(seq)
    res <- .netsts_lstm_run(X, W, b, d, forget_bias)
    res$hs[[length(res$hs)]]
  }

  if (strategy == "recursive") {
    n_samples <- n - p
    Xf <- vector("list", n_samples)
    yf <- numeric(n_samples)
    for (t in (p + 1):n) {
      f <- .netsts_features(zs[(t - p):(t - 1)])
      Xf[[t - p]] <- c(1.0, f)
      yf[t - p] <- zs[t]
    }
    beta <- .netsts_lstsq(Xf, yf, ridge)
    st <- zs
    out <- numeric(H)
    for (hh in seq_len(H)) {
      f <- c(1.0, .netsts_features(st[(length(st) - p + 1):length(st)]))
      nxt <- sum(f * beta)
      st <- c(st, nxt)
      out[hh] <- nxt
    }
    betas <- list(beta)
  } else {
    out <- numeric(H)
    betas <- vector("list", H)
    for (hstep in seq_len(H)) {
      n_samples <- n - p - hstep + 1
      Xf <- vector("list", n_samples)
      yf <- numeric(n_samples)
      for (t in (p + 1):(n - hstep + 1)) {
        f <- .netsts_features(zs[(t - p):(t - 1)])
        Xf[[t - p]] <- c(1.0, f)
        yf[t - p] <- zs[t + hstep - 1]
      }
      bh <- .netsts_lstsq(Xf, yf, ridge)
      betas[[hstep]] <- bh
      f <- c(1.0, .netsts_features(zs[(n - p + 1):n]))
      out[hstep] <- sum(f * bh)
    }
  }

  fc <- out * sd + mu

  list(
    estimate = fc, forecast = fc, strategy = strategy,
    hidden = d, n_lags = p, forget_bias = as.numeric(forget_bias),
    mean = mu, sd = sd, n_models = length(betas),
    retention_10 = .netsts_gradient_retention(
      .netsts_sigmoid(forget_bias), 10),
    method = "LSTM forecaster, Hochreiter & Schmidhuber (1997) cell with Gers, Schmidhuber & Cummins (2000) forget gate"
  )
}
