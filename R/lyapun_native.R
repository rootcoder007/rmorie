# Largest Lyapunov exponent from a scalar time series.
# Sources: Rosenstein, M. T., Collins, J. J., & De Luca, C. J. (1993)
# "A practical method for calculating largest Lyapunov exponents from
# small data sets", *Physica D* 65(1-2), 117-134.
#
# The attractor is reconstructed by delay embedding (the paper's eq. 5),
# each point's nearest neighbour is found subject to a temporal separation
# larger than the mean period (eqs. 7 and 8), and the neighbours are then
# assumed to separate at the rate of the largest exponent,
# d_j(i) ~= C_j exp(lambda_1 * i * dt) (eq. 11). Taking logs (eq. 12)
# turns that into a family of roughly parallel lines, and the exponent
# is the slope of their average (eq. 13),
#
#   y(i) = (1/dt) * <ln d_j(i)>_j.
#
# The averaging over j is what makes the estimate work on short series,
# and the paper's point about C_j is that no normalisation by d_j(0) is
# needed, since a constant offset does not change a slope.
#
# Three routes are available, all printed in the paper, and all
# reachable through `method`:
#   "rosenstein" (default) - eq. 13, least squares on y(i) over the
#     initial rise.
#   "sato" - Sato et al.'s eq. 9, read at the end of the fitting window.
#   "sato_k" - Sato et al.'s eq. 10, lambda_1 read off the plateau in i.
#
# Table 1 of the paper gives the expected exponents this module is
# anchored against: 0.693 for the logistic map at mu = 4 and 0.418 for
# the Henon map at a = 1.4, b = 0.3.

#' .lyapun_as_series
#'
#' A step of the lyapun_native implementation. Called by \code{autocorrelation_lag}, \code{divergence_curve}, \code{mean_period} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.lyapun_as_series <- function(y) {
  if (is.matrix(y) || is.list(y)) {
    out <- as.numeric(unlist(y))
  } else {
    out <- as.numeric(y)
  }
  if (length(out) < 10L)
    stop("lyapun: need at least 10 observations, got ", length(out))
  if (any(!is.finite(out)))
    stop("lyapun: the series contains a non-finite value")
  out
}

#' morie_lyapun_embed
#'
#' A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{morie_lyapun}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param m A count; the body uses it as \code{seq_len(...)}.
#' @param tau Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_lyapun_embed <- function(y, m, tau) {
  y <- .lyapun_as_series(y)
  m <- as.integer(m); tau <- as.integer(tau)
  if (m < 1L)
    stop("lyapun: the embedding dimension must be >= 1")
  if (tau < 1L)
    stop("lyapun: the reconstruction delay must be >= 1")
  n_pts <- length(y) - (m - 1L) * tau
  if (n_pts < 3L)
    stop("lyapun: m = ", m, " and J = ", tau,
         " leave only ", n_pts, " reconstructed points")
  out <- vector("list", n_pts)
  for (j in seq_len(n_pts)) {
    row <- numeric(m)
    for (k in seq_len(m)) row[k] <- y[j + (k - 1L) * tau]
    out[[j]] <- row
  }
  out
}

#' autocorrelation_lag
#'
#' A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{morie_lyapun}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param threshold Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
autocorrelation_lag <- function(y, threshold = NULL) {
  y <- .lyapun_as_series(y)
  n <- length(y)
  if (is.null(threshold)) threshold <- 1.0 - 1.0 / exp(1)
  mu <- sum(y) / n
  c0 <- sum((y - mu)^2) / n
  if (c0 <= 0)
    stop("lyapun: the series is constant, so no delay can be chosen from its autocorrelation")
  for (lag in seq_len(n - 1L)) {
    c <- 0
    for (t in seq_len(n - lag)) c <- c + (y[t] - mu) * (y[t + lag] - mu)
    c <- c / n
    if (c / c0 <= threshold) return(lag)
  }
  1L
}

#' mean_period
#'
#' A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{morie_lyapun}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param dt Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
mean_period <- function(y, dt = 1.0) {
  y <- .lyapun_as_series(y)
  n <- length(y)
  mu <- sum(y) / n
  centred <- y - mu
  spec <- fft(centred)
  half <- n %/% 2 + 1L
  re <- Re(spec[seq_len(half)])
  im <- Im(spec[seq_len(half)])
  power <- re * re + im * im
  freqs <- (seq_len(half) - 1L) / (n * dt)
  if (half < 2L) return(1.0)
  wsum <- sum(power[2:half])
  if (wsum <= 0) return(1.0)
  num <- 0
  for (k in 2:half) num <- num + freqs[k] * power[k]
  f_mean <- num / wsum
  if (f_mean <= 0) return(as.numeric(n))
  (1.0 / f_mean) / dt
}

#' .lyapun_nearest_neighbours
#'
#' A step of the lyapun_native implementation. Called by \code{divergence_curve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pts A vector; its length is taken and its elements indexed.
#' @param min_sep See Usage.
#' @return A list with \code{nn}, \code{d0}.
#' @export
.lyapun_nearest_neighbours <- function(pts, min_sep) {
  n_pts <- length(pts)
  m <- length(pts[[1L]])
  nn <- rep(-1L, n_pts)
  d0 <- rep(0.0, n_pts)
  for (j in seq_len(n_pts)) {
    best <- -1L; best_d <- Inf
    pj <- pts[[j]]
    for (jp in seq_len(n_pts)) {
      if (abs(j - jp) <= min_sep) next
      pk <- pts[[jp]]
      s <- 0.0
      aborted <- FALSE
      for (k in seq_len(m)) {
        diff <- pj[k] - pk[k]
        s <- s + diff * diff
        if (s >= best_d) { aborted <- TRUE; break }
      }
      if (!aborted && s < best_d) { best_d <- s; best <- jp }
    }
    nn[j] <- best
    d0[j] <- if (best >= 0L) sqrt(best_d) else NaN
  }
  list(nn = nn, d0 = d0)
}

#' .lyapun_distance
#'
#' A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{lyapunov_exponent}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pts A vector; indexed elementwise.
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.lyapun_distance <- function(pts, a, b) {
  pa <- pts[[a]]; pb <- pts[[b]]
  s <- 0.0
  for (k in seq_along(pa)) { diff <- pa[k] - pb[k]; s <- s + diff * diff }
  sqrt(s)
}

#' divergence_curve
#'
#' A step of the lyapun_native implementation. Called by \code{lyapunov_exponent}, \code{morie_lyapun}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param m Optional; may be \code{NULL}. Passed to \code{morie_lyapun_embed}.
#' @param tau Optional; may be \code{NULL}. Passed to \code{morie_lyapun_embed}.
#' @param dt Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param min_sep Optional; may be \code{NULL}. Passed to \code{.lyapun_nearest_neighbours}.
#' @param max_steps Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{time}, \code{log_divergence}, \code{log_ratio}, \code{n_pairs}, \code{neighbour}, \code{d0}, \code{points}, \code{m}, \code{tau}, \code{min_sep}, \code{n_points}, \code{n_obs}.
#' @export
divergence_curve <- function(y, m = NULL, tau = NULL, dt = 1.0,
                             min_sep = NULL, max_steps = NULL) {
  y <- .lyapun_as_series(y)
  n <- length(y)
  if (is.null(tau)) tau <- autocorrelation_lag(y)
  if (is.null(m)) m <- 3L
  if (dt <= 0)
    stop("lyapun: the sampling period must be positive")
  pts <- morie_lyapun_embed(y, m, tau)
  n_pts <- length(pts)
  if (is.null(min_sep))
    min_sep <- as.integer(round(mean_period(y, dt)))
  min_sep <- as.integer(min_sep)
  if (min_sep < 0L)
    stop("lyapun: min_sep must be >= 0")
  if (min_sep >= n_pts - 2L)
    stop("lyapun: the mean period (", min_sep,
         " samples) leaves no admissible neighbours among ",
         n_pts, " reconstructed points; pass min_sep explicitly")
  nn_d0 <- .lyapun_nearest_neighbours(pts, min_sep)
  nn <- nn_d0$nn; d0 <- nn_d0$d0
  usable <- which(nn >= 0L & d0 > 0)
  if (length(usable) < 3L)
    stop("lyapun: fewer than three usable neighbour pairs")
  if (is.null(max_steps)) max_steps <- max(1L, n_pts %/% 4L)
  max_steps <- as.integer(max_steps)

  times <- numeric(0); curve <- numeric(0); counts <- integer(0)
  ratio <- numeric(0)
  for (i in seq_len(max_steps + 1L) - 1L) {
    tot <- 0.0; tot_ratio <- 0.0; cnt <- 0L
    for (j in usable) {
      jp <- nn[j]
      if ((j + i) > n_pts || (jp + i) > n_pts) next
      d <- .lyapun_distance(pts, j + i, jp + i)
      if (d <= 0) next
      tot <- tot + log(d)
      tot_ratio <- tot_ratio + log(d / d0[j])
      cnt <- cnt + 1L
    }
    if (cnt == 0L) break
    times <- c(times, i * dt)
    curve <- c(curve, tot / cnt)
    ratio <- c(ratio, tot_ratio / cnt)
    counts <- c(counts, cnt)
  }
  list(time = times, log_divergence = curve, log_ratio = ratio,
       n_pairs = counts, neighbour = nn, d0 = d0, points = pts,
       m = m, tau = tau, min_sep = min_sep, n_points = n_pts, n_obs = n)
}

#' .lyapun_linear_region
#'
#' A step of the lyapun_native implementation. Called by \code{lyapunov_exponent}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param curve A vector; its length is taken and its elements indexed.
#' @param lo_frac Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param hi_frac Numeric; combined arithmetically in the body. Defaults to \code{0.8}.
#' @return A list with \code{lo}, \code{hi}.
#' @export
.lyapun_linear_region <- function(curve, lo_frac = 0.1, hi_frac = 0.8) {
  n <- length(curve)
  if (n < 4L) return(list(lo = 0L, hi = n))
  c_lo <- min(curve); c_hi <- max(curve)
  span <- c_hi - c_lo
  if (span <= 0) return(list(lo = 0L, hi = n))
  top <- which.max(curve)
  if (top < 4L) return(list(lo = 0L, hi = n))
  lo_level <- c_lo + lo_frac * span
  hi_level <- c_lo + hi_frac * span
  lo <- 1L
  while (lo < top && curve[lo] < lo_level) lo <- lo + 1L
  hi <- lo
  while (hi < top && curve[hi] < hi_level) hi <- hi + 1L
  hi <- min(hi + 1L, n)
  if (hi - lo < 3L) return(list(lo = 0L, hi = n))
  list(lo = lo, hi = hi)
}

#' .lyapun_ols_slope
#'
#' A step of the lyapun_native implementation. Called by \code{lyapunov_exponent}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param xs A vector; its length is taken and its elements indexed.
#' @param ys A vector; indexed elementwise.
#' @return A list with \code{slope}, \code{intercept}, \code{se}, \code{r2}.
#' @export
.lyapun_ols_slope <- function(xs, ys) {
  n <- length(xs)
  mx <- sum(xs) / n
  my <- sum(ys) / n
  sxx <- sum((xs - mx)^2)
  if (sxx <= 0)
    stop("lyapun: the fitting window has no spread in time")
  sxy <- 0
  for (k in seq_len(n)) sxy <- sxy + (xs[k] - mx) * (ys[k] - my)
  slope <- sxy / sxx
  intercept <- my - slope * mx
  resid <- ys - intercept - slope * xs
  sse <- sum(resid^2)
  sst <- sum((ys - my)^2)
  se <- if (n > 2L && sse > 0) sqrt(sse / (n - 2L) / sxx) else 0
  r2 <- if (sst > 0) 1 - sse / sst else 1
  list(slope = slope, intercept = intercept, se = se, r2 = r2)
}

#' lyapunov_exponent
#'
#' A step of the lyapun_native implementation. Called by \code{morie_lyapun}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param embedding Defaults to \code{NULL}.
#' @param tau Defaults to \code{NULL}.
#' @param dt Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param fit Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param min_sep Defaults to \code{NULL}.
#' @param max_steps Defaults to \code{NULL}.
#' @param method One of \code{"rosenstein"}, \code{"sato"}, \code{"sato_k"}. Defaults to \code{"rosenstein"}.
#' @param k Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{estimate}, \code{lambda1}, \code{rosenstein}, \code{sato}, \code{sato_k}, \code{sato_k_curve}, \code{se}, \code{r_squared}, \code{intercept}, \code{time}, \code{log_divergence}, \code{log_ratio}, \code{n_pairs}, \code{fit_range}, \code{k}, \code{m}, \code{tau}, \code{min_sep}, \code{n_points}, \code{n}, \code{dt}, \code{method}, \code{note}.
#' @export
lyapunov_exponent <- function(y, embedding = NULL, tau = NULL, dt = 1.0,
                              fit = NULL, min_sep = NULL,
                              max_steps = NULL, method = "rosenstein",
                              k = NULL) {
  if (!(method %in% c("rosenstein", "sato", "sato_k")))
    stop("lyapun: method must be 'rosenstein', 'sato' or 'sato_k'")
  dv <- divergence_curve(y, m = embedding, tau = tau, dt = dt,
                         min_sep = min_sep, max_steps = max_steps)
  times <- dv$time
  curve <- dv$log_divergence
  n_steps <- length(curve)
  if (is.null(fit)) {
    lr <- .lyapun_linear_region(curve); lo <- lr$lo; hi <- lr$hi
  } else {
    lo <- as.integer(fit[1L]); hi <- as.integer(fit[2L])
    if (lo < 0L || hi > n_steps || hi - lo < 2L)
      stop("lyapun: the fitting window must lie inside 0..", n_steps,
           " and span at least two steps")
  }
  ols <- .lyapun_ols_slope(times[lo:hi], curve[lo:hi])
  slope <- ols$slope; intercept <- ols$intercept
  se <- ols$se; r2 <- ols$r2

  i_end <- hi - 1L
  sato <- if (i_end > 0L) dv$log_ratio[i_end] / (i_end * dt) else NaN

  if (is.null(k)) k <- max(1L, hi - lo)
  k <- as.integer(k)
  sato_k <- NaN; sato_k_curve <- numeric(0)
  if (k >= 1L && n_steps > k) {
    pts <- dv$points
    nn <- dv$neighbour; n_pts <- dv$n_points
    d0 <- dv$d0
    usable <- which(nn >= 0L & d0 > 0)
    for (i in seq_len(n_steps - k) - 1L) {
      tot <- 0.0; cnt <- 0L
      for (j in usable) {
        jp <- nn[j]
        if (max(j, jp) + i + k > n_pts) next
        d_i <- .lyapun_distance(pts, j + i, jp + i)
        d_ik <- .lyapun_distance(pts, j + i + k, jp + i + k)
        if (d_i <= 0 || d_ik <= 0) next
        tot <- tot + log(d_ik / d_i)
        cnt <- cnt + 1L
      }
      if (cnt == 0L) break
      sato_k_curve <- c(sato_k_curve, tot / cnt / (k * dt))
    }
    if (length(sato_k_curve) > 0L) {
      cap <- max(3L, min(hi, length(sato_k_curve)))
      search <- sato_k_curve[seq_len(cap)]
      w <- max(2L, length(search) %/% 4L)
      best <- 1L; best_var <- Inf
      for (s in seq_len(length(search) - w + 1L) - 1L) {
        seg <- search[(s + 1L):(s + w)]
        mu <- sum(seg) / w
        var <- sum((seg - mu)^2) / w
        if (var < best_var) { best_var <- var; best <- s + 1L }
      }
      seg <- search[best:(best + w - 1L)]
      sato_k <- sum(seg) / length(seg)
    }
  }

  estimate <- switch(method,
                     "rosenstein" = slope,
                     "sato" = sato,
                     "sato_k" = sato_k)
  list(estimate = estimate, lambda1 = estimate,
       rosenstein = slope, sato = sato, sato_k = sato_k,
       sato_k_curve = sato_k_curve,
       se = se, r_squared = r2, intercept = intercept,
       time = times, log_divergence = curve,
       log_ratio = dv$log_ratio, n_pairs = dv$n_pairs,
       fit_range = c(lo, hi), k = k,
       m = dv$m, tau = dv$tau, min_sep = dv$min_sep,
       n_points = dv$n_points, n = dv$n_obs, dt = dt,
       method = paste0("largest Lyapunov exponent, Rosenstein, Collins & De Luca (1993), route '", method, "'"),
       note = "the exponent is the slope of <ln d_j(i)> over the initial rise; a positive value indicates chaos, and the fitting window is the caller's to choose because the curve saturates once the neighbours are as far apart as the attractor allows")
}

largest_lyapunov <- lyapunov_exponent

#' .lyapun_cheatsheet
#'
#' A step of the lyapun_native implementation. Called by \code{morie_lyapun}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.lyapun_cheatsheet <- function() {
  paste("lyapun: largest Lyapunov exponent (Rosenstein, Collins & De ",
        "Luca 1993). Embed with delay J and dimension m, find each ",
        "point's nearest neighbour at least a mean period away, and ",
        "take lambda_1 as the slope of <ln d_j(i)> against i*dt over ",
        "the initial rise -- no normalisation by d_j(0) is needed, ",
        "since a constant offset does not change a slope. Expected ",
        "values from the paper's table 1: 0.693 for the logistic map ",
        "at mu = 4, 0.418 for the Henon map. Routes: 'rosenstein' ",
        "(eq. 13, default), 'sato' (eq. 9), 'sato_k' (eq. 10, whose ",
        "plateau the paper itself calls unreliable).", sep = "")
}

#' morie_lyapun
#'
#' A step of the lyapun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param op A vector; its length is taken.
#' @param ... Passed through.
#' @return The value of \code{switch}.
#' @export
morie_lyapun <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("lyapun: op must be one of lyapunov_exponent, morie_lyapun_embed, autocorrelation_lag, mean_period, divergence_curve, cheatsheet")
  op <- as.character(op)
  switch(op,
    "lyapunov_exponent" = lyapunov_exponent(...),
    "largest_lyapunov" = lyapunov_exponent(...),
    "morie_lyapun_embed" = morie_lyapun_embed(...),
    "autocorrelation_lag" = autocorrelation_lag(...),
    "mean_period" = mean_period(...),
    "divergence_curve" = divergence_curve(...),
    "cheatsheet" = list(cheatsheet = .lyapun_cheatsheet()),
    stop("lyapun: unknown op ", shQuote(op))
  )
}
