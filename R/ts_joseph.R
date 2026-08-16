# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Modern time-series forecasting shelf -- R mirror of
# morie/fn/_joseph.py and the thirty-seven shelf modules.
#
# Spec: Joseph, M. and Tackes, J. (2024), Modern Time Series
# Forecasting with Python, 2nd ed., Packt. Locators are the printed
# page numbers of that edition.
#
# Sourcing rule: where the book prints the formula, the formula is
# quoted and the page given. Where the book only NAMES a method and
# cites its paper -- which is every deep architecture here -- the
# equations come from the paper itself, quoted with the paper's own
# equation numbers and arXiv id. Nothing is reconstructed from memory.
#
# Collision scan: ts_joseph.R and all thirty-seven exported names were
# free in both R trees. morie_mape, morie_diffs, morie_nhits and
# morie_tft were already taken, hence morie_mapets, morie_diffser,
# morie_nhitsnet and morie_tftnet.
#
# Determinism: every learned weight is CALLER-SUPPLIED. The linear
# solver below is a hand-written Gaussian elimination that mirrors
# morie/fn/_joseph._solve step for step -- base R's solve() uses
# LAPACK and would drift from the Python arm in the last few bits,
# which a 1e-9 parity gate on forecasts of order 30 would catch.

#' .morie_jo_vec
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_pair}, \code{morie_adfur}, \code{morie_autocorf} and 23 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param name Defaults to \code{"x"}.
#' @return The value of \code{v}, as built in the body.
#' @export
.morie_jo_vec <- function(x, name = "x") {
  v <- as.numeric(x)
  if (length(v) == 0L) stop(sprintf("%s must be non-empty.", name), call. = FALSE)
  v
}

#' .morie_jo_pair
#'
#' A step of the ts_joseph implementation. Called by \code{morie_mapets}, \code{morie_pinball}, \code{morie_relmae} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.morie_jo_vec}.
#' @param yhat Passed to \code{.morie_jo_vec}.
#' @return A list with \code{a}, \code{b}.
#' @export
.morie_jo_pair <- function(y, yhat) {
  a <- .morie_jo_vec(y, "y")
  b <- .morie_jo_vec(yhat, "yhat")
  if (length(a) != length(b)) {
    stop("y and yhat must be the same length.", call. = FALSE)
  }
  list(a = a, b = b)
}

#' .morie_jo_med
#'
#' A step of the ts_joseph implementation. Called by \code{morie_mapets}, \code{morie_smape}, \code{morie_stldecomp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_jo_med <- function(v) {
  s <- sort(v)
  n <- length(s)
  mid <- n %/% 2L
  if (n %% 2L == 1L) s[mid + 1L] else 0.5 * (s[mid] + s[mid + 1L])
}

#' .morie_jo_solve
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_ols}, \code{morie_adfur}, \code{morie_quantreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a See Usage.
#' @param b A vector; its length is taken.
#' @return A vector, from \code{vapply}.
#' @export
.morie_jo_solve <- function(a, b) {
  n <- length(b)
  m <- cbind(matrix(as.numeric(unlist(a)), n, n, byrow = TRUE), as.numeric(b))
  for (cc in seq_len(n)) {
    col <- abs(m[cc:n, cc])
    piv <- cc + which.max(col) - 1L
    if (abs(m[piv, cc]) < 1e-300) stop("singular system.", call. = FALSE)
    if (piv != cc) {
      tmp <- m[cc, ]; m[cc, ] <- m[piv, ]; m[piv, ] <- tmp
    }
    pv <- m[cc, cc]
    for (r in seq_len(n)) {
      if (r == cc) next
      f <- m[r, cc] / pv
      if (f == 0) next
      for (k in cc:(n + 1L)) m[r, k] <- m[r, k] - f * m[cc, k]
    }
  }
  vapply(seq_len(n), function(i) m[i, n + 1L] / m[i, i], numeric(1))
}

#' .morie_jo_ols
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_fitpred}, \code{morie_adfur}, \code{morie_quantreg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; indexed by row and column.
#' @param y Numeric; combined arithmetically in the body.
#' @return The value of \code{.morie_jo_solve}.
#' @export
.morie_jo_ols <- function(x, y) {
  x <- as.matrix(x)
  n <- nrow(x); p <- ncol(x)
  xtx <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p)) xtx[a, b] <- sum(x[, a] * x[, b])
  xty <- vapply(seq_len(p), function(a) sum(x[, a] * y), numeric(1))
  for (a in seq_len(p)) xtx[a, a] <- xtx[a, a] + 1e-12
  .morie_jo_solve(lapply(seq_len(p), function(i) xtx[i, ]), xty)
}

# =====================================================================
# Forecast error metrics -- ch. 19
# =====================================================================

#' Root mean squared error (ch. 19 p. 566)
#' @param y observed values
#' @param yhat forecasts
#' @return list(rmse, mse, mae, bias, n)
#' @export
morie_rmse <- function(y, yhat) {
  p <- .morie_jo_pair(y, yhat)
  e <- p$a - p$b
  mse <- sum(e * e) / length(e)
  list(rmse = sqrt(mse), mse = mse, mae = sum(abs(e)) / length(e),
       bias = mean(e), n = length(e))
}

#' Mean absolute percentage error (ch. 19 p. 568)
#'
#' The book's own warning is honoured: MAPE breaks down when an actual
#' observation is zero, so zero actuals raise rather than silently
#' returning infinity.
#' @param y observed values
#' @param yhat forecasts
#' @return list(mape, mdape, maxape, n)
#' @export
morie_mapets <- function(y, yhat) {
  p <- .morie_jo_pair(y, yhat)
  if (any(p$a == 0)) {
    stop("MAPE is undefined when an actual value is zero.", call. = FALSE)
  }
  pe <- 100 * abs(p$a - p$b) / abs(p$a)
  list(mape = mean(pe), mdape = .morie_jo_med(pe), maxape = max(pe),
       n = length(pe))
}

#' Symmetric mean absolute percentage error (ch. 19 p. 569)
#'
#' Quoted from p. 569: sMAPE = (1/H) sum_t 200 |e_t| / (|y_t| +
#' |yhat_t|). Note the 200: the symmetric denominator is the SUM of
#' the magnitudes, not their average.
#' @param y observed values
#' @param yhat forecasts
#' @return list(smape, smdape, n)
#' @export
morie_smape <- function(y, yhat) {
  p <- .morie_jo_pair(y, yhat)
  den <- abs(p$a) + abs(p$b)
  if (any(den == 0)) {
    stop("sMAPE is undefined when |y| + |yhat| is zero.", call. = FALSE)
  }
  terms <- 200 * abs(p$a - p$b) / den
  list(smape = mean(terms), smdape = .morie_jo_med(terms), n = length(terms))
}

#' Root mean squared scaled error (ch. 19 p. 572)
#'
#' Quoted from p. 572: the squared errors are scaled by the in-sample
#' mean squared naive error. This is the scaled error used in the M5
#' Forecasting Competition in 2020.
#' @param y observed values
#' @param yhat forecasts
#' @param insample the training series the naive scale comes from
#' @param season naive lag; 1 reproduces the printed formula
#' @return list(rmsse, scale, mase, n)
#' @export
morie_rmsse <- function(y, yhat, insample, season = 1L) {
  p <- .morie_jo_pair(y, yhat)
  ins <- .morie_jo_vec(insample, "insample")
  season <- as.integer(season)
  if (season < 1L || length(ins) <= season) {
    stop("insample must be longer than season >= 1.", call. = FALSE)
  }
  idx <- (season + 1L):length(ins)
  den <- sum((ins[idx] - ins[idx - season])^2) / (length(ins) - season)
  if (den <= 0) stop("in-sample naive error is zero.", call. = FALSE)
  num <- sum((p$a - p$b)^2) / length(p$a)
  mase <- (sum(abs(p$a - p$b)) / length(p$a)) /
    (sum(abs(ins[idx] - ins[idx - season])) / (length(ins) - season))
  list(rmsse = sqrt(num / den), scale = den, mase = mase, n = length(p$a))
}

#' Relative mean absolute error against a benchmark (ch. 19 p. 571)
#' @param y observed values
#' @param yhat model forecasts
#' @param benchmark benchmark forecasts
#' @return list(relmae, mae, benchmae, better, n)
#' @export
morie_relmae <- function(y, yhat, benchmark) {
  p <- .morie_jo_pair(y, yhat)
  cb <- .morie_jo_vec(benchmark, "benchmark")
  if (length(cb) != length(p$a)) {
    stop("benchmark must be the same length as y.", call. = FALSE)
  }
  mae <- sum(abs(p$a - p$b)) / length(p$a)
  base <- sum(abs(p$a - cb)) / length(p$a)
  if (base <= 0) stop("benchmark MAE is zero.", call. = FALSE)
  list(relmae = mae / base, mae = mae, benchmae = base, better = mae < base,
       n = length(p$a))
}

#' Pinball (quantile) loss (ch. 17 p. 494)
#'
#' The book points at the quantile loss for probabilistic forecasts.
#' The canonical statement is TFT eq. (25): QL(y, yhat, q) =
#' q (y - yhat)_+ + (1 - q)(yhat - y)_+ -- Lim, Arik, Loeff and
#' Pfister, arXiv:1912.09363.
#' @param y observed values
#' @param qhat forecast quantiles
#' @param q the quantile level, in (0, 1)
#' @return list(loss, total, coverage, q, n)
#' @export
morie_pinball <- function(y, qhat, q) {
  p <- .morie_jo_pair(y, qhat)
  q <- as.numeric(q)
  if (!(q > 0 && q < 1)) stop("q must lie strictly in (0, 1).", call. = FALSE)
  d <- p$a - p$b
  losses <- q * pmax(d, 0) + (1 - q) * pmax(-d, 0)
  list(loss = mean(losses), total = sum(losses),
       coverage = sum(p$a <= p$b) / length(p$a), q = q, n = length(p$a))
}

#' Winkler interval score (ch. 17)
#'
#' NOT LOCATED IN THE EXTRACTED TEXT: the corpus copy of Joseph and
#' Tackes never prints the Winkler score, so it is taken from the
#' primary source: W = (u - l) + (2/alpha)(l - y) if y < l, +
#' (2/alpha)(y - u) if y > u -- Winkler, R. L. (1972), JASA
#' 67(337):187-191, in the form of Gneiting and Raftery (2007), JASA
#' 102(477):359-378, eq. (43). Lower is better.
#' @param y observed values
#' @param lower,upper interval endpoints
#' @param alpha nominal miscoverage
#' @return list(score, total, coverage, meanwidth, n)
#' @export
morie_winkler <- function(y, lower, upper, alpha = 0.1) {
  a <- .morie_jo_vec(y, "y")
  lo <- .morie_jo_vec(lower, "lower")
  up <- .morie_jo_vec(upper, "upper")
  if (length(a) != length(lo) || length(a) != length(up)) {
    stop("y, lower and upper must be the same length.", call. = FALSE)
  }
  alpha <- as.numeric(alpha)
  if (!(alpha > 0 && alpha < 1)) {
    stop("alpha must lie strictly in (0, 1).", call. = FALSE)
  }
  if (any(up < lo)) stop("upper must be at least lower everywhere.", call. = FALSE)
  s <- up - lo
  s <- s + ifelse(a < lo, (2 / alpha) * (lo - a), 0)
  s <- s + ifelse(a > up, (2 / alpha) * (a - up), 0)
  list(score = mean(s), total = sum(s),
       coverage = sum(a >= lo & a <= up) / length(a),
       meanwidth = mean(up - lo), n = length(a))
}

# =====================================================================
# Transformations -- ch. 6
# =====================================================================

#' Box-Cox transformation (ch. 6 p. 164)
#'
#' w = (x^lambda - 1)/lambda for lambda != 0, w = log(x) otherwise.
#' @param x strictly positive series
#' @param lam the Box-Cox parameter
#' @return list(w, lam, mean, var, n)
#' @export
morie_boxcox <- function(x, lam) {
  v <- .morie_jo_vec(x)
  lam <- as.numeric(lam)
  if (any(v <= 0)) stop("Box-Cox needs strictly positive values.", call. = FALSE)
  w <- if (lam == 0) log(v) else (v^lam - 1) / lam
  list(w = w, lam = lam, mean = mean(w),
       var = sum((w - mean(w))^2) / length(w), n = length(w))
}

#' Log transformation with an optional offset (ch. 6 p. 163)
#' @param x the series
#' @param offset added before taking logs, the book's remedy for zeros
#' @return list(w, mean, sd, cvbefore, cvafter, n)
#' @export
morie_logtrans <- function(x, offset = 0) {
  v <- .morie_jo_vec(x)
  offset <- as.numeric(offset)
  if (any(v + offset <= 0)) {
    stop("log transform needs x + offset strictly positive.", call. = FALSE)
  }
  w <- log(v + offset)
  mv <- mean(v); mw <- mean(w)
  sv <- sqrt(sum((v - mv)^2) / length(v))
  sw <- sqrt(sum((w - mw)^2) / length(w))
  list(w = w, mean = mw, sd = sw,
       cvbefore = if (mv != 0) sv / abs(mv) else NaN,
       cvafter = if (mw != 0) sw / abs(mw) else NaN, n = length(w))
}

#' Differencing, ordinary and seasonal (ch. 6 pp. 155-158)
#' @param x the series
#' @param order number of successive differences
#' @param season the differencing lag; use the period for seasonal
#' @return list(w, mean, var, dropped, n)
#' @export
morie_diffser <- function(x, order = 1L, season = 1L) {
  v <- .morie_jo_vec(x)
  order <- as.integer(order); season <- as.integer(season)
  if (order < 1L || season < 1L) {
    stop("order and season must be at least 1.", call. = FALSE)
  }
  if (length(v) <= order * season) {
    stop("series is too short for this differencing.", call. = FALSE)
  }
  w <- v
  for (i in seq_len(order)) {
    idx <- (season + 1L):length(w)
    w <- w[idx] - w[idx - season]
  }
  list(w = w, mean = mean(w), var = sum((w - mean(w))^2) / length(w),
       dropped = length(v) - length(w), n = length(w))
}

# =====================================================================
# Feature engineering -- ch. 6
# =====================================================================

#' Lag features (ch. 6 p. 170)
#' @param x the series
#' @param lags positive integer lags
#' @return list(rows, target, lags, nrows, ncols, mean)
#' @export
morie_lagfeat <- function(x, lags) {
  v <- .morie_jo_vec(x)
  lags <- sort(unique(as.integer(lags)))
  if (length(lags) == 0L || lags[1] < 1L) {
    stop("lags must be positive integers.", call. = FALSE)
  }
  start <- lags[length(lags)]
  if (length(v) <= start) {
    stop("series is too short for the largest lag.", call. = FALSE)
  }
  idx <- (start + 1L):length(v)
  rows <- t(vapply(idx, function(i) v[i - lags], numeric(length(lags))))
  if (length(lags) == 1L) rows <- matrix(rows, ncol = 1L)
  list(rows = rows, target = v[idx], lags = lags, nrows = length(idx),
       ncols = length(lags), mean = mean(as.numeric(rows)))
}

#' Rolling-window features (ch. 6 p. 176)
#' @param x the series
#' @param window trailing window length
#' @param minperiods smallest usable window; defaults to the full window
#' @return list(mean, sd, min, max, nrows, lastmean, meanofmeans)
#' @export
morie_rollfeat <- function(x, window, minperiods = NULL) {
  v <- .morie_jo_vec(x)
  window <- as.integer(window)
  if (window < 1L) stop("window must be at least 1.", call. = FALSE)
  mp <- if (is.null(minperiods)) window else as.integer(minperiods)
  if (mp < 1L || mp > window) {
    stop("minperiods must lie in [1, window].", call. = FALSE)
  }
  means <- numeric(0); sds <- numeric(0)
  mins <- numeric(0); maxs <- numeric(0)
  for (i in seq_along(v)) {
    lo <- max(1L, i - window + 1L)
    w <- v[lo:i]
    if (length(w) < mp) next
    m <- mean(w)
    means <- c(means, m)
    sds <- c(sds, sqrt(sum((w - m)^2) / length(w)))
    mins <- c(mins, min(w)); maxs <- c(maxs, max(w))
  }
  list(mean = means, sd = sds, min = mins, max = maxs, nrows = length(means),
       lastmean = if (length(means)) means[length(means)] else NaN,
       meanofmeans = if (length(means)) mean(means) else NaN)
}

#' Fourier seasonality terms (ch. 4 p. 61, ch. 5 p. 95)
#'
#' Column pair j is sin(2 pi j t / m), cos(2 pi j t / m) for j = 1..k.
#' The book calls these trigonometric seasonality.
#' @param n number of rows
#' @param period the seasonal period m
#' @param k number of harmonic pairs
#' @param start first time index
#' @return list(rows, nrows, ncols, k, period, mean, sumsq)
#' @export
morie_fourfeat <- function(n, period, k, start = 0) {
  n <- as.integer(n); period <- as.numeric(period); k <- as.integer(k)
  if (n < 1L || period <= 0 || k < 1L) {
    stop("need n >= 1, period > 0 and k >= 1.", call. = FALSE)
  }
  if (2 * k > period) {
    stop("k must not exceed period / 2 (Nyquist).", call. = FALSE)
  }
  rows <- matrix(0, n, 2L * k)
  for (i in seq_len(n)) {
    tt <- as.numeric(start) + i - 1
    for (j in seq_len(k)) {
      ang <- 2 * pi * j * tt / period
      rows[i, 2L * j - 1L] <- sin(ang)
      rows[i, 2L * j] <- cos(ang)
    }
  }
  flat <- as.numeric(rows)
  list(rows = rows, nrows = n, ncols = 2L * k, k = k, period = period,
       mean = mean(flat), sumsq = sum(flat * flat))
}

.morie_jo_mlen <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

#' .morie_jo_leap
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_daynum}, \code{morie_calfeat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @return A logical value.
#' @export
.morie_jo_leap <- function(y) (y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0

#' .morie_jo_daynum
#'
#' A step of the ts_joseph implementation. Called by \code{morie_calfeat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @param m Numeric; combined arithmetically in the body.
#' @param d Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_jo_daynum <- function(y, m, d) {
  days <- 0
  if (y >= 1970) {
    for (yy in seq_len(y - 1970) + 1969) days <- days + if (.morie_jo_leap(yy)) 366 else 365
  } else {
    for (yy in y:1969) days <- days - if (.morie_jo_leap(yy)) 366 else 365
  }
  if (m > 1L) {
    for (mm in seq_len(m - 1L)) {
      days <- days + .morie_jo_mlen[mm] + if (mm == 2L && .morie_jo_leap(y)) 1 else 0
    }
  }
  days + d - 1
}

#' Calendar and time features (ch. 6 p. 168)
#'
#' dates is a matrix or list of (year, month, day) triples. Produces
#' the book's time-based features plus the cyclic sine/cosine encoding
#' of month and day of week, so December sits next to January. The
#' calendar arithmetic is proleptic Gregorian and written out here, so
#' both language arms agree without depending on a date library.
#' @param dates matrix or list of c(year, month, day)
#' @return list(rows, n, nweekend, meandoy, meanmonthsin)
#' @export
morie_calfeat <- function(dates) {
  dm <- if (is.matrix(dates)) dates else do.call(rbind, dates)
  storage.mode(dm) <- "integer"
  rows <- list()
  for (i in seq_len(nrow(dm))) {
    y <- dm[i, 1]; m <- dm[i, 2]; d <- dm[i, 3]
    if (m < 1L || m > 12L) stop("month must lie in 1..12.", call. = FALSE)
    mlen <- .morie_jo_mlen[m] + if (m == 2L && .morie_jo_leap(y)) 1 else 0
    if (d < 1L || d > mlen) stop("day is out of range for that month.", call. = FALSE)
    dn <- .morie_jo_daynum(y, m, d)
    dow <- (dn + 4) %% 7
    doy <- dn - .morie_jo_daynum(y, 1L, 1L) + 1
    rows[[i]] <- list(year = y, month = m, day = d, dow = dow, doy = doy,
                      quarter = (m - 1L) %/% 3L + 1L,
                      week = (doy - 1) %/% 7 + 1,
                      weekend = if (dow >= 5) 1L else 0L,
                      monthstart = if (d == 1L) 1L else 0L,
                      monthend = if (d == mlen) 1L else 0L,
                      monthsin = sin(2 * pi * m / 12),
                      monthcos = cos(2 * pi * m / 12),
                      dowsin = sin(2 * pi * dow / 7),
                      dowcos = cos(2 * pi * dow / 7))
  }
  list(rows = rows, n = length(rows),
       nweekend = sum(vapply(rows, function(r) r$weekend, integer(1))),
       meandoy = mean(vapply(rows, function(r) as.numeric(r$doy), numeric(1))),
       meanmonthsin = mean(vapply(rows, function(r) r$monthsin, numeric(1))))
}

#' Missing-data imputation for time series (ch. 2 pp. 44-52)
#'
#' x may contain NA for a gap. method is one of the book's own
#' options: ffill, bfill, linear, mean, or seasonal. Leading or
#' trailing gaps a method cannot reach fall back to the series mean,
#' so the output never contains a hole.
#' @param x the series, with NA for gaps
#' @param method one of ffill, bfill, linear, mean, seasonal
#' @param season the seasonal period, for method = "seasonal"
#' @return list(x, nmissing, n, mean, method, imputedmean)
#' @export
morie_tsimpute <- function(x, method = "linear", season = 1L) {
  raw <- as.numeric(x)
  if (length(raw) == 0L) stop("x must be non-empty.", call. = FALSE)
  obs <- which(!is.na(raw))
  if (length(obs) == 0L) stop("x contains no observed values.", call. = FALSE)
  gm <- mean(raw[obs])
  n <- length(raw)
  season <- as.integer(season)
  if (season < 1L) stop("season must be at least 1.", call. = FALSE)
  out <- numeric(n)
  for (i in seq_len(n)) {
    if (!is.na(raw[i])) { out[i] <- raw[i]; next }
    prev <- obs[obs < i]
    nxt <- obs[obs > i]
    out[i] <- if (method == "ffill") {
      if (length(prev)) raw[prev[length(prev)]] else gm
    } else if (method == "bfill") {
      if (length(nxt)) raw[nxt[1]] else gm
    } else if (method == "mean") {
      gm
    } else if (method == "seasonal") {
      same <- obs[(obs - i) %% season == 0]
      if (length(same)) mean(raw[same]) else gm
    } else if (method == "linear") {
      if (length(prev) && length(nxt)) {
        a <- prev[length(prev)]; b <- nxt[1]
        w <- (i - a) / (b - a)
        raw[a] + w * (raw[b] - raw[a])
      } else if (length(prev)) {
        raw[prev[length(prev)]]
      } else if (length(nxt)) {
        raw[nxt[1]]
      } else gm
    } else stop("unknown method.", call. = FALSE)
  }
  miss <- setdiff(seq_len(n), obs)
  list(x = out, nmissing = n - length(obs), n = n, mean = mean(out),
       method = method,
       imputedmean = if (length(miss)) mean(out[miss]) else NaN)
}

# =====================================================================
# Diagnostics -- ch. 3 and ch. 6
# =====================================================================

#' Sample autocorrelation function (ch. 3)
#'
#' The biased (divide-by-n) estimator the book's ACF plots use;
#' ci is the +/- 1.96/sqrt(n) band drawn on them.
#' @param x the series
#' @param maxlag largest lag
#' @return list(acf, ci, maxlag, n, r1, nsignif)
#' @export
morie_autocorf <- function(x, maxlag = 20L) {
  v <- .morie_jo_vec(x)
  n <- length(v)
  maxlag <- as.integer(maxlag)
  if (maxlag < 1L || maxlag >= n) {
    stop("maxlag must lie in [1, n - 1].", call. = FALSE)
  }
  m <- mean(v)
  den <- sum((v - m)^2)
  if (den <= 0) stop("series is constant; the ACF is undefined.", call. = FALSE)
  r <- vapply(0:maxlag, function(k) {
    if (k == 0L) return(1)
    idx <- (k + 1L):n
    sum((v[idx] - m) * (v[idx - k] - m)) / den
  }, numeric(1))
  ci <- 1.96 / sqrt(n)
  list(acf = r, ci = ci, maxlag = maxlag, n = n, r1 = r[2],
       nsignif = sum(abs(r[2:(maxlag + 1L)]) > ci))
}

#' Partial autocorrelation by the Durbin-Levinson recursion (ch. 3)
#'
#' Fixed recursion depth, no tolerance test, so both arms take
#' identical steps.
#' @param x the series
#' @param maxlag largest lag
#' @return list(pacf, ci, maxlag, n, p1, nsignif)
#' @export
morie_pacfts <- function(x, maxlag = 20L) {
  v <- .morie_jo_vec(x)
  n <- length(v)
  maxlag <- as.integer(maxlag)
  if (maxlag < 1L || maxlag >= n) {
    stop("maxlag must lie in [1, n - 1].", call. = FALSE)
  }
  r <- morie_autocorf(v, maxlag)$acf
  phi <- matrix(0, maxlag + 1L, maxlag + 1L)
  pacf <- c(1, r[2])
  if (r[2] == 1) stop("series is perfectly autocorrelated at lag 1.", call. = FALSE)
  phi[2, 2] <- r[2]
  if (maxlag >= 2L) {
    for (k in 2:maxlag) {
      num <- r[k + 1L] - sum(vapply(seq_len(k - 1L),
                                    function(j) phi[k, j + 1L] * r[k - j + 1L],
                                    numeric(1)))
      den <- 1 - sum(vapply(seq_len(k - 1L),
                            function(j) phi[k, j + 1L] * r[j + 1L], numeric(1)))
      if (abs(den) < 1e-300) {
        stop("Durbin-Levinson recursion broke down.", call. = FALSE)
      }
      phi[k + 1L, k + 1L] <- num / den
      for (j in seq_len(k - 1L)) {
        phi[k + 1L, j + 1L] <- phi[k, j + 1L] -
          phi[k + 1L, k + 1L] * phi[k, k - j + 1L]
      }
      pacf <- c(pacf, phi[k + 1L, k + 1L])
    }
  }
  ci <- 1.96 / sqrt(n)
  list(pacf = pacf, ci = ci, maxlag = maxlag, n = n, p1 = pacf[2],
       nsignif = sum(abs(pacf[2:(maxlag + 1L)]) > ci))
}

#' Augmented Dickey-Fuller unit-root test (ch. 6 p. 149)
#'
#' Regresses diff(x)_t on a constant, x_{t-1} and lags lagged
#' differences; the statistic is the t-ratio on x_{t-1}. The null is a
#' unit root, so a statistic BELOW the critical value rejects
#' non-stationarity. Critical values are MacKinnon's (1991) response
#' surface for the constant-only regression, tau = b0 + b1/n + b2/n^2;
#' they are returned rather than a p-value, because interpolating a
#' p-value would need a table the book does not print.
#' @param x the series
#' @param lags number of augmenting lagged differences
#' @return list(stat, gamma, se, lags, n, crit1, crit5, crit10,
#'   stationary5)
#' @export
morie_adfur <- function(x, lags = 1L) {
  v <- .morie_jo_vec(x)
  lags <- as.integer(lags)
  if (lags < 0L) stop("lags must be non-negative.", call. = FALSE)
  d <- v[-1] - v[-length(v)]
  start <- lags
  n <- length(d) - start
  if (n <= lags + 3L) {
    stop("series is too short for this many augmenting lags.", call. = FALSE)
  }
  p <- 2L + lags
  rows <- matrix(0, n, p)
  yv <- numeric(n)
  for (q in seq_len(n)) {
    i <- start + q
    rows[q, 1] <- 1
    rows[q, 2] <- v[i]
    if (lags >= 1L) for (j in seq_len(lags)) rows[q, 2L + j] <- d[i - j]
    yv[q] <- d[i]
  }
  beta <- .morie_jo_ols(rows, yv)
  resid <- yv - as.numeric(rows %*% beta)
  dof <- n - p
  if (dof < 1L) stop("not enough degrees of freedom.", call. = FALSE)
  s2 <- sum(resid * resid) / dof
  xtx <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p)) xtx[a, b] <- sum(rows[, a] * rows[, b])
  for (a in seq_len(p)) xtx[a, a] <- xtx[a, a] + 1e-12
  e1 <- as.numeric(seq_len(p) == 2L)
  se <- sqrt(s2 * .morie_jo_solve(lapply(seq_len(p), function(i) xtx[i, ]), e1)[2])
  stat <- beta[2] / se
  cv <- function(b0, b1, b2) b0 + b1 / n + b2 / (n * n)
  c1 <- cv(-3.43035, -6.5393, -16.786)
  c5 <- cv(-2.86154, -2.8903, -4.234)
  c10 <- cv(-2.56677, -1.5384, -2.809)
  list(stat = stat, gamma = beta[2], se = se, lags = lags, n = n,
       crit1 = c1, crit5 = c5, crit10 = c10, stationary5 = stat < c5)
}

#' Seasonal-trend decomposition (ch. 3 p. 64)
#'
#' The book's STL uses LOESS smoothers. This routine uses the
#' classical moving-average form of the same three-part model --
#' centred moving-average trend, seasonal means of the detrended
#' series, remainder -- iterated iters times. That substitution is
#' OURS and is stated here rather than passed off as STL: the LOESS
#' smoother has bandwidth and robustness choices whose defaults differ
#' between implementations, and a decomposition whose numbers depend on
#' which library you call cannot be checked across two languages.
#' robust switches the seasonal aggregate from the mean to the median.
#' @param x the series
#' @param period the seasonal period
#' @param robust use the median rather than the mean seasonal aggregate
#' @param iters number of refinement passes
#' @return list(trend, seasonal, remainder, period, n, seasonalstrength,
#'   remaindervar, seasonalrange)
#' @export
morie_stldecomp <- function(x, period, robust = FALSE, iters = 2L) {
  v <- .morie_jo_vec(x)
  period <- as.integer(period); iters <- as.integer(iters)
  if (period < 2L || length(v) < 2L * period) {
    stop("need period >= 2 and at least two full periods.", call. = FALSE)
  }
  if (iters < 1L) stop("iters must be at least 1.", call. = FALSE)
  n <- length(v)
  half <- period %/% 2L
  seasonal <- numeric(n)
  trend <- numeric(n)
  for (it in seq_len(iters)) {
    deseas <- v - seasonal
    for (i in seq_len(n)) {
      lo <- max(1L, i - half); hi <- min(n, i + half)
      trend[i] <- mean(deseas[lo:hi])
    }
    detr <- v - trend
    agg <- numeric(period)
    for (s in seq_len(period)) {
      grp <- detr[seq(s, n, by = period)]
      agg[s] <- if (isTRUE(robust)) .morie_jo_med(grp) else mean(grp)
    }
    agg <- agg - sum(agg) / period
    seasonal <- agg[((seq_len(n) - 1L) %% period) + 1L]
  }
  remainder <- v - trend - seasonal
  vr <- sum(remainder * remainder) / n
  vv <- sum((v - mean(v))^2) / n
  list(trend = trend, seasonal = seasonal, remainder = remainder,
       period = period, n = n,
       seasonalstrength = max(0, 1 - vr / max(vv, 1e-300)),
       remaindervar = vr, seasonalrange = max(seasonal) - min(seasonal))
}

# =====================================================================
# Multi-step strategies -- ch. 18
# =====================================================================

#' Time series recast as a regression problem (ch. 5 p. 118)
#' @param x the series
#' @param lags positive integer lags
#' @param horizon steps ahead the target sits
#' @return list(rows, y, lags, horizon, nrows, ncols, ymean, xmean)
#' @export
morie_tsregmat <- function(x, lags, horizon = 1L) {
  v <- .morie_jo_vec(x)
  lags <- sort(unique(as.integer(lags)))
  horizon <- as.integer(horizon)
  if (length(lags) == 0L || lags[1] < 1L || horizon < 1L) {
    stop("lags must be positive and horizon at least 1.", call. = FALSE)
  }
  start <- lags[length(lags)]
  idx <- seq_len(max(0L, length(v) - horizon + 1L - start)) + start
  if (length(idx) == 0L) {
    stop("series is too short for these lags and horizon.", call. = FALSE)
  }
  rows <- t(vapply(idx, function(i) v[i - lags], numeric(length(lags))))
  if (length(lags) == 1L) rows <- matrix(rows, ncol = 1L)
  yv <- v[idx + horizon - 1L]
  list(rows = rows, y = yv, lags = lags, horizon = horizon,
       nrows = length(idx), ncols = length(lags), ymean = mean(yv),
       xmean = mean(as.numeric(rows)))
}

#' .morie_jo_fitpred
#'
#' A step of the ts_joseph implementation. Called by \code{morie_dirmulti}, \code{morie_dirrec}, \code{morie_recmulti}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rows A matrix; passed to \code{nrow}.
#' @param y Passed to \code{.morie_jo_ols}.
#' @param newrow See Usage.
#' @return A numeric value.
#' @export
.morie_jo_fitpred <- function(rows, y, newrow) {
  design <- cbind(rep(1, nrow(rows)), rows)
  beta <- .morie_jo_ols(design, y)
  sum(c(1, newrow) * beta)
}

#' Recursive multi-step forecasting (ch. 18 p. 546)
#'
#' One model, trained for a single step, applied repeatedly with its
#' own forecasts fed back in as lags. The base learner is ordinary
#' least squares on the lag design, so the STRATEGY -- which is what
#' the book teaches -- is what is demonstrated, and nothing is fitted
#' at random.
#' @param x the series
#' @param lags positive integer lags
#' @param horizon steps to forecast
#' @return list(forecast, horizon, nmodels, ntrain, first, last, mean)
#' @export
morie_recmulti <- function(x, lags, horizon) {
  v <- .morie_jo_vec(x)
  lags <- sort(unique(as.integer(lags)))
  horizon <- as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1.", call. = FALSE)
  tr <- morie_tsregmat(v, lags, 1L)
  hist <- v
  preds <- numeric(0)
  for (h in seq_len(horizon)) {
    newrow <- hist[length(hist) - lags + 1L]
    p <- .morie_jo_fitpred(tr$rows, tr$y, newrow)
    preds <- c(preds, p)
    hist <- c(hist, p)
  }
  list(forecast = preds, horizon = horizon, nmodels = 1L, ntrain = tr$nrows,
       first = preds[1], last = preds[horizon], mean = mean(preds))
}

#' Direct multi-step forecasting (ch. 18 p. 548)
#'
#' One model PER horizon, each trained to predict h steps ahead
#' directly from the same observed lags, so no forecast is ever fed
#' back and errors cannot compound.
#' @param x the series
#' @param lags positive integer lags
#' @param horizon steps to forecast
#' @return list(forecast, horizon, nmodels, first, last, mean)
#' @export
morie_dirmulti <- function(x, lags, horizon) {
  v <- .morie_jo_vec(x)
  lags <- sort(unique(as.integer(lags)))
  horizon <- as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1.", call. = FALSE)
  newrow <- v[length(v) - lags + 1L]
  preds <- numeric(0)
  for (h in seq_len(horizon)) {
    tr <- morie_tsregmat(v, lags, h)
    preds <- c(preds, .morie_jo_fitpred(tr$rows, tr$y, newrow))
  }
  list(forecast = preds, horizon = horizon, nmodels = horizon,
       first = preds[1], last = preds[horizon], mean = mean(preds))
}

#' DirRec strategy (ch. 18 p. 551)
#'
#' The hybrid the book names: like Direct, a separate model per
#' horizon; like Recursive, each successive model may also use the
#' forecasts already produced, so the input space GROWS by one column
#' at every step.
#' @param x the series
#' @param lags positive integer lags
#' @param horizon steps to forecast
#' @return list(forecast, horizon, nmodels, ncolsfirst, ncolslast,
#'   first, last, mean)
#' @export
morie_dirrec <- function(x, lags, horizon) {
  v <- .morie_jo_vec(x)
  lags <- sort(unique(as.integer(lags)))
  horizon <- as.integer(horizon)
  if (horizon < 1L) stop("horizon must be at least 1.", call. = FALSE)
  preds <- numeric(0)
  ncols <- integer(0)
  maxlag <- lags[length(lags)]
  for (h in seq_len(horizon)) {
    extra <- length(preds)
    base <- morie_tsregmat(v, lags, h)
    keep <- integer(0)
    for (i in seq_len(base$nrows)) {
      pos <- maxlag + i - 1L
      if (pos + extra < length(v)) keep <- c(keep, i)
    }
    if (length(keep) == 0L) {
      stop("series is too short for the DirRec expansion.", call. = FALSE)
    }
    rows <- base$rows[keep, , drop = FALSE]
    if (extra > 0L) {
      add <- t(vapply(keep, function(i) {
        pos <- maxlag + i - 1L
        v[pos + seq_len(extra)]
      }, numeric(extra)))
      if (extra == 1L) add <- matrix(add, ncol = 1L)
      rows <- cbind(rows, add)
    }
    ys <- base$y[keep]
    newrow <- c(v[length(v) - lags + 1L], preds)
    preds <- c(preds, .morie_jo_fitpred(rows, ys, newrow))
    ncols <- c(ncols, length(newrow))
  }
  list(forecast = preds, horizon = horizon, nmodels = horizon,
       ncolsfirst = ncols[1], ncolslast = ncols[horizon],
       first = preds[1], last = preds[horizon], mean = mean(preds))
}

#' Seasonal naive baseline (ch. 8 p. 219)
#'
#' Each forecast repeats the observation from the same point in the
#' previous season -- the benchmark the scaled metrics divide by.
#' @param x the series
#' @param season the seasonal period
#' @param horizon steps to forecast
#' @return list(forecast, season, horizon, first, last, mean)
#' @export
morie_seasnaive <- function(x, season, horizon) {
  v <- .morie_jo_vec(x)
  season <- as.integer(season); horizon <- as.integer(horizon)
  if (season < 1L || horizon < 1L) {
    stop("season and horizon must be at least 1.", call. = FALSE)
  }
  if (length(v) < season) stop("series is shorter than one season.", call. = FALSE)
  preds <- vapply(seq_len(horizon) - 1L,
                  function(h) v[length(v) - season + (h %% season) + 1L],
                  numeric(1))
  list(forecast = preds, season = season, horizon = horizon,
       first = preds[1], last = preds[horizon], mean = mean(preds))
}

# =====================================================================
# Validation -- ch. 5
# =====================================================================

#' Sliding-window cross-validation (ch. 5 p. 128)
#'
#' A fixed-length training window slides forward, so old data drops
#' out. Fold boundaries are half-open [start, end) index pairs,
#' zero-based to match the Python arm.
#' @param n number of observations
#' @param trainsize training window length
#' @param testsize test window length
#' @param step how far the window advances; defaults to testsize
#' @return list(folds, nfolds, trainsize, testsize, step, firsttest,
#'   lasttest)
#' @export
morie_slidecv <- function(n, trainsize, testsize, step = NULL) {
  n <- as.integer(n); trainsize <- as.integer(trainsize)
  testsize <- as.integer(testsize)
  step <- if (is.null(step)) testsize else as.integer(step)
  if (min(n, trainsize, testsize, step) < 1L) {
    stop("all arguments must be positive.", call. = FALSE)
  }
  folds <- list()
  s <- 0L
  while (s + trainsize + testsize <= n) {
    folds[[length(folds) + 1L]] <- c(s, s + trainsize, s + trainsize,
                                     s + trainsize + testsize)
    s <- s + step
  }
  if (length(folds) == 0L) stop("n is too small for this layout.", call. = FALSE)
  list(folds = folds, nfolds = length(folds), trainsize = trainsize,
       testsize = testsize, step = step, firsttest = folds[[1]][3],
       lasttest = folds[[length(folds)]][4])
}

#' Expanding-window cross-validation (ch. 5 p. 130)
#'
#' The training window GROWS: every fold starts at index 0, so no
#' history is ever discarded.
#' @param n number of observations
#' @param initial first training window length
#' @param testsize test window length
#' @param step how far the training end advances; defaults to testsize
#' @return list(folds, nfolds, initial, testsize, step, firsttrainend,
#'   lasttrainend)
#' @export
morie_expandcv <- function(n, initial, testsize, step = NULL) {
  n <- as.integer(n); initial <- as.integer(initial)
  testsize <- as.integer(testsize)
  step <- if (is.null(step)) testsize else as.integer(step)
  if (min(n, initial, testsize, step) < 1L) {
    stop("all arguments must be positive.", call. = FALSE)
  }
  folds <- list()
  end <- initial
  while (end + testsize <= n) {
    folds[[length(folds) + 1L]] <- c(0L, end, end, end + testsize)
    end <- end + step
  }
  if (length(folds) == 0L) stop("n is too small for this layout.", call. = FALSE)
  list(folds = folds, nfolds = length(folds), initial = initial,
       testsize = testsize, step = step, firsttrainend = folds[[1]][2],
       lasttrainend = folds[[length(folds)]][2])
}

#' Walk-forward validation (ch. 5 p. 126)
#'
#' Scores an already-produced forecast series fold by fold on an
#' expanding-window layout and reports the fold RMSEs plus their mean
#' and spread.
#' @param y observed values
#' @param yhat forecasts
#' @param initial first training window length
#' @param testsize test window length
#' @param step how far the training end advances
#' @return list(scores, nfolds, rmse, sd, best, worst)
#' @export
morie_walkfwd <- function(y, yhat, initial, testsize, step = NULL) {
  p <- .morie_jo_pair(y, yhat)
  lay <- morie_expandcv(length(p$a), initial, testsize, step)
  scores <- vapply(lay$folds, function(f) {
    idx <- (f[3] + 1L):f[4]
    sqrt(sum((p$a[idx] - p$b[idx])^2) / length(idx))
  }, numeric(1))
  m <- mean(scores)
  list(scores = scores, nfolds = length(scores), rmse = m,
       sd = sqrt(sum((scores - m)^2) / length(scores)),
       best = min(scores), worst = max(scores))
}

# =====================================================================
# Probabilistic forecasting -- ch. 17
# =====================================================================

#' Linear quantile regression (ch. 17 p. 500)
#'
#' Fitted by iteratively reweighted least squares on the pinball loss
#' with a fixed iteration count and a fixed smoothing floor -- no
#' convergence test, so both arms take identical steps. An intercept is
#' added to x.
#' @param x matrix of feature rows
#' @param y responses
#' @param q the quantile level, in (0, 1)
#' @param iters number of IRLS sweeps
#' @return list(beta, fitted, loss, q, intercept, n, p)
#' @export
morie_quantreg <- function(x, y, q, iters = 25L) {
  xm <- cbind(rep(1, nrow(as.matrix(x))), as.matrix(x))
  storage.mode(xm) <- "double"
  yv <- .morie_jo_vec(y, "y")
  if (nrow(xm) != length(yv)) {
    stop("x and y must have the same number of rows.", call. = FALSE)
  }
  q <- as.numeric(q)
  if (!(q > 0 && q < 1)) stop("q must lie strictly in (0, 1).", call. = FALSE)
  iters <- as.integer(iters)
  if (iters < 1L) stop("iters must be at least 1.", call. = FALSE)
  n <- nrow(xm); p <- ncol(xm)
  beta <- .morie_jo_ols(xm, yv)
  eps <- 1e-6
  for (it in seq_len(iters)) {
    r <- yv - as.numeric(xm %*% beta)
    w <- ifelse(r > 0, q, 1 - q) / pmax(abs(r), eps)
    xtx <- matrix(0, p, p)
    for (a in seq_len(p)) for (b in seq_len(p)) {
      xtx[a, b] <- sum(w * xm[, a] * xm[, b])
    }
    xty <- vapply(seq_len(p), function(a) sum(w * xm[, a] * yv), numeric(1))
    for (a in seq_len(p)) xtx[a, a] <- xtx[a, a] + 1e-10
    beta <- .morie_jo_solve(lapply(seq_len(p), function(i) xtx[i, ]), xty)
  }
  fit <- as.numeric(xm %*% beta)
  list(beta = beta, fitted = fit, loss = morie_pinball(yv, fit, q)$loss,
       q = q, intercept = beta[1], n = n, p = p)
}

#' Conformalized quantile regression (ch. 17 pp. 514-515)
#'
#' The book's own non-conformity score, quoted from p. 514:
#' s(x, y) = max{yhat_t^{alpha/2} - y, y - yhat_t^{1-(alpha/2)}}.
#' The conformal quantile of the calibration scores is added to both
#' ends of the test interval, using the finite-sample rank
#' ceil((n+1)(1-alpha)) that delivers the coverage guarantee.
#' The method is Romano, Y., Patterson, E. and Candes, E. (2019),
#' Conformalized Quantile Regression, NeurIPS 32 (arXiv:1905.03222),
#' the book's Reference 11.
#' @param callo,calhi calibration interval endpoints
#' @param caly calibration observations
#' @param lo,hi test interval endpoints to conformalize
#' @param alpha nominal miscoverage
#' @return list(qhat, lower, upper, k, n, meanwidth, widening)
#' @export
morie_cqr <- function(callo, calhi, caly, lo, hi, alpha = 0.1) {
  cl <- .morie_jo_vec(callo, "callo")
  ch <- .morie_jo_vec(calhi, "calhi")
  cy <- .morie_jo_vec(caly, "caly")
  if (length(cl) != length(ch) || length(cl) != length(cy)) {
    stop("calibration arrays must be the same length.", call. = FALSE)
  }
  alpha <- as.numeric(alpha)
  if (!(alpha > 0 && alpha < 1)) {
    stop("alpha must lie strictly in (0, 1).", call. = FALSE)
  }
  scores <- sort(pmax(cl - cy, cy - ch))
  n <- length(scores)
  k <- ceiling((n + 1) * (1 - alpha))
  qhat <- scores[max(1L, min(as.integer(k), n))]
  lo <- .morie_jo_vec(lo, "lo"); hi <- .morie_jo_vec(hi, "hi")
  if (length(lo) != length(hi)) {
    stop("lo and hi must be the same length.", call. = FALSE)
  }
  newlo <- lo - qhat
  newhi <- hi + qhat
  list(qhat = qhat, lower = newlo, upper = newhi, k = as.integer(k), n = n,
       meanwidth = mean(newhi - newlo), widening = 2 * qhat)
}

#' Adaptive conformal inference (ch. 17 p. 519)
#'
#' The book's own online update, quoted from p. 519: err_t = 1 if Y_t
#' is outside Chat(alpha_t) else 0; alpha_{t+1} = alpha_t + gamma
#' (alpha - err_t), with alpha_1 = alpha. inside is the sequence of
#' coverage outcomes, so this is a pure recursion over caller data.
#' The method is Gibbs, I. and Candes, E. (2021), Adaptive Conformal
#' Inference Under Distribution Shift, NeurIPS 34 (arXiv:2106.00170),
#' the book's Reference 13.
#' @param inside logical vector, TRUE when the observation was covered
#' @param alpha target miscoverage
#' @param gamma step size
#' @return list(alpha, final, empirical, target, gamma, n, minalpha,
#'   maxalpha)
#' @export
morie_aci <- function(inside, alpha = 0.1, gamma = 0.01) {
  seqv <- as.logical(inside)
  if (length(seqv) == 0L) stop("inside must be non-empty.", call. = FALSE)
  alpha <- as.numeric(alpha); gamma <- as.numeric(gamma)
  if (!(alpha > 0 && alpha < 1) || gamma <= 0) {
    stop("alpha must lie in (0, 1) and gamma be positive.", call. = FALSE)
  }
  at <- alpha
  path <- at
  nerr <- 0L
  for (ok in seqv) {
    err <- if (ok) 0 else 1
    nerr <- nerr + as.integer(err)
    at <- at + gamma * (alpha - err)
    path <- c(path, at)
  }
  list(alpha = path, final = at, empirical = nerr / length(seqv),
       target = alpha, gamma = gamma, n = length(seqv),
       minalpha = min(path), maxalpha = max(path))
}

# =====================================================================
# Deep architectures -- equations from the papers, not the book
# =====================================================================

#' .morie_jo_matvec
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_glu}, \code{.morie_jo_resblock}, \code{morie_itrans} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param w A matrix; the body checks with \code{is.matrix}.
#' @param v A matrix; passed to \code{\%*\%}.
#' @return A vector, from \code{as.numeric}.
#' @export
.morie_jo_matvec <- function(w, v) {
  wm <- if (is.matrix(w)) w else do.call(rbind, w)
  if (ncol(wm) != length(v)) {
    stop("weight row length must match the input length.", call. = FALSE)
  }
  as.numeric(wm %*% v)
}

#' .morie_jo_softmax
#'
#' A step of the ts_joseph implementation. Called by \code{morie_autoform}, \code{morie_itrans}, \code{morie_tftnet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.morie_jo_softmax <- function(v) {
  ex <- exp(v - max(v))
  ex / sum(ex)
}

#' .morie_jo_ln
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_resblock}, \code{morie_itrans}, \code{morie_tftnet} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return A numeric value.
#' @export
.morie_jo_ln <- function(v, eps = 1e-5) {
  m <- mean(v)
  (v - m) / sqrt(sum((v - m)^2) / length(v) + eps)
}

#' .morie_jo_elu
#'
#' A step of the ts_joseph implementation. Called by \code{morie_tftnet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t See Usage.
#' @return The value of \code{ifelse}.
#' @export
.morie_jo_elu <- function(t) ifelse(t > 0, t, expm1(t))
#' .morie_jo_relu
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_resblock}, \code{morie_itrans}, \code{morie_tsmixer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t See Usage.
#' @return The value of \code{ifelse}.
#' @export
.morie_jo_relu <- function(t) ifelse(t > 0, t, 0)
#' .morie_jo_sigmoid
#'
#' A step of the ts_joseph implementation. Called by \code{.morie_jo_glu}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Numeric; passed to \code{exp}.
#' @return The value of \code{ifelse}.
#' @export
.morie_jo_sigmoid <- function(t) ifelse(t >= 0, 1 / (1 + exp(-t)),
                                        exp(t) / (1 + exp(t)))

#' .morie_jo_maxpool
#'
#' A step of the ts_joseph implementation. Called by \code{morie_nhitsnet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @param k Numeric; combined arithmetically in the body.
#' @return A vector, from \code{vapply}.
#' @export
.morie_jo_maxpool <- function(v, k) {
  k <- as.integer(k)
  if (k < 1L) stop("pool kernel must be at least 1.", call. = FALSE)
  if (k == 1L) return(v)
  starts <- seq(1L, length(v), by = k)
  vapply(starts, function(s) max(v[s:min(s + k - 1L, length(v))]), numeric(1))
}

#' .morie_jo_interp
#'
#' A step of the ts_joseph implementation. Called by \code{morie_nhitsnet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta A vector; its length is taken and its elements indexed.
#' @param length_ A count; the body uses it as \code{seq_len(...)}.
#' @return A vector, from \code{vapply}.
#' @export
.morie_jo_interp <- function(theta, length_) {
  n <- length(theta)
  if (n < 1L || length_ < 1L) stop("theta and length must be non-empty.", call. = FALSE)
  if (n == 1L) return(rep(theta[1], length_))
  vapply(seq_len(length_) - 1L, function(i) {
    pos <- if (length_ > 1L) (i * (n - 1)) / (length_ - 1) else 0
    lo <- floor(pos)
    hi <- min(lo + 1, n - 1)
    w <- pos - lo
    theta[lo + 1L] + (theta[hi + 1L] - theta[lo + 1L]) * w
  }, numeric(1))
}

#' Autoformer series decomposition block, eq. (1)
#'
#' Quoted from the paper: "X_t = AvgPool(Padding(X)), X_s = X - X_t"
#' -- Wu, H., Xu, J., Wang, J. and Long, M., Autoformer:
#' Decomposition Transformers with Auto-Correlation for Long-Term
#' Series Forecasting, NeurIPS 2021 (arXiv:2106.13008), eq. (1).
#' @param x the series
#' @param kernel moving-average kernel size
#' @return list(trend, seasonal, kernel, n, trendmean, seasmean,
#'   seasrange)
#' @export
morie_seriesdecomp <- function(x, kernel) {
  v <- .morie_jo_vec(x)
  kernel <- as.integer(kernel)
  if (kernel < 1L) stop("kernel must be at least 1.", call. = FALSE)
  half <- kernel %/% 2L
  pad <- c(rep(v[1], half), v, rep(v[length(v)], kernel - 1L - half))
  trend <- vapply(seq_along(v), function(i) mean(pad[i:(i + kernel - 1L)]),
                  numeric(1))
  seas <- v - trend
  list(trend = trend, seasonal = seas, kernel = kernel, n = length(v),
       trendmean = mean(trend), seasmean = mean(seas),
       seasrange = max(seas) - min(seas))
}

#' Autoformer decomposition plus Auto-Correlation, eqs. (1), (5), (6)
#'
#' Quoted from the paper: (5) "R_XX(tau) = lim (1/L) sum_t X_t
#' X_{t-tau}"; (6) "tau_1..tau_k = arg Topk(R_{Q,K}(tau))" with
#' "k = floor(c x log L)", "Rhat = SoftMax(R(tau_1)..R(tau_k))",
#' "Auto-Correlation(Q,K,V) = sum_i Roll(V, tau_i) Rhat(tau_i)"
#' -- Wu, Xu, Wang and Long, NeurIPS 2021 (arXiv:2106.13008). Ties in
#' the Topk are broken by the smaller lag, so selection is
#' deterministic.
#' @param q,k,v the query, key and value series
#' @param kernel decomposition kernel size
#' @param c the Topk constant
#' @return list(out, taus, weights, k, L, r1, outmean, outmax,
#'   trendmean, seasrange)
#' @export
morie_autoform <- function(q, k, v, kernel = 3L, c = 1) {
  qv <- .morie_jo_vec(q, "q"); kv <- .morie_jo_vec(k, "k")
  vv <- .morie_jo_vec(v, "v")
  if (length(kv) != length(qv) || length(vv) != length(qv)) {
    stop("q, k and v must be the same length.", call. = FALSE)
  }
  L <- length(qv)
  dec <- morie_seriesdecomp(qv, kernel)
  r <- vapply(0:(L - 1L), function(tau) {
    idx <- ((seq_len(L) - 1L - tau) %% L) + 1L
    sum(qv * kv[idx]) / L
  }, numeric(1))
  kk <- if (L > 1L) floor(c * log(L)) else 1
  kk <- max(1L, min(as.integer(kk), L - 1L))
  cand <- 1:(L - 1L)
  ord <- cand[order(-r[cand + 1L], cand)][seq_len(kk)]
  taus <- sort(ord)
  weights <- .morie_jo_softmax(r[taus + 1L])
  out <- numeric(L)
  for (i in seq_along(taus)) {
    idx <- ((seq_len(L) - 1L - taus[i]) %% L) + 1L
    out <- out + weights[i] * vv[idx]
  }
  list(out = out, taus = taus, weights = weights, k = kk, L = L,
       r1 = if (L > 1L) r[2] else NaN, outmean = mean(out), outmax = max(out),
       trendmean = dec$trendmean, seasrange = dec$seasrange)
}

#' PatchTST patching with reversible instance normalization
#'
#' Quoted from the paper: "N = floor((L - P)/S) + 2", with "S repeated
#' numbers of the last value" padded before patching; each series is
#' normalized to zero mean and unit standard deviation;
#' channel-independence means a multivariate series is "split to M
#' univariate series ... each of them is fed independently into the
#' Transformer backbone" -- Nie, Y., Nguyen, N. H., Sinthong, P. and
#' Kalagnanam, J., A Time Series is Worth 64 Words, ICLR 2023
#' (arXiv:2211.14730), sec. 3.1.
#' @param x a series, or a list of channels
#' @param patchlen patch length P
#' @param stride stride S
#' @param eps variance floor for the instance normalization
#' @return list(patches, npatches, n, patchlen, stride, nchannels,
#'   mean, sd, patchmean, patchsumsq)
#' @export
morie_patchts <- function(x, patchlen, stride, eps = 1e-5) {
  chans <- if (is.list(x)) lapply(x, .morie_jo_vec) else list(.morie_jo_vec(x))
  P <- as.integer(patchlen); S <- as.integer(stride)
  if (P < 1L || S < 1L) stop("patchlen and stride must be positive.", call. = FALSE)
  L <- length(chans[[1]])
  if (any(vapply(chans, length, integer(1)) != L)) {
    stop("all channels must be the same length.", call. = FALSE)
  }
  if (L < P) stop("series is shorter than one patch.", call. = FALSE)
  N <- (L - P) %/% S + 2L
  allp <- list()
  stats <- list()
  for (ci in seq_along(chans)) {
    cv <- chans[[ci]]
    m <- mean(cv)
    sd <- sqrt(sum((cv - m)^2) / length(cv) + eps)
    z <- (cv - m) / sd
    padded <- c(z, rep(z[length(z)], S))
    ps <- list()
    for (i in seq_len(N) - 1L) {
      s <- i * S
      if (s + P > length(padded)) break
      ps[[length(ps) + 1L]] <- padded[(s + 1L):(s + P)]
    }
    allp[[ci]] <- ps
    stats[[ci]] <- c(m, sd)
  }
  flat <- unlist(allp)
  list(patches = allp, npatches = length(allp[[1]]), n = N, patchlen = P,
       stride = S, nchannels = length(chans), mean = stats[[1]][1],
       sd = stats[[1]][2], patchmean = mean(flat),
       patchsumsq = sum(flat * flat))
}

#' N-HiTS multi-rate sampling with hierarchical interpolation
#'
#' Quoted from the paper: (1) "y^(p)_l = MaxPool(y_l, k_l)"; (2)
#' "h_l = MLP_l(y^(p)_l); theta^f_l = LINEAR^f(h_l); theta^b_l =
#' LINEAR^b(h_l)"; (3) "yhat_{tau,l} = g(tau, theta^f_l)" with
#' "|theta^f_l| = ceil(r_l H)"; (4) "g(tau, theta) = theta[t1] +
#' ((theta[t2]-theta[t1])/(t2-t1))(tau-t1)"; doubly residual stacking
#' "yhat = sum_l yhat_l; y_{l+1} = y_l - ytilde_l" -- Challu, C.,
#' Olivares, K. G., Oreshkin, B. N., Garza, F., Mergenthaler-Canseco,
#' M. and Dubrawski, A., N-HiTS, AAAI 2023 (arXiv:2201.12886).
#' wf[[l]] and wb[[l]] stand in for MLP_l followed by LINEAR: a single
#' caller-supplied linear map. That collapse is stated rather than
#' hidden; the expressivity ratio r_l still governs the coefficient
#' count, which is the hierarchical part the paper is about.
#' @param y the lookback window
#' @param horizon forecast length H
#' @param kernels per-block pooling kernels k_l
#' @param ratios per-block expressivity ratios r_l
#' @param wf,wb per-block forecast and backcast weight matrices
#' @return list(forecast, residual, nblocks, sizes, first, last, mean,
#'   residnorm)
#' @export
morie_nhitsnet <- function(y, horizon, kernels, ratios, wf, wb) {
  v <- .morie_jo_vec(y, "y")
  H <- as.integer(horizon)
  if (H < 1L) stop("horizon must be at least 1.", call. = FALSE)
  ks <- as.integer(kernels); rs <- as.numeric(ratios)
  if (length(ks) == 0L || length(ks) != length(rs) ||
      length(ks) != length(wf) || length(ks) != length(wb)) {
    stop("kernels, ratios, wf and wb must line up.", call. = FALSE)
  }
  resid <- v
  fc <- numeric(H)
  sizes <- integer(0)
  for (l in seq_along(ks)) {
    pooled <- .morie_jo_maxpool(resid, ks[l])
    need <- ceiling(rs[l] * H)
    if (need < 1) stop("ratio gives no coefficients.", call. = FALSE)
    thf <- .morie_jo_matvec(wf[[l]], pooled)
    thb <- .morie_jo_matvec(wb[[l]], pooled)
    if (length(thf) != need) {
      stop("wf must produce ceil(r_l H) coefficients.", call. = FALSE)
    }
    sizes <- c(sizes, as.integer(need))
    fc <- fc + .morie_jo_interp(thf, H)
    resid <- resid - .morie_jo_interp(thb, length(resid))
  }
  list(forecast = fc, residual = resid, nblocks = length(ks), sizes = sizes,
       first = fc[1], last = fc[H], mean = mean(fc),
       residnorm = sqrt(sum(resid * resid)))
}

#' .morie_jo_glu
#'
#' A step of the ts_joseph implementation. Called by \code{morie_tftnet}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gamma Passed to \code{.morie_jo_matvec}.
#' @param w4 Passed to \code{.morie_jo_matvec}.
#' @param b4 Numeric; combined arithmetically in the body.
#' @param w5 Passed to \code{.morie_jo_matvec}.
#' @param b5 Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_jo_glu <- function(gamma, w4, b4, w5, b5) {
  a <- .morie_jo_matvec(w4, gamma) + b4
  b <- .morie_jo_matvec(w5, gamma) + b5
  .morie_jo_sigmoid(a) * b
}

#' Temporal Fusion Transformer gating and variable selection
#'
#' Quoted from the paper: (2)-(4) "GRN_omega(a, c) = LayerNorm(a +
#' GLU_omega(eta_1))" with eta_2 = ELU(W_2 a + W_3 c + b_2); (5)
#' "GLU_omega(gamma) = sigma(W_4 gamma + b_4) * (W_5 gamma + b_5)";
#' (6) "v_chi_t = Softmax(GRN_v_chi(Xi_t, c_s))"; (23) "yhat(q,t,tau)
#' = W_q psitilde(t,tau) + b_q"; (25) "QL(y, yhat, q) = q(y - yhat)_+ +
#' (1 - q)(yhat - y)_+" -- Lim, B., Arik, S. O., Loeff, N. and
#' Pfister, T., Temporal Fusion Transformers, International Journal of
#' Forecasting 37(4):1748-1764 (arXiv:1912.09363). All weights are
#' caller-supplied.
#' @param a the input vector
#' @param w1,b1,w2,b2 the GRN linear maps
#' @param w4,b4,w5,b5 the GLU linear maps of eq. (5)
#' @param wsel,bsel the variable-selection map of eq. (6)
#' @param wq,bq the quantile output map of eq. (23)
#' @param c optional static context of eq. (3)
#' @param wc projection for the static context
#' @param y optional observations, to evaluate eq. (25)
#' @param q the quantile level
#' @return list(grn, gate, weights, yhat, topvar, maxweight, entropy,
#'   grnnorm, yhatmean, and ql when y is supplied)
#' @export
morie_tftnet <- function(a, w1, b1, w2, b2, w4, b4, w5, b5, wsel, bsel,
                         wq, bq, c = NULL, wc = NULL, y = NULL, q = 0.5) {
  av <- .morie_jo_vec(a, "a")
  eta2 <- .morie_jo_matvec(w2, av) + b2
  if (!is.null(c)) {
    if (is.null(wc)) stop("wc is required when c is given.", call. = FALSE)
    eta2 <- eta2 + .morie_jo_matvec(wc, .morie_jo_vec(c, "c"))
  }
  eta2 <- .morie_jo_elu(eta2)
  eta1 <- .morie_jo_matvec(w1, eta2) + b1
  gated <- .morie_jo_glu(eta1, w4, b4, w5, b5)
  grn <- .morie_jo_ln(av + gated)
  sel <- .morie_jo_softmax(.morie_jo_matvec(wsel, grn) + bsel)
  yhat <- .morie_jo_matvec(wq, grn) + bq
  pos <- sel[sel > 0]
  out <- list(grn = grn, gate = gated, weights = sel, yhat = yhat,
              topvar = as.integer(which.max(sel) - 1L), maxweight = max(sel),
              entropy = -sum(pos * log(pos)),
              grnnorm = sqrt(sum(grn * grn)), yhatmean = mean(yhat))
  if (!is.null(y)) out$ql <- morie_pinball(.morie_jo_vec(y, "y"), yhat, q)$loss
  out
}

#' .morie_jo_resblock
#'
#' A step of the ts_joseph implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.morie_jo_matvec}.
#' @param w1 Passed to \code{.morie_jo_matvec}.
#' @param b1 Numeric; combined arithmetically in the body.
#' @param w2 Passed to \code{.morie_jo_matvec}.
#' @param b2 Numeric; combined arithmetically in the body.
#' @param wskip Passed to \code{.morie_jo_matvec}.
#' @return The value of \code{.morie_jo_ln}.
#' @export
.morie_jo_resblock <- function(x, w1, b1, w2, b2, wskip) {
  h <- .morie_jo_relu(.morie_jo_matvec(w1, x) + b1)
  out <- .morie_jo_matvec(w2, h) + b2
  .morie_jo_ln(out + .morie_jo_matvec(wskip, x))
}

#' TiDE dense encoder-decoder
#'
#' Quoted from the paper: (3) "xtilde_{i,t} = ResidualBlock(x_{i,t})";
#' (4) "e^(i) = Encoder(y^i; xtilde^i; a^(i))"; (5) "g^(i) =
#' Decoder(e^(i))"; (6) "D^(i) = Reshape(g^(i))"; "yhat^i_{L+t} =
#' TemporalDecoder(d_{i,t}; xtilde^i_{L+t})" -- Das, A., Kong, W.,
#' Leach, A., Mathur, S., Sen, R. and Yu, R., Long-term Forecasting
#' with TiDE, TMLR 2023 (arXiv:2304.08424). The paper states the
#' residual block and the global linear residual in prose rather than
#' as numbered equations; both are implemented from that prose.
#' @param y the lookback window
#' @param feats list of covariate vectors, or NULL
#' @param fproj,enc,dec,tdec residual blocks, each
#'   list(w1, b1, w2, b2, wskip)
#' @param wglobal global linear map from lookback to horizon
#' @param horizon forecast length
#' @return list(forecast, temporal, global, horizon, p, encdim, nfeat,
#'   first, last, mean)
#' @export
morie_tide <- function(y, feats, fproj, enc, dec, tdec, wglobal, horizon) {
  v <- .morie_jo_vec(y, "y")
  H <- as.integer(horizon)
  if (H < 1L) stop("horizon must be at least 1.", call. = FALSE)
  proj <- if (is.null(feats)) list() else
    lapply(feats, function(f) do.call(.morie_jo_resblock,
                                      c(list(.morie_jo_vec(f, "feature")), fproj)))
  flat <- v
  for (pr in proj) flat <- c(flat, pr)
  e <- do.call(.morie_jo_resblock, c(list(flat), enc))
  g <- do.call(.morie_jo_resblock, c(list(e), dec))
  if (length(g) %% H != 0L) {
    stop("decoder output length must be a multiple of horizon.", call. = FALSE)
  }
  p <- length(g) %/% H
  temporal <- vapply(seq_len(H), function(t) {
    d <- g[((t - 1L) * p + 1L):(t * p)]
    o <- do.call(.morie_jo_resblock, c(list(d), tdec))
    if (length(o) != 1L) stop("tdec must produce one value per step.", call. = FALSE)
    o[1]
  }, numeric(1))
  glob <- .morie_jo_matvec(wglobal, v)
  if (length(glob) != H) {
    stop("wglobal must map the lookback to the horizon.", call. = FALSE)
  }
  out <- temporal + glob
  list(forecast = out, temporal = temporal, global = glob, horizon = H,
       p = p, encdim = length(e), nfeat = length(proj), first = out[1],
       last = out[H], mean = mean(out))
}

#' TSMixer time-mixing and feature-mixing, all-MLP
#'
#' Quoted from the paper: (4) "TP_{L->T}(X)_{*,i} = W_1 X_{*,i} + b_1";
#' (5) "TM(X)_{*,i} = Norm(X_{*,i} + Drop(sigma(TP_{L->L}(X)_{*,i})))"
#' -- Chen, S.-A., Li, C.-L., Yoder, N. C., Arik, S. O. and Pfister,
#' T., TSMixer: An All-MLP Architecture for Time Series Forecasting,
#' TMLR 2023 (arXiv:2303.06053), Appendix B.3.1. Feature mixing and
#' the 2D normalization are described in prose and implemented from it.
#' Dropout is omitted: it is a training-time stochastic operation and
#' this routine is evaluation-time and deterministic.
#' @param x list of C channels, each of length L
#' @param wtime,btime the time-mixing MLP
#' @param wfeat,bfeat the feature-mixing MLP
#' @param wproj,bproj the temporal projection of eq. (4)
#' @param horizon forecast length
#' @return list(forecast, mixed, nchannels, L, horizon, mean, first,
#'   last, sumsq)
#' @export
morie_tsmixer <- function(x, wtime, btime, wfeat, bfeat, wproj, bproj, horizon) {
  chans <- lapply(x, .morie_jo_vec)
  C <- length(chans)
  if (C < 1L) stop("need at least one channel.", call. = FALSE)
  L <- length(chans[[1]])
  if (any(vapply(chans, length, integer(1)) != L)) {
    stop("all channels must be the same length.", call. = FALSE)
  }
  H <- as.integer(horizon)
  mixed <- lapply(chans, function(cv) {
    h <- .morie_jo_relu(.morie_jo_matvec(wtime, cv) + btime)
    .morie_jo_ln(cv + h)
  })
  out <- lapply(seq_len(C), function(i) numeric(L))
  for (t in seq_len(L)) {
    col <- vapply(mixed, function(m) m[t], numeric(1))
    h <- .morie_jo_relu(.morie_jo_matvec(wfeat, col) + bfeat)
    newcol <- .morie_jo_ln(col + h)
    for (i in seq_len(C)) out[[i]][t] <- newcol[i]
  }
  preds <- lapply(out, function(o) .morie_jo_matvec(wproj, o) + bproj)
  if (any(vapply(preds, length, integer(1)) != H)) {
    stop("wproj must map L to the horizon.", call. = FALSE)
  }
  flat <- unlist(preds)
  list(forecast = preds, mixed = out, nchannels = C, L = L, horizon = H,
       mean = mean(flat), first = preds[[1]][1], last = preds[[C]][H],
       sumsq = sum(flat * flat))
}

#' iTransformer: variates as tokens, attention across variates
#'
#' Quoted from the paper: (1) "h^0_n = Embedding(X_{:,n}); H^{l+1} =
#' TrmBlock(H^l); Yhat_{:,n} = Projection(h^L_n)"; (2) "LayerNorm(H) =
#' {[h_n - Mean(h_n)]/sqrt(Var(h_n)) | n = 1..N}"; attention scores
#' "A_{i,j} = (Q K^T / sqrt(d_k))_{i,j}" -- Liu, Y., Hu, T., Zhang, H.,
#' Wu, H., Wang, S., Ma, L. and Long, M., iTransformer, ICLR 2024
#' (arXiv:2310.06625). The inversion is the point: each VARIATE series
#' becomes one token, so attention is N x N over variates rather than
#' T x T over time.
#' @param x list of N variate series, each of length T
#' @param wembed,bembed the embedding map
#' @param wq,wk,wv the attention projections
#' @param wffn1,bffn1,wffn2,bffn2 the feed-forward network
#' @param wproj,bproj the output projection
#' @return list(forecast, attn, tokens, nvariates, T, D, horizon,
#'   attndiag, mean, first, sumsq)
#' @export
morie_itrans <- function(x, wembed, bembed, wq, wk, wv, wffn1, bffn1,
                         wffn2, bffn2, wproj, bproj) {
  chans <- lapply(x, .morie_jo_vec)
  N <- length(chans)
  if (N < 1L) stop("need at least one variate.", call. = FALSE)
  Tn <- length(chans[[1]])
  if (any(vapply(chans, length, integer(1)) != Tn)) {
    stop("all variates must be the same length.", call. = FALSE)
  }
  toks <- lapply(chans, function(cv) .morie_jo_ln(.morie_jo_matvec(wembed, cv) + bembed))
  D <- length(toks[[1]])
  Q <- lapply(toks, function(t) .morie_jo_matvec(wq, t))
  K <- lapply(toks, function(t) .morie_jo_matvec(wk, t))
  V <- lapply(toks, function(t) .morie_jo_matvec(wv, t))
  dk <- length(Q[[1]])
  attn <- lapply(seq_len(N), function(i) {
    .morie_jo_softmax(vapply(seq_len(N),
                             function(j) sum(Q[[i]] * K[[j]]) / sqrt(dk),
                             numeric(1)))
  })
  ctx <- lapply(seq_len(N), function(i) {
    s <- numeric(length(V[[1]]))
    for (j in seq_len(N)) s <- s + attn[[i]][j] * V[[j]]
    s
  })
  h1 <- lapply(seq_len(N), function(i) .morie_jo_ln(toks[[i]] + ctx[[i]]))
  ffn <- lapply(h1, function(t) {
    u <- .morie_jo_relu(.morie_jo_matvec(wffn1, t) + bffn1)
    .morie_jo_ln(t + (.morie_jo_matvec(wffn2, u) + bffn2))
  })
  preds <- lapply(ffn, function(t) .morie_jo_matvec(wproj, t) + bproj)
  flat <- unlist(preds)
  list(forecast = preds, attn = attn, tokens = ffn, nvariates = N, T = Tn,
       D = D, horizon = length(preds[[1]]),
       attndiag = sum(vapply(seq_len(N), function(i) attn[[i]][i], numeric(1))) / N,
       mean = mean(flat), first = preds[[1]][1], sumsq = sum(flat * flat))
}
