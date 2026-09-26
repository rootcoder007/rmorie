# Prophet: piecewise trend, Fourier seasonality, holidays.
# Sources: Taylor, S. J. & Letham, B. (2018) "Forecasting at Scale",
# The American Statistician 72(1), 37-45, doi:10.1080/00031305.2017.1380080.
# Harvey & Peters (1990) J. Forecasting 9(2) (decomposable structural
# model); Hyndman & Athanasopoulos (2021) FPP3 (Fourier seasonality).
# Native R mirroring morie.fn.prphet exactly: same changepoint schedule,
# same continuity gamma_j = -s_j delta_j carried as the (t - s_j)+ column,
# same Fourier seasonality, same per-holiday indicator, same cyclic
# coordinate descent with the Laplace L1 penalty on the deltas only.

.prphet_EPS <- 1e-12

#' .changepoints
#'
#' A step of the prphet_native implementation. Called by \code{morie_prphet_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param n.cp Coerced to integer by the body, with \code{as.integer}.
#' @param range Numeric; combined arithmetically in the body. Defaults to \code{0.8}.
#' @param cps Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.changepoints <- function(t, n.cp, range = 0.8, cps = NULL) {
  if (!is.null(cps)) return(as.numeric(cps))
  n <- length(t)
  hi <- t[1] + range * (t[length(t)] - t[1])
  m <- as.integer(n.cp)
  if (m < 1L) return(numeric(0))
  step <- (hi - t[1]) / (m + 1L)
  t[1] + step * seq_len(m)
}

#' morie_prphet_piecewise_trend
#'
#' A step of the prphet_native implementation. Called by
#' \code{.prnFil_simulate_future_trend}, \code{morie_prphet_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param k.rate Numeric; combined arithmetically in the body.
#' @param m.off Numeric; combined arithmetically in the body.
#' @param deltas Numeric; combined arithmetically in the body.
#' @param cps Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' morie_prphet_piecewise_trend(t = c(1, 2, 3, 4, 5, 6, 7, 8), k.rate = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   m.off = c(1, 2, 3, 4, 5, 6, 7, 8), deltas = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   cps = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
morie_prphet_piecewise_trend <- function(t, k.rate, m.off, deltas, cps) {
  out <- numeric(length(t))
  for (i in seq_along(t)) {
    tv <- t[i]
    a <- ifelse(tv >= cps, 1, 0)
    rate <- k.rate + sum(a * deltas)
    off <- m.off + sum(a * (-cps * deltas))
    out[i] <- rate * tv + off
  }
  out
}

#' morie_prphet_trend_matrix
#'
#' A step of the prphet_native implementation. Called by \code{morie_prphet_design}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken.
#' @param cps A vector; its length is taken and its elements indexed.
#' @return The value of \code{rows}, as built in the body.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_prphet_trend_matrix(V, V)
#' @keywords internal
morie_prphet_trend_matrix <- function(t, cps) {
  rows <- matrix(0, nrow = length(t), ncol = 2L + length(cps))
  rows[, 1] <- t
  rows[, 2] <- 1
  if (length(cps) > 0L) {
    for (j in seq_along(cps)) {
      rows[, 2L + j] <- ifelse(t >= cps[j], t - cps[j], 0)
    }
  }
  rows
}

#' morie_prphet_fourier_terms
#'
#' A step of the prphet_native implementation. Called by \code{morie_prphet_design},
#' \code{prophe_additive_components}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param period Numeric; combined arithmetically in the body.
#' @param order Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{rows}, as built in the body.
#' @export
#' @examples
#' morie_prphet_fourier_terms(t = c(1, 2, 3, 4, 5, 6, 7, 8), period = 5L, order = 5L)
#' @keywords internal
morie_prphet_fourier_terms <- function(t, period, order) {
  if (period <= 0) stop("prphet: period must be positive")
  if (order < 1L) stop("prphet: order must be at least 1")
  ord <- as.integer(order)
  rows <- matrix(0, nrow = length(t), ncol = 2L * ord)
  for (i in seq_along(t)) {
    for (n in seq_len(ord)) {
      ang <- 2 * pi * n * t[i] / period
      rows[i, 2L * n - 1L] <- cos(ang)
      rows[i, 2L * n] <- sin(ang)
    }
  }
  rows
}

#' morie_prphet_holiday_matrix
#'
#' A step of the prphet_native implementation. Called by \code{morie_prphet_design},
#' \code{prophe_additive_components}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param holidays A vector; indexed elementwise.
#' @param lower Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param upper Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A list with \code{rows}, \code{names}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_prphet_holiday_matrix(V, V)
#' @keywords internal
morie_prphet_holiday_matrix <- function(t, holidays, lower = 0, upper = 0) {
  names.v <- sort(names(holidays))
  rows <- matrix(0, nrow = length(t), ncol = length(names.v))
  for (i in seq_along(t)) {
    tv <- t[i]
    for (k in seq_along(names.v)) {
      ds <- holidays[[names.v[k]]]
      hit <- any(tv >= ds - lower & tv <= ds + upper)
      rows[i, k] <- as.numeric(hit)
    }
  }
  list(rows = rows, names = names.v)
}

#' morie_prphet_design
#'
#' A step of the prphet_native implementation. Called by \code{morie_prphet_fit},
#' \code{morie_prphet_predict}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Passed to \code{morie_prphet_trend_matrix}.
#' @param cps A vector; its length is taken.
#' @param seasonalities Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param holidays Optional; may be \code{NULL}. A vector; its length is taken.
#' @param holiday_window A vector; indexed elementwise. Defaults to \code{c(0, 0)}.
#' @return A list with \code{X}, \code{cols}, \code{holiday.names}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_prphet_design(V, V)
#' @keywords internal
morie_prphet_design <- function(t, cps, seasonalities = NULL, holidays = NULL,
                                holiday_window = c(0, 0)) {
  tm <- morie_prphet_trend_matrix(t, cps)
  cols <- c("k", "m", paste0("delta_", seq_along(cps) - 1L))
  blocks <- list(tm)
  seas <- if (is.null(seasonalities)) list() else seasonalities
  for (s in seas) {
    nm <- s[[1]]
    per <- s[[2]]
    ord <- s[[3]]
    blocks[[length(blocks) + 1L]] <- morie_prphet_fourier_terms(t, per, ord)
    for (n in seq_len(as.integer(ord))) {
      cols <- c(cols, paste0(nm, "_cos", n), paste0(nm, "_sin", n))
    }
  }
  hn <- character(0)
  if (!is.null(holidays) && length(holidays) > 0L) {
    hm <- morie_prphet_holiday_matrix(t, holidays, holiday_window[1],
                                      holiday_window[2])
    blocks[[length(blocks) + 1L]] <- hm$rows
    hn <- hm$names
    cols <- c(cols, paste0("holiday_", hn))
  }
  X <- do.call(cbind, blocks)
  list(X = X, cols = cols, holiday.names = hn)
}

#' morie_prphet_fit
#'
#' A step of the prphet_native implementation. Called by \code{.prnFil_changepoint_path},
#' \code{.prnFil_select_changepoints}, \code{prophe_additive_components}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_changepoints Passed to \code{.changepoints}. Defaults to \code{10L}.
#' @param changepoint_range Passed to \code{.changepoints}. Defaults to \code{0.8}.
#' @param changepoints Passed to \code{.changepoints}.
#' @param seasonalities Optional; may be \code{NULL}. Passed to \code{morie_prphet_design}.
#' @param holidays Passed to \code{morie_prphet_design}.
#' @param holiday_window Passed to \code{morie_prphet_design}. Defaults to \code{c(0, 0)}.
#' @param changepoint_prior Coerced to numeric by the body, with \code{as.numeric}.
#' Defaults to \code{0.05}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @return A list with \code{estimate}, \code{fitted}, \code{residual}, \code{coef},
#' \code{beta}, \code{columns}, \code{changepoints}, \code{deltas}, \code{k}, \code{m},
#' \code{trend}, \code{holiday.names}, \code{t}, \code{n}, \code{changepoint_prior},
#' \code{n.active.changepoints}, \code{sigma}, \code{seasonalities}, \code{method}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_prphet_fit(V, V)
#' @keywords internal
morie_prphet_fit <- function(t, y, n_changepoints = 10L, changepoint_range = 0.8,
                             changepoints = NULL, seasonalities = NULL,
                             holidays = NULL, holiday_window = c(0, 0),
                             changepoint_prior = 0.05, ridge = 1e-8) {
  tv <- as.numeric(t)
  yv <- as.numeric(y)
  n <- length(tv)
  if (length(yv) != n)
    stop(paste0("prphet: ", n, " times but ", length(yv), " observations"))
  if (n < 8L) stop(paste0("prphet: need at least 8 observations, got ", n))
  tau <- as.numeric(changepoint_prior)
  if (tau <= 0)
    stop(paste0("prphet: changepoint_prior must be positive, got ",
                tau))
  cps <- .changepoints(tv, n_changepoints, changepoint_range, changepoints)
  des <- morie_prphet_design(tv, cps, seasonalities, holidays, holiday_window)
  X <- des$X
  cols <- des$cols
  p <- length(cols)
  pen <- rep(0, p)
  for (j in seq_along(cols)) {
    if (substr(cols[j], 1, 6) == "delta_") pen[j] <- 1 / tau
  }
  XtX <- crossprod(X)
  Xty <- as.numeric(crossprod(X, yv))
  beta <- rep(0, p)
  for (it in seq_len(400L)) {
    shift <- 0
    for (a in seq_len(p)) {
      gaa <- XtX[a, a] + ridge
      if (gaa <= 0) next
      r <- Xty[a] - sum(XtX[a, -a] * beta[-a])
      nb <- if (pen[a] > 0) {
        if (abs(r) <= pen[a]) 0
        else (r - sign(r) * pen[a]) / gaa
      } else r / gaa
      if (abs(nb - beta[a]) > shift) shift <- abs(nb - beta[a])
      beta[a] <- nb
    }
    if (shift < 1e-12) break
  }
  fitted <- as.numeric(X %*% beta)
  resid <- yv - fitted
  named <- as.list(beta)
  names(named) <- cols
  deltas <- vapply(seq_along(cps) - 1L, function(j) named[[paste0("delta_", j)]],
                   numeric(1))
  list(estimate = fitted, fitted = fitted, residual = resid,
       coef = named, beta = beta, columns = cols,
       changepoints = cps, deltas = deltas,
       k = named[["k"]], m = named[["m"]],
       trend = morie_prphet_piecewise_trend(tv, named[["k"]], named[["m"]],
                                            deltas, cps),
       holiday.names = des$holiday.names, t = tv, n = n,
       changepoint_prior = tau,
       n.active.changepoints = sum(deltas != 0),
       sigma = sqrt(sum(resid^2) / max(n - p, 1)),
       seasonalities = if (is.null(seasonalities)) character(0)
         else vapply(seasonalities, function(s) s[[1]], character(1)),
       method = "Prophet decomposable model, Taylor & Letham (2018) eq. (1) and (4)")
}

#' morie_prphet_predict
#'
#' A step of the prphet_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$beta}, \code{$changepoints}, \code{$columns} from it.
#' @param t.new Coerced to numeric by the body, with \code{as.numeric}.
#' @param seasonalities Passed to \code{morie_prphet_design}.
#' @param holidays Passed to \code{morie_prphet_design}.
#' @param holiday_window Passed to \code{morie_prphet_design}. Defaults to \code{c(0, 0)}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @keywords internal
morie_prphet_predict <- function(fit, t.new, seasonalities = NULL,
                                 holidays = NULL, holiday_window = c(0, 0)) {
  tn <- as.numeric(t.new)
  des <- morie_prphet_design(tn, fit$changepoints, seasonalities, holidays,
                              holiday_window)
  if (!identical(des$cols, fit$columns))
    stop("prphet: the prediction design does not match the fitted one; pass the same seasonalities and holidays")
  as.numeric(des$X %*% fit$beta)
}

# house entry point: the package exports one morie_<module>
morie_prphet <- morie_prphet_piecewise_trend

# -- restored: morie-only definition kept through the rmorie sync --
#' Cosine and sine Fourier pairs, exactly periodic with \code{period}
#'
#' @param t Numeric vector of times.
#' @param period Positive period.
#' @param order Integer, number of harmonics.
#' @return A list of rows, each \code{2 * order} long.
#' @export
fourier_terms <- function(t, period, order) {
  if (period <= 0)
    stop(sprintf("prphet: period must be positive, got %r", period))
  order <- as.integer(order)
  if (order < 1L)
    stop(sprintf("prphet: order must be at least 1, got %d", order))
  rows <- list()
  for (tv in t) {
    row <- numeric(0)
    for (n in seq_len(order)) {
      ang <- 2.0 * pi * n * tv / period
      row <- c(row, cos(ang), sin(ang))
    }
    rows[[length(rows) + 1L]] <- row
  }
  rows
}

# -- restored: morie-only definition kept through the rmorie sync --
#' One indicator per holiday, optionally widened by a window
#'
#' @param t Numeric vector of times.
#' @param holidays Named list of dates per holiday.
#' @param lower Integer, days before the holiday to flag.
#' @param upper Integer, days after the holiday to flag.
#' @return A list with \code{matrix} (list of rows) and \code{names}
#'   (sorted holiday names).
#' @export
holiday_matrix <- function(t, holidays, lower = 0, upper = 0) {
  names_ <- sort(names(holidays))
  rows <- list()
  for (tv in t) {
    row <- numeric(0)
    for (nm in names_) {
      hit <- 0.0
      for (d in holidays[[nm]]) {
        if (d - lower <= tv && tv <= d + upper) {
          hit <- 1.0
          break
        }
      }
      row <- c(row, hit)
    }
    rows[[length(rows) + 1L]] <- row
  }
  list(matrix = rows, names = names_)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Evaluate the piecewise-linear trend (Eq. 4 of Taylor & Letham 2018)
#'
#' The offsets carry \code{gamma_j = -s_j delta_j} so the segments join
#' by construction.
#'
#' @param t Numeric vector of times.
#' @param k_rate Initial rate.
#' @param m_off Initial offset.
#' @param deltas Numeric vector of rate adjustments, one per changepoint.
#' @param cps Numeric vector of changepoints.
#' @return Numeric vector of trend values.
#' @export
piecewise_trend <- function(t, k_rate, m_off, deltas, cps) {
  out <- numeric(length(t))
  for (i in seq_along(t)) {
    tv <- t[i]
    a <- ifelse(tv >= cps, 1.0, 0.0)
    rate <- k_rate + sum(a * deltas)
    off <- m_off + sum(a * (-cps * deltas))
    out[i] <- rate * tv + off
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Stack trend, seasonality and holiday columns into one design
#'
#' @param t Numeric vector of times.
#' @param cps Numeric vector of changepoints.
#' @param seasonalities Optional list of \code{c(name, period, order)}.
#' @param holidays Optional named list of dates per holiday.
#' @param holiday_window \code{c(lower, upper)} window around each date.
#' @return A list with \code{X} (list of rows), \code{cols}, \code{hn}.
#' @export
prophet_design <- function(t, cps, seasonalities = NULL, holidays = NULL,
                           holiday_window = c(0, 0)) {
  tm <- trend_matrix(t, cps)
  cols <- c("k", "m", paste0("delta_", seq_along(cps) - 1L))
  blocks <- list(tm)
  seas <- seasonalities
  if (!is.null(seas)) {
    for (s in seas) {
      blocks[[length(blocks) + 1L]] <-
        fourier_terms(t, s[[2L]], s[[3L]])
      for (n in seq_len(as.integer(s[[3L]]))) {
        cols <- c(cols, paste0(s[[1L]], "_cos", n),
                  paste0(s[[1L]], "_sin", n))
      }
    }
  }
  hn <- character(0)
  if (!is.null(holidays)) {
    hm <- holiday_matrix(t, holidays, holiday_window[1L],
                         holiday_window[2L])
    blocks[[length(blocks) + 1L]] <- hm$matrix
    hn <- hm$names
    cols <- c(cols, paste0("holiday_", hn))
  }
  X <- lapply(seq_along(t), function(i)
    unlist(lapply(blocks, function(b) b[[i]])))
  list(X = X, cols = cols, hn = hn)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Forecast at new times, reusing the fitted coefficients
#'
#' @param fit A fit object as returned by \code{\link{morie_prphet_fit}}.
#' @param t_new Numeric vector of new times.
#' @param seasonalities Same as for the fit.
#' @param holidays Same as for the fit.
#' @param holiday_window Same as for the fit.
#' @return Numeric vector of forecasts.
#' @export
prophet_predict <- function(fit, t_new, seasonalities = NULL,
                            holidays = NULL, holiday_window = c(0, 0)) {
  tn <- as.numeric(t_new)
  ds <- prophet_design(tn, fit$changepoints, seasonalities, holidays,
                       holiday_window)
  if (!identical(ds$cols, fit$columns))
    stop("prphet: the prediction design does not match the fitted one; ",
         "pass the same seasonalities and holidays")
  X <- ds$X
  beta <- fit$beta
  vapply(seq_along(tn), function(i)
    sum(X[[i]] * beta), numeric(1))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' prphet cheatsheet
#'
#' One-paragraph summary of the method and the traps in using it.
#' @return A character string.
#' @examples
#' prphet_cheatsheet()
#' @export
prphet_cheatsheet <- function() {
  paste0("prphet: y = g(t) + s(t) + h(t) + eps. Trend g = (k + ",
         "a(t)'delta)t + (m + a(t)'gamma) with gamma_j = -s_j ",
         "delta_j -- that is what JOINS the segments; without it the ",
         "curve jumps at every changepoint and least squares hides ",
         "it in the residual. s(t) is a Fourier series, exactly ",
         "periodic. Holidays need their own indicators because they ",
         "move. Penalise the deltas ONLY.")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Design columns for k, m and each delta_j
#'
#' The delta column is \code{a_j(t)(t - s_j)}, which already carries
#' the \code{-s_j delta_j} offset -- so continuity holds by construction.
#'
#' @param t Numeric vector of times.
#' @param cps Numeric vector of changepoints.
#' @return A list of rows, each a numeric vector.
#' @export
trend_matrix <- function(t, cps) {
  rows <- list()
  for (tv in t) {
    row <- c(tv, 1.0)
    for (s in cps) row <- c(row, if (tv >= s) tv - s else 0.0)
    rows[[length(rows) + 1L]] <- row
  }
  rows
}

# -- restored: pre-sync definition (prophet) --
#' @noRd
prophet <- morie_prphet_fit

# -- restored: pre-sync definition (prophetfit) --
#' @noRd
prophetfit <- morie_prphet_fit
