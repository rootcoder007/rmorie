# Prophet: piecewise trend, Fourier seasonality, holidays.
# Sources: Taylor, S. J. & Letham, B. (2018) "Forecasting at Scale",
# The American Statistician 72(1), 37-45, doi:10.1080/00031305.2017.1380080.
# Harvey & Peters (1990) J. Forecasting 9(2) (decomposable structural
# model); Hyndman & Athanasopoulos (2021) FPP3 (Fourier seasonality).
# Native R mirroring morie.fn.prphet exactly: same changepoint schedule,
# same continuity gamma_j = -s_j delta_j carried as the (t - s_j)+ column,
# same Fourier seasonality, same per-holiday indicator, same cyclic
# coordinate descent with the Laplace L1 penalty on the deltas only.

.EPS <- 1e-12

.changepoints <- function(t, n.cp, range = 0.8, cps = NULL) {
  if (!is.null(cps)) return(as.numeric(cps))
  n <- length(t)
  hi <- t[1] + range * (t[length(t)] - t[1])
  m <- as.integer(n.cp)
  if (m < 1L) return(numeric(0))
  step <- (hi - t[1]) / (m + 1L)
  t[1] + step * seq_len(m)
}

morie_prphet_piecewise_trend <- function(t, k.rate, m.off, deltas, cps) {
  out <- numeric(length(t))
  for (i in seq_along(t)) {
    tv <- t[i]; a <- ifelse(tv >= cps, 1, 0)
    rate <- k.rate + sum(a * deltas)
    off <- m.off + sum(a * (-cps * deltas))
    out[i] <- rate * tv + off
  }
  out
}

morie_prphet_trend_matrix <- function(t, cps) {
  rows <- matrix(0, nrow = length(t), ncol = 2L + length(cps))
  rows[, 1] <- t; rows[, 2] <- 1
  if (length(cps) > 0L) {
    for (j in seq_along(cps)) {
      rows[, 2L + j] <- ifelse(t >= cps[j], t - cps[j], 0)
    }
  }
  rows
}

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

morie_prphet_design <- function(t, cps, seasonalities = NULL, holidays = NULL,
                                holiday.window = c(0, 0)) {
  tm <- morie_prphet_trend_matrix(t, cps)
  cols <- c("k", "m", paste0("delta_", seq_along(cps) - 1L))
  blocks <- list(tm)
  seas <- if (is.null(seasonalities)) list() else seasonalities
  for (s in seas) {
    nm <- s[[1]]; per <- s[[2]]; ord <- s[[3]]
    blocks[[length(blocks) + 1L]] <- morie_prphet_fourier_terms(t, per, ord)
    for (n in seq_len(as.integer(ord))) {
      cols <- c(cols, paste0(nm, "_cos", n), paste0(nm, "_sin", n))
    }
  }
  hn <- character(0)
  if (!is.null(holidays) && length(holidays) > 0L) {
    hm <- morie_prphet_holiday_matrix(t, holidays, holiday.window[1],
                                      holiday.window[2])
    blocks[[length(blocks) + 1L]] <- hm$rows
    hn <- hm$names
    cols <- c(cols, paste0("holiday_", hn))
  }
  X <- do.call(cbind, blocks)
  list(X = X, cols = cols, holiday.names = hn)
}

morie_prphet_fit <- function(t, y, n.changepoints = 10L, changepoint.range = 0.8,
                             changepoints = NULL, seasonalities = NULL,
                             holidays = NULL, holiday.window = c(0, 0),
                             changepoint.prior = 0.05, ridge = 1e-8) {
  tv <- as.numeric(t); yv <- as.numeric(y)
  n <- length(tv)
  if (length(yv) != n)
    stop(paste0("prphet: ", n, " times but ", length(yv), " observations"))
  if (n < 8L) stop(paste0("prphet: need at least 8 observations, got ", n))
  tau <- as.numeric(changepoint.prior)
  if (tau <= 0)
    stop(paste0("prphet: changepoint_prior must be positive, got ",
                tau))
  cps <- .changepoints(tv, n.changepoints, changepoint.range, changepoints)
  des <- morie_prphet_design(tv, cps, seasonalities, holidays, holiday.window)
  X <- des$X; cols <- des$cols
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
       changepoint.prior = tau,
       n.active.changepoints = sum(deltas != 0),
       sigma = sqrt(sum(resid^2) / max(n - p, 1)),
       seasonalities = if (is.null(seasonalities)) character(0)
         else vapply(seasonalities, function(s) s[[1]], character(1)),
       method = "Prophet decomposable model, Taylor & Letham (2018) eq. (1) and (4)")
}

morie_prphet_predict <- function(fit, t.new, seasonalities = NULL,
                                 holidays = NULL, holiday.window = c(0, 0)) {
  tn <- as.numeric(t.new)
  des <- morie_prphet_design(tn, fit$changepoints, seasonalities, holidays,
                              holiday.window)
  if (!identical(des$cols, fit$columns))
    stop("prphet: the prediction design does not match the fitted one; pass the same seasonalities and holidays")
  as.numeric(des$X %*% fit$beta)
}

# house entry point: the package exports one morie_<module>
morie_prphet <- morie_prphet_piecewise_trend
