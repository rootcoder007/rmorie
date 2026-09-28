# SPDX-License-Identifier: AGPL-3.0-or-later
.isotn_pava <- function(y, w) {
  v <- numeric(0)
  wt <- numeric(0)
  sz <- integer(0)
  for (i in seq_along(y)) {
    v <- c(v, y[i])
    wt <- c(wt, w[i])
    sz <- c(sz, 1L)
    k <- length(v)
    while (k > 1 && v[k - 1] > v[k]) {
      tw <- wt[k - 1] + wt[k]
      v[k - 1] <- (v[k - 1] * wt[k - 1] + v[k] * wt[k]) / tw
      wt[k - 1] <- tw
      sz[k - 1] <- sz[k - 1] + sz[k]
      v <- v[-k]
      wt <- wt[-k]
      sz <- sz[-k]
      k <- k - 1
    }
  }
  rep(v, sz)
}

#' Isotonic regression via PAVA (Barlow et al. 1972)
#'
#' Returns the non-decreasing (or non-increasing) weighted least-squares fit
#' by the pool-adjacent-violators algorithm (each pooled block at its
#' weighted mean; equal to stats::isoreg for unit weights).
#'
#' @param x numeric predictor.
#' @param y numeric outcome.
#' @param weights optional non-negative weights.
#' @param increasing logical (default TRUE).
#' @return list: x_sorted, fitted, residuals, sse, r2, n, method.
#' @keywords internal
#' @examples
#' x <- 0:9
#' y <- c(1, 3, 2, 5, 4, 6, 7, 8, 7, 10)
#' res <- isotn(x, y)
#' res$fitted
#' @export
isotn <- function(x, y, weights = NULL, increasing = TRUE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  if (n < 2L || length(y) != n) {
    return(list(estimate = NA_real_, n = n, method = "Isotonic (n<2)"))
  }
  if (is.null(weights)) weights <- rep(1, n)
  ord <- order(x)
  xs <- x[ord]
  ys <- y[ord]
  ws <- weights[ord]
  fitted <- .isotn_pava(if (increasing) ys else -ys, ws)
  if (!increasing) fitted <- -fitted
  resid <- ys - fitted
  sse <- sum(ws * resid^2)
  sst <- sum(ws * (ys - stats::weighted.mean(ys, ws))^2)
  r2 <- if (sst > 0) 1 - sse / sst else NA_real_
  list(
    x_sorted = xs, fitted = fitted, residuals = resid,
    sse = sse, r2 = as.numeric(r2),
    estimate = mean(fitted), n = as.integer(n),
    method = "Isotonic regression (Barlow et al. 1972, PAVA)"
  )
}

# CANONICAL TEST
# x <- 0:9; y <- c(1, 3, 2, 5, 4, 6, 7, 8, 7, 10)
# r <- isotn(x, y)
# stopifnot(all(diff(r$fitted) >= -1e-9))

#' @rdname isotn
#' @keywords internal
#' @export
morie_isotonic_regression <- isotn
