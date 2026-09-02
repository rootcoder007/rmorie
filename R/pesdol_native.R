# R arm of pesdol -- ARDL conditional ECM and the Pesaran-Shin-Smith bounds
# test. Pesaran, M. H. & Shin, Y. (1998), Ch. 11 in Econometrics and Economic
# Theory in the 20th Century; Pesaran, Shin & Smith (2001) J. Appl.
# Econometrics 16(3), 289-326. Mirrors src/morie/fn/pesdol.py.

.pesdol_EPS <- 1e-12

#' .pesdol_ols
#'
#' A step of the pesdol_native implementation. Called by \code{morie_pesdol_ardl_bounds}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y A matrix; passed to \code{crossprod}.
#' @return A list with \code{beta}, \code{fit}, \code{res}.
#' @export
.pesdol_ols <- function(X, y) {
  n <- nrow(X)
  p <- ncol(X)
  XtX <- crossprod(X)
  # numerical floor scaled to the matrix, matching the Python arm
  scale <- sum(diag(XtX)) / ncol(X)
  ridge <- if (scale > 1e-300) 1e-8 * scale else 1e-10
  diag(XtX) <- diag(XtX) + ridge
  Xty <- as.numeric(crossprod(X, y))
  Lc <- chol(XtX)
  beta <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), Xty)))
  fit <- as.numeric(X %*% beta)
  list(beta = beta, fit = fit, res = y - fit)
}

#' morie_pesdol_ardl_bounds
#'
#' A step of the pesdol_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param x A matrix; passed to \code{as.matrix}.
#' @param p Numeric; passed to \code{max}. Defaults to \code{1}.
#' @param q Numeric; passed to \code{max}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{long_run}, \code{coefficients}, \code{residuals}, \code{fitted}, \code{speed_of_adjustment}, \code{f_statistic}, \code{bound_lower}, \code{bound_upper}, \code{verdict}, \code{n_used}, \code{n_params}, \code{rss_unrestricted}, \code{rss_restricted}, \code{p}, \code{q}, \code{n_regressors}, \code{method}, \code{note}.
#' @export
morie_pesdol_ardl_bounds <- function(y, x, p = 1, q = 1) {
  yv <- as.numeric(y)
  Xm <- as.matrix(x)
  storage.mode(Xm) <- "double"
  n0 <- length(yv)
  if (n0 != nrow(Xm))
    stop(sprintf("pesdol: %d responses but %d regressor rows", n0, nrow(Xm)))
  m <- ncol(Xm)
  p <- as.integer(p)
  q <- as.integer(q)
  if (p < 1L || q < 0L) stop("pesdol: need p >= 1 and q >= 0")
  start <- max(p, q) + 1L
  if (n0 - start < p + q * m + m + 3L)
    stop(sprintf(paste0("pesdol: too few observations for ARDL(%d, %d) with ",
                        "%d regressors"), p, q, m))

  rows <- list()
  dep <- numeric(0)
  for (t in seq(start + 1L, n0)) {
    r <- c(1.0, yv[t - 1L])
    for (j in seq_len(m)) r <- c(r, Xm[t - 1L, j])
    if (p > 1L) for (i in seq_len(p - 1L))
      r <- c(r, yv[t - i] - yv[t - i - 1L])
    for (j in seq_len(m)) for (l in seq(0L, q))
      r <- c(r, Xm[t - l, j] - Xm[t - l - 1L, j])
    rows[[length(rows) + 1L]] <- r
    dep <- c(dep, yv[t] - yv[t - 1L])
  }
  X <- do.call(rbind, rows)
  f <- .pesdol_ols(X, dep)
  n <- nrow(X)
  kk <- ncol(X)
  rss_u <- sum(f$res ^ 2)

  keep <- c(1L, seq(1L + m + 2L, kk))
  fr <- .pesdol_ols(X[, keep, drop = FALSE], dep)
  rss_r <- sum(fr$res ^ 2)
  n_rest <- 1L + m
  dfe <- n - kk
  F <- if (dfe > 0L && rss_u > .pesdol_EPS)
    ((rss_r - rss_u) / n_rest) / (rss_u / dfe) else NaN

  phi <- f$beta[2L]
  theta <- vapply(seq_len(m), function(j)
    if (abs(phi) > .pesdol_EPS) -f$beta[2L + j] / phi else NaN, numeric(1))

  TAB <- list("1" = c(4.94, 5.73), "2" = c(3.79, 4.85), "3" = c(3.23, 4.35),
              "4" = c(2.86, 4.01), "5" = c(2.62, 3.79))
  bd <- TAB[[as.character(m)]]
  if (is.null(bd)) bd <- c(NaN, NaN)
  lo <- bd[1]
  hi <- bd[2]
  verdict <- if (is.nan(F) || is.nan(lo)) "unavailable" else
    if (F > hi) "cointegrated" else
      if (F < lo) "no long-run relationship" else "inconclusive"

  list(estimate = theta, long_run = theta,
       coefficients = f$beta, residuals = f$res, fitted = f$fit,
       speed_of_adjustment = phi,
       f_statistic = F, bound_lower = lo, bound_upper = hi,
       verdict = verdict, n_used = as.integer(n), n_params = as.integer(kk),
       rss_unrestricted = rss_u, rss_restricted = rss_r,
       p = as.integer(p), q = as.integer(q), n_regressors = as.integer(m),
       method = paste0("ARDL conditional ECM with the Pesaran-Shin-Smith ",
                       "bounds test (Pesaran & Shin 1998; Pesaran, Shin & ",
                       "Smith 2001)"),
       note = paste0("the bounds test avoids a unit-root PRE-TEST; between ",
                     "the two critical values the answer is INCONCLUSIVE, ",
                     "which is the method working rather than failing"))
}

#' .pesdol_cheatsheet
#'
#' A step of the pesdol_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.pesdol_cheatsheet <- function() {
  paste0("pesdol: morie_pesdol_ardl_bounds(y, x, p, q) -> ARDL long-run ",
         "coefficients and the bounds test (Pesaran, Shin & Smith 2001)")
}

morie_pesdol <- morie_pesdol_ardl_bounds
