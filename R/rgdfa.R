#' Detrended fluctuation analysis -- Rangayyan Ch 7
#'
#' DFA-alpha scaling exponent (Peng et al. 1994).
#'
#' @param x Numeric vector.
#' @param scales Optional integer box sizes (default geometric 4..N/4).
#' @param order Detrending polynomial order (default 1 = DFA-1).
#' @return Named list `alpha`, `scales`, `fluct`, `log_scales`, `log_F`.
#' @references Peng et al. (1994), Phys Rev E 49:1685; Rangayyan Ch 7.
#' @export
#' @examples
#' set.seed(0)
#' rgdfa(rnorm(500))$alpha
rgdfa <- function(x, scales = NULL, order = 1L) {
  N <- length(x)
  if (N < 32) stop("DFA needs at least 32 samples.")
  if (is.null(scales)) {
    log_n <- seq(log(4), log(max(8, N %/% 4)), length.out = 12)
    scales <- unique(round(exp(log_n)))
    scales <- scales[scales >= 4]
  }
  scales <- as.integer(scales)
  order <- as.integer(order)
  if (order < 0L) stop(sprintf("`order` must be >= 0, got %d.", order))
  ## A box of n points cannot support a polynomial of degree `order` unless
  ## n >= order + 2; at n == order + 1 the fit is exact, every residual is
  ## zero, F(n) collapses to 0 and log F = -Inf silently poisons the slope.
  too_small <- scales[scales < order + 2L]
  if (length(too_small)) {
    stop(sprintf(
      "box sizes %s are too small for order=%d: need at least %d points per box.",
      paste(too_small, collapse = ", "), order, order + 2L
    ))
  }
  y <- cumsum(x - mean(x))
  fluct <- numeric(length(scales))
  for (j in seq_along(scales)) {
    n <- scales[j]
    nseg <- N %/% n
    if (nseg < 1) {
      fluct[j] <- NA_real_
      next
    }
    rms <- numeric(nseg)
    for (k in seq_len(nseg)) {
      seg <- y[((k - 1) * n + 1):(k * n)]
      t_ <- seq_len(n)
      p <- stats::lm(seg ~ stats::poly(t_, order, raw = TRUE))
      rms[k] <- mean(stats::residuals(p)^2)
    }
    fluct[j] <- sqrt(mean(rms))
  }
  mask <- is.finite(fluct) & fluct > 0
  log_n <- log(scales[mask])
  log_F <- log(fluct[mask])
  ## alpha is the slope of a log-log fit, so it needs at least two usable
  ## scales. With one, lm() fits an arbitrary line through a single point and
  ## returns an alpha that looks like a number and means nothing.
  if (length(log_n) < 2L) {
    stop(sprintf(
      "need at least 2 usable box sizes to fit a slope, got %d (from scales=%s).",
      length(log_n), paste(scales, collapse = ", ")
    ))
  }
  fit <- stats::lm(log_F ~ log_n)
  list(
    alpha = unname(stats::coef(fit)[2]),
    scales = scales, fluct = fluct,
    log_scales = log_n, log_F = log_F
  )
}

#' @rdname rgdfa
#' @keywords internal
#' @export
morie_rangayyan_dfa <- rgdfa
