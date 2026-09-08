# SPDX-License-Identifier: AGPL-3.0-or-later
#' Curve registration by shift
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 7 "Registration: aligning features": two curves differing only in
#' phase are aligned by minimising integral (y_1(t) - y_2(h(t)))^2 dt over
#' warpings h.  Section 7.2 treats the simplest and commonest case, SHIFT
#' registration, where h(t) = t + delta and one constant is estimated per
#' curve.  That is what this function does.
#'
#' Search strategy, chosen so both language arms land on identical numbers
#' rather than merely the same optimum: the criterion is evaluated at every
#' integer sample lag in \[-max_lag, max_lag\] over the overlapping part of the
#' two curves, normalised by the overlap length so lags are comparable, and
#' the integer minimiser is refined by a parabola through the criterion at the
#' minimiser and its two neighbours.  No optimiser, no random restarts.
#'
#' The parabolic step is the three-point vertex
#' delta = 0.5 (c_\{-1\} - c_\{+1\}) / (c_\{-1\} - 2 c_0 + c_\{+1\}).  It is zero only
#' when the criterion happens to be symmetric about the integer minimiser,
#' which a real signal will not be, so the refined estimate generally differs
#' from the integer lag by a fraction of a sample even when the two curves are
#' an exact whole-sample shift of one another.  The INTEGER lag is the
#' quantity that is exact in that case, and the criterion there is exactly
#' zero.  Callers who want only whole-sample alignment should read the shift
#' field, not the estimate field.
#'
#' @param y1,y2 the target and the curve to register, on a common grid.
#' @param max_lag largest lag searched; defaults to floor(n / 2).
#' The reported shift is the delta such that y2(t + delta) best matches
#' y1(t).  It is NEGATIVE when y2 leads y1 and positive when y2 lags it --
#' the sign follows from the criterion being evaluated as y1\[i\] - y2\[i+delta\].
#'
#' @return list: estimate, shift, refinement, criterion, profile, lags, n,
#'   method.
#' @keywords internal
#' @examples
#' Freg(c(0, 1, 2, 1, 0), c(1, 2, 1, 0, 0))$shift
#' @export
Freg <- function(y1, y2, max_lag = NULL) {
  a <- .s03vec(y1)
  b <- .s03vec(y2)
  n <- length(a)
  if (n == 0L) stop("function_register: y1 is empty")
  if (length(b) != n) stop("function_register: y1 and y2 must have the same length")
  if (n < 3L) stop("function_register: need at least three sampling points")
  M <- if (is.null(max_lag)) n %/% 2L else as.integer(max_lag)
  if (is.na(M) || M < 1L) stop("function_register: max_lag must be at least 1")
  if (M > n - 2L) M <- n - 2L
  lags <- seq(-M, M)
  prof <- numeric(length(lags))
  for (q in seq_along(lags)) {
    d <- lags[q]
    lo <- max(0L, -d)
    hi <- min(n, n - d)
    m <- hi - lo
    if (m <= 0L) { prof[q] <- Inf
    next }
    s <- 0
    for (i in seq(lo, hi - 1L)) {
      r <- a[i + 1L] - b[i + d + 1L]
      s <- s + r * r
    }
    prof[q] <- s / m
  }
  best <- 1L
  for (q in seq_along(lags)) if (prof[q] < prof[best]) best <- q
  ref <- 0
  if (best > 1L && best < length(lags)) {
    cm <- prof[best - 1L]
    c0 <- prof[best]
    cp <- prof[best + 1L]
    den <- cm - 2 * c0 + cp
    if (den > 0) ref <- 0.5 * (cm - cp) / den
  }
  list(estimate = lags[best] + ref, shift = lags[best], refinement = ref,
       criterion = prof[best], profile = prof, lags = as.numeric(lags), n = n,
       method = "Ramsay-Silverman (2005) Sect. 7.2 shift registration, integer lag search plus three-point parabolic refinement")
}
