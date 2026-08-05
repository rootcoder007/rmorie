# SPDX-License-Identifier: AGPL-3.0-or-later
#' Regression kink design: the ratio of slope changes at the threshold
#'
#' The Python module \code{morie.fn.rgknd} is the other implementation of
#' this design in the tree. It is not aliased here because its standard
#' error comes from a 500-draw bootstrap on Python's native generator
#' stream, which no R arm can reproduce; the point estimate is
#' deterministic but the reported uncertainty is not. This version keeps
#' the estimand and gives it an analytic HC0 standard error via the delta
#' method, so both language arms agree to the last digits.
#'
#' With \code{D} omitted the design is sharp: the assignment slope
#' changes by exactly one and the estimator is the change in outcome
#' slope.
#'
#' Formula: \code{tau = (b_Y+ - b_Y-) / (b_D+ - b_D-)}.
#'
#' @param y Outcome.
#' @param x Running variable.
#' @param D Assignment or dose; omit for the sharp kink.
#' @param cutoff Kink threshold.
#' @param bandwidth Half-window, positive.
#' @return List with \code{estimate}, \code{tau}, \code{se}, \code{z},
#'   \code{slope_Y_right}, \code{slope_Y_left}, \code{slope_D_right},
#'   \code{slope_D_left}, \code{first_stage}, \code{n_right},
#'   \code{n_left}, \code{bandwidth}.
#' @references Card, D., Lee, D. S., Pei, Z. & Weber, A. (2015).
#'   Inference on causal effects in a generalized regression kink design.
#'   Econometrica 83(6):2453-2483. \doi{10.3982/ECTA11224}.
#' @export
Rdkkin <- function(y, x, D = NULL, cutoff = 0, bandwidth = 1) {
  y <- as.numeric(unlist(y)); x <- as.numeric(unlist(x))
  if (length(y) == 0L) stop("Rdkkin: y is empty")
  if (length(x) != length(y)) stop("Rdkkin: x must have one entry per observation")
  s <- .rd_sides(x, cutoff, bandwidth, "Rdkkin")
  yR <- .rd_wls(s$r[s$right], y[s$right], s$w[s$right])
  yL <- .rd_wls(s$r[s$left], y[s$left], s$w[s$left])
  num <- yR$b - yL$b; vn <- yR$var_b + yL$var_b
  if (is.null(D)) {
    bdR <- 1; bdL <- 0; vd <- 0
  } else {
    D <- as.numeric(unlist(D))
    if (length(D) != length(y)) stop("Rdkkin: D must have one entry per observation")
    dR <- .rd_wls(s$r[s$right], D[s$right], s$w[s$right])
    dL <- .rd_wls(s$r[s$left], D[s$left], s$w[s$left])
    bdR <- dR$b; bdL <- dL$b; vd <- dR$var_b + dL$var_b
  }
  den <- bdR - bdL
  if (abs(den) < 1e-10) stop("Rdkkin: no kink in assignment; the denominator is zero")
  tau <- num / den
  se <- sqrt(vn / (den * den) + (num * num) * vd / (den^4))
  .t1_result(estimate = tau, tau = tau, se = se,
             z = if (se > 0) tau / se else NA_real_,
             slope_Y_right = yR$b, slope_Y_left = yL$b,
             slope_D_right = bdR, slope_D_left = bdL, first_stage = den,
             n_right = yR$n, n_left = yL$n, bandwidth = s$h,
             method = "Regression kink design, ratio of local linear slope changes")
}
