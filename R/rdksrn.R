# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weighted line fit in closed form with an HC0 variance (internal)
#'
#' Solved from the five weighted sums rather than through a general
#' solver: two arms calling two different least-squares routines is the
#' easiest way to lose the last digits, and the 2-by-2 normal equations
#' have an exact solution.
#'
#' @param r Centred running variable on one side.
#' @param y Outcome on that side.
#' @param w Kernel weights on that side.
#' @return List with \code{a}, \code{b}, \code{var_a}, \code{var_b}, \code{n}.
#' @keywords internal
#' @examples
#' set.seed(1)
#' r <- .rd_wls(r = rnorm(10), y = rnorm(10), w = matrix(rnorm(20), 5, 4)); TRUE
.rd_wls <- function(r, y, w) {
  S0 <- sum(w); S1 <- sum(w * r); S2 <- sum(w * r * r)
  Sy <- sum(w * y); Sry <- sum(w * r * y)
  det <- S0 * S2 - S1 * S1
  if (abs(det) < 1e-300)
    stop("Rdksrn: a side has no variation in the running variable")
  a <- (S2 * Sy - S1 * Sry) / det
  b <- (S0 * Sry - S1 * Sy) / det
  e <- y - a - b * r
  cc <- w * w * e * e
  m00 <- sum(cc); m01 <- sum(cc * r); m11 <- sum(cc * r * r)
  u0 <- S2 / det; u1 <- -S1 / det
  v0 <- -S1 / det; v1 <- S0 / det
  list(a = a, b = b,
       var_a = u0 * u0 * m00 + 2 * u0 * u1 * m01 + u1 * u1 * m11,
       var_b = v0 * v0 * m00 + 2 * v0 * v1 * m01 + v1 * v1 * m11,
       n = length(r))
}

#' Centre, window and triangular-weight the running variable (internal)
#'
#' @param x Running variable.
#' @param cutoff Threshold.
#' @param bandwidth Half-window, positive.
#' @param who Caller name for error messages.
#' @return List with \code{r}, \code{w}, \code{left}, \code{right}, \code{h}.
#' @keywords internal
.rd_sides <- function(x, cutoff, bandwidth, who) {
  r <- as.numeric(x) - as.numeric(cutoff)
  h <- as.numeric(bandwidth)
  if (h <= 0) stop(paste0(who, ": bandwidth must be positive"))
  right <- which(r >= 0 & r <= h)
  left <- which(r >= -h & r < 0)
  if (length(right) < 2L || length(left) < 2L)
    stop(paste0(who, ": each side of the cutoff needs at least two points inside the bandwidth"))
  list(r = r, w = pmax(0, 1 - abs(r) / h), left = left, right = right, h = h)
}

#' Sharp regression discontinuity by local linear regression
#'
#' Local linear rather than local constant is not a refinement: a kernel
#' mean at a boundary point is biased at first order because the data lie
#' on one side only, and the linear term removes that bias. The
#' triangular kernel is the boundary-optimal one for this estimand.
#'
#' Formula: \code{tau = lim_{x -> c+} E[Y|X=x] - lim_{x -> c-} E[Y|X=x]},
#' each limit the intercept of a triangular-kernel weighted line fit on
#' its own side.
#'
#' @param y Outcome.
#' @param x Running variable.
#' @param cutoff Threshold.
#' @param bandwidth Half-window, positive.
#' @return List with \code{estimate}, \code{tau}, \code{se}, \code{z},
#'   \code{mu_right}, \code{mu_left}, \code{slope_right},
#'   \code{slope_left}, \code{n_right}, \code{n_left}, \code{bandwidth}.
#' @references Hahn, J., Todd, P. & Van der Klaauw, W. (2001).
#'   Identification and estimation of treatment effects with a
#'   regression-discontinuity design. Econometrica 69(1):201-209.
#'   \doi{10.1111/1468-0262.00183}.
#' @export
#' @examples
#' set.seed(1)
#' r <- Rdksrn(y = rnorm(10), x = rnorm(10)); TRUE
Rdksrn <- function(y, x, cutoff = 0, bandwidth = 1) {
  y <- as.numeric(unlist(y)); x <- as.numeric(unlist(x))
  if (length(y) == 0L) stop("Rdksrn: y is empty")
  if (length(x) != length(y)) stop("Rdksrn: x must have one entry per observation")
  s <- .rd_sides(x, cutoff, bandwidth, "Rdksrn")
  R <- .rd_wls(s$r[s$right], y[s$right], s$w[s$right])
  L <- .rd_wls(s$r[s$left], y[s$left], s$w[s$left])
  tau <- R$a - L$a
  se <- sqrt(R$var_a + L$var_a)
  .t1_result(estimate = tau, tau = tau, se = se,
             z = if (se > 0) tau / se else NA_real_,
             mu_right = R$a, mu_left = L$a,
             slope_right = R$b, slope_left = L$b,
             n_right = R$n, n_left = L$n, bandwidth = s$h,
             method = "Sharp RDD, triangular-kernel local linear")
}
