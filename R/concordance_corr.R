# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lin's concordance correlation coefficient
#'
#' rho_c = 2 s_xy / (s_x^2 + s_y^2 + (xbar - ybar)^2) with moments on divisor n
#' (Lin 1989; Hedderich, Sachs & Reynarowych 2023, Sec. 6.16.4), and Lin's
#' interval on z = atanh(rho_c) with se = sqrt(((1 - r^2) rho_c^2 (1 - rho_c^2) /
#' r^2 + 2 rho_c^3 (1 - rho_c) u^2 / r - rho_c^4 u^4 / (2 r^2)) / (n - 2)) / (1 -
#' rho_c^2), u = (ybar - xbar) / (s_x^2 s_y^2)^(1/4): DescTools' CCC(ci =
#' "z-transform"). At |rho_c| = 1 the interval is the point itself.
#'
#' @param x,y Paired numeric measurements.
#' @param confidence Interval level.
#' @return Named list: estimate, ci_lower, ci_upper, se, n, pearson_r,
#'   asymptotic_ci, scale_shift, location_shift, C_b.
#' @references Lin, L. I.-K. (1989). Biometrics 45, 255-268; correction (2000)
#'   Biometrics 56, 324-325.
#' @examples
#' morie_concordance_corr(c(1, 2, 3.2, 4.1, 5), c(1.1, 2.3, 2.9, 4.4, 5.2))$estimate
#' @export
morie_concordance_corr <- function(x, y, confidence = 0.95) {
  ok <- is.finite(x) & is.finite(y)
  x <- as.numeric(x[ok])
  y <- as.numeric(y[ok])
  n <- length(x)
  if (n < 3) stop("Need >= 3 paired observations.", call. = FALSE)
  mx <- mean(x)
  my <- mean(y)
  sx2 <- mean((x - mx)^2)
  sy2 <- mean((y - my)^2)
  sxy <- mean((x - mx) * (y - my))
  p <- 2 * sxy / (sx2 + sy2 + (my - mx)^2)
  r <- sxy / sqrt(sx2 * sy2)
  u <- (my - mx) / (sx2 * sy2)^0.25
  sep <- sqrt(max(0, (1 - r^2) * p^2 * (1 - p^2) / r^2 + 2 * p^3 * (1 - p) * u^2 / r -
                   0.5 * p^4 * u^4 / r^2) / (n - 2))
  zq <- stats::qnorm(1 - (1 - confidence) / 2)
  if (abs(p) >= 1) {
    p <- sign(p)
    se_t <- 0
    ci <- c(p, p)
  } else {
    se_t <- sep / (1 - p^2)
    ci <- tanh(atanh(p) + c(-1, 1) * zq * se_t)
  }
  list(estimate = p, ci_lower = ci[1L], ci_upper = ci[2L], se = se_t,
       n = n, pearson_r = r, asymptotic_ci = c(p - zq * sep, p + zq * sep),
       scale_shift = sqrt(sy2 / sx2), location_shift = u, C_b = p / r)
}
