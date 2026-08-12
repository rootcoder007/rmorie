# Quantile (pinball) loss.
# Source: Koenker & Bassett (1978), Econometrica 46(1), 33-50, Sec. 2
# (fetched-wave3/Koenker-RegressionQuantiles-1978.pdf); Koenker
# (2005), Quantile Regression, CUP.  Mirrors Python morie.fn.qrF
# exactly.

#' Quantile (check / pinball) loss
#'
#' rho_theta(u) = theta u for u >= 0, (theta - 1) u for u < 0; the
#' asymmetric absolute loss whose empirical minimizer over constants
#' is the theta-th sample quantile.
#'
#' @param y_true,y_pred Paired numeric vectors.
#' @param theta Quantile level in (0, 1).
#' @return A list with elements \code{estimate} (mean loss),
#'   \code{total}, \code{losses}, \code{theta}, \code{n},
#'   \code{method}.
#' @references Koenker, R. and Bassett, G. (1978). Regression
#'   quantiles. Econometrica, 46(1), 33-50.  Koenker, R. (2005).
#'   Quantile Regression. Cambridge University Press.
#' @export
morie_qrf <- function(y_true, y_pred, theta = 0.5) {
  yt <- as.numeric(y_true)
  yp <- as.numeric(y_pred)
  n <- length(yt)
  if (length(yp) != n || n == 0) {
    stop("y_true and y_pred must be non-empty and paired")
  }
  theta <- as.numeric(theta)
  if (theta <= 0 || theta >= 1) stop("theta must be in (0, 1)")
  u <- yt - yp
  losses <- ifelse(u >= 0, theta * u, (theta - 1) * u)
  list(estimate = mean(losses), total = sum(losses), losses = losses,
       theta = theta, n = n,
       method = "quantile/pinball loss (Koenker-Bassett 1978)")
}
