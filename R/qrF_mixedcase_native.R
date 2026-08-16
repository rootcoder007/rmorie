# morie native arm -- qrF
# Quantile (check / pinball) loss.
#
#   rho_theta(u) = theta * u        if u >= 0
#                  (theta - 1) * u  if u <  0
#
# Under-predictions are weighted theta and over-predictions 1 - theta;
# theta = 1/2 gives half the absolute error. The empirical minimiser
# over constant predictions is the theta-th sample quantile, which is
# what makes this the loss that DEFINES regression quantiles rather
# than merely scoring them.
#
# Koenker, R. & Bassett, G. (1978) Econometrica 46(1), 33-50, Sec. 2.

#' morie_qrF
#'
#' A step of the qrF_mixedcase_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param y_true See Usage.
#' @param y_pred See Usage.
#' @param theta Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @return A list with \code{estimate}, \code{total}, \code{losses}, \code{theta}, \code{n}, \code{method}.
#' @export
morie_qrF <- function(y_true, y_pred, theta = 0.5) {
  yt <- as.numeric(y_true)
  yp <- as.numeric(y_pred)
  n <- length(yt)
  if (length(yp) != n || n == 0L) {
    stop("y_true and y_pred must be non-empty and paired")
  }
  theta <- as.numeric(theta)
  if (!(theta > 0 && theta < 1)) stop("theta must be in (0, 1)")
  u <- yt - yp
  losses <- ifelse(u >= 0, theta * u, (theta - 1) * u)
  tot <- sum(losses)
  list(
    estimate = tot / n,
    total = tot,
    losses = losses,
    theta = theta,
    n = n,
    method = "quantile/pinball loss (Koenker-Bassett 1978)"
  )
}

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_qrf <- morie_qrF
