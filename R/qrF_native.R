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
