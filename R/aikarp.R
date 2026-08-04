# SPDX-License-Identifier: AGPL-3.0-or-later
#' AIC order selection for an autoregression.
#'
#' Formula: AIC(p) = log(sigmahat_p^2) + 2(p+1)/T, sigmahat_p^2 from the Levinson-Durbin recursion
#'
#' @param x The series.
#' @param max_p Largest order considered.
#' @param demean Subtract the sample mean first.

#' @return List with ``p``, ``aic``, ``aic_unnormalised``, ``sigma2``, ``pacf``, ``n``.
#' @references Akaike (1973), Information theory and an extension of the maximum likelihood principle, in Petrov and Csaki (eds), 2nd International Symposium on Information Theory. Not held locally; AIC = -2 log L + 2k and its AR(p) specialisation via the Levinson-Durbin innovation variance are the standard published forms.
#' @export
Aicar <- function(x, max_p = 10, demean = TRUE) {
  x <- .t1_vec(x); T <- length(x); P <- as.integer(max_p)
  if (T < P + 2) stop("series too short for max_p")
  mu <- if (isTRUE(demean)) mean(x) else 0
  z <- x - mu
  g <- vapply(0:P, function(k) sum(z[(k + 1):T] * z[1:(T - k)]) / T, numeric(1))
  if (g[1] <= 0) stop("series has zero variance")
  sig <- g[1]; phi <- numeric(0); pacf <- numeric(0)
  for (k in seq_len(P)) {
    num <- g[k + 1] - if (k > 1) sum(phi * g[k:2]) else 0
    kk <- num / sig[k]
    pacf <- c(pacf, kk)
    phi <- c(if (k > 1) phi - kk * rev(phi) else numeric(0), kk)
    sig <- c(sig, sig[k] * (1 - kk^2))
  }
  p <- 0:P
  aic <- ifelse(sig > 0, log(sig) + 2 * (p + 1) / T, Inf)
  unn <- ifelse(sig > 0, T * log(sig) + 2 * (p + 1), Inf)
  .t1_result(p = which.min(aic) - 1L, aic = aic, aic_unnormalised = unn,
             sigma2 = sig, pacf = pacf, n = T,
             method = "AIC order selection for AR(p)")
}
