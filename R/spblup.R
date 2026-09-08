# SPDX-License-Identifier: AGPL-3.0-or-later
#' Best linear unbiased predictor: ordinary kriging
#'
#' When the mean is unknown but constant, unbiasedness requires the
#' weights to sum to one. The constraint is carried by a Lagrange
#' multiplier m, bordering the kriging system with a row and column of
#' ones, and the variance becomes sigma^2 - lambda' sigma - m.
#'
#' The BLUP is never better than simple kriging in mean-squared error --
#' it pays for not knowing the mean -- but it does not require one.
#'
#' @param coords Observation coordinates (n by d).
#' @param z Observed values, length n.
#' @param target Prediction locations (m by d).
#' @param cov_model List with `model`, `nugget`, `sill`, `range`.
#' @return Named list: prediction, variance, weights, lagrange.
#' @references Schabenberger & Gotway (2005), Secs 5.1-5.2.
#' @examples
#' co <- matrix(runif(40), 20, 2) * 5
#' spblup(co, rnorm(20), matrix(c(2, 2), 1, 2),
#'        list(model = "exponential", sill = 1, range = 2))
#' @export
spblup <- function(coords, z, target, cov_model = NULL) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  target <- as.matrix(target)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows")
  }
  n <- length(z)
  Sigma <- .sp_cov_from_model(.sp_cross_dist(coords, coords), cov_model)
  sig <- .sp_cov_from_model(.sp_cross_dist(coords, target), cov_model)
  sigma2 <- .sp_cov_from_model(0, cov_model)
  A <- matrix(0, n + 1, n + 1)
  A[seq_len(n), seq_len(n)] <- Sigma
  A[seq_len(n), n + 1] <- 1
  A[n + 1, seq_len(n)] <- 1
  b <- rbind(sig, matrix(1, 1, ncol(sig)))
  sol <- solve(A, b)
  lam <- sol[seq_len(n), , drop = FALSE]
  m <- sol[n + 1, ]
  pred <- as.numeric(crossprod(lam, z))
  vr <- as.numeric(sigma2 - colSums(sig * lam) - m)
  list(prediction = pred, variance = pmax(vr, 0), weights = lam, lagrange = m)
}
