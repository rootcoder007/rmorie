# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal: covariance C(h) from a nugget / sill / range / model spec.
#'
#' C(h) = sill * R(h) for h > 0 and C(0) = nugget + sill: the nugget is a
#' variance component present only at zero lag (Sec 4.3.6).
#'
#' @param h Numeric matrix or vector of distances.
#' @param cov_model List with `model`, `nugget`, `sill`, `range`.
#' @return Numeric object shaped like `h`.
#' @references Schabenberger & Gotway (2005), Ch 4-5.
#' @noRd
.sp_cov_from_model <- function(h, cov_model = NULL) {
  cm <- if (is.null(cov_model)) list() else cov_model
  nugget <- if (is.null(cm$nugget)) 0 else as.numeric(cm$nugget)
  sill <- if (is.null(cm$sill)) 1 else as.numeric(cm$sill)
  rng <- if (is.null(cm$range)) 1 else as.numeric(cm$range)
  model <- if (is.null(cm$model)) "exponential" else cm$model
  out <- sill * .sp_correlogram(as.numeric(h), rng, model)
  out[as.numeric(h) == 0] <- nugget + sill
  if (!is.null(dim(h))) dim(out) <- dim(h)
  out
}

#' Internal: pairwise Euclidean distances between two coordinate sets.
#' @param a Matrix (n by d).
#' @param b Matrix (m by d).
#' @return Numeric matrix (n by m).
#' @noRd
.sp_cross_dist <- function(a, b) {
  a <- as.matrix(a); b <- as.matrix(b)
  out <- matrix(0, nrow(a), nrow(b))
  for (j in seq_len(nrow(b))) {
    out[, j] <- sqrt(colSums((t(a) - b[j, ])^2))
  }
  out
}

#' Internal: simple kriging solution (Sec 5.2.1).
#'
#' lambda = Sigma^-1 sigma; p = mu + sigma' Sigma^-1 (Z - mu) (5.10);
#' variance = sigma^2 - sigma' Sigma^-1 sigma (5.11).
#'
#' @param coords Observation coordinates (n by d).
#' @param z Observed values, length n.
#' @param target Prediction locations (m by d).
#' @param cov_model Covariance spec.
#' @param mu Known mean; sample mean of `z` when NULL.
#' @return List with prediction, variance, weights, mu.
#' @references Schabenberger & Gotway (2005), Sec 5.2.1.
#' @noRd
.sp_simple_kriging <- function(coords, z, target, cov_model = NULL, mu = NULL) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  target <- as.matrix(target)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows")
  }
  mu <- if (is.null(mu)) mean(z) else as.numeric(mu)
  Sigma <- .sp_cov_from_model(.sp_cross_dist(coords, coords), cov_model)
  sig <- .sp_cov_from_model(.sp_cross_dist(coords, target), cov_model)
  sigma2 <- .sp_cov_from_model(0, cov_model)
  lam <- solve(Sigma, sig)
  pred <- as.numeric(mu + crossprod(lam, z - mu))
  vr <- as.numeric(sigma2 - colSums(sig * lam))
  list(prediction = pred, variance = pmax(vr, 0), weights = lam, mu = mu)
}
