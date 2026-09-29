#' Space-time cokriging and the diffusion covariance
#'
#' \code{StCokriging}: ordinary space-time cokriging of variable 0 under a
#' separable intrinsic coregionalisation with exponential space and time
#' correlations. \code{DiffusionStCovariance}: covariance of a Gaussian field
#' propagated by the diffusion equation. Identical to the Python arm
#' \code{morie.fn.stcokrig}.
#'
#' @param coords Observation coordinates (two-column).
#' @param times Observation times.
#' @param variable Variable index (0 or 1) of each observation.
#' @param values Observed values.
#' @param targets Prediction coordinates.
#' @param target_times Prediction times.
#' @param coreg 2 by 2 coregionalisation matrix.
#' @param range_s,range_t Exponential ranges.
#' @param nugget Nuggets of the two variables.
#' @param h,u Spatial and temporal lags.
#' @param sigma2,xi Initial variance and correlation length.
#' @param diffusivity Diffusion coefficient.
#' @param dim Spatial dimension.
#' @return A list, or the covariance values.
#' @references Kolovos, A., Christakos, G., Hristopulos, D. T. and Serre,
#'   M. L. (2004). Methods for generating non-separable spatiotemporal
#'   covariance models. Advances in Water Resources 27, 815-830.
#'
#'   Wackernagel, H. (2003). Multivariate Geostatistics, 3rd edn. Springer.
#' @examples
#' DiffusionStCovariance(1, 0.5, 2, 1, 0.5)
#' @export
StCokriging <- function(coords, times, variable, values, targets, target_times, coreg, range_s, range_t,
                        nugget = c(0, 0)) {
  P <- as.matrix(coords)
  Tg <- as.matrix(targets)
  n <- nrow(P)
  D <- as.matrix(stats::dist(P))
  U <- abs(outer(times, times, "-"))
  K <- matrix(0, n + 2, n + 2)
  K[1:n, 1:n] <- coreg[cbind(rep(variable + 1, n), rep(variable + 1, each = n))] * exp(-D / range_s) * exp(-U / range_t) +
    diag(nugget[variable + 1], n)
  K[cbind(1:n, n + variable + 1)] <- 1
  K[cbind(n + variable + 1, 1:n)] <- 1
  est <- numeric(nrow(Tg))
  kv <- numeric(nrow(Tg))
  for (q in seq_len(nrow(Tg))) {
    d <- sqrt(colSums((t(P) - Tg[q, ])^2))
    rhs <- c(coreg[variable + 1, 1] * exp(-d / range_s) * exp(-abs(times - target_times[q]) / range_t), 1, 0)
    w <- solve(K, rhs)
    est[q] <- sum(w[1:n] * values)
    kv[q] <- coreg[1, 1] + nugget[1] - sum(w[1:n] * rhs[1:n]) - w[n + 1]
  }
  list(estimate = est, variance = kv)
}

#' @rdname StCokriging
#' @export
DiffusionStCovariance <- function(h, u, sigma2 = 1, xi = 1, diffusivity = 1, dim = 2) {
  s <- xi^2 + 4 * diffusivity * abs(u)
  sigma2 * (xi^2 / s)^(dim / 2) * exp(-h^2 / s)
}
