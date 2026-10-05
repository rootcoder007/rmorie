.vx_bins <- function(P, z, z2, B, direction, tol, estimator) {
  nb <- length(B) - 1
  acc <- vector("list", nb)
  dst <- vector("list", nb)
  if (!is.null(direction)) {
    u <- direction / sqrt(sum(direction^2))
    ct <- cos(tol * pi / 180)
  }
  n <- nrow(P)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      h <- P[j, ] - P[i, ]
      d <- sqrt(sum(h^2))
      if (d == 0) next
      if (!is.null(direction) && abs(sum(h * u)) / d < ct - 1e-12) next
      k <- which(B[-length(B)] < d & d <= B[-1])[1]
      if (is.na(k)) next
      dz <- z[i] - z[j]
      v <- switch(estimator, classical = dz^2, madogram = abs(dz), rodogram = sqrt(abs(dz)),
                  cross = dz * (z2[i] - z2[j]), stop("estimator must be classical, madogram, rodogram or cross"))
      acc[[k]] <- c(acc[[k]], v)
      dst[[k]] <- c(dst[[k]], d)
    }
  }
  list(acc = acc, dist = dst)
}

#' Variogram extras
#'
#' \code{SampleVariogramNd}: n-dimensional, optionally directional sample
#' variogram (classical, madogram, rodogram or cross). \code{AnisotropicLag}:
#' GSLIB 3-D geometric anisotropy. \code{ZonalSemivariance}: nested zonal
#' anisotropy. \code{VariogramEnvelope}: permutation or bootstrap envelopes.
#' \code{VariogramJackknife}: delete-one jackknife standard errors.
#' \code{VariogramCloudBox}: variogram-cloud box statistics.
#' \code{VariogramFractal}: Hurst exponent and fractal dimension.
#' \code{WindowedSemivariance}: temporal semivariance in moving windows.
#' Identical to the Python arm \code{morie.fn.vgextra}.
#'
#' @param z,z2 Data values.
#' @param coords Coordinate matrix (any dimension).
#' @param boundaries Lag-bin boundaries.
#' @param estimator Estimator.
#' @param direction Direction vector.
#' @param tol Angular tolerance (degrees).
#' @param h Separation vector.
#' @param azimuth,dip,rake Rotation angles (degrees).
#' @param ratio1,ratio2 Anisotropy ratios.
#' @param model,zonal_model Variogram models (see \code{VgmSemivariance}).
#' @param axis Zonal axis (1-based).
#' @param nsim Number of simulations.
#' @param level Envelope level.
#' @param method \code{"permutation"} or \code{"bootstrap"}.
#' @param seed Philox seed.
#' @param dist,gamma Lags and semivariances.
#' @param dim Embedding dimension.
#' @param max_lag Largest lag in the fit.
#' @param series Time series.
#' @param lags Integer lags.
#' @param window,step Window length and step.
#' @return List or numeric.
#' @references Deutsch, C. V. and Journel, A. G. (1998). GSLIB, 2nd ed.
#'   Oxford University Press.
#'
#'   Diggle, P. J. and Ribeiro, P. J. (2007). Model-based Geostatistics.
#'   Springer.
#'
#'   Shafer, J. M. and Varljen, M. D. (1990). Approximation of confidence
#'   limits on sample semivariograms from single realizations of spatially
#'   correlated random fields. Water Resources Research 26, 1787-1802.
#' @examples
#' SampleVariogramNd(c(1, 2, 4), rbind(c(0, 0, 0), c(1, 0, 0), c(2, 0, 0)), c(0, 1.5, 2.5))$gamma
#' AnisotropicLag(c(1, 0, 0), ratio1 = 0.5)
#' @export
SampleVariogramNd <- function(z, coords, boundaries, estimator = "classical", direction = NULL, tol = 22.5,
                              z2 = NULL) {
  P <- as.matrix(coords)
  b <- .vx_bins(P, z, z2, boundaries, direction, tol, estimator)
  keep <- lengths(b$acc) > 0
  list(np = lengths(b$acc)[keep], dist = vapply(b$dist[keep], mean, 0),
       gamma = vapply(b$acc[keep], function(a) sum(a) / (2 * length(a)), 0), estimator = estimator)
}

#' @rdname SampleVariogramNd
#' @export
AnisotropicLag <- function(h, azimuth = 0, dip = 0, rake = 0, ratio1 = 1, ratio2 = 1) {
  .morie_arg(h, "n")
  a <- (90 - azimuth) * pi / 180
  b <- -dip * pi / 180
  t <- rake * pi / 180
  rot <- rbind(c(cos(b) * cos(a), cos(b) * sin(a), -sin(b)),
               c(-cos(t) * sin(a) + sin(t) * sin(b) * cos(a), cos(t) * cos(a) + sin(t) * sin(b) * sin(a), sin(t) * cos(b)) /
                 ratio1,
               c(sin(t) * sin(a) + cos(t) * sin(b) * cos(a), -sin(t) * cos(a) + cos(t) * sin(b) * sin(a), cos(t) * cos(b)) /
                 ratio2)
  sqrt(sum((rot %*% h)^2))
}

#' @rdname SampleVariogramNd
#' @export
ZonalSemivariance <- function(h, model, zonal_model, axis = 3) {
  VgmSemivariance(sqrt(sum(h^2)), model) + VgmSemivariance(abs(h[axis]), zonal_model)
}

#' @rdname SampleVariogramNd
#' @export
VariogramEnvelope <- function(z, coords, boundaries, nsim = 99, level = 0.95, method = "permutation", seed = 1) {
  P <- as.matrix(coords)
  obs <- SampleVariogramNd(z, P, boundaries)
  n <- length(z)
  sims <- t(vapply(seq_len(nsim) - 1, function(s) {
    u <- .morie_random_uniform(n, seed = seed, stream = s)
    zs <- switch(method,
      permutation = {
        idx <- seq_len(n)
        for (t in seq_len(n)) {
          j <- t + floor(u[t] * (n - t + 1))
          tmp <- idx[t]
          idx[t] <- idx[j]
          idx[j] <- tmp
        }
        z[idx]
      },
      bootstrap = z[pmin(n, floor(u * n) + 1)],
      stop("method must be permutation or bootstrap")
    )
    SampleVariogramNd(zs, P, boundaries)$gamma
  }, numeric(length(obs$gamma))))
  sims <- matrix(sims, nsim)
  lo <- apply(sims, 2, stats::quantile, probs = (1 - level) / 2, type = 7, names = FALSE)
  hi <- apply(sims, 2, stats::quantile, probs = (1 + level) / 2, type = 7, names = FALSE)
  list(gamma = obs$gamma, dist = obs$dist, lower = lo, upper = hi, outside = !(obs$gamma >= lo & obs$gamma <= hi))
}

#' @rdname SampleVariogramNd
#' @export
VariogramJackknife <- function(z, coords, boundaries) {
  P <- as.matrix(coords)
  full <- SampleVariogramNd(z, P, boundaries)
  n <- length(z)
  reps <- lapply(seq_len(n), function(i) SampleVariogramNd(z[-i], P[-i, , drop = FALSE], boundaries)$gamma)
  k <- length(full$gamma)
  if (any(lengths(reps) != k)) stop("a lag bin loses all its pairs when a datum is removed")
  R <- do.call(rbind, reps)
  m <- colMeans(R)
  list(gamma = full$gamma, se = sqrt((n - 1) / n * colSums(sweep(R, 2, m)^2)), bias_corrected = n * full$gamma - (n - 1) * m,
       dist = full$dist)
}

#' @rdname SampleVariogramNd
#' @export
VariogramCloudBox <- function(z, coords, boundaries) {
  b <- .vx_bins(as.matrix(coords), z, NULL, boundaries, NULL, 90, "classical")
  st <- lapply(b$acc[lengths(b$acc) > 0], function(a) {
    s <- sort(a / 2)
    q <- stats::quantile(s, c(0, 0.25, 0.5, 0.75, 1), type = 7, names = FALSE)
    iqr <- q[4] - q[2]
    ins <- s[s >= q[2] - 1.5 * iqr & s <= q[4] + 1.5 * iqr]
    c(q, min(ins), max(ins), length(s) - length(ins), length(s))
  })
  M <- do.call(rbind, st)
  list(min = M[, 1], q1 = M[, 2], median = M[, 3], q3 = M[, 4], max = M[, 5], lower_whisker = M[, 6],
       upper_whisker = M[, 7], outliers = M[, 8], n = M[, 9])
}

#' @rdname SampleVariogramNd
#' @export
VariogramFractal <- function(dist, gamma, dim = 1, max_lag = NULL) {
  keep <- gamma > 0 & dist > 0 & (if (is.null(max_lag)) TRUE else dist <= max_lag)
  x <- log(dist[keep])
  y <- log(gamma[keep])
  slope <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  list(hurst = slope / 2, fractal_dimension = dim + 1 - slope / 2, slope = slope, intercept = mean(y) - slope * mean(x))
}

#' @rdname SampleVariogramNd
#' @export
WindowedSemivariance <- function(series, lags, window, step = 1) {
  starts <- seq(0, length(series) - window, by = step)
  G <- t(vapply(starts, function(s) {
    w <- series[s + seq_len(window)]
    vapply(lags, function(k) if (k < window) sum(diff(w, lag = k)^2) / (2 * (window - k)) else NaN, 0)
  }, numeric(length(lags))))
  list(start = starts, gamma = matrix(G, length(starts)), lags = lags)
}
