# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native geostatistics engines (feat/native-specializations,
# module 19): empirical semivariogram, maximum-likelihood fitting of
# spherical / exponential / Gaussian covariance models, and ordinary
# kriging. Complements the native GWR in R/gwreg.R and the native
# Moran machinery in R/tps_spatial.R / R/mrm_lisa.R; tests/cross
# validates against gstat where installed.

#' Internal helper: covariance-model semivariance
#' @noRd
.morie_vgm_gamma <- function(h, model, nugget, psill, range_) {
  s <- switch(model,
    spherical = ifelse(h >= range_, 1,
                       1.5 * h / range_ - 0.5 * (h / range_)^3),
    exponential = 1 - exp(-h / range_),
    gaussian = 1 - exp(-(h / range_)^2),
    stop("Unknown variogram model: ", model))
  out <- nugget + psill * s
  out[h == 0] <- 0
  out
}

#' Empirical semivariogram
#'
#' Classical Matheron estimator: half the average squared difference
#' of values within each distance bin.
#'
#' @param coords Two-column matrix (or data frame) of coordinates.
#' @param values Numeric vector of observations.
#' @param n_bins Number of distance bins (default 15).
#' @param cutoff Maximum distance considered (default one third of the
#'   maximum pairwise distance, the gstat convention).
#' @return A data frame with \code{dist} (bin centre), \code{gamma}
#'   (semivariance), and \code{np} (pair count per bin).
#' @references Matheron, G. (1963). Principles of geostatistics.
#'   \emph{Economic Geology}, 58(8), 1246--1266.
#' @export
morie_spatial_variogram <- function(coords, values, n_bins = 15L,
                                    cutoff = NULL) {
  coords <- as.matrix(coords)
  values <- as.numeric(values)
  d <- as.matrix(stats::dist(coords))
  dv <- d[upper.tri(d)]
  sq <- (outer(values, values, "-")^2)[upper.tri(d)] / 2
  if (is.null(cutoff)) cutoff <- max(dv) / 3
  keep <- dv <= cutoff & dv > 0
  dv <- dv[keep]; sq <- sq[keep]
  breaks <- seq(0, cutoff, length.out = n_bins + 1L)
  bin <- cut(dv, breaks, include.lowest = TRUE)
  gamma <- tapply(sq, bin, mean)
  np <- tapply(sq, bin, length)
  mid <- (breaks[-1L] + breaks[-length(breaks)]) / 2
  out <- data.frame(dist = mid, gamma = as.numeric(gamma),
                    np = as.integer(ifelse(is.na(np), 0L, np)))
  out[!is.na(out$gamma), , drop = FALSE]
}

#' Fit a variogram model by Gaussian maximum likelihood
#'
#' Fits (nugget, partial sill, range) of a spherical / exponential /
#' Gaussian covariance model by maximizing the Gaussian log-likelihood
#' of the (constant-mean) data, via \code{stats::optim} on a log
#' parameterization.
#'
#' @param coords,values As in \code{\link{morie_spatial_variogram}}.
#' @param model \code{"exponential"} (default), \code{"spherical"},
#'   or \code{"gaussian"}.
#' @return A list with \code{model}, \code{nugget}, \code{psill},
#'   \code{range}, \code{loglik}, \code{converged}, \code{method}.
#' @srrstats {G1.0} ML covariance estimation per Mardia & Marshall
#'   (1984), Biometrika 71(1).
#' @export
morie_spatial_variogram_fit <- function(coords, values,
                                        model = "exponential") {
  coords <- as.matrix(coords)
  y <- as.numeric(values)
  n <- length(y)
  D <- as.matrix(stats::dist(coords))
  v0 <- stats::var(y)
  r0 <- max(D) / 4
  negll <- function(p) {
    nug <- exp(p[1]); ps <- exp(p[2]); rg <- exp(p[3])
    # covariance = (nug+ps) - gamma(h)
    C <- (nug + ps) - .morie_vgm_gamma(D, model, nug, ps, rg)
    ch <- tryCatch(chol(C + diag(1e-8 * v0, n)),
                   error = function(e) NULL)
    if (is.null(ch)) return(1e10)
    mu <- mean(y)
    a <- backsolve(ch, forwardsolve(t(ch), y - mu))
    sum(log(diag(ch))) + 0.5 * sum(a^2)
  }
  opt <- stats::optim(log(c(v0 * 0.1, v0 * 0.9, r0)), negll,
                      method = "Nelder-Mead",
                      control = list(maxit = 800L))
  p <- exp(opt$par)
  list(model = model, nugget = p[1], psill = p[2], range = p[3],
       loglik = -opt$value, converged = opt$convergence == 0,
       method = "variogram ML (rmorie native)")
}

#' Ordinary kriging prediction
#'
#' Solves the ordinary-kriging system with the supplied (or ML-fitted)
#' variogram model and predicts at new locations, returning kriging
#' variances.
#'
#' @param coords,values Observed locations and values.
#' @param new_coords Matrix of prediction locations.
#' @param vgm A fit from \code{\link{morie_spatial_variogram_fit}}, or
#'   \code{NULL} to fit an exponential model first.
#' @return A data frame with \code{pred} and \code{var}.
#' @references Cressie, N. (1993). \emph{Statistics for Spatial Data}.
#' @export

# Fast WLS fit of a variogram model on the binned empirical variogram
# (Cressie 1985 weights n_h / h^2) -- the gstat::fit.variogram
# analogue. Milliseconds at any n; the full Gaussian-likelihood MLE
# stays available via morie_spatial_variogram_fit().
.morie_vgm_wls_fit <- function(coords, values, model = "exponential") {
  emp <- morie_spatial_variogram(coords, values)
  h <- emp$dist
  g <- emp$gamma
  w <- emp$np / pmax(h^2, 1e-12)
  ok <- is.finite(h) & is.finite(g) & is.finite(w) & h > 0
  h <- h[ok]; g <- g[ok]; w <- w[ok]
  v0 <- max(g, na.rm = TRUE)
  obj <- function(p) {
    nug <- exp(p[1]); ps <- exp(p[2]); rg <- exp(p[3])
    fit <- .morie_vgm_gamma(h, model, nug, ps, rg)
    sum(w * (g - fit)^2)
  }
  opt <- stats::optim(log(c(v0 * 0.1 + 1e-8, v0 * 0.9 + 1e-8,
                            max(h) / 3)), obj,
                      method = "Nelder-Mead",
                      control = list(maxit = 500L))
  list(model = model, nugget = exp(opt$par[1]),
       psill = exp(opt$par[2]), range = exp(opt$par[3]),
       method = "WLS (Cressie weights)")
}

morie_spatial_krige <- function(coords, values, new_coords,
                                vgm = NULL) {
  coords <- as.matrix(coords)
  new_coords <- as.matrix(new_coords)
  y <- as.numeric(values)
  n <- length(y)
  if (is.null(vgm)) {
    # Default to the fast WLS fit; pass vgm =
    # morie_spatial_variogram_fit(...) explicitly for the full
    # Gaussian-likelihood MLE (slower, higher quality).
    vgm <- .morie_vgm_wls_fit(coords, y)
  }
  G <- .morie_vgm_gamma(as.matrix(stats::dist(coords)),
                        vgm$model, vgm$nugget, vgm$psill, vgm$range)
  A <- rbind(cbind(G, 1), c(rep(1, n), 0))
  A_inv <- tryCatch(solve(A), error = function(e) .morie_ginv(A))
  # cross-distances obs x new
  cross2 <- outer(rowSums(coords^2), rowSums(new_coords^2), "+") -
    2 * coords %*% t(new_coords)
  cross <- sqrt(pmax(cross2, 0))
  g_new <- .morie_vgm_gamma(cross, vgm$model, vgm$nugget, vgm$psill,
                            vgm$range)
  B <- rbind(g_new, 1)
  W <- A_inv %*% B
  pred <- as.numeric(t(W[seq_len(n), , drop = FALSE]) %*% y)
  kv <- colSums(W * B)
  data.frame(pred = pred, var = pmax(as.numeric(kv), 0))
}
