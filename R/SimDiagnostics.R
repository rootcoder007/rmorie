.sd_model <- function(cov_model, sill, range_, nu) {
  m <- list(model = switch(cov_model, exponential = "Exp", gaussian = "Gau", matern = "Mat"), psill = sill, range = range_)
  if (cov_model == "matern") m$kappa <- nu
  m
}

#' Turning-bands conditioning, ensembles and diagnostics of simulations
#'
#' \code{ConditionalTurningBands}: simple-kriging correction of an
#' unconditional turning-bands realisation (Journel 1974). \code{TbEnsemble}:
#' pointwise and realisation moments and the ensemble variogram against the
#' model. \code{TbBandConvergence}: RMSE of the ensemble covariance against
#' the model by number of bands. \code{StandardiseRealisations}: affine
#' correction to a target mean and variance. \code{DirectionalVariogram}:
#' directional semivariograms (degrees clockwise from north), as
#' \code{gstat::variogram} with \code{alpha} and \code{tol.hor}.
#' \code{ClassProportions}, \code{BoundaryProbability},
#' \code{Connectivity} and \code{IndicatorVariogram}: diagnostics of
#' categorical realisations (row-major grids). Identical to the Python arm
#' \code{morie.fn.simdiag}; cell indices are 1-based here.
#'
#' @param z Conditioning data.
#' @param data_coords,coords Data and simulation coordinates.
#' @param cov_model,sill,range_,nu Covariance (as \code{TurningBands}).
#' @param mean,variance Target moments.
#' @param n_bands,n_waves,seed Turning-bands settings.
#' @param nsim Number of realisations.
#' @param bounds,boundaries Distance class boundaries.
#' @param bands Numbers of bands to compare.
#' @param realisations List of realisations.
#' @param values Values.
#' @param alphas Directions in degrees clockwise from north.
#' @param tol Angular tolerance in degrees.
#' @param classes Class labels.
#' @param target Target proportions or class.
#' @param nrow,ncol Grid size.
#' @param pairs List of cell-index pairs.
#' @param neighbourhood 4 or 8.
#' @param lags Integer lags.
#' @return List.
#' @references Journel, A. G. (1974). Geostatistics for conditional
#'   simulation of ore bodies. Economic Geology 69, 673-687.
#'
#'   Emery, X. and Lantuejoul, C. (2006). TBSIM: a computer program for
#'   conditional simulation of three-dimensional Gaussian random fields via
#'   the turning bands method. Computers and Geosciences 32, 1615-1628.
#'
#'   Renard, P. and Allard, D. (2013). Connectivity metrics for subsurface
#'   flow and transport. Advances in Water Resources 51, 168-196.
#' @examples
#' ConditionalTurningBands(2, rbind(c(0, 0)), rbind(c(0, 0), c(3, 0)), mean = 1)$field
#' Connectivity(list(c(1, 1, 0, 1)), 1, 4, 1, pairs = list(c(1, 2), c(1, 4)))$pair_probability
#' @export
ConditionalTurningBands <- function(z, data_coords, coords, cov_model = "exponential", sill = 1, range_ = 1, nu = 0.5,
                                    mean = 0, n_bands = 64L, n_waves = 50L, seed = 1) {
  D <- as.matrix(data_coords)
  Q <- as.matrix(coords)
  nd <- nrow(D)
  f <- TurningBands(rbind(D, Q), cov_model, sill = sill, range_ = range_, nu = nu, n_bands = n_bands,
                    n_waves = n_waves, seed = seed)$field
  m <- .sd_model(cov_model, sill, range_, nu)
  kz <- Krige(z, D, Q, m, beta = mean)$prediction
  ks <- Krige(f[seq_len(nd)], D, Q, m, beta = 0)$prediction
  list(field = kz + f[nd + seq_len(nrow(Q))] - ks, unconditional = f[nd + seq_len(nrow(Q))], kriging = kz)
}

.sd_semivariogram <- function(v, P, b) {
  D <- as.matrix(stats::dist(P))
  ut <- upper.tri(D)
  h <- D[ut]
  sq <- (0.5 * outer(v, v, "-")^2)[ut]
  vapply(seq_len(length(b) - 1), function(k) {
    s <- h > b[k] & h <= b[k + 1]
    if (any(s)) mean(sq[s]) else NaN
  }, 0)
}

#' @rdname ConditionalTurningBands
#' @export
TbEnsemble <- function(coords, cov_model = "exponential", sill = 1, range_ = 1, nu = 0.5, nsim = 20L, n_bands = 64L,
                       n_waves = 50L, seed = 1, bounds = NULL) {
  P <- as.matrix(coords)
  sims <- lapply(seq_len(nsim) - 1, function(r) {
    TurningBands(P, cov_model, sill = sill, range_ = range_, nu = nu, n_bands = n_bands, n_waves = n_waves,
                 seed = seed + r)$field
  })
  S <- do.call(rbind, sims)
  out <- list(realisations = sims, pointwise_mean = colMeans(S), pointwise_variance = apply(S, 2, stats::var),
              realisation_variance = apply(S, 1, stats::var))
  if (!is.null(bounds)) {
    out$variogram <- colMeans(do.call(rbind, lapply(sims, .sd_semivariogram, P = P, b = bounds)))
    D <- as.matrix(stats::dist(P))
    h <- D[upper.tri(D)]
    mids <- vapply(seq_len(length(bounds) - 1), function(k) {
      s <- h > bounds[k] & h <= bounds[k + 1]
      if (any(s)) mean(h[s]) else NaN
    }, 0)
    m <- .sd_model(cov_model, sill, range_, nu)
    out$model_variogram <- ifelse(is.na(mids), NaN, sill - KrigingCovariance(ifelse(is.na(mids), 0, mids), m))
  }
  out
}

#' @rdname ConditionalTurningBands
#' @export
TbBandConvergence <- function(coords, cov_model = "exponential", sill = 1, range_ = 1, nu = 0.5, bands = c(4, 16, 64),
                              nsim = 50L, n_waves = 50L, seed = 1) {
  P <- as.matrix(coords)
  m <- .sd_model(cov_model, sill, range_, nu)
  C0 <- matrix(KrigingCovariance(as.matrix(stats::dist(P)), m), nrow(P))
  rmse <- vapply(bands, function(L) {
    S <- do.call(rbind, lapply(seq_len(nsim) - 1, function(r) {
      TurningBands(P, cov_model, sill = sill, range_ = range_, nu = nu, n_bands = L, n_waves = n_waves,
                   seed = seed + r)$field
    }))
    E <- stats::cov(S) - C0
    sqrt(mean(E[upper.tri(E, diag = TRUE)]^2))
  }, 0)
  list(bands = bands, rmse = rmse)
}

#' @rdname ConditionalTurningBands
#' @export
StandardiseRealisations <- function(realisations, mean = 0, variance = 1) {
  k <- vapply(realisations, function(s) sqrt(variance) / stats::sd(s), 0)
  list(realisations = lapply(seq_along(realisations), function(r) {
    s <- realisations[[r]]
    mean + (s - base::mean(s)) * k[r]
  }), scale = k)
}

#' @rdname ConditionalTurningBands
#' @export
DirectionalVariogram <- function(values, coords, alphas, boundaries, tol = 22.5) {
  P <- as.matrix(coords)
  n <- nrow(P)
  ij <- which(upper.tri(diag(n)), arr.ind = TRUE)
  dx <- P[ij[, 2], 1] - P[ij[, 1], 1]
  dy <- P[ij[, 2], 2] - P[ij[, 1], 2]
  h <- sqrt(dx^2 + dy^2)
  ang <- (atan2(dx, dy) * 180 / pi) %% 180
  sq <- 0.5 * (values[ij[, 1]] - values[ij[, 2]])^2
  res <- lapply(alphas, function(a) {
    d <- abs(ang - a %% 180)
    d <- pmin(d, 180 - d)
    t(vapply(seq_len(length(boundaries) - 1), function(k) {
      s <- d <= tol & h > boundaries[k] & h <= boundaries[k + 1]
      c(if (any(s)) mean(sq[s]) else NaN, sum(s), if (any(s)) mean(h[s]) else NaN)
    }, numeric(3)))
  })
  G <- lapply(res, function(r) r[, 1])
  ai <- vapply(seq_len(length(boundaries) - 1), function(k) {
    v <- vapply(G, `[`, 0, k)
    v <- v[!is.na(v)]
    if (length(v) && min(v) > 0) max(v) / min(v) else NaN
  }, 0)
  list(gamma = G, np = lapply(res, function(r) as.integer(r[, 2])), dist = lapply(res, function(r) r[, 3]),
       anisotropy_index = ai)
}

#' @rdname ConditionalTurningBands
#' @export
ClassProportions <- function(realisations, classes, target = NULL) {
  props <- t(vapply(realisations, function(s) vapply(classes, function(c) mean(s == c), 0), numeric(length(classes))))
  out <- list(proportions = props, mean = colMeans(props), sd = if (nrow(props) > 1) apply(props, 2, stats::sd) else 0)
  if (!is.null(target)) out$deviation <- out$mean - target
  out
}

#' @rdname ConditionalTurningBands
#' @export
BoundaryProbability <- function(realisations, nrow, ncol, classes) {
  R <- length(realisations)
  B <- matrix(0, nrow, ncol)
  Pc <- lapply(classes, function(c) matrix(0, nrow, ncol))
  for (s in realisations) {
    g <- matrix(s, nrow, ncol, byrow = TRUE)
    for (i in seq_len(nrow)) {
      for (j in seq_len(ncol)) {
        nb <- c(if (i > 1) g[i - 1, j], if (i < nrow) g[i + 1, j], if (j > 1) g[i, j - 1], if (j < ncol) g[i, j + 1])
        if (any(nb != g[i, j])) B[i, j] <- B[i, j] + 1 / R
      }
    }
    for (k in seq_along(classes)) Pc[[k]] <- Pc[[k]] + (g == classes[k]) / R
  }
  ent <- -Reduce(`+`, lapply(Pc, function(p) ifelse(p > 0, p * log(p), 0)))
  list(boundary = B, class_probability = Pc, entropy = ent)
}

#' @rdname ConditionalTurningBands
#' @export
Connectivity <- function(realisations, nrow, ncol, target, pairs = list(), neighbourhood = 4L) {
  offs <- if (neighbourhood == 4) rbind(c(-1, 0), c(1, 0), c(0, -1), c(0, 1)) else
    rbind(c(-1, -1), c(-1, 0), c(-1, 1), c(0, -1), c(0, 1), c(1, -1), c(1, 0), c(1, 1))
  R <- length(realisations)
  conn <- numeric(length(pairs))
  res <- lapply(realisations, function(s) {
    N <- nrow * ncol
    par <- seq_len(N)
    find <- function(a) {
      while (par[a] != a) {
        par[a] <<- par[par[a]]
        a <- par[a]
      }
      a
    }
    for (idx in seq_len(N)) {
      if (s[idx] != target) next
      i <- (idx - 1) %/% ncol
      j <- (idx - 1) %% ncol
      for (o in seq_len(nrow(offs))) {
        x <- i + offs[o, 1]
        y <- j + offs[o, 2]
        if (x >= 0 && x < nrow && y >= 0 && y < ncol && s[x * ncol + y + 1] == target) {
          ra <- find(idx)
          rb <- find(x * ncol + y + 1)
          if (ra != rb) par[max(ra, rb)] <- min(ra, rb)
        }
      }
    }
    on <- which(s == target)
    roots <- vapply(on, find, 0)
    left <- unique(vapply(Filter(function(r) s[r * ncol + 1] == target, 0:(nrow - 1)), function(r) find(r * ncol + 1), 0))
    right <- unique(vapply(Filter(function(r) s[r * ncol + ncol] == target, 0:(nrow - 1)), function(r) find(r * ncol + ncol), 0))
    pc <- vapply(pairs, function(p) s[p[1]] == target && s[p[2]] == target && find(p[1]) == find(p[2]), TRUE)
    list(nc = length(unique(roots)), sizes = sort(as.vector(table(roots)), decreasing = TRUE),
         perc = length(intersect(left, right)) > 0, pc = pc)
  })
  perc <- vapply(res, `[[`, TRUE, "perc")
  list(n_components = vapply(res, `[[`, 0L, "nc"), sizes = lapply(res, `[[`, "sizes"), percolates = perc,
       percolation_probability = mean(perc),
       pair_probability = if (length(pairs)) colMeans(do.call(rbind, lapply(res, `[[`, "pc"))) else numeric(0))
}

#' @rdname ConditionalTurningBands
#' @export
IndicatorVariogram <- function(realisations, nrow, ncol, target, lags) {
  one <- function(s) {
    g <- matrix(as.numeric(s == target), nrow, ncol, byrow = TRUE)
    gx <- vapply(lags, function(h) if (h < ncol) mean((g[, 1:(ncol - h), drop = FALSE] - g[, (1 + h):ncol, drop = FALSE])^2) / 2 else NaN, 0)
    gy <- vapply(lags, function(h) if (h < nrow) mean((g[1:(nrow - h), , drop = FALSE] - g[(1 + h):nrow, , drop = FALSE])^2) / 2 else NaN, 0)
    list(x = gx, y = gy)
  }
  r <- lapply(realisations, one)
  X <- do.call(rbind, lapply(r, `[[`, "x"))
  Y <- do.call(rbind, lapply(r, `[[`, "y"))
  list(x = X, y = Y, mean_x = colMeans(X), mean_y = colMeans(Y))
}
