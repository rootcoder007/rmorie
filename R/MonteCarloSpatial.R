.mc_in_poly <- function(pt, P) {
  inside <- FALSE
  n <- nrow(P)
  for (i in seq_len(n)) {
    j <- if (i == n) 1 else i + 1
    if ((P[i, 2] > pt[2]) != (P[j, 2] > pt[2]) &&
          pt[1] < P[i, 1] + (pt[2] - P[i, 2]) * (P[j, 1] - P[i, 1]) / (P[j, 2] - P[i, 2])) inside <- !inside
  }
  inside
}

.mc_area <- function(P) {
  n <- nrow(P)
  j <- c(2:n, 1)
  abs(sum(P[, 1] * P[j, 2] - P[j, 1] * P[, 2])) / 2
}

.mc_points <- function(n, bounds, seed, stream, method) {
  d <- nrow(bounds)
  if (method == "lhs") {
    U <- vapply(seq_len(d), function(j) {
      u <- .morie_random_uniform(2 * n, seed = seed, stream = stream + j - 1)
      perm <- seq_len(n) - 1
      for (t in seq_len(n)) {
        k <- t + floor(u[t] * (n - t + 1))
        tmp <- perm[t]
        perm[t] <- perm[k]
        perm[k] <- tmp
      }
      (perm + u[n + seq_len(n)]) / n
    }, numeric(n))
    U <- matrix(U, n, d)
  } else {
    m <- if (method == "antithetic") n %/% 2 else n
    U <- matrix(.morie_random_uniform(m * d, seed = seed, stream = stream), m, d, byrow = TRUE)
    if (method == "antithetic") U <- rbind(U, 1 - U)
  }
  sweep(sweep(U, 2, bounds[, 2] - bounds[, 1], `*`), 2, bounds[, 1], `+`)
}

#' Spatial Monte Carlo estimation
#'
#' \code{McIntegrate}: integral and mean of \code{f} over a box or polygon
#' (plain, antithetic or Latin hypercube sampling). \code{McStratified}:
#' stratified estimate with proportional or Neyman allocation.
#' \code{McZonal}: hit-or-miss zone area and zone mean.
#' \code{McControlVariate}: control-variate estimator. \code{McConvergence}:
#' running mean band and batch-means standard error. Identical to the Python
#' arm \code{morie.fn.mcspatial}.
#'
#' @param f Function of the coordinates.
#' @param bounds Matrix of (lower, upper) rows per dimension.
#' @param polygon Two-column polygon vertices.
#' @param n Number of points.
#' @param seed Philox seed.
#' @param method \code{"plain"}, \code{"antithetic"} or \code{"lhs"}.
#' @param strata List of polygons.
#' @param allocation \code{"proportional"} or \code{"neyman"}.
#' @param pilot_sd Pilot standard deviations per stratum.
#' @param indicator Zone membership function.
#' @param value Function averaged over the zone.
#' @param y,g Samples and control variate.
#' @param g_mean Known mean of the control.
#' @param samples Monte Carlo sample.
#' @param batches Number of batches.
#' @return List.
#' @references Cochran, W. G. (1977). Sampling Techniques, 3rd ed. Wiley.
#'
#'   McKay, M. D., Beckman, R. J. and Conover, W. J. (1979). A comparison of
#'   three methods for selecting values of input variables in the analysis of
#'   output from a computer code. Technometrics 21, 239-245.
#'
#'   Flegal, J. M., Haran, M. and Jones, G. L. (2008). Markov chain Monte
#'   Carlo: can we trust the third significant figure? Statistical Science 23,
#'   250-260.
#' @examples
#' McIntegrate(function(x, y) x + y, bounds = rbind(c(0, 1), c(0, 1)), n = 2000,
#'   method = "lhs")$integral
#' McConvergence(c(1, 3, 2, 4), batches = 2)$batch_se
#' @export
McIntegrate <- function(f, bounds = NULL, polygon = NULL, n = 10000, seed = 1, method = "plain") {
  if (!is.null(polygon)) {
    P <- as.matrix(polygon)
    bounds <- rbind(range(P[, 1]), range(P[, 2]))
    area <- .mc_area(P)
  } else {
    bounds <- as.matrix(bounds)
    area <- prod(bounds[, 2] - bounds[, 1])
  }
  X <- .mc_points(n, bounds, seed, 0, method)
  if (!is.null(polygon)) X <- X[apply(X, 1, function(x) .mc_in_poly(x, P)), , drop = FALSE]
  vals <- apply(X, 1, function(x) do.call(f, as.list(x)))
  m <- length(vals)
  mn <- mean(vals)
  se <- if (method == "antithetic" && is.null(polygon)) {
    h <- m %/% 2
    pairs <- (vals[seq_len(h)] + vals[h + seq_len(h)]) / 2
    sqrt(sum((pairs - mn)^2) / (h - 1) / h)
  } else {
    sqrt(sum((vals - mn)^2) / (m - 1) / m)
  }
  list(mean = mn, integral = mn * area, se_mean = se, se_integral = se * area, n_used = m, area = area)
}

#' @rdname McIntegrate
#' @export
McStratified <- function(f, strata, n = 1000, allocation = "proportional", pilot_sd = NULL, seed = 1) {
  polys <- lapply(strata, as.matrix)
  areas <- vapply(polys, .mc_area, 0)
  W <- areas / sum(areas)
  nh <- switch(allocation,
    proportional = pmax(2, round(n * W)),
    neyman = pmax(2, round(n * W * pilot_sd / sum(W * pilot_sd))),
    stop("allocation must be proportional or neyman")
  )
  res <- lapply(seq_along(polys), function(h) {
    p <- polys[[h]]
    bx <- rbind(range(p[, 1]), range(p[, 2]))
    got <- matrix(0, 0, 2)
    stream <- 0
    while (nrow(got) < nh[h]) {
      cand <- .mc_points(4 * nh[h], bx, seed, 1000 * (h - 1) + stream, "plain")
      got <- rbind(got, cand[apply(cand, 1, function(x) .mc_in_poly(x, p)), , drop = FALSE])
      stream <- stream + 1
    }
    v <- apply(got[seq_len(nh[h]), , drop = FALSE], 1, function(x) do.call(f, as.list(x)))
    c(mean(v), stats::var(v))
  })
  mh <- vapply(res, `[`, 0, 1)
  vh <- vapply(res, `[`, 0, 2)
  list(mean = sum(W * mh), se = sqrt(sum(W^2 * vh / nh)), n_h = nh, stratum_means = mh, weights = W)
}

#' @rdname McIntegrate
#' @export
McZonal <- function(indicator, bounds, value = NULL, n = 10000, seed = 1) {
  B <- as.matrix(bounds)
  X <- .mc_points(n, B, seed, 0, "plain")
  hit <- apply(X, 1, function(x) isTRUE(do.call(indicator, as.list(x))))
  p <- mean(hit)
  box <- prod(B[, 2] - B[, 1])
  out <- list(area = box * p, se_area = box * sqrt(p * (1 - p) / n), hits = sum(hit))
  if (!is.null(value) && any(hit)) {
    v <- apply(X[hit, , drop = FALSE], 1, function(x) do.call(value, as.list(x)))
    out$zone_mean <- mean(v)
    out$zone_mean_se <- if (length(v) > 1) stats::sd(v) / sqrt(length(v)) else NaN
  }
  out
}

#' @rdname McIntegrate
#' @export
McControlVariate <- function(y, g, g_mean) {
  n <- length(y)
  b <- stats::cov(y, g) / stats::var(g)
  r2 <- stats::cor(y, g)^2
  list(estimate = mean(y) - b * (mean(g) - g_mean), b = b, se = sqrt(stats::var(y) * (1 - r2) / n),
       variance_reduction = 1 - r2)
}

#' @rdname McIntegrate
#' @export
McConvergence <- function(samples, batches = 20) {
  .morie_arg(samples, "n")
  x <- samples
  n <- length(x)
  run <- cumsum(x) / seq_len(n)
  sds <- vapply(seq_len(n), function(i) if (i > 1) stats::sd(x[seq_len(i)]) else NaN, 0)
  b <- n %/% batches
  bm <- vapply(seq_len(batches), function(k) mean(x[(k - 1) * b + seq_len(b)]), 0)
  bse <- stats::sd(bm) / sqrt(batches)
  iid <- stats::sd(x) / sqrt(n)
  fac <- if (iid > 0) bse / iid else NaN
  list(running_mean = run, lower = run - 1.96 * sds / sqrt(seq_len(n)), upper = run + 1.96 * sds / sqrt(seq_len(n)),
       batch_se = bse, iid_se = iid, inefficiency = fac, ess = n / fac^2)
}
