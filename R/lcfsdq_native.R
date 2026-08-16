# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of lcfsdq -- local-cluster first-order (nearest-neighbour)
# distance query. Mirrors src/morie/fn/lcfsdq.py operation for
# operation, on the shared numerics in R/aaa_helpers_w3num.R.
#
# Two questions run together here because they answer each other.
#
# The first is about the POINTS: are they clustered, spread out, or
# consistent with complete spatial randomness? The first-order
# nearest-neighbour distance is the classical way in -- under a
# homogeneous Poisson process of intensity lambda the expected distance
# to the nearest other point is 1 / (2 sqrt(lambda)), so the ratio of the
# observed mean to that expectation is a scale-free index. Below one is
# clustering, above one is regularity, and Clark and Evans' z statistic
# says whether the departure is bigger than sampling noise.
#
# The second is about a VALUE attached to the points: given a
# neighbourhood defined by those same distances, how does the local mean
# of x compare with the global one? The neighbourhood radius is not
# invented -- it is the mean nearest-neighbour distance plus a multiple
# of its standard deviation, so it is the scale the pattern itself sets.
# That is what "first-order SD" names.
#
# Edge effects are the thing that quietly ruins this. A point near the
# boundary has neighbours outside the window that were never observed,
# so its nearest-neighbour distance is too long and the index drifts
# towards "regular". Three treatments, all selectable, and the choice
# travels in the result:
#
#   "none"      no correction. Honest for a window much larger than the
#               typical spacing, wrong otherwise.
#   "donnelly"  Donnelly's correction to the expectation and variance,
#               adding a perimeter term. Cheap, and the standard choice
#               for a rectangular window.
#   "buffer"    discard points within one neighbourhood radius of the
#               boundary when computing the summary, but still allow
#               them to serve as neighbours. This throws data away and
#               is the most defensible: the retained points have
#               complete neighbourhoods by construction.
#
# Metrics: euclidean, manhattan, chebyshev. The metric is not decoration
# -- on a street grid the Manhattan distance is the real one, and using
# Euclidean there understates every distance by up to a factor of
# sqrt(2).
#
# References
#   Clark, P.J. and Evans, F.C. (1954) "Distance to nearest neighbor as
#     a measure of spatial relationships in populations." Ecology 35(4),
#     445-453.
#   Donnelly, K. (1978) "Simulations to determine the variance and
#     edge-effect of total nearest neighbour distance." In I. Hodder
#     (ed.), Simulation Methods in Archaeology, Cambridge University
#     Press, 91-95.
#   Diggle, P.J. (2003) "Statistical Analysis of Spatial Point
#     Patterns," 2nd edition. Arnold, chapter 2.

.LCFSDQ_METRICS <- c("euclidean", "manhattan", "chebyshev")
.LCFSDQ_EDGE <- c("none", "donnelly", "buffer")

#' .lcfsdq_d
#'
#' A step of the lcfsdq_native implementation. Called by \code{morie_lcfsdq}, \code{morie_lcfsdq_nn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param metric One of \code{"chebyshev"}, \code{"euclidean"}, \code{"manhattan"}.
#' @return Nothing; this branch always raises.
#' @export
.lcfsdq_d <- function(a, b, metric) {
  if (metric == "euclidean") return(sqrt(.w3_csum((a - b) * (a - b))))
  if (metric == "manhattan") return(.w3_csum(abs(a - b)))
  if (metric == "chebyshev") return(max(abs(a - b)))
  stop("metric must be one of ", paste(.LCFSDQ_METRICS, collapse = ", "))
}

#' Distance from each point to its k-th nearest OTHER point
#'
#' Ties are broken by index so the two arms select the same neighbour
#' when two are equidistant -- which happens constantly on a lattice and
#' would otherwise make the local statistics disagree while the
#' distances matched.
#'
#' @param coords Point coordinate matrix.
#' @param k Which neighbour.
#' @param metric A member of the metric list.
#' @return A list with the distances and the neighbour indices, the
#'   latter zero-based to match the Python arm.
#' @export
morie_lcfsdq_nn <- function(coords, k = 1L, metric = "euclidean") {
  n <- nrow(coords)
  k <- as.integer(k)
  if (k < 1L || k >= n) stop("k must lie in 1..n-1")
  dd <- numeric(n)
  ii <- integer(n)
  for (i in seq_len(n)) {
    js <- setdiff(seq_len(n), i)
    dv <- vapply(js, function(j) .lcfsdq_d(coords[i, ], coords[j, ], metric),
                 numeric(1))
    o <- order(dv, js)
    dd[i] <- dv[o[k]]
    ii[i] <- js[o[k]] - 1L
  }
  list(dist = dd, index = ii)
}

# Bounding box area and perimeter, in the first two coordinates.
#' Bounding box area and perimeter, in the first two coordinates
#'
#' A step of the lcfsdq_native implementation. Called by \code{morie_lcfsdq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param coords A matrix; indexed by row and column.
#' @return A list with \code{area}, \code{perimeter}, \code{bb}.
#' @export
.lcfsdq_window <- function(coords) {
  xs <- coords[, 1]; ys <- coords[, 2]
  w <- max(xs) - min(xs)
  h <- max(ys) - min(ys)
  list(area = w * h, perimeter = 2 * (w + h),
       bb = c(min(xs), max(xs), min(ys), max(ys)))
}

#' The Clark-Evans index and its z statistic
#'
#' R is the observed mean over the expectation 1 / (2 sqrt(lambda))
#' under complete spatial randomness. Donnelly's correction adds a
#' perimeter term to both the expectation and the variance, which is
#' what stops a small window reading as regular.
#'
#' @param dists Nearest-neighbour distances.
#' @param n Number of points in the pattern.
#' @param area Window area.
#' @param perimeter Window perimeter.
#' @param edge "none" or "donnelly".
#' @return A list with R, the observed and expected means, the standard
#'   error, z and p.
#' @export
morie_lcfsdq_clark_evans <- function(dists, n, area, perimeter,
                                     edge = "none") {
  if (!(edge %in% .LCFSDQ_EDGE))
    stop("edge must be one of ", paste(.LCFSDQ_EDGE, collapse = ", "))
  if (area <= 0) stop("the window has zero area")
  lam <- n / area
  obs <- .w3_csum(dists) / length(dists)
  exp_d <- 0.5 / sqrt(lam)
  var_d <- (4 - pi) / (4 * pi * lam * n)
  if (edge == "donnelly") {
    exp_d <- 0.5 * sqrt(area / n) + (0.0514 + 0.041 / sqrt(n)) * perimeter / n
    var_d <- 0.0703 * area / (n * n) +
      0.037 * perimeter * sqrt(area / (n * n * n * n * n))
  }
  se <- if (var_d > 0) sqrt(var_d) else NaN
  z <- if (!is.nan(se) && se > 0) (obs - exp_d) / se else NaN
  list(R = if (exp_d > 0) obs / exp_d else NaN, observed = obs,
       expected = exp_d, se = se, z = z,
       p = if (!is.nan(z)) 2 * (1 - .w3_ncdf(abs(z))) else NaN,
       lambda = lam, edge = edge)
}

#' Nearest-neighbour summary of a pattern and a local query on x
#'
#' @param x A value attached to each point. Pass a constant if only the
#'   pattern is of interest.
#' @param coords Point coordinates. The first two are taken as the plane
#'   for the window; the distance uses all of them.
#' @param k Which nearest neighbour to use for the first-order distance.
#' @param metric A member of the metric list.
#' @param edge A member of the edge list.
#' @param sd_multiplier The neighbourhood radius is the mean distance
#'   plus this times its standard deviation.
#' @param area Window area. Taken from the bounding box when omitted,
#'   which UNDERSTATES the true window whenever the points do not reach
#'   its edges -- so the index reads as more clustered than it is, and
#'   passing the real window is worth doing.
#' @param perimeter Window perimeter, likewise.
#' @param grid Radii at which the empirical G function is reported.
#' @return A list with the nearest-neighbour distances, the Clark-Evans
#'   index and test, the neighbourhood radius, per-point local means and
#'   counts, the points flagged as locally clustered or isolated, and
#'   the G function against its complete-spatial-randomness expectation.
#' @export
morie_lcfsdq <- function(x, coords, k = 1L, metric = "euclidean",
                         edge = "none", sd_multiplier = 1, area = NULL,
                         perimeter = NULL, grid = NULL) {
  if (!(metric %in% .LCFSDQ_METRICS))
    stop("metric must be one of ", paste(.LCFSDQ_METRICS, collapse = ", "))
  if (!(edge %in% .LCFSDQ_EDGE))
    stop("edge must be one of ", paste(.LCFSDQ_EDGE, collapse = ", "))
  pts <- as.matrix(coords)
  storage.mode(pts) <- "double"
  n <- nrow(pts)
  if (n < 3L) stop("need at least three points")
  if (ncol(pts) < 2L)
    stop("coordinates must have at least two dimensions")
  xv <- as.numeric(x)
  if (length(xv) != n) stop("x and coords must have the same length")

  nn <- morie_lcfsdq_nn(pts, k, metric)
  dists <- nn$dist
  mean_d <- .w3_csum(dists) / n
  sd_d <- sqrt(.w3_csum((dists - mean_d) * (dists - mean_d)) / (n - 1))
  radius <- mean_d + as.numeric(sd_multiplier) * sd_d

  win <- .lcfsdq_window(pts)
  A <- if (is.null(area)) win$area else as.numeric(area)
  P <- if (is.null(perimeter)) win$perimeter else as.numeric(perimeter)
  bb <- win$bb

  if (edge == "buffer") {
    keep <- which(pts[, 1] - bb[1] >= radius & bb[2] - pts[, 1] >= radius &
                    pts[, 2] - bb[3] >= radius & bb[4] - pts[, 2] >= radius)
    if (length(keep) < 3L)
      stop("the buffer left fewer than three points; use a smaller ",
           "sd_multiplier or another edge correction")
    ce <- morie_lcfsdq_clark_evans(dists[keep], n, A, P, "none")
    ce$edge <- "buffer"
    ce$n_kept <- length(keep)
  } else {
    ce <- morie_lcfsdq_clark_evans(dists, n, A, P, edge)
    ce$n_kept <- n
  }

  gmean <- .w3_csum(xv) / n
  gsd <- if (n > 1L) sqrt(.w3_csum((xv - gmean) * (xv - gmean)) / (n - 1)) else 0

  local_mean <- numeric(n); local_count <- integer(n); local_z <- numeric(n)
  for (i in seq_len(n)) {
    js <- setdiff(seq_len(n), i)
    members <- js[vapply(js, function(j)
      .lcfsdq_d(pts[i, ], pts[j, ], metric) <= radius, logical(1))]
    lm <- if (length(members)) .w3_csum(xv[members]) / length(members) else NaN
    local_mean[i] <- lm
    local_count[i] <- length(members)
    # A z against the sampling distribution of a mean of that many
    # draws, which is the only comparison fair to a neighbourhood of two
    # and one of twenty at the same time.
    local_z[i] <- if (length(members) && gsd > 0)
      (lm - gmean) / (gsd / sqrt(length(members))) else NaN
  }

  clustered <- which(dists < mean_d - as.numeric(sd_multiplier) * sd_d) - 1L
  isolated <- which(dists > mean_d + as.numeric(sd_multiplier) * sd_d) - 1L

  if (is.null(grid)) grid <- vapply(1:8, function(t) radius * t / 8, numeric(1))
  grid <- as.numeric(grid)
  lam <- n / A
  gfun <- vapply(grid, function(r) sum(dists <= r) / n, numeric(1))
  # The CSR expectation for the nearest-neighbour distance, which is a
  # two-dimensional statement: an area, not a length.
  gcsr <- vapply(grid, function(r) 1 - exp(-lam * pi * r * r), numeric(1))

  list(nn_distance = dists, nn_index = nn$index, mean_nn = mean_d,
       sd_nn = sd_d, radius = radius, clark_evans = ce, R = ce$R, z = ce$z,
       p = ce$p, local_mean = local_mean, local_count = local_count,
       local_z = local_z, clustered = clustered, isolated = isolated,
       n_clustered = length(clustered), n_isolated = length(isolated),
       grid = grid, G = gfun, G_csr = gcsr, area = A, perimeter = P,
       global_mean = gmean, global_sd = gsd, n = n, k = as.integer(k),
       metric = metric, edge = edge, estimate = ce$R, se = ce$se,
       method = "first-order nearest-neighbour cluster query")
}

#' One-line summary of the lcfsdq module
#'
#' @return A character scalar.
#' @export
morie_lcfsdq_cheatsheet <- function()
  paste0("lcfsdq: first-order nearest-neighbour cluster query. metrics ",
         paste(.LCFSDQ_METRICS, collapse = ", "), "; edge ",
         paste(.LCFSDQ_EDGE, collapse = ", "))
