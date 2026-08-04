# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal: bounding region as c(xmin, ymin, xmax, ymax).
#' @param region Either a length-4 box or an (m, 2) matrix of vertices.
#' @param points Fallback point set whose bounding box is used.
#' @return Numeric vector of length 4.
#' @noRd
.sp_region <- function(region = NULL, points = NULL) {
  if (is.null(region)) {
    if (is.null(points)) stop("`region` is required when `points` is not given")
    p <- as.matrix(points)
    r <- c(min(p[, 1]), min(p[, 2]), max(p[, 1]), max(p[, 2]))
  } else if (is.null(dim(region)) && length(region) == 4) {
    r <- as.numeric(region)
  } else {
    m <- as.matrix(region)
    r <- c(min(m[, 1]), min(m[, 2]), max(m[, 1]), max(m[, 2]))
  }
  if (!(r[3] > r[1] && r[4] > r[2])) stop("`region` must have positive area")
  r
}

#' Internal: first-order intensity lambda_hat = N(A) / nu(A), eq (3.8).
#' @param points Event coordinates (n by 2).
#' @param region Bounding region from .sp_region.
#' @return Numeric scalar.
#' @noRd
.sp_intensity <- function(points, region) {
  nrow(as.matrix(points)) / ((region[3] - region[1]) * (region[4] - region[2]))
}

#' Internal: Ripley's K estimated as in Sec 3.4.2.
#'
#' E_tilde(h) = (1/n) sum_i sum_{j != i} I(h_ij <= h); K = E / lambda_hat.
#' The naive form is negatively biased because events outside the window
#' are unobserved, so the border correction keeps only events further
#' than h from the boundary.
#'
#' @param points Event coordinates (n by 2).
#' @param region Bounding region.
#' @param r Numeric vector of distances.
#' @param correction "border" or "none".
#' @return Numeric vector K(r).
#' @noRd
.sp_k <- function(points, region, r, correction = "border") {
  p <- as.matrix(points)
  n <- nrow(p)
  r <- as.numeric(r)
  if (any(r < 0)) stop("`r` must be non-negative")
  lam <- .sp_intensity(p, region)
  d <- as.matrix(stats::dist(p))
  diag(d) <- Inf
  if (identical(correction, "none")) {
    return(vapply(r, function(h) sum(d <= h) / n / lam, numeric(1)))
  }
  if (!identical(correction, "border")) {
    stop("`correction` must be 'border' or 'none'")
  }
  db <- pmin(
    p[, 1] - region[1], region[3] - p[, 1],
    p[, 2] - region[2], region[4] - p[, 2]
  )
  vapply(r, function(h) {
    keep <- db > h
    m <- sum(keep)
    if (m > 0) sum(d[keep, , drop = FALSE] <= h) / m / lam else NA_real_
  }, numeric(1))
}

#' Internal: nearest-neighbour distances h_i.
#' @param points Event coordinates (n by 2).
#' @return Numeric vector of length n.
#' @noRd
.sp_nn <- function(points) {
  p <- as.matrix(points)
  if (nrow(p) < 2) {
    stop("at least two events are needed for a nearest-neighbour distance")
  }
  d <- as.matrix(stats::dist(p))
  diag(d) <- Inf
  apply(d, 1, min)
}
