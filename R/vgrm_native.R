# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical (Matheron) variogram
#'
#' Matheron method-of-moments semivariogram estimator,
#' \eqn{\hat\gamma(h_k) = \frac{1}{2 |N(h_k)|} \sum_{(i,j) \in N(h_k)} (Z(s_i)-Z(s_j))^2},
#' where the pair set of lag bin k collects the pairs whose separation
#' distance falls in that bin. With integer `bins` the bins are
#' equal-width up to half the maximum pairwise distance; a numeric
#' vector gives explicit ascending bin edges.
#'
#' @param coords Site coordinates, n by d matrix.
#' @param values Observations, length n.
#' @param bins Number of lag bins (integer) or explicit bin edges
#'   (numeric vector, ascending).
#' @return List with lag (mean pair distance per bin), gamma, n_pairs,
#'   edges, n.
#' @references Matheron, G. (1963). Principles of geostatistics.
#'   Economic Geology, 58(8), 1246-1266.
#'
#'   Schabenberger, O. and Gotway, C. A. (2005). Statistical Methods for
#'   Spatial Data Analysis. Chapman and Hall/CRC, Sec. 4.2, eq. (4.1).
#'   Local PDF: WD_BLACK/library/pdf/Statistical_Methods_for_Spatial_Data_Analysis.pdf.
#' @examples
#' Vgrm(cbind(0:3, 0), c(0, 1, 3, 6), bins = c(0.5, 1.5, 2.5, 3.5))
#' @export
Vgrm <- function(coords, values, bins = 15L) {
  coords <- as.matrix(coords)
  values <- as.numeric(values)
  n <- length(values)
  if (nrow(coords) != n) stop("`coords` and `values` must have the same number of rows")
  pr <- utils::combn(n, 2L)
  i <- pr[1L, ]
  j <- pr[2L, ]
  d <- sqrt(rowSums((coords[i, , drop = FALSE] - coords[j, , drop = FALSE])^2))
  sq <- (values[i] - values[j])^2
  if (length(bins) == 1L) {
    n_bins <- as.integer(bins)
    if (n_bins < 1L) stop("`bins` must be a positive integer or explicit edges")
    max_dist <- if (length(d)) max(d) / 2 else 1
    edges <- seq(0, max_dist, length.out = n_bins + 1L)
    keep <- d <= max_dist
  } else {
    edges <- as.numeric(bins)
    if (length(edges) < 2L || any(diff(edges) <= 0)) {
      stop("explicit `bins` edges must be ascending with >= 2 entries")
    }
    n_bins <- length(edges) - 1L
    keep <- d >= edges[1L] & d <= edges[length(edges)]
  }
  d <- d[keep]
  sq <- sq[keep]
  idx <- pmin(pmax(findInterval(d, edges, rightmost.closed = FALSE), 1L), n_bins)
  lag <- rep(NA_real_, n_bins)
  gam <- rep(NA_real_, n_bins)
  cnt <- integer(n_bins)
  for (b in seq_len(n_bins)) {
    m <- idx == b
    cnt[b] <- sum(m)
    if (cnt[b] > 0L) {
      lag[b] <- mean(d[m])
      gam[b] <- sum(sq[m]) / (2 * cnt[b])
    }
  }
  list(lag = lag, gamma = gam, n_pairs = cnt, edges = edges, n = n,
       method = "Matheron empirical semivariogram (Schabenberger-Gotway eq. 4.1)")
}

#' @rdname Vgrm
#' @export
variogram <- Vgrm
