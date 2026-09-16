# SPDX-License-Identifier: AGPL-3.0-or-later
#' Border-corrected nearest-neighbour distance CDF G(r)
#'
#' The raw empirical CDF of nearest-neighbour distances is biased
#' downward near the edge of the window: a point close to the boundary
#' may have its true nearest neighbour outside it, so the observed
#' distance is too large. The reduced-sample (border) correction
#' removes exactly the points that could be affected at each radius,
#' \code{G_hat(r) = #{i : d_i <= r and b_i > r} / #{i : b_i > r}},
#' where \code{d_i} is the observed nearest-neighbour distance and
#' \code{b_i} the distance from point i to the nearest window edge. The
#' complete spatial randomness benchmark is
#' \code{G_csr(r) = 1 - exp(-lambda pi r^2)}, so \code{G_hat} above it
#' indicates clustering and below it regularity.
#'
#' @param coords Point coordinates, n by 2.
#' @param r_grid Non-negative radii at which to evaluate G.
#' @param window \code{c(xmin, ymin, xmax, ymax)} or an (m, 2) vertex
#'   matrix; defaults to the bounding box of \code{coords}.
#' @return List with \code{G}, \code{G_csr}, \code{r}, \code{m_used},
#'   \code{estimate}, \code{lambda_hat}, \code{n}.
#' @references Diggle, P. J. (2003). Statistical Analysis of Spatial
#'   Point Patterns, 2nd edition, Arnold; section 2.3 defines G and
#'   section 4.3 the border correction.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Nndist(M, V)
Nndist <- function(coords, r_grid, window = NULL) {
  p <- as.matrix(coords)
  n <- nrow(p)
  if (n < 2L) stop("Nndist: need at least two points")
  region <- .sp_region(window, p)
  lam <- .sp_intensity(p, region)
  d <- .sp_nn(p)
  b <- pmin(p[, 1] - region[1], region[3] - p[, 1],
            p[, 2] - region[2], region[4] - p[, 2])
  rs <- as.numeric(r_grid)
  if (!length(rs)) stop("Nndist: r_grid is empty")
  if (any(rs < 0)) stop("Nndist: r_grid must be non-negative")
  G <- numeric(length(rs))
  Gc <- numeric(length(rs))
  mu <- numeric(length(rs))
  for (k in seq_along(rs)) {
    h <- rs[k]
    keep <- b > h
    m <- sum(keep)
    mu[k] <- m
    G[k] <- if (m > 0) sum(d[keep] <= h) / m else NA_real_
    Gc[k] <- 1 - exp(-lam * pi * h * h)
  }
  .t1_result(G = G, G_csr = Gc, r = rs, m_used = mu,
             estimate = sum(d) / n, lambda_hat = lam, n = n,
             method = "Border-corrected nearest-neighbour distance CDF G(r)")
}
