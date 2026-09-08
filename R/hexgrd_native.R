# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hexagonal grid binning (Carr hbin)
#'
#' Bins 2-d points on a hexagon lattice using the Dan Carr hbin
#' algorithm: two interleaved rectangular lattices of hexagon centers,
#' lattice A at \eqn{(jw, iw\sqrt{3})} and lattice B at
#' \eqn{((j+1/2)w, (i+1/2)w\sqrt{3})}, with hexagon x-spacing w. In
#' scaled coordinates \eqn{s_x = (x - x_{min})/w},
#' \eqn{s_y = (y - y_{min})/(w\sqrt{3})} a point belongs to the nearer
#' candidate center under \eqn{d^2 = (s_x - j)^2 + 3(s_y - i)^2}, the
#' Euclidean distance in data units up to the factor \eqn{w^2}. The
#' fast-path thresholds 1/4 and 1/3 are the con1 and con2 constants of
#' the Fortran original.
#'
#' @param coords Point locations, n by 2.
#' @param values Optional values to average per cell.
#' @param cell_size Hexagon width w (center spacing along x).
#' @return List with cell_id (1-based, per point), centers, counts,
#'   xcm, ycm, cell_size, n, and value_mean if values were given.
#' @references Carr, D. B., Littlefield, R. J., Nicholson, W. L. and
#'   Littlefield, J. S. (1987). Scatterplot matrix techniques for large
#'   N. Journal of the American Statistical Association, 82(398),
#'   424-436.
#'
#'   Carr, D. B., Olsen, A. R. and White, D. (1992). Hexagon mosaic
#'   maps for display of univariate and bivariate geographical data.
#'   Cartography and Geographic Information Systems, 19(4), 228-236.
#'
#'   Reference implementation: Fortran subroutine hbin, src/hbin.f in
#'   CRAN hexbin 1.28.6. Archived:
#'   fetched-wave3/carr-hexbin_1.28.6-cran-source.tar.gz.
#' @examples
#' Hexgrd(cbind(runif(50), runif(50)), cell_size = 0.3)
#' @export
Hexgrd <- function(coords, values = NULL, cell_size = 1) {
  coords <- as.matrix(coords)
  if (ncol(coords) != 2L) stop("`coords` must be (n, 2)")
  n <- nrow(coords)
  w <- as.numeric(cell_size)
  if (w <= 0) stop("`cell_size` must be positive")
  if (!is.null(values)) {
    values <- as.numeric(values)
    if (length(values) != n) stop("`values` must match `coords` length")
  }
  xmin <- min(coords[, 1])
  ymin <- min(coords[, 2])
  sx <- (coords[, 1] - xmin) / w
  sy <- (coords[, 2] - ymin) / (w * sqrt(3))
  con1 <- 0.25
  con2 <- 1 / 3
  keyj <- integer(n)
  keyi <- integer(n)
  keyo <- integer(n)
  for (i in seq_len(n)) {
    sxi <- sx[i]
    syi <- sy[i]
    j1 <- as.integer(floor(sxi + 0.5))
    i1 <- as.integer(floor(syi + 0.5))
    d1 <- (sxi - j1)^2 + 3 * (syi - i1)^2
    if (d1 < con1) {
      keyj[i] <- j1
      keyi[i] <- i1
      keyo[i] <- 0L
    } else {
      j2 <- as.integer(floor(sxi))
      i2 <- as.integer(floor(syi))
      d2 <- (sxi - j2 - 0.5)^2 + 3 * (syi - i2 - 0.5)^2
      if (d1 > con2 || d1 > d2) {
        keyj[i] <- j2
        keyi[i] <- i2
        keyo[i] <- 1L
      } else {
        keyj[i] <- j1
        keyi[i] <- i1
        keyo[i] <- 0L
      }
    }
  }
  key <- paste(keyj, keyi, keyo)
  uk <- unique(data.frame(j = keyj, i = keyi, o = keyo))
  uk <- uk[order(uk$j, uk$i, uk$o), , drop = FALSE]
  ukey <- paste(uk$j, uk$i, uk$o)
  cell_id <- match(key, ukey)
  m <- nrow(uk)
  counts <- integer(m)
  xcm <- numeric(m)
  ycm <- numeric(m)
  vsum <- numeric(m)
  for (i in seq_len(n)) {
    ci <- cell_id[i]
    counts[ci] <- counts[ci] + 1L
    xcm[ci] <- xcm[ci] + coords[i, 1]
    ycm[ci] <- ycm[ci] + coords[i, 2]
    if (!is.null(values)) vsum[ci] <- vsum[ci] + values[i]
  }
  xcm <- xcm / counts
  ycm <- ycm / counts
  centers <- cbind(xmin + (uk$j + 0.5 * uk$o) * w,
                   ymin + (uk$i + 0.5 * uk$o) * w * sqrt(3))
  out <- list(cell_id = cell_id, centers = centers, counts = counts,
              xcm = xcm, ycm = ycm, cell_size = w, n = n,
              method = "Carr hexagon binning (hbin transcription)")
  if (!is.null(values)) out$value_mean <- vsum / counts
  out
}

#' @rdname Hexgrd
#' @export
hexagonal_grid <- Hexgrd
