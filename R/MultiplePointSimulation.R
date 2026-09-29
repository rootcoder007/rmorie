.mp_setup <- function(nx, ny, conditioning) {
  sg <- matrix(NaN, ny, nx)
  if (!is.null(conditioning)) {
    cd <- matrix(conditioning, ncol = 3)
    for (r in seq_len(nrow(cd))) sg[cd[r, 1] + 1, cd[r, 2] + 1] <- cd[r, 3]
  }
  sg
}

.mp_path <- function(sg, u) {
  free <- which(t(is.na(sg))) - 1
  free <- cbind(free %/% ncol(sg) + 1, free %% ncol(sg) + 1)
  free[order(u[seq_len(nrow(free))], seq_len(nrow(free))), , drop = FALSE]
}

#' Multiple-point simulation from a training image
#'
#' \code{DirectSampling}: direct sampling (Mariethoz et al. 2010).
#' \code{Snesim}: single-grid SNESIM (Strebelle 2002). Conditioning data are
#' rows of (row, column, value) with zero-based indices, as in the Python arm
#' \code{morie.fn.mpsgeo}, whose simulations these equal.
#'
#' @param ti Training image (matrix).
#' @param nx,ny Simulation grid size (columns, rows).
#' @param n_neighbors Data event size.
#' @param threshold Acceptance distance.
#' @param max_fraction Maximum scanned fraction of the training image.
#' @param conditioning Matrix of zero-based (row, column, value) hard data.
#' @param categorical Categorical (mismatch) or continuous distance.
#' @param seed Philox seed.
#' @return A list with the simulated grid.
#' @references Mariethoz, G., Renard, P. and Straubhaar, J. (2010). The
#'   direct sampling method to perform multiple-point geostatistical
#'   simulations. Water Resources Research 46, W11536.
#'
#'   Strebelle, S. (2002). Conditional simulation of complex geological
#'   structures using multiple-point statistics. Mathematical Geology 34, 1-21.
#' @examples
#' ti <- matrix(rep(c(0, 0, 1, 1), 4), 4, byrow = TRUE)
#' DirectSampling(ti, 4, 3, seed = 1)$grid
#' @export
DirectSampling <- function(ti, nx, ny, n_neighbors = 12, threshold = 0.05, max_fraction = 0.5, conditioning = NULL,
                           categorical = TRUE, seed = 0) {
  Tm <- as.matrix(ti)
  tr <- nrow(Tm)
  tc <- ncol(Tm)
  sg <- .mp_setup(nx, ny, conditioning)
  ntot <- nx * ny
  u <- .morie_random_uniform(2 * ntot + 1, seed = seed)
  path <- .mp_path(sg, u)
  rng <- diff(range(Tm))
  if (rng == 0) rng <- 1
  inf <- which(!is.na(t(sg))) - 1
  informed <- cbind(inf %/% nx + 1, inf %% nx + 1)
  maxscan <- max(1, floor(max_fraction * tr * tc))
  for (step in seq_len(nrow(path))) {
    i <- path[step, 1]
    j <- path[step, 2]
    start <- floor(u[ntot + step] * tr * tc)
    ev <- NULL
    if (nrow(informed)) {
      d2 <- (informed[, 1] - i)^2 + (informed[, 2] - j)^2
      o <- order(d2, informed[, 1], informed[, 2])[seq_len(min(n_neighbors, nrow(informed)))]
      nb <- informed[o, , drop = FALSE]
      ev <- cbind(nb[, 1] - i, nb[, 2] - j, sg[nb])
    }
    best <- NULL
    best_d <- Inf
    for (s in seq_len(maxscan) - 1) {
      pos <- (start + s) %% (tr * tc)
      a <- pos %/% tc + 1
      b <- pos %% tc + 1
      if (!is.null(ev) && any(a + ev[, 1] < 1 | a + ev[, 1] > tr | b + ev[, 2] < 1 | b + ev[, 2] > tc)) next
      if (is.null(ev)) {
        best <- c(a, b)
        break
      }
      tv <- Tm[cbind(a + ev[, 1], b + ev[, 2])]
      d <- if (categorical) mean(tv != ev[, 3]) else sqrt(mean((tv - ev[, 3])^2)) / rng
      if (d < best_d) {
        best <- c(a, b)
        best_d <- d
      }
      if (d <= threshold) break
    }
    if (is.null(best)) best <- c(start %/% tc + 1, start %% tc + 1)
    sg[i, j] <- Tm[best[1], best[2]]
    informed <- rbind(informed, c(i, j))
  }
  list(grid = sg, path = path - 1)
}
