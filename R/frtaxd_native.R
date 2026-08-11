# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spatial Shannon-Wiener taxon diversity on a rectangular grid
#'
#' Shannon-Wiener diversity \eqn{H = -\sum_k p_k \ln p_k} per grid cell
#' and overall (natural-log convention, nats), with richness S (count
#' of distinct taxa) and Pielou evenness \eqn{J = H/\ln S} (NaN when
#' S = 1). Points are assigned to a `grid` by `grid` lattice of cells
#' over the bounding box.
#'
#' @param coords Point locations, n by 2.
#' @param species Taxon labels, length n.
#' @param grid Number of grid cells per axis.
#' @return List with H (grid by grid, NaN where empty), richness,
#'   counts, H_overall, S_overall, J_overall, grid, n.
#' @references Shannon, C. E. (1948). A mathematical theory of
#'   communication. Bell System Technical Journal, 27, 379-423, Sec. 6
#'   (entropy H). Archived:
#'   fetched-wave3/shannon-1948-mathematical-theory-of-communication.pdf.
#'
#'   Pielou, E. C. (1966). The measurement of diversity in different
#'   types of biological collections. Journal of Theoretical Biology,
#'   13, 131-144.
#'
#'   Magurran, A. E. (2004). Measuring Biological Diversity. Blackwell.
#' @examples
#' Frtaxd(cbind(runif(20), runif(20)), sample(letters[1:3], 20, TRUE), grid = 2)
#' @export
Frtaxd <- function(coords, species, grid = 4L) {
  coords <- as.matrix(coords)
  if (ncol(coords) != 2L) stop("`coords` must be (n, 2)")
  n <- nrow(coords)
  species <- as.character(species)
  if (length(species) != n) stop("`coords` and `species` must have equal length")
  g <- as.integer(grid)
  if (g < 1L) stop("`grid` must be a positive integer")
  shannon <- function(labels) {
    if (!length(labels)) return(list(h = NaN, s = 0L, j = NaN))
    cnt <- table(labels)
    tot <- length(labels)
    pk <- as.numeric(cnt) / tot
    h <- -sum(pk * log(pk))
    s <- length(cnt)
    j <- if (s > 1L) h / log(s) else NaN
    list(h = h, s = s, j = j)
  }
  xmin <- min(coords[, 1]); xmax <- max(coords[, 1])
  ymin <- min(coords[, 2]); ymax <- max(coords[, 2])
  xr <- if (xmax > xmin) xmax - xmin else 1
  yr <- if (ymax > ymin) ymax - ymin else 1
  ix <- pmin(pmax(as.integer(floor((coords[, 1] - xmin) / xr * g)), 0L), g - 1L)
  iy <- pmin(pmax(as.integer(floor((coords[, 2] - ymin) / yr * g)), 0L), g - 1L)
  H <- matrix(NaN, g, g)
  richness <- matrix(0L, g, g)
  counts <- matrix(0L, g, g)
  for (cx in 0:(g - 1L)) {
    for (cy in 0:(g - 1L)) {
      labels <- species[ix == cx & iy == cy]
      res <- shannon(labels)
      H[cx + 1L, cy + 1L] <- res$h
      richness[cx + 1L, cy + 1L] <- res$s
      counts[cx + 1L, cy + 1L] <- length(labels)
    }
  }
  overall <- shannon(species)
  list(H = H, richness = richness, counts = counts,
       H_overall = overall$h, S_overall = overall$s, J_overall = overall$j,
       grid = g, n = n,
       method = "Shannon-Wiener diversity per grid cell (nats)")
}

#' @rdname Frtaxd
#' @export
forest_taxon_diversity <- Frtaxd
