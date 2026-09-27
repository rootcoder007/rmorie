.ub_sum <- function(v) {
  s <- 0
  for (q in v) s <- s + q
  s
}

#' Spatial accessibility
#'
#' Two-step floating catchment (2SFCA; Gaussian 2SFCA with G(d) =
#' (exp(-d^2/(2 d0^2)) - exp(-1/2)) / (1 - exp(-1/2)) inside d0), Hansen
#' (sum_j S_j exp(-beta d_ij)), the Joseph-Bantock gravity model with population
#' competition, or distance to the nearest site.
#'
#' @param population Demand per location.
#' @param supply Capacity per site.
#' @param dist Demand-by-site distance matrix.
#' @param method "2sfca", "g2sfca", "hansen", "gravity" or "nearest".
#' @param d0 Catchment size.
#' @param beta Decay parameter.
#' @return list(accessibility, ratio, nearest, within).
#' @references Luo, W. and Wang, F. (2003). Measures of spatial accessibility to
#'   health care in a GIS environment. Environment and Planning B 30, 865-884.
#'   Dai, D. (2010). Health and Place 16, 1038-1052. Hansen, W. G. (1959). How
#'   accessibility shapes land use. Journal of the American Institute of Planners
#'   25, 73-76.
#' @examples
#' SpatialAccessibility(c(100, 100), 10, matrix(c(1, 3)), d0 = 2)$accessibility
#' @export
SpatialAccessibility <- function(population, supply, dist, method = c("2sfca", "g2sfca", "hansen", "gravity", "nearest"),
                                 d0 = NULL, beta = 1) {
  method <- match.arg(method)
  P <- as.numeric(population)
  S <- as.numeric(supply)
  D <- matrix(as.numeric(dist), nrow = length(P))
  if (ncol(D) != length(S)) stop("dist must be length(population) x length(supply)", call. = FALSE)
  if (method %in% c("2sfca", "g2sfca", "nearest") && is.null(d0)) stop(sprintf('method "%s" needs d0', method), call. = FALSE)
  if (method %in% c("2sfca", "g2sfca")) {
    e <- exp(-0.5)
    W <- if (method == "2sfca") (D <= d0) * 1 else ifelse(D <= d0, (exp(-0.5 * (D / d0)^2) - e) / (1 - e), 0)
    den <- vapply(seq_along(S), function(j) .ub_sum(P * W[, j]), 0)
    R <- ifelse(den > 0, S / den, 0)
    return(list(ratio = R, accessibility = vapply(seq_along(P), function(i) .ub_sum(R * W[i, ]), 0)))
  }
  if (method == "hansen") return(list(accessibility = vapply(seq_along(P), function(i) .ub_sum(S * exp(-beta * D[i, ])), 0)))
  if (method == "gravity") {
    V <- vapply(seq_along(S), function(j) .ub_sum(P * D[, j]^-beta), 0)
    return(list(ratio = S / V, accessibility = vapply(seq_along(P), function(i) .ub_sum(S * D[i, ]^-beta / V), 0)))
  }
  nr <- apply(D, 1, min)
  list(nearest = nr, within = rowSums(D <= d0), accessibility = nr)
}

#' Service coverage
#'
#' Coverage of demand by facilities within a standard distance, with backup
#' multiplicity and the demand weight each facility covers.
#'
#' @param dist Demand-by-facility distance matrix.
#' @param radius Standard distance.
#' @param weights Optional demand weights.
#' @return list(covered, multiplicity, nearest, covered_weight, coverage_share,
#'   mean_nearest, uncovered (1-based), facility_load).
#' @references Daskin, M. S. and Stern, E. H. (1981). A hierarchical objective
#'   set covering model for emergency medical service vehicle deployment.
#'   Transportation Science 15, 137-152.
#' @examples
#' ServiceCoverage(rbind(c(1, 5), c(4, 2.5), c(6, 7)), 3, c(10, 20, 30))$coverage_share
#' @export
ServiceCoverage <- function(dist, radius, weights = NULL) {
  D <- as.matrix(dist)
  n <- nrow(D)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  mult <- rowSums(D <= radius)
  near <- apply(D, 1, min)
  tot <- .ub_sum(w)
  covw <- .ub_sum(w[mult > 0])
  list(covered = mult > 0, multiplicity = mult, nearest = near, covered_weight = covw, coverage_share = covw / tot,
       mean_nearest = .ub_sum(w * near) / tot, uncovered = which(mult == 0),
       facility_load = vapply(seq_len(ncol(D)), function(j) .ub_sum(w[D[, j] <= radius]), 0))
}

#' USDA low-income, low-access (LILA) food deserts
#'
#' Low income: poverty rate at least 20 percent or median family income at most 80
#' percent of the area median. Low access: at least \code{count} people or
#' \code{share} of the population beyond the access threshold (1 mile urban, 10
#' miles rural) from the nearest supermarket.
#'
#' @param population Tract population.
#' @param beyond Population beyond the access threshold.
#' @param poverty_rate Poverty proportion.
#' @param family_income Median family income.
#' @param area_median_income Area median income.
#' @param urban Logical, urban tract.
#' @param share,count Low-access thresholds.
#' @return list(low_income, low_access, lila, threshold_miles, n_lila,
#'   lila_population).
#' @references Dutko, P., Ver Ploeg, M. and Farrigan, T. (2012). Characteristics
#'   and Influential Factors of Food Deserts. USDA Economic Research Service,
#'   Economic Research Report 140.
#' @examples
#' FoodDesert(4000, 1500, 0.25, 40000, 60000, TRUE)$lila
#' @export
FoodDesert <- function(population, beyond, poverty_rate, family_income, area_median_income, urban, share = 1 / 3, count = 500) {
  li <- poverty_rate >= 0.20 | family_income <= 0.80 * area_median_income
  la <- beyond >= count | (population > 0 & beyond / population >= share)
  lila <- li & la
  list(low_income = li, low_access = la, lila = lila, threshold_miles = ifelse(urban, 1, 10), n_lila = sum(lila),
       lila_population = .ub_sum(population[lila]))
}

#' Landscape fragmentation
#'
#' Effective mesh size, splitting index and landscape division of one class in
#' a categorical raster (Jaeger 2000); equals landscapemetrics' lsm_c_mesh (in
#' units of \code{cell_area}), lsm_c_split and lsm_c_division.
#'
#' @param grid Matrix of class codes.
#' @param cls Class code.
#' @param cell_area Area of one cell.
#' @param eight 8-neighbour connectivity.
#' @return list(mesh, splitting, division, n_patches, patch_areas, labels).
#' @references Jaeger, J. A. G. (2000). Landscape division, splitting index, and
#'   effective mesh size: new measures of landscape fragmentation. Landscape
#'   Ecology 15, 115-130.
#' @examples
#' LandscapeFragmentation(rbind(c(1, 1, 0, 1), c(1, 1, 0, 1)))$n_patches
#' @export
LandscapeFragmentation <- function(grid, cls = 1, cell_area = 1, eight = TRUE) {
  G <- as.matrix(grid)
  h <- nrow(G)
  w <- ncol(G)
  lab <- matrix(0L, h, w)
  steps <- rbind(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  if (eight) steps <- rbind(steps, c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))
  sizes <- integer(0)
  for (y in seq_len(h)) for (x in seq_len(w)) {
    if (G[y, x] == cls && lab[y, x] == 0L) {
      id <- length(sizes) + 1L
      lab[y, x] <- id
      stack <- list(c(y, x))
      size <- 0L
      while (length(stack)) {
        cur <- stack[[length(stack)]]
        stack[[length(stack)]] <- NULL
        size <- size + 1L
        for (s in seq_len(nrow(steps))) {
          ny <- cur[1] + steps[s, 1]
          nx <- cur[2] + steps[s, 2]
          if (ny >= 1 && ny <= h && nx >= 1 && nx <= w && G[ny, nx] == cls && lab[ny, nx] == 0L) {
            lab[ny, nx] <- id
            stack[[length(stack) + 1]] <- c(ny, nx)
          }
        }
      }
      sizes <- c(sizes, size)
    }
  }
  A <- h * w * cell_area
  sq <- .ub_sum((sizes * cell_area)^2)
  list(mesh = sq / A, splitting = if (sq > 0) A^2 / sq else Inf, division = 1 - sq / A^2, n_patches = length(sizes),
       patch_areas = sizes * cell_area, labels = lab)
}

#' Sprawl entropy
#'
#' Relative Shannon entropy H / log(n) of built-up land (or density) over n
#' zones; near 1 for dispersed growth, near 0 for compact growth.
#'
#' @param built Built-up area per zone.
#' @param area Optional zone areas.
#' @return list(entropy, relative, max, shares).
#' @references Yeh, A. G.-O. and Li, X. (2001). Measurement and monitoring of
#'   urban sprawl in a rapidly growing region using entropy. Photogrammetric
#'   Engineering and Remote Sensing 67, 83-90.
#' @examples
#' SprawlEntropy(c(5, 5, 5, 5))$relative
#' @export
SprawlEntropy <- function(built, area = NULL) {
  b <- as.numeric(built)
  n <- length(b)
  if (n < 2 || any(b < 0)) stop("need at least two zones with non-negative built-up area", call. = FALSE)
  dens <- if (is.null(area)) b else b / as.numeric(area)
  p <- dens / .ub_sum(dens)
  H <- -.ub_sum(ifelse(p > 0, p * log(p), 0))
  list(entropy = H, relative = H / log(n), max = log(n), shares = p)
}

#' Impervious-surface indices
#'
#' NDBI, MNDWI and, with a thermal band rescaled to the unit interval, NDISI (Xu 2010);
#' pixels above \code{threshold} (NDISI, else NDBI) are mapped impervious.
#'
#' @param green,nir,swir1 Reflectance vectors.
#' @param tir Optional thermal band rescaled to the unit interval.
#' @param threshold Classification threshold.
#' @return list(ndbi, mndwi, ndisi, impervious, impervious_share).
#' @references Xu, H. (2010). Analysis of impervious surface and its impact on
#'   urban heat environment using the normalized difference impervious surface
#'   index (NDISI). Photogrammetric Engineering and Remote Sensing 76, 557-565.
#' @examples
#' ImperviousIndices(0.1, 0.2, 0.3)$ndbi
#' @export
ImperviousIndices <- function(green, nir, swir1, tir = NULL, threshold = 0) {
  nd <- function(a, b) ifelse(a + b != 0, (a - b) / (a + b), 0)
  out <- list(ndbi = nd(swir1, nir), mndwi = nd(green, swir1))
  score <- out$ndbi
  if (!is.null(tir)) {
    out$ndisi <- nd(tir, (out$mndwi + nir + swir1) / 3)
    score <- out$ndisi
  }
  out$impervious <- score > threshold
  out$impervious_share <- mean(out$impervious)
  out
}

#' Surface urban heat island intensity
#'
#' Mean urban minus mean rural land surface temperature, and the per-pixel
#' anomaly relative to the rural mean.
#'
#' @param lst Land surface temperature vector.
#' @param urban Logical urban mask.
#' @param rural Optional logical rural reference mask (default: not urban).
#' @return list(intensity, urban_mean, rural_mean, anomaly).
#' @references Peng, S. et al. (2012). Surface urban heat island across 419
#'   global big cities. Environmental Science and Technology 46, 696-703.
#' @examples
#' UhiIntensity(c(30, 32, 26, 24), c(TRUE, TRUE, FALSE, FALSE))$intensity
#' @export
UhiIntensity <- function(lst, urban, rural = NULL) {
  urban <- as.logical(urban)
  rural <- if (is.null(rural)) !urban else as.logical(rural)
  if (!any(urban) || !any(rural)) stop("need at least one urban and one rural pixel", call. = FALSE)
  um <- .ub_sum(lst[urban]) / sum(urban)
  rm <- .ub_sum(lst[rural]) / sum(rural)
  list(intensity = um - rm, urban_mean = um, rural_mean = rm, anomaly = lst - rm)
}

#' Population density surface
#'
#' Quartic (biweight) kernel density of point counts per unit area,
#' sum_i c_i 3 / (pi h^2) (1 - d^2 / h^2)^2 for d < h.
#'
#' @param points Two-column matrix of point coordinates.
#' @param counts Population at each point.
#' @param grid Two-column matrix of evaluation locations.
#' @param bandwidth Kernel radius h.
#' @return list(density, total).
#' @references Silverman, B. W. (1986). Density Estimation for Statistics and
#'   Data Analysis. Chapman and Hall, Sec. 4.2.
#' @examples
#' PopulationDensitySurface(matrix(0, 1, 2), 10, matrix(0, 1, 2), 1)$density * pi
#' @export
PopulationDensitySurface <- function(points, counts, grid, bandwidth) {
  if (bandwidth <= 0) stop("bandwidth must be positive", call. = FALSE)
  Pm <- matrix(as.numeric(points), ncol = 2)
  G <- matrix(as.numeric(grid), ncol = 2)
  h2 <- bandwidth^2
  dens <- vapply(seq_len(nrow(G)), function(k) {
    d2 <- (G[k, 1] - Pm[, 1])^2 + (G[k, 2] - Pm[, 2])^2
    .ub_sum(ifelse(d2 < h2, counts * 3 / (pi * h2) * (1 - d2 / h2)^2, 0))
  }, 0)
  list(density = dens, total = .ub_sum(counts))
}
