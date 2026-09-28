#' Forest inventory and LiDAR canopy analysis
#'
#' \code{BasalArea}: tree and stand basal area. \code{StandDensityIndex}:
#' Reineke or summation stand density index. \code{TreeBiomass}: Chave et al.
#' (2014) above-ground biomass. \code{CarbonStock}: carbon and CO2-equivalent.
#' \code{RasterizePoints}: point-to-raster gridding (row 1 at the largest y).
#' \code{CanopyHeightModel}: DSM minus DTM. \code{CanopyGaps}: connected gaps
#' below a height threshold. \code{TreeTops}: circular local-maximum filter
#' with a fixed or height-dependent window. \code{CrownSegmentation}:
#' Dalponte-Coomes region growing. \code{PlotEstimate},
#' \code{StratifiedEstimate}, \code{AdaptiveClusterEstimate} and
#' \code{LineIntersectVolume}: inventory estimators. Cell positions are
#' returned zero-based, as in the Python arm \code{morie.fn.forestry}.
#'
#' @param dbh Diameters at breast height (cm).
#' @param expansion Trees per hectare represented by each tree.
#' @param method \code{"reineke"} or \code{"summation"} (stand density);
#'   \code{"dalponte"} or \code{"itcsegment"} (crown segmentation).
#' @param exponent,reference Reineke slope and reference diameter.
#' @param height Tree heights (m).
#' @param wood_density Wood density (g per cubic cm).
#' @param stress Environmental stress index E.
#' @param model Allometric model (\code{"chave2014"}).
#' @param biomass Biomass values.
#' @param carbon_fraction Carbon fraction of dry biomass.
#' @param root_shoot Root-to-shoot ratio.
#' @param x,y,z Point coordinates and heights.
#' @param res Cell size.
#' @param fun \code{"max"}, \code{"min"} or \code{"mean"}.
#' @param extent Optional c(xmin, ymax, ncol, nrow).
#' @param dsm,dtm Surface and terrain models (matrices).
#' @param floor Lower bound of the canopy height.
#' @param chm Canopy height model (matrix).
#' @param height_threshold Gap height threshold.
#' @param min_area,max_area Gap area limits.
#' @param connectivity 4 or 8.
#' @param hmin Minimum tree height.
#' @param window Fixed window diameter (NULL for the variable window).
#' @param tops Two-column matrix of zero-based tree-top cells (NULL: detect
#'   them with the search window).
#' @param search_window Odd window size of the seed search.
#' @param th_seed,th_cr Seed and crown-mean height fractions.
#' @param dist Maximum seed distance in cells.
#' @param th Height below which cells are ignored.
#' @param smooth Apply the 3 by 3 mean filter first.
#' @param values Plot totals or sampled values.
#' @param plot_area Plot area (ha).
#' @param N Population size (number of possible plots or units).
#' @param strata Stratum labels of the sampled values.
#' @param stratum_sizes Named vector of stratum sizes.
#' @param networks List of network value vectors.
#' @param ids Optional network identifiers.
#' @param diameters Intersect diameters (cm).
#' @param transect_length Total transect length (m).
#' @return A number, vector, matrix or list.
#' @references Reineke, L. H. (1933). Perfecting a stand-density index for
#'   even-aged forests. Journal of Agricultural Research 46, 627-638.
#'
#'   Chave, J. et al. (2014). Improved allometric models to estimate the
#'   aboveground biomass of tropical trees. Global Change Biology 20,
#'   3177-3190.
#'
#'   Popescu, S. C. and Wynne, R. H. (2004). Seeing the trees in the forest.
#'   Photogrammetric Engineering and Remote Sensing 70, 589-604.
#'
#'   Dalponte, M. and Coomes, D. A. (2016). Tree-centric mapping of forest
#'   carbon density from airborne laser scanning and hyperspectral data.
#'   Methods in Ecology and Evolution 7, 1236-1245.
#'
#'   Thompson, S. K. (1990). Adaptive cluster sampling. Journal of the
#'   American Statistical Association 85, 1050-1059.
#'
#'   Van Wagner, C. E. (1968). The line intersect method in forest fuel
#'   sampling. Forest Science 14, 20-26.
#' @examples
#' BasalArea(c(20, 30), 25)$total
#' StandDensityIndex(c(20, 30, 25.4), 100)$sdi
#' LineIntersectVolume(c(10, 20, 15), 100)
#' @export
BasalArea <- function(dbh, expansion = 1) {
  g <- pi * dbh^2 / 40000
  list(tree = g, total = sum(g * rep_len(expansion, length(dbh))))
}

#' @rdname BasalArea
#' @export
StandDensityIndex <- function(dbh, expansion = 1, method = "reineke", exponent = 1.605, reference = 25.4) {
  e <- rep_len(expansion, length(dbh))
  n <- sum(e)
  qmd <- sqrt(sum(e * dbh^2) / n)
  sdi <- switch(method,
    reineke = n * (qmd / reference)^exponent,
    summation = sum(e * (dbh / reference)^exponent),
    stop("method must be 'reineke' or 'summation'")
  )
  list(sdi = sdi, qmd = qmd, trees_per_ha = n)
}

#' @rdname BasalArea
#' @export
TreeBiomass <- function(dbh, height = NULL, wood_density = 0.6, stress = NULL, model = "chave2014") {
  if (model != "chave2014") stop("model must be 'chave2014'")
  rho <- rep_len(wood_density, length(dbh))
  if (!is.null(height)) return(0.0673 * (rho * dbh^2 * rep_len(height, length(dbh)))^0.976)
  if (is.null(stress)) stop("give heights or the environmental stress index E")
  exp(-1.803 - 0.976 * stress + 0.976 * log(rho) + 2.673 * log(dbh) - 0.0299 * log(dbh)^2)
}

#' @rdname BasalArea
#' @export
CarbonStock <- function(biomass, carbon_fraction = 0.47, root_shoot = 0) {
  cc <- sum(biomass) * carbon_fraction * (1 + root_shoot)
  list(carbon = cc, co2e = cc * 44 / 12)
}

#' @rdname BasalArea
#' @export
RasterizePoints <- function(x, y, z, res, fun = "max", extent = NULL) {
  if (is.null(extent)) {
    xmin <- floor(min(x) / res) * res
    ymax <- (floor(max(y) / res) + 1) * res
    ncol <- floor((max(x) - xmin) / res) + 1
    nrow <- floor((ymax - min(y)) / res - 1e-12) + 1
  } else {
    xmin <- extent[1]
    ymax <- extent[2]
    ncol <- extent[3]
    nrow <- extent[4]
  }
  if (!fun %in% c("max", "min", "mean")) stop("fun must be 'max', 'min' or 'mean'")
  r <- floor((ymax - y) / res)
  cl <- floor((x - xmin) / res)
  ok <- r >= 0 & r < nrow & cl >= 0 & cl < ncol
  g <- matrix(NaN, nrow, ncol)
  cell <- r[ok] * ncol + cl[ok]
  zz <- z[ok]
  for (k in unique(cell)) {
    v <- zz[cell == k]
    g[k %/% ncol + 1, k %% ncol + 1] <- switch(fun, max = max(v), min = min(v), mean = sum(v) / length(v))
  }
  list(grid = g, extent = c(xmin, ymax, ncol, nrow), res = res)
}

#' @rdname BasalArea
#' @export
CanopyHeightModel <- function(dsm, dtm, floor = 0) {
  out <- pmax(dsm - dtm, floor)
  out[is.na(dsm) | is.na(dtm)] <- NaN
  out
}

.fo_components <- function(mask, connectivity) {
  nr <- nrow(mask)
  nc <- ncol(mask)
  lab <- matrix(0L, nr, nc)
  nb <- rbind(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  if (connectivity == 8) nb <- rbind(nb, c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))
  k <- 0L
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (mask[i, j] && lab[i, j] == 0) {
        k <- k + 1L
        lab[i, j] <- k
        stack <- list(c(i, j))
        while (length(stack)) {
          p <- stack[[length(stack)]]
          stack[[length(stack)]] <- NULL
          for (q in seq_len(nrow(nb))) {
            u <- p[1] + nb[q, 1]
            v <- p[2] + nb[q, 2]
            if (u >= 1 && u <= nr && v >= 1 && v <= nc && mask[u, v] && lab[u, v] == 0) {
              lab[u, v] <- k
              stack[[length(stack) + 1]] <- c(u, v)
            }
          }
        }
      }
    }
  }
  list(lab = lab, k = k)
}

#' @rdname BasalArea
#' @export
CanopyGaps <- function(chm, height_threshold, min_area = 0, max_area = Inf, res = 1, connectivity = 8) {
  mask <- !is.na(chm) & chm < height_threshold
  cp <- .fo_components(mask, connectivity)
  area <- tabulate(cp$lab[cp$lab > 0], cp$k) * res^2
  keep <- which(area >= min_area & area <= max_area)
  map <- integer(cp$k)
  map[keep] <- seq_along(keep)
  labels <- matrix(0L, nrow(chm), ncol(chm))
  labels[cp$lab > 0] <- map[cp$lab[cp$lab > 0]]
  list(labels = labels, areas = area[keep], gap_fraction = sum(area[keep]) / (sum(!is.na(chm)) * res^2))
}

#' @rdname BasalArea
#' @export
TreeTops <- function(chm, res = 1, hmin = 2, window = NULL) {
  nr <- nrow(chm)
  nc <- ncol(chm)
  tops <- NULL
  hts <- NULL
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      h <- chm[i, j]
      if (is.na(h) || h < hmin) next
      ws <- if (is.null(window)) 2.51503 + 0.00901 * h^2 else window
      rad <- ws / 2 / res
      k <- floor(rad)
      rr <- max(1, i - k):min(nr, i + k)
      cc <- max(1, j - k):min(nc, j + k)
      sub <- chm[rr, cc, drop = FALSE]
      inside <- outer((rr - i)^2, (cc - j)^2, "+") <= rad^2
      if (!any(inside & !is.na(sub) & sub > h)) {
        tops <- rbind(tops, c(i - 1, j - 1))
        hts <- c(hts, h)
      }
    }
  }
  list(cells = tops, heights = hts)
}

.fo_focal_mean <- function(G) {
  nr <- nrow(G)
  nc <- ncol(G)
  out <- matrix(NaN, nr, nc)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      v <- G[max(1, i - 1):min(nr, i + 1), max(1, j - 1):min(nc, j + 1)]
      v <- v[!is.na(v)]
      if (length(v)) out[i, j] <- sum(v) / length(v)
    }
  }
  out
}

#' @rdname BasalArea
#' @export
CrownSegmentation <- function(chm, tops = NULL, search_window = 3, th_seed = 0.45, th_cr = 0.55, dist = 10,
                              th = 0, smooth = TRUE, method = "dalponte") {
  if (!method %in% c("dalponte", "itcsegment")) stop("method must be 'dalponte' or 'itcsegment'")
  Z <- if (smooth) .fo_focal_mean(chm) else chm
  nr <- nrow(Z)
  nc <- ncol(Z)
  G <- t(Z[nr:1, , drop = FALSE])
  G[is.na(G) | G < th] <- 0
  nx <- nc
  ny <- nr
  idx <- matrix(0L, nx, ny)
  seeds <- NULL
  if (is.null(tops)) {
    h <- search_window %/% 2
    lo <- ceiling(search_window / 2)
    mark <- matrix(0L, nx, ny)
    for (y in seq_len(ny)) {
      for (x in seq_len(nx)) {
        if (G[x, y] == 0 || x < lo || x > nx - lo || y < lo || y > ny - lo) next
        win <- G[(x - h):(x + h), (y - h):(y + h)]
        if (G[x, y] == max(win) && max(win) != 0 && all(mark[(x - h):(x + h), (y - h):(y + h)] == 0)) {
          mark[x, y] <- 1L
          seeds <- rbind(seeds, c(x, y))
          idx[x, y] <- nrow(seeds)
        }
      }
    }
  } else {
    tops <- matrix(tops, ncol = 2)
    for (k in seq_len(nrow(tops))) {
      seeds <- rbind(seeds, c(tops[k, 2] + 1, nr - tops[k, 1]))
      idx[seeds[k, 1], seeds[k, 2]] <- k
    }
  }
  crowns <- idx
  sums <- G[seeds]
  cnt <- rep(1L, nrow(seeds))
  check <- matrix(0L, nx, ny)
  old <- crowns
  grown <- TRUE
  while (grown) {
    grown <- FALSE
    todo <- which(crowns != 0 & check == 0, arr.ind = TRUE)
    for (q in seq_len(nrow(todo))) {
      x <- todo[q, 1]
      y <- todo[q, 2]
      if (x == 1 || x == nx || y == 1 || y == ny) next
      k <- crowns[x, y]
      sx <- seeds[k, 1]
      sy <- seeds[k, 2]
      hs <- G[sx, sy]
      mh <- sums[k] / cnt[k]
      nb <- rbind(c(x - 1, y), c(x, y - 1), c(x, y + 1), c(x + 1, y))
      hv <- G[nb]
      pass <- hv != 0 & hv > hs * th_seed & hv > mh * th_cr & hv <= hs + hs * 0.05 &
        sqrt((sx - nb[, 1])^2 + (sy - nb[, 2])^2) < dist
      if (method == "itcsegment" && sum(pass) < 2) next
      for (p in which(pass)) {
        if (crowns[nb[p, 1], nb[p, 2]] == 0) {
          crowns[nb[p, 1], nb[p, 2]] <- k
          sums[k] <- sums[k] + hv[p]
          cnt[k] <- cnt[k] + 1L
          grown <- TRUE
        }
      }
    }
    check <- old
    old <- crowns
  }
  list(labels = t(crowns)[ny:1, , drop = FALSE], tops = cbind(nr - seeds[, 2], seeds[, 1] - 1),
       crown_cells = cnt, mean_height = sums / cnt)
}

#' @rdname BasalArea
#' @export
PlotEstimate <- function(values, plot_area, N = NULL) {
  y <- values / plot_area
  n <- length(y)
  m <- sum(y) / n
  f <- if (is.null(N)) 0 else n / N
  list(mean = m, se = sqrt(sum((y - m)^2) / (n - 1) / n * (1 - f)), n = n)
}

#' @rdname BasalArea
#' @export
StratifiedEstimate <- function(values, strata, stratum_sizes) {
  ntot <- sum(stratum_sizes)
  m <- 0
  v <- 0
  means <- list()
  for (h in names(stratum_sizes)) {
    yh <- values[strata == h]
    nh <- length(yh)
    if (nh < 2) stop("every stratum needs at least two sampled units")
    mh <- sum(yh) / nh
    w <- stratum_sizes[[h]] / ntot
    m <- m + w * mh
    v <- v + w^2 * sum((yh - mh)^2) / (nh - 1) / nh * (1 - nh / stratum_sizes[[h]])
    means[[h]] <- mh
  }
  list(mean = m, se = sqrt(v), stratum_means = means)
}

#' @rdname BasalArea
#' @export
AdaptiveClusterEstimate <- function(networks, N, ids = NULL) {
  n <- length(networks)
  w <- vapply(networks, function(v) sum(v) / length(v), numeric(1))
  mhh <- sum(w) / n
  vv <- (N - n) / (N * n * (n - 1)) * sum((w - mhh)^2)
  keys <- if (is.null(ids)) seq_len(n) else ids
  logc <- lchoose(N, n)
  tot <- 0
  for (i in which(!duplicated(keys))) {
    mk <- length(networks[[i]])
    alpha <- if (N - mk >= n) 1 - exp(lchoose(N - mk, n) - logc) else 1
    tot <- tot + sum(networks[[i]]) / alpha
  }
  list(mean_hh = mhh, se_hh = sqrt(vv), mean_ht = tot / N)
}

#' @rdname BasalArea
#' @export
LineIntersectVolume <- function(diameters, transect_length) pi^2 * sum(diameters^2) / (8 * transect_length)
