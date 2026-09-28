.dem_codes <- c(1, 2, 4, 8, 16, 32, 64, 128)
.dem_off <- rbind(c(0, 1), c(1, 1), c(1, 0), c(1, -1), c(0, -1), c(-1, -1), c(-1, 0), c(-1, 1))

#' Terrain derivatives of a DEM (terra terrain conventions)
#'
#' Slope (radians) from Horn's (1981) finite differences (\code{neighbors =
#' 4}: Fleming and Hoffer), aspect (radians clockwise from north), TPI, TRI
#' (Wilson et al. 2007), TRI of Riley et al. (1999), TRI root mean square,
#' roughness; edge cells NA. Identical to the Python arm
#' \code{morie.fn.demops.terrain_indices}.
#'
#' @param dem Elevation matrix (rows from north to south).
#' @param res Cell size.
#' @param neighbors 4 or 8.
#' @return List of matrices.
#' @references Horn, B. K. P. (1981). Hill shading and the reflectance map.
#'   Proceedings of the IEEE 69, 14-47.
#'
#'   Riley, S. J., DeGloria, S. D. and Elliot, R. (1999). A terrain
#'   ruggedness index that quantifies topographic heterogeneity.
#'   Intermountain Journal of Sciences 5, 23-27.
#' @examples
#' TerrainIndices(rbind(c(3, 3, 3), c(2, 2, 2), c(1, 1, 1)))$aspect
#' @export
TerrainIndices <- function(dem, res = 1, neighbors = 8) {
  if (!neighbors %in% c(4, 8)) stop("neighbors must be 4 or 8")
  G <- as.matrix(dem)
  nr <- nrow(G)
  nc <- ncol(G)
  mk <- function() matrix(NaN, nr, nc)
  out <- list(slope = mk(), aspect = mk(), tpi = mk(), tri = mk(), tri_riley = mk(), tri_rmsd = mk(), roughness = mk())
  if (nr < 3 || nc < 3) return(out)
  for (i in 2:(nr - 1)) {
    for (j in 2:(nc - 1)) {
      w <- as.vector(t(G[(i - 1):(i + 1), (j - 1):(j + 1)]))
      if (anyNA(w)) next
      if (neighbors == 8) {
        dx <- ((w[3] + 2 * w[6] + w[9]) - (w[1] + 2 * w[4] + w[7])) / (8 * res)
        dy <- ((w[7] + 2 * w[8] + w[9]) - (w[1] + 2 * w[2] + w[3])) / (8 * res)
      } else {
        dx <- (w[6] - w[4]) / (2 * res)
        dy <- (w[8] - w[2]) / (2 * res)
      }
      out$slope[i, j] <- atan(sqrt(dx^2 + dy^2))
      out$aspect[i, j] <- if (dx != 0 || dy != 0) (pi / 2 - atan2(dy, -dx)) %% (2 * pi) else NaN
      nb <- w[-5]
      out$tpi[i, j] <- w[5] - mean(nb)
      out$tri[i, j] <- mean(abs(nb - w[5]))
      out$tri_riley[i, j] <- sqrt(sum((nb - w[5])^2))
      out$tri_rmsd[i, j] <- sqrt(mean((nb - w[5])^2))
      out$roughness[i, j] <- max(w) - min(w)
    }
  }
  out
}

#' Hillshade (terra shade)
#'
#' \eqn{\cos z \cos s + \sin z \sin s \cos(\phi - a)} with sun zenith
#' \eqn{z = 90 - angle}.
#'
#' @param slope,aspect Radians (\code{\link{TerrainIndices}}).
#' @param angle Sun elevation (degrees).
#' @param direction Sun azimuth (degrees).
#' @return Matrix.
#' @examples
#' Hillshade(matrix(0), matrix(0))
#' @export
Hillshade <- function(slope, aspect, angle = 45, direction = 315) {
  z <- (90 - angle) * pi / 180
  az <- direction * pi / 180
  h <- cos(z) * cos(slope) + sin(z) * sin(slope) * cos(az - aspect)
  h[!is.nan(slope) & slope == 0 & is.nan(aspect)] <- cos(z)
  h
}

#' D8 flow direction, accumulation and watershed (terra conventions)
#'
#' \code{D8FlowDirection}: steepest descent \eqn{(z - z_k)/d_k} coded 1 E, 2
#' SE, 4 S, 8 SW, 16 W, 32 NW, 64 N, 128 NE, 0 for pits and flats, ties to
#' the first code (O'Callaghan and Mark 1984). \code{FlowAccumulation}:
#' cells (or weights) draining through each cell, itself included.
#' \code{Watershed}: 1 for cells draining to \code{outlet} (row, column).
#' Identical to the Python arm \code{morie.fn.demops}.
#'
#' @param dem Elevation matrix.
#' @param res Cell size.
#' @param flowdir D8 code matrix.
#' @param weight Optional weight matrix.
#' @param outlet Row and column of the outlet (1-based).
#' @return Matrix.
#' @references O'Callaghan, J. F. and Mark, D. M. (1984). The extraction of
#'   drainage networks from digital elevation data. Computer Vision,
#'   Graphics, and Image Processing 28, 323-344.
#' @examples
#' D8FlowDirection(rbind(c(3, 3, 3), c(2, 2, 2), c(1, 1, 1)))
#' FlowAccumulation(rbind(c(4, 4), c(1, 0)))
#' Watershed(rbind(c(4, 4), c(1, 0)), c(2, 2))
#' @export
D8FlowDirection <- function(dem, res = 1) {
  G <- as.matrix(dem)
  nr <- nrow(G)
  nc <- ncol(G)
  out <- matrix(0, nr, nc)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (is.na(G[i, j])) next
      best <- 0
      code <- 0
      for (k in 1:8) {
        x <- i + .dem_off[k, 1]
        y <- j + .dem_off[k, 2]
        if (x < 1 || x > nr || y < 1 || y > nc || is.na(G[x, y])) next
        s <- (G[i, j] - G[x, y]) / (res * (if (all(.dem_off[k, ] != 0)) sqrt(2) else 1))
        if (s > best) {
          best <- s
          code <- .dem_codes[k]
        }
      }
      out[i, j] <- code
    }
  }
  out
}

.dem_receiver <- function(fd) {
  nr <- nrow(fd)
  nc <- ncol(fd)
  rec <- rep(NA_integer_, nr * nc)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      k <- match(fd[i, j], .dem_codes)
      if (is.na(k)) next
      x <- i + .dem_off[k, 1]
      y <- j + .dem_off[k, 2]
      if (x >= 1 && x <= nr && y >= 1 && y <= nc) rec[(j - 1) * nr + i] <- (y - 1) * nr + x
    }
  }
  rec
}

#' @rdname D8FlowDirection
#' @export
FlowAccumulation <- function(flowdir, weight = NULL) {
  fd <- as.matrix(flowdir)
  rec <- .dem_receiver(fd)
  acc <- if (is.null(weight)) rep(1, length(fd)) else as.vector(as.matrix(weight))
  indeg <- tabulate(rec[!is.na(rec)], length(fd))
  stack <- which(indeg == 0)
  while (length(stack)) {
    c <- stack[length(stack)]
    stack <- stack[-length(stack)]
    d <- rec[c]
    if (is.na(d)) next
    acc[d] <- acc[d] + acc[c]
    indeg[d] <- indeg[d] - 1
    if (indeg[d] == 0) stack <- c(stack, d)
  }
  matrix(acc, nrow(fd))
}

#' @rdname D8FlowDirection
#' @export
Watershed <- function(flowdir, outlet) {
  fd <- as.matrix(flowdir)
  rec <- .dem_receiver(fd)
  out <- rep(0, length(fd))
  stack <- (outlet[2] - 1) * nrow(fd) + outlet[1]
  while (length(stack)) {
    c <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if (out[c] == 1) next
    out[c] <- 1
    stack <- c(stack, which(rec == c))
  }
  matrix(out, nrow(fd))
}

#' Priority-flood depression filling
#'
#' Raises each cell to the lowest spill elevation reachable from the grid
#' edge or NA cells (Barnes, Lehman and Mulla 2014); \code{epsilon > 0}
#' adds a minimal gradient.
#'
#' @param dem Elevation matrix.
#' @param epsilon Gradient increment.
#' @return Filled matrix.
#' @references Barnes, R., Lehman, C. and Mulla, D. (2014). Priority-flood:
#'   an optimal depression-filling and watershed-labeling algorithm for
#'   digital elevation models. Computers and Geosciences 62, 117-127.
#' @examples
#' FillSinks(rbind(c(5, 5, 5), c(5, 1, 5), c(5, 5, 5)))
#' @export
FillSinks <- function(dem, epsilon = 0) {
  Z <- as.matrix(dem) * 1
  nr <- nrow(Z)
  nc <- ncol(Z)
  done <- is.na(Z)
  pq_z <- numeric(0)
  pq_i <- integer(0)
  pq_j <- integer(0)
  push <- function(z, i, j) {
    pq_z <<- c(pq_z, z)
    pq_i <<- c(pq_i, i)
    pq_j <<- c(pq_j, j)
  }
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (is.na(Z[i, j])) next
      edge <- i == 1 || i == nr || j == 1 || j == nc
      if (!edge) {
        for (k in 1:8) if (is.na(Z[i + .dem_off[k, 1], j + .dem_off[k, 2]])) edge <- TRUE
      }
      if (edge) {
        push(Z[i, j], i, j)
        done[i, j] <- TRUE
      }
    }
  }
  while (length(pq_z)) {
    m <- order(pq_z, pq_i, pq_j)[1]
    z <- pq_z[m]
    i <- pq_i[m]
    j <- pq_j[m]
    pq_z <- pq_z[-m]
    pq_i <- pq_i[-m]
    pq_j <- pq_j[-m]
    for (k in 1:8) {
      x <- i + .dem_off[k, 1]
      y <- j + .dem_off[k, 2]
      if (x >= 1 && x <= nr && y >= 1 && y <= nc && !done[x, y]) {
        done[x, y] <- TRUE
        Z[x, y] <- max(Z[x, y], z + if (epsilon > 0) epsilon else 0)
        push(Z[x, y], x, y)
      }
    }
  }
  Z
}

#' Stream order and topographic indices
#'
#' \code{StreamOrder}: network of cells with accumulation at least
#' \code{threshold}, Strahler (1957) or Shreve (1966) order.
#' \code{TopographicWetnessIndex}: \eqn{\ln(a/\tan\beta)} (Beven and Kirkby
#' 1979); \code{StreamPowerIndex}: \eqn{a\tan\beta} (Moore et al. 1991),
#' \eqn{a} the accumulated cells times \code{res}.
#'
#' @param flowdir D8 codes.
#' @param acc Accumulation.
#' @param threshold Network threshold.
#' @param method \code{"strahler"} or \code{"shreve"}.
#' @param slope Slope (radians).
#' @param res Cell size.
#' @param min_slope Slope floor.
#' @return Matrix.
#' @references Strahler, A. N. (1957). Quantitative analysis of watershed
#'   geomorphology. Transactions, American Geophysical Union 38, 913-920.
#'
#'   Beven, K. J. and Kirkby, M. J. (1979). A physically based, variable
#'   contributing area model of basin hydrology. Hydrological Sciences
#'   Bulletin 24, 43-69.
#' @examples
#' fd <- rbind(c(2, 4, 8), c(0, 4, 0), c(0, 0, 0))
#' StreamOrder(fd, FlowAccumulation(fd), 1)
#' TopographicWetnessIndex(matrix(4), matrix(atan(0.5)), res = 2)
#' @export
StreamOrder <- function(flowdir, acc, threshold, method = "strahler") {
  if (!method %in% c("strahler", "shreve")) stop("method must be strahler or shreve")
  fd <- as.matrix(flowdir)
  net <- as.vector(as.matrix(acc) >= threshold)
  rec <- .dem_receiver(fd)
  rec[!net] <- NA
  rec[!is.na(rec) & !net[pmax(rec, 1, na.rm = TRUE)]] <- NA
  ord <- rep(0, length(fd))
  indeg <- tabulate(rec[!is.na(rec)], length(fd))
  stack <- which(net & indeg == 0)
  while (length(stack)) {
    c <- stack[length(stack)]
    stack <- stack[-length(stack)]
    ds <- which(rec == c)
    ord[c] <- if (!length(ds)) 1 else if (method == "shreve") sum(ord[ds]) else {
      m <- max(ord[ds])
      if (sum(ord[ds] == m) >= 2) m + 1 else m
    }
    d <- rec[c]
    if (!is.na(d)) {
      indeg[d] <- indeg[d] - 1
      if (indeg[d] == 0) stack <- c(stack, d)
    }
  }
  matrix(ord, nrow(fd))
}

#' @rdname StreamOrder
#' @export
TopographicWetnessIndex <- function(acc, slope, res = 1, min_slope = 1e-6) log(acc * res / tan(pmax(slope, min_slope)))

#' @rdname StreamOrder
#' @export
StreamPowerIndex <- function(acc, slope, res = 1) acc * res * tan(slope)

#' D-infinity and multiple flow direction routing
#'
#' \code{DinfFlowDirection}: steepest facet direction (radians
#' counter-clockwise from east; Tarboton 1997). \code{MfdFlowAccumulation}:
#' Freeman (1991) multiple flow direction, shares \eqn{\propto (\tan\beta_k)^p}.
#'
#' @param dem Elevation matrix.
#' @param res Cell size.
#' @param p Exponent.
#' @return Matrix.
#' @references Tarboton, D. G. (1997). A new method for the determination of
#'   flow directions and upslope areas in grid digital elevation models.
#'   Water Resources Research 33, 309-319.
#'
#'   Freeman, T. G. (1991). Calculating catchment area with divergent flow
#'   based on a regular grid. Computers and Geosciences 17, 413-422.
#' @examples
#' DinfFlowDirection(rbind(c(3, 3, 3), c(2, 2, 2), c(1, 1, 1)))
#' MfdFlowAccumulation(rbind(c(2, 1), c(1, 0)))
#' @export
DinfFlowDirection <- function(dem, res = 1) {
  G <- as.matrix(dem)
  nr <- nrow(G)
  nc <- ncol(G)
  fac <- list(list(c(0, 1), c(-1, 1), 0, 1), list(c(-1, 0), c(-1, 1), 1, -1), list(c(-1, 0), c(-1, -1), 1, 1),
              list(c(0, -1), c(-1, -1), 2, -1), list(c(0, -1), c(1, -1), 2, 1), list(c(1, 0), c(1, -1), 3, -1),
              list(c(1, 0), c(1, 1), 3, 1), list(c(0, 1), c(1, 1), 4, -1))
  out <- matrix(NaN, nr, nc)
  if (nr < 3 || nc < 3) return(out)
  for (i in 2:(nr - 1)) {
    for (j in 2:(nc - 1)) {
      if (anyNA(G[(i - 1):(i + 1), (j - 1):(j + 1)])) next
      e0 <- G[i, j]
      best <- 0
      ang <- NaN
      for (f in fac) {
        e1 <- G[i + f[[1]][1], j + f[[1]][2]]
        e2 <- G[i + f[[2]][1], j + f[[2]][2]]
        s1 <- (e0 - e1) / res
        s2 <- (e1 - e2) / res
        r <- atan2(s2, s1)
        s <- sqrt(s1^2 + s2^2)
        if (r < 0) {
          r <- 0
          s <- s1
        } else if (r > pi / 4) {
          r <- pi / 4
          s <- (e0 - e2) / (res * sqrt(2))
        }
        if (s > best) {
          best <- s
          ang <- f[[3]] * pi / 2 + f[[4]] * r
        }
      }
      out[i, j] <- if (is.nan(ang)) NaN else ang %% (2 * pi)
    }
  }
  out
}

#' @rdname DinfFlowDirection
#' @export
MfdFlowAccumulation <- function(dem, res = 1, p = 1.1) {
  G <- as.matrix(dem)
  nr <- nrow(G)
  nc <- ncol(G)
  acc <- ifelse(is.na(G), NaN, 1)
  o <- order(-G, -row(G), -col(G), na.last = NA)
  for (c in o) {
    i <- row(G)[c]
    j <- col(G)[c]
    z <- G[i, j]
    tgt <- NULL
    w <- NULL
    for (k in 1:8) {
      x <- i + .dem_off[k, 1]
      y <- j + .dem_off[k, 2]
      if (x >= 1 && x <= nr && y >= 1 && y <= nc && !is.na(G[x, y]) && G[x, y] < z) {
        tgt <- rbind(tgt, c(x, y))
        w <- c(w, ((z - G[x, y]) / (res * (if (all(.dem_off[k, ] != 0)) sqrt(2) else 1)))^p)
      }
    }
    if (length(w)) for (t in seq_along(w)) acc[tgt[t, 1], tgt[t, 2]] <- acc[tgt[t, 1], tgt[t, 2]] + acc[i, j] * w[t] / sum(w)
  }
  acc
}

#' Curvature and viewshed
#'
#' \code{Curvature}: profile, plan and total curvature (Zevenbergen and
#' Thorne 1987). \code{Viewshed}: straight line-of-sight visibility from
#' \code{observer} (row, column), no earth curvature (Franklin and Ray
#' 1994).
#'
#' @param dem Elevation matrix.
#' @param res Cell size.
#' @param observer Row and column (1-based).
#' @param observer_height,target_height Heights above ground.
#' @return List of matrices or a 0/1 matrix.
#' @references Zevenbergen, L. W. and Thorne, C. R. (1987). Quantitative
#'   analysis of land surface topography. Earth Surface Processes and
#'   Landforms 12, 47-56.
#' @examples
#' Curvature(rbind(c(2, 1, 2), c(1, 0, 1), c(2, 1, 2)))$total
#' Viewshed(rbind(c(0, 0, 0, 0), c(0, 0, 9, 0)), c(1, 1))
#' @export
Curvature <- function(dem, res = 1) {
  G <- as.matrix(dem)
  nr <- nrow(G)
  nc <- ncol(G)
  out <- list(profile = matrix(NaN, nr, nc), plan = matrix(NaN, nr, nc), total = matrix(NaN, nr, nc))
  if (nr < 3 || nc < 3) return(out)
  L <- res
  for (i in 2:(nr - 1)) {
    for (j in 2:(nc - 1)) {
      w <- as.vector(t(G[(i - 1):(i + 1), (j - 1):(j + 1)]))
      if (anyNA(w)) next
      D <- ((w[4] + w[6]) / 2 - w[5]) / L^2
      E <- ((w[2] + w[8]) / 2 - w[5]) / L^2
      F <- (w[3] - w[1] + w[7] - w[9]) / (4 * L^2)
      Gx <- (w[6] - w[4]) / (2 * L)
      H <- (w[2] - w[8]) / (2 * L)
      out$total[i, j] <- -2 * (D + E)
      g2 <- Gx^2 + H^2
      if (g2 > 0) {
        out$profile[i, j] <- -2 * (D * Gx^2 + E * H^2 + F * Gx * H) / g2
        out$plan[i, j] <- 2 * (D * H^2 + E * Gx^2 - F * Gx * H) / g2
      }
    }
  }
  out
}

#' @rdname Curvature
#' @export
Viewshed <- function(dem, observer, res = 1, observer_height = 1.7, target_height = 0) {
  G <- as.matrix(dem)
  oi <- observer[1]
  oj <- observer[2]
  eye <- G[oi, oj] + observer_height
  out <- matrix(0, nrow(G), ncol(G))
  for (ti in seq_len(nrow(G))) {
    for (tj in seq_len(ncol(G))) {
      if (is.na(G[ti, tj])) next
      n <- max(abs(ti - oi), abs(tj - oj))
      if (n == 0) {
        out[ti, tj] <- 1
        next
      }
      tz <- G[ti, tj] + target_height
      vis <- 1
      if (n > 1) {
        for (k in 1:(n - 1)) {
          t <- k / n
          # round half to even on 0-based positions, as the Python arm
          ci <- round((oi - 1) + t * (ti - oi)) + 1
          cj <- round((oj - 1) + t * (tj - oj)) + 1
          z <- G[ci, cj]
          if (!is.na(z) && z > eye + t * (tz - eye)) {
            vis <- 0
            break
          }
        }
      }
      out[ti, tj] <- vis
    }
  }
  out
}
