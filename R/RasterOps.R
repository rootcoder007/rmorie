.ro_window <- function(G, i, j, h, w) {
  nr <- nrow(G)
  nc <- ncol(G)
  out <- numeric(0)
  for (a in (i - h):(i + h)) {
    for (b in (j - w):(j + w)) out <- c(out, if (a >= 1 && a <= nr && b >= 1 && b <= nc) G[a, b] else NA_real_)
  }
  out
}

.ro_stat <- function(v, fun) {
  switch(fun,
    mean = mean(v), sum = sum(v), min = min(v), max = max(v), range = max(v) - min(v), median = stats::median(v),
    var = if (length(v) < 2) NaN else stats::var(v), sd = if (length(v) < 2) NaN else stats::sd(v),
    entropy = {
      p <- as.vector(table(v)) / length(v)
      -sum(p * log(p))
    },
    stop("unknown focal function")
  )
}

#' Focal statistics, filters and grey morphology
#'
#' \code{FocalStatistics}: moving-window mean, sum, min, max, range, median,
#' sample variance or standard deviation, or Shannon entropy, as
#' \code{terra::focal} (cells outside the grid are \code{NA}).
#' \code{FilterKernel}: mean, binomial, Gaussian, Laplacian, sharpen,
#' Prewitt, Sobel and Gabor kernels. \code{FocalFilter}: weighted window sum
#' (correlation) as \code{terra::focal(x, w, fun = "sum")}. \code{CannyEdges}:
#' Canny (1986) detector. \code{GreyMorphology}: erosion, dilation, opening
#' and closing with a flat square element. Identical to the Python arm
#' \code{morie.fn.rastops}.
#'
#' @param grid Numeric matrix (row 1 = north).
#' @param size Window or kernel size (odd).
#' @param fun Statistic.
#' @param na_rm Drop \code{NA} values.
#' @param name Kernel name.
#' @param sigma,theta,wavelength,gamma,psi Gaussian and Gabor parameters.
#' @param kernel Weight matrix.
#' @param low,high Hysteresis thresholds (fractions of the maximum gradient).
#' @param operation \code{"erosion"}, \code{"dilation"}, \code{"opening"} or
#'   \code{"closing"}.
#' @return Matrix.
#' @references Canny, J. (1986). A computational approach to edge detection.
#'   IEEE Transactions on Pattern Analysis and Machine Intelligence 8,
#'   679-698.
#'
#'   Serra, J. (1982). Image Analysis and Mathematical Morphology. Academic
#'   Press.
#' @examples
#' FocalStatistics(matrix(1:9, 3, byrow = TRUE), fun = "median", na_rm = TRUE)
#' FocalFilter(matrix(1:9, 3, byrow = TRUE), FilterKernel("prewitt_x"))[2, 2]
#' @export
FocalStatistics <- function(grid, size = 3, fun = "mean", na_rm = FALSE) {
  if (size %% 2 != 1) stop("size must be odd")
  G <- matrix(as.numeric(grid), nrow(as.matrix(grid)))
  h <- size %/% 2
  out <- G
  for (i in seq_len(nrow(G))) {
    for (j in seq_len(ncol(G))) {
      v <- .ro_window(G, i, j, h, h)
      if (anyNA(v)) {
        if (!na_rm) {
          out[i, j] <- NA_real_
          next
        }
        v <- v[!is.na(v)]
        if (!length(v)) {
          out[i, j] <- NA_real_
          next
        }
      }
      out[i, j] <- .ro_stat(v, fun)
    }
  }
  out
}

#' @rdname FocalStatistics
#' @export
FilterKernel <- function(name, size = 3, sigma = 1, theta = 0, wavelength = 4, gamma = 0.5, psi = 0) {
  h <- size %/% 2
  off <- seq_len(size) - 1 - h
  switch(name,
    mean = matrix(1 / size^2, size, size),
    binomial = {
      b <- choose(size - 1, 0:(size - 1))
      outer(b, b) / sum(b)^2
    },
    gaussian = {
      K <- exp(-outer(off^2, off^2, `+`) / (2 * sigma^2))
      K / sum(K)
    },
    gabor = {
      x <- matrix(off, size, size, byrow = TRUE)
      y <- matrix(off, size, size)
      xp <- x * cos(theta) + y * sin(theta)
      yp <- -x * sin(theta) + y * cos(theta)
      exp(-(xp^2 + gamma^2 * yp^2) / (2 * sigma^2)) * cos(2 * pi * xp / wavelength + psi)
    },
    laplacian = rbind(c(0, 1, 0), c(1, -4, 1), c(0, 1, 0)),
    laplacian8 = rbind(c(1, 1, 1), c(1, -8, 1), c(1, 1, 1)),
    sharpen = rbind(c(0, -1, 0), c(-1, 5, -1), c(0, -1, 0)),
    prewitt_x = rbind(c(-1, 0, 1), c(-1, 0, 1), c(-1, 0, 1)),
    prewitt_y = rbind(c(-1, -1, -1), c(0, 0, 0), c(1, 1, 1)),
    sobel_x = rbind(c(-1, 0, 1), c(-2, 0, 2), c(-1, 0, 1)),
    sobel_y = rbind(c(-1, -2, -1), c(0, 0, 0), c(1, 2, 1)),
    stop("unknown kernel")
  )
}

#' @rdname FocalStatistics
#' @export
FocalFilter <- function(grid, kernel, na_rm = FALSE) {
  G <- matrix(as.numeric(grid), nrow(as.matrix(grid)))
  K <- as.matrix(kernel)
  kh <- nrow(K) %/% 2
  kw <- ncol(K) %/% 2
  wv <- as.vector(t(K))
  out <- G
  for (i in seq_len(nrow(G))) {
    for (j in seq_len(ncol(G))) {
      v <- .ro_window(G, i, j, kh, kw)
      if (anyNA(v) && !na_rm) {
        out[i, j] <- NA_real_
        next
      }
      ok <- !is.na(v)
      out[i, j] <- if (any(ok)) sum(wv[ok] * v[ok]) else NA_real_
    }
  }
  out
}

#' @rdname FocalStatistics
#' @export
CannyEdges <- function(grid, sigma = 1, low = 0.1, high = 0.2, size = 5) {
  K <- FilterKernel("gaussian", size = size, sigma = sigma)
  G0 <- matrix(as.numeric(grid), nrow(as.matrix(grid)))
  num <- FocalFilter(G0, K, na_rm = TRUE)
  den <- FocalFilter(ifelse(is.na(G0), NA_real_, 1), K, na_rm = TRUE)
  S <- ifelse(!is.na(num) & den > 0, num / den, NA_real_)
  gx <- FocalFilter(S, FilterKernel("sobel_x"))
  gy <- FocalFilter(S, FilterKernel("sobel_y"))
  nr <- nrow(S)
  nc <- ncol(S)
  M <- ifelse(is.na(gx), 0, sqrt(gx^2 + gy^2))
  scale <- if (all(is.na(S))) 0 else max(abs(S), na.rm = TRUE)
  M[M <= 1e-12 * scale] <- 0
  mx <- max(M)
  if (mx == 0) mx <- 1
  N <- matrix(0, nr, nc)
  if (nr > 2 && nc > 2) {
    for (i in 2:(nr - 1)) {
      for (j in 2:(nc - 1)) {
        if (M[i, j] == 0) next
        ang <- (atan2(gy[i, j], gx[i, j]) * 180 / pi) %% 180
        nb <- if (ang < 22.5 || ang >= 157.5) {
          c(M[i, j - 1], M[i, j + 1])
        } else if (ang < 67.5) {
          c(M[i + 1, j + 1], M[i - 1, j - 1])
        } else if (ang < 112.5) {
          c(M[i - 1, j], M[i + 1, j])
        } else {
          c(M[i + 1, j - 1], M[i - 1, j + 1])
        }
        tol <- 1e-9 * M[i, j]
        if (M[i, j] >= nb[1] - tol && M[i, j] >= nb[2] - tol) N[i, j] <- M[i, j] / mx
      }
    }
  }
  E <- matrix(0L, nr, nc)
  st <- which(N >= high, arr.ind = TRUE)
  E[st] <- 1L
  stack <- st
  while (nrow(stack)) {
    p <- stack[nrow(stack), ]
    stack <- stack[-nrow(stack), , drop = FALSE]
    for (a in -1:1) {
      for (b in -1:1) {
        u <- p[1] + a
        v <- p[2] + b
        if (u >= 1 && u <= nr && v >= 1 && v <= nc && !E[u, v] && N[u, v] >= low) {
          E[u, v] <- 1L
          stack <- rbind(stack, c(u, v))
        }
      }
    }
  }
  E
}

#' @rdname FocalStatistics
#' @export
GreyMorphology <- function(grid, operation, size = 3) {
  ops <- list(erosion = "min", dilation = "max", opening = c("min", "max"), closing = c("max", "min"))
  if (is.null(ops[[operation]])) stop("operation must be erosion, dilation, opening or closing")
  G <- grid
  for (f in ops[[operation]]) G <- FocalStatistics(G, size, f, na_rm = TRUE)
  G
}

#' Raster aggregation, resampling, zonal statistics, masks and distances
#'
#' \code{RasterAggregate}: block statistics as \code{terra::aggregate}
#' (partial edge blocks kept). \code{RasterDisaggregate}: nearest-value
#' split. \code{RasterResample}: bilinear or nearest values at points.
#' \code{ZonalStatistics}: statistic per zone. \code{RasterMask}: as
#' \code{terra::mask}. \code{DistanceTransform}: Euclidean distance to the
#' nearest non-\code{NA} cell. \code{CostDistance}: accumulated least cost
#' with 8 neighbours (mean friction times step length, the ArcGIS Cost
#' Distance and gdistance convention).
#'
#' @param grid,zones,mask,cost Numeric matrices (row 1 = north).
#' @param fact Aggregation factor.
#' @param fun Statistic.
#' @param na_rm Drop \code{NA} values.
#' @param extent \code{c(xmin, xmax, ymin, ymax)} of the source grid.
#' @param new_x,new_y Target coordinates.
#' @param method \code{"bilinear"} or \code{"near"}.
#' @param maskvalue Value treated as masked.
#' @param inverse Invert the mask.
#' @param res Cell size.
#' @param sources Two-column matrix of source (row, column) cells.
#' @return Matrix or list.
#' @references Hijmans, R. J. (2024). terra: Spatial Data Analysis. R package.
#' @examples
#' RasterAggregate(matrix(1:9, 3, byrow = TRUE), 2)
#' CostDistance(rbind(c(1, 1, 1), c(1, 9, 1), c(1, 1, 1)), cbind(1, 1))[3, 3]
#' @export
RasterAggregate <- function(grid, fact, fun = "mean", na_rm = FALSE) {
  G <- as.matrix(grid)
  ri <- seq(1, nrow(G), by = fact)
  ci <- seq(1, ncol(G), by = fact)
  out <- matrix(NA_real_, length(ri), length(ci))
  for (a in seq_along(ri)) {
    for (b in seq_along(ci)) {
      v <- as.vector(t(G[ri[a]:min(ri[a] + fact - 1, nrow(G)), ci[b]:min(ci[b] + fact - 1, ncol(G)), drop = FALSE]))
      if (anyNA(v)) {
        if (!na_rm) next
        v <- v[!is.na(v)]
      }
      if (length(v)) out[a, b] <- .ro_stat(v, fun)
    }
  }
  out
}

#' @rdname RasterAggregate
#' @export
RasterDisaggregate <- function(grid, fact) {
  G <- as.matrix(grid)
  G[rep(seq_len(nrow(G)), each = fact), rep(seq_len(ncol(G)), each = fact), drop = FALSE]
}

#' @rdname RasterAggregate
#' @export
RasterResample <- function(grid, extent, new_x, new_y, method = "bilinear") {
  if (!method %in% c("bilinear", "near")) stop("method must be bilinear or near")
  G <- as.matrix(grid)
  nr <- nrow(G)
  nc <- ncol(G)
  dx <- (extent[2] - extent[1]) / nc
  dy <- (extent[4] - extent[3]) / nr
  out <- matrix(NA_real_, length(new_y), length(new_x))
  for (a in seq_along(new_y)) {
    for (b in seq_along(new_x)) {
      x <- new_x[b]
      y <- new_y[a]
      if (method == "near") {
        j <- floor((x - extent[1]) / dx) + 1
        i <- floor((extent[4] - y) / dy) + 1
        if (i >= 1 && i <= nr && j >= 1 && j <= nc) out[a, b] <- G[i, j]
        next
      }
      fx <- (x - extent[1]) / dx - 0.5
      fy <- (extent[4] - y) / dy - 0.5
      j0 <- floor(fx)
      i0 <- floor(fy)
      if (fx == nc - 1) j0 <- j0 - 1
      if (fy == nr - 1) i0 <- i0 - 1
      if (!(j0 >= 0 && j0 < nc - 1 && i0 >= 0 && i0 < nr - 1) || fx < 0 || fy < 0) next
      tx <- fx - j0
      ty <- fy - i0
      z <- c(G[i0 + 1, j0 + 1], G[i0 + 1, j0 + 2], G[i0 + 2, j0 + 1], G[i0 + 2, j0 + 2])
      if (anyNA(z)) next
      out[a, b] <- (1 - ty) * ((1 - tx) * z[1] + tx * z[2]) + ty * ((1 - tx) * z[3] + tx * z[4])
    }
  }
  out
}

#' @rdname RasterAggregate
#' @export
ZonalStatistics <- function(grid, zones, fun = "mean") {
  v <- as.vector(t(as.matrix(grid)))
  z <- as.vector(t(as.matrix(zones)))
  ok <- !is.na(v) & !is.na(z)
  keys <- sort(unique(z[ok]))
  list(zone = keys, value = vapply(keys, function(k) .ro_stat(v[ok & z == k], fun), 0),
       count = vapply(keys, function(k) sum(ok & z == k), 0))
}

#' @rdname RasterAggregate
#' @export
RasterMask <- function(grid, mask, maskvalue = NULL, inverse = FALSE) {
  G <- as.matrix(grid) + 0
  M <- as.matrix(mask)
  hit <- if (is.null(maskvalue)) is.na(M) else (!is.na(M) & M == maskvalue)
  G[hit != inverse] <- NA_real_
  G
}

#' @rdname RasterAggregate
#' @export
DistanceTransform <- function(grid, res = 1) {
  G <- as.matrix(grid)
  tg <- which(!is.na(G), arr.ind = TRUE)
  if (!nrow(tg)) stop("no target cells")
  out <- matrix(0, nrow(G), ncol(G))
  for (i in seq_len(nrow(G))) {
    for (j in seq_len(ncol(G))) {
      if (is.na(G[i, j])) out[i, j] <- res * min(sqrt((tg[, 1] - i)^2 + (tg[, 2] - j)^2))
    }
  }
  out
}

#' @rdname RasterAggregate
#' @export
CostDistance <- function(cost, sources, res = 1) {
  C <- as.matrix(cost)
  nr <- nrow(C)
  nc <- ncol(C)
  D <- matrix(Inf, nr, nc)
  done <- matrix(FALSE, nr, nc)
  S <- matrix(sources, ncol = 2)
  D[S] <- 0
  repeat {
    cand <- which(!done & is.finite(D))
    if (!length(cand)) break
    k <- cand[which.min(D[cand])]
    i <- (k - 1) %% nr + 1
    j <- (k - 1) %/% nr + 1
    done[i, j] <- TRUE
    for (a in -1:1) {
      for (b in -1:1) {
        u <- i + a
        v <- j + b
        if ((a != 0 || b != 0) && u >= 1 && u <= nr && v >= 1 && v <= nc && !is.na(C[u, v]) && !done[u, v]) {
          nd <- D[i, j] + res * (if (a != 0 && b != 0) sqrt(2) else 1) * (C[i, j] + C[u, v]) / 2
          if (nd < D[u, v]) D[u, v] <- nd
        }
      }
    }
  }
  D[is.na(C)] <- NA_real_
  D
}
