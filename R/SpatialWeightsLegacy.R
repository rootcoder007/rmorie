.swl_wrap <- function(G, style) {
  if (style == "W") {
    rs <- rowSums(G)
    G <- G / ifelse(rs > 0, rs, 1)
  } else if (style != "B") {
    stop("style must be 'B' or 'W'")
  }
  nb <- lapply(seq_len(nrow(G)), function(i) which(G[i, ] != 0) - 1L)
  list(statistic = sum(lengths(nb)) / nrow(G), W = G, neighbours = nb, style = style)
}

#' Spatial weights: builders, lags and graph operations
#'
#' Front-ends to `SpatialWeights`: `swdist` distance band
#' (`spdep::dnearneigh`), `swknn` k nearest neighbours, `swgab` Gabriel graph,
#' `swtri` Delaunay triangulation, `swinv` inverse distance, `swkern` fixed
#' and `swadapt` adaptive (k-th neighbour) kernel weights.  `swblk` block
#' weights of units sharing a group, `swsph` great-circle (haversine)
#' distance bands in km, `swlag` and `swlag2` the lags `Wy` and `W^2 y`,
#' `swlagf` the multiplier `(I - rho W)^-1`, `swpower` the matrix power
#' `W^p`, `swspars` thresholding of weak links and `swpath` the contiguity
#' order (breadth-first) or Dijkstra length between two units (0-based
#' indices, as in the Python arm).
#'
#' @param coords Coordinates (n x 2).
#' @param d Distance band (km for `swsph`); `NULL` in `swinv` means all pairs.
#' @param style Coding style, "B" or "W" (all `SpatialWeights` styles for the
#'   point-pattern builders).
#' @param d_min Lower distance bound.
#' @param k Number of neighbours.
#' @param power Inverse-distance power.
#' @param bw Kernel bandwidth.
#' @param kernel Kernel name.
#' @param groups Group label of each unit.
#' @param lat,lon Latitudes and longitudes in degrees.
#' @param radius Sphere radius in km.
#' @param W Spatial weights matrix.
#' @param y Numeric vector.
#' @param rho Autoregressive parameter.
#' @param p Non-negative integer power.
#' @param thr Threshold below which links are dropped.
#' @param row_standardize Rescale the surviving rows to sum to one.
#' @param i,j 0-based unit indices.
#' @param weighted Use the weights as edge lengths (Dijkstra).
#' @return Builders return a list with `statistic` (mean number of
#'   neighbours), `W` and `neighbours` (0-based); `swlag`, `swlag2` a vector;
#'   `swlagf`, `swpower`, `swspars` a matrix; `swpath` a number.
#' @references Bivand, R. S., Pebesma, E. and Gomez-Rubio, V. (2013).
#'   Applied Spatial Data Analysis with R, 2nd ed. Springer. Gabriel, K. R.
#'   and Sokal, R. R. (1969). A new statistical approach to geographic
#'   variation analysis. Systematic Zoology 18, 259-278. Getis, A. and
#'   Aldstadt, J. (2004). Constructing the spatial weights matrix using a
#'   local statistic. Geographical Analysis 36, 90-104. Dijkstra, E. W.
#'   (1959). A note on two problems in connexion with graphs. Numerische
#'   Mathematik 1, 269-271.
#' @examples
#' xy <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.4), c(2, 0.5))
#' swknn(xy, k = 2)$neighbours
#' swlag(rbind(c(0, 1, 0), c(0.5, 0, 0.5), c(0, 1, 0)), c(1, 2, 4))
#' @export
swdist <- function(coords, d = 1, style = "B", d_min = 0) {
  .swl_sw(SpatialWeights(coords, "distance", threshold = d, style = style, d_min = d_min))
}

.swl_sw <- function(r) {
  nb <- lapply(seq_len(nrow(r$W)), function(i) which(r$W[i, ] != 0) - 1L)
  list(statistic = sum(lengths(nb)) / nrow(r$W), W = r$W, neighbours = nb, style = r$style)
}

#' @rdname swdist
#' @export
swknn <- function(coords, k = 4, style = "B") .swl_sw(SpatialWeights(coords, "knn", k = k, style = style))

#' @rdname swdist
#' @export
swgab <- function(coords, style = "B") .swl_sw(SpatialWeights(coords, "gabriel", style = style))

#' @rdname swdist
#' @export
swtri <- function(coords, style = "B") .swl_sw(SpatialWeights(coords, "delaunay", style = style))

#' @rdname swdist
#' @export
swinv <- function(coords, power = 1, d = NULL, style = "B") {
  .swl_sw(SpatialWeights(coords, "inverse", threshold = if (is.null(d)) Inf else d, style = style, alpha = power))
}

#' @rdname swdist
#' @export
swkern <- function(coords, bw = 1, kernel = "gaussian", style = "B") {
  .swl_sw(SpatialWeights(coords, "kernel", bandwidth = bw, kernel = kernel, style = style))
}

#' @rdname swdist
#' @export
swadapt <- function(coords, k = 5, kernel = "gaussian", style = "B") {
  .swl_sw(SpatialWeights(coords, "kernel", k = k, kernel = kernel, style = style))
}

#' @rdname swdist
#' @export
swblk <- function(groups, style = "B") .swl_wrap(BlockWeights(groups), style)

#' @rdname swdist
#' @export
swsph <- function(lat, lon, d = 500, radius = 6371, style = "B") {
  la <- as.numeric(lat) * pi / 180
  lo <- as.numeric(lon) * pi / 180
  a <- outer(la, la, function(p1, p2) sin((p2 - p1) / 2)^2) +
    outer(cos(la), cos(la)) * outer(lo, lo, function(l1, l2) sin((l2 - l1) / 2)^2)
  D <- 2 * radius * asin(pmin(sqrt(a), 1))
  G <- 1 * (D <= d)
  diag(G) <- 0
  r <- .swl_wrap(G, style)
  r$D <- D
  r
}

#' @rdname swdist
#' @export
swlag <- function(W, y) LagOperator(W, y, 1L)

#' @rdname swdist
#' @export
swlag2 <- function(W, y) LagOperator(W, y, 2L)

#' @rdname swdist
#' @export
swlagf <- function(W, rho) ErrorOperator(W, rho)

#' @rdname swdist
#' @export
swpower <- function(W, p = 2) {
  .morie_arg(W, "m")
  A <- unname(as.matrix(W)) * 1
  if (p < 0) stop("p must be non-negative")
  P <- diag(nrow(A))
  for (it in seq_len(p)) P <- P %*% A
  P
}

#' @rdname swdist
#' @export
swspars <- function(W, thr = 0.1, row_standardize = FALSE) {
  S <- unname(as.matrix(W)) * 1
  S[abs(S) < thr] <- 0
  if (row_standardize) {
    rs <- rowSums(S)
    S <- S / ifelse(rs != 0, rs, 1)
  }
  S
}

#' @rdname swdist
#' @export
swpath <- function(W, i = 0, j = 2, weighted = FALSE) {
  A <- unname(as.matrix(W)) * 1
  n <- nrow(A)
  adj <- (A != 0) | (t(A) != 0)
  diag(adj) <- FALSE
  src <- i + 1
  if (!weighted) {
    dist <- rep(Inf, n)
    dist[src] <- 0
    frontier <- src
    while (length(frontier)) {
      nxt <- integer(0)
      for (a in frontier) for (b in which(adj[a, ] & is.infinite(dist))) {
        dist[b] <- dist[a] + 1
        nxt <- c(nxt, b)
      }
      frontier <- nxt
    }
    return(dist[j + 1])
  }
  L <- ifelse(A != 0, A, t(A))
  best <- rep(Inf, n)
  best[src] <- 0
  done <- rep(FALSE, n)
  repeat {
    cand <- which(!done & is.finite(best))
    if (!length(cand)) break
    a <- cand[which.min(best[cand])]
    done[a] <- TRUE
    for (b in which(adj[a, ])) if (best[a] + L[a, b] < best[b]) best[b] <- best[a] + L[a, b]
  }
  best[j + 1]
}
