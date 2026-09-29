#' Grid generation and grid-based sampling
#'
#' \code{RegularGrid}: square or hexagonal cell centres. \code{TriangularGrid}:
#' equilateral triangular mesh (zero-based vertex indices).
#' \code{TransfiniteGrid}: Coons-patch curvilinear grid.
#' \code{SystematicSample}: square grid sample with a Philox random start.
#' \code{SpaceTimeSample}: static, synchronous, static-synchronous and
#' rotating designs (zero-based unit indices). Identical to the Python arm
#' \code{morie.fn.gridgen}.
#'
#' @param bbox c(xmin, ymin, xmax, ymax).
#' @param cellsize Cell size (centre spacing).
#' @param shape \code{"square"} or \code{"hexagon"}.
#' @param side Triangle side.
#' @param bottom,top Boundary curves sampled at nu points (two-column).
#' @param left,right Boundary curves sampled at nv points.
#' @param spacing Grid spacing.
#' @param seed Philox seed.
#' @param polygon Optional polygon vertices (two-column).
#' @param n_population,n_sample,n_times Population size, sample size, occasions.
#' @param design Design name.
#' @param n_static Static panel size.
#' @param rotation Occasions each unit stays in a rotating panel.
#' @return A matrix or list.
#' @references Gordon, W. J. and Hall, C. A. (1973). Construction of
#'   curvilinear co-ordinate systems. International Journal for Numerical
#'   Methods in Engineering 7, 461-477.
#'
#'   de Gruijter, J. J., Brus, D. J., Bierkens, M. F. P. and Knotters, M.
#'   (2006). Sampling for Natural Resource Monitoring. Springer.
#' @examples
#' RegularGrid(c(0, 0, 2, 1), 1)
#' @export
RegularGrid <- function(bbox, cellsize, shape = "square") {
  d <- cellsize
  if (shape == "square") {
    nx <- ceiling((bbox[3] - bbox[1]) / d - 1e-12)
    ny <- ceiling((bbox[4] - bbox[2]) / d - 1e-12)
    g <- expand.grid(i = seq_len(nx) - 1, j = seq_len(ny) - 1)
    return(cbind(bbox[1] + (g$i + 0.5) * d, bbox[2] + (g$j + 0.5) * d))
  }
  if (shape == "hexagon") {
    dy <- d * sqrt(3) / 2
    out <- NULL
    j <- 0
    while (bbox[2] + j * dy <= bbox[4] + dy / 2) {
      off <- if (j %% 2 == 1) d / 2 else 0
      i <- 0
      while (bbox[1] + off + i * d <= bbox[3] + d / 2) {
        out <- rbind(out, c(bbox[1] + off + i * d, bbox[2] + j * dy))
        i <- i + 1
      }
      j <- j + 1
    }
    return(out)
  }
  stop("shape must be 'square' or 'hexagon'")
}

#' @rdname RegularGrid
#' @export
TriangularGrid <- function(bbox, side) {
  h <- side * sqrt(3) / 2
  ny <- ceiling((bbox[4] - bbox[2]) / h - 1e-12) + 1
  nx <- ceiling((bbox[3] - bbox[1]) / side - 1e-12) + 1
  g <- expand.grid(i = seq_len(nx) - 1, j = seq_len(ny) - 1)
  V <- cbind(bbox[1] + ifelse(g$j %% 2 == 1, side / 2, 0) + g$i * side, bbox[2] + g$j * h)
  Tr <- NULL
  for (j in seq_len(ny - 1) - 1) {
    for (i in seq_len(nx - 1) - 1) {
      a <- j * nx + i
      b <- a + 1
      cc <- (j + 1) * nx + i
      d <- cc + 1
      Tr <- rbind(Tr, if (j %% 2 == 0) rbind(c(a, b, cc), c(b, d, cc)) else rbind(c(a, d, cc), c(a, b, d)))
    }
  }
  list(vertices = V, triangles = Tr)
}

#' @rdname RegularGrid
#' @export
TransfiniteGrid <- function(bottom, top, left, right) {
  B <- as.matrix(bottom)
  Tp <- as.matrix(top)
  L <- as.matrix(left)
  R <- as.matrix(right)
  nu <- nrow(B)
  nv <- nrow(L)
  out <- array(0, c(nv, nu, 2))
  for (j in seq_len(nv)) {
    v <- (j - 1) / (nv - 1)
    for (i in seq_len(nu)) {
      u <- (i - 1) / (nu - 1)
      out[j, i, ] <- (1 - v) * B[i, ] + v * Tp[i, ] + (1 - u) * L[j, ] + u * R[j, ] -
        ((1 - u) * (1 - v) * B[1, ] + u * (1 - v) * B[nu, ] + (1 - u) * v * Tp[1, ] + u * v * Tp[nu, ])
    }
  }
  out
}

.gg_inside <- function(p, poly) {
  inside <- FALSE
  n <- nrow(poly)
  for (i in seq_len(n)) {
    a <- poly[i, ]
    b <- poly[if (i == n) 1 else i + 1, ]
    if ((a[2] > p[2]) != (b[2] > p[2]) && p[1] < (b[1] - a[1]) * (p[2] - a[2]) / (b[2] - a[2]) + a[1]) {
      inside <- !inside
    }
  }
  inside
}

#' @rdname RegularGrid
#' @export
SystematicSample <- function(bbox, spacing, seed = 0, polygon = NULL) {
  u <- .morie_random_uniform(2, seed = seed)
  xs <- numeric(0)
  x <- bbox[1] + u[1] * spacing
  while (x <= bbox[3]) {
    xs <- c(xs, x)
    x <- x + spacing
  }
  ys <- numeric(0)
  y <- bbox[2] + u[2] * spacing
  while (y <= bbox[4]) {
    ys <- c(ys, y)
    y <- y + spacing
  }
  out <- as.matrix(expand.grid(xs, ys))
  dimnames(out) <- NULL
  if (!is.null(polygon)) out <- out[apply(out, 1, .gg_inside, poly = as.matrix(polygon)), , drop = FALSE]
  out
}

#' @rdname RegularGrid
#' @export
SpaceTimeSample <- function(n_population, n_sample, n_times, design = "static", n_static = NULL, rotation = 2,
                            seed = 0) {
  N <- n_population
  u <- .morie_random_uniform(N * (n_times + 1), seed = seed)
  block <- function(t) u[t * N + seq_len(N)]
  srs <- function(n, b) sort(order(b, seq_len(N))[seq_len(n)] - 1)
  if (design == "static") return(rep(list(srs(n_sample, block(0))), n_times))
  if (design == "synchronous") return(lapply(seq_len(n_times) - 1, function(t) srs(n_sample, block(t))))
  if (design == "static_synchronous") {
    k <- if (is.null(n_static)) n_sample %/% 2 else n_static
    st <- srs(k, block(0))
    rest <- setdiff(seq_len(N) - 1, st)
    return(lapply(seq_len(n_times), function(t) {
      b <- block(t)
      sort(c(st, rest[order(b[rest + 1], rest)][seq_len(n_sample - k)]))
    }))
  }
  if (design == "rotating") {
    g <- n_sample %/% rotation
    if (N < g * (n_times + rotation - 1)) stop("population too small for this rotating panel")
    ord <- order(u[seq_len(N)], seq_len(N)) - 1
    return(lapply(seq_len(n_times), function(t) sort(ord[(t - 1) * g + seq_len(rotation * g)])))
  }
  stop("design must be static, synchronous, static_synchronous or rotating")
}
