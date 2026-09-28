.nn_clip <- function(poly, a, b) {
  m <- nrow(poly)
  if (!m) return(poly)
  out <- matrix(numeric(0), 0, 2)
  for (k in seq_len(m)) {
    p <- poly[k, ]
    q <- poly[if (k == m) 1 else k + 1, ]
    fp <- a[1] * p[1] + a[2] * p[2] - b
    fq <- a[1] * q[1] + a[2] * q[2] - b
    if (fp <= 0) out <- rbind(out, p)
    if ((fp < 0 && 0 < fq) || (fq < 0 && 0 < fp)) {
      t <- fp / (fp - fq)
      out <- rbind(out, p + t * (q - p))
    }
  }
  unname(out)
}

.nn_area <- function(poly) {
  m <- nrow(poly)
  j <- c(2:m, 1)
  0.5 * sum(poly[, 1] * poly[j, 2] - poly[j, 1] * poly[, 2])
}

.nn_half <- function(p, q) {
  a <- q - p
  list(a = a, b = a[1] * (p[1] + q[1]) / 2 + a[2] * (p[2] + q[2]) / 2)
}

.nn_window <- function(P, x, scale = 10) {
  xs <- c(P[, 1], x[1])
  ys <- c(P[, 2], x[2])
  span <- max(diff(range(xs)), diff(range(ys)), 1)
  rbind(c(min(xs) - scale * span, min(ys) - scale * span), c(max(xs) + scale * span, min(ys) - scale * span),
        c(max(xs) + scale * span, max(ys) + scale * span), c(min(xs) - scale * span, max(ys) + scale * span))
}

# Voronoi cell of x among P plus x; the window grows until the cell no longer reaches it (bounded cell).
.nn_cell <- function(P, x) {
  for (scale in c(10, 1e3, 1e5, 1e7)) {
    W <- .nn_window(P, x, scale)
    cell <- W
    for (j in seq_len(nrow(P))) {
      h <- .nn_half(x, P[j, ])
      cell <- .nn_clip(cell, h$a, h$b)
    }
    tol <- 1e-9 * (W[2, 1] - W[1, 1])
    touch <- abs(cell[, 1] - W[1, 1]) < tol | abs(cell[, 1] - W[2, 1]) < tol | abs(cell[, 2] - W[1, 2]) < tol |
      abs(cell[, 2] - W[3, 2]) < tol
    if (!any(touch)) break
  }
  cell
}

.nn_hull <- function(P) {
  S <- unique(P[order(P[, 1], P[, 2]), , drop = FALSE])
  if (nrow(S) <= 2) return(S)
  cross <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  build <- function(idx) {
    h <- integer(0)
    for (k in idx) {
      while (length(h) >= 2 && cross(S[h[length(h) - 1], ], S[h[length(h)], ], S[k, ]) <= 0) h <- h[-length(h)]
      h <- c(h, k)
    }
    h
  }
  lo <- build(seq_len(nrow(S)))
  up <- build(rev(seq_len(nrow(S))))
  S[c(lo[-length(lo)], up[-length(up)]), , drop = FALSE]
}

.nn_in_hull <- function(H, x, tol = 1e-12) {
  n <- nrow(H)
  for (k in seq_len(n)) {
    p <- H[k, ]
    q <- H[if (k == n) 1 else k + 1, ]
    if ((q[1] - p[1]) * (x[2] - p[2]) - (q[2] - p[2]) * (x[1] - p[1]) < -tol * max(1, abs(x[1]) + abs(x[2]))) {
      return(FALSE)
    }
  }
  TRUE
}

.nn_weights <- function(P, x, method) {
  hit <- which(P[, 1] == x[1] & P[, 2] == x[2])
  if (length(hit)) return(stats::setNames(1, hit[1]))
  cell <- .nn_cell(P, x)
  ax <- abs(.nn_area(cell))
  w <- numeric(0)
  if (method == "sibson") {
    for (i in seq_len(nrow(P))) {
      reg <- cell
      for (j in seq_len(nrow(P))) if (j != i && nrow(reg)) {
        h <- .nn_half(P[i, ], P[j, ])
        reg <- .nn_clip(reg, h$a, h$b)
      }
      if (nrow(reg) >= 3) {
        s <- abs(.nn_area(reg))
        if (s > 1e-15 * ax) w[as.character(i)] <- s / ax
      }
    }
    return(w)
  }
  if (method != "laplace") stop("method must be sibson or laplace", call. = FALSE)
  m <- nrow(cell)
  for (k in seq_len(m)) {
    u <- cell[k, ]
    v <- cell[if (k == m) 1 else k + 1, ]
    L <- sqrt(sum((v - u)^2))
    if (L <= 0) next
    mid <- (u + v) / 2
    dx <- sqrt(sum((mid - x)^2))
    dev <- abs(sqrt((mid[1] - P[, 1])^2 + (mid[2] - P[, 2])^2) - dx)
    best <- which.min(dev)
    if (dev[best] <= 1e-9 * max(1, dx)) {
      key <- as.character(best)
      w[key] <- (if (is.na(w[key])) 0 else w[key]) + L / sqrt(sum((P[best, ] - x)^2))
    }
  }
  w / sum(w)
}

#' Natural-neighbour interpolation (Sibson and Laplace coordinates)
#'
#' \code{NaturalNeighbourWeights}: Sibson (area-stealing) or Laplace
#' (non-Sibsonian: facet length over distance) coordinates of a query point,
#' positive only on its natural neighbours, summing to one and reproducing
#' linear functions. \code{NnInterpolate}: \eqn{f(x) = \sum_i w_i z_i}, with the
#' natural-neighbour variance and the bounds of the neighbours' values;
#' \code{NA} outside the convex hull unless \code{outside = "extrapolate"}.
#' \code{NnGradientInterpolate}: \eqn{\sum_i w_i (z_i + g_i \cdot (x - x_i))}
#' with gradients from inverse-squared-distance weighted planes through each
#' datum and its natural neighbours. \code{NnCrossValidation}: leave-one-out
#' predictions and RMSE. \code{NnInDomain}: the convex-hull domain (Sibson 1981;
#' Belikov et al. 1997; Watson 1992). Identical to the Python arm
#' \code{morie.fn.natnbr}; weight names are 1-based point indices.
#'
#' @param points Two-column data locations.
#' @param x Query point.
#' @param xs Two-column query points.
#' @param values Data values.
#' @param method \code{"sibson"} or \code{"laplace"}.
#' @param outside \code{"nan"} or \code{"extrapolate"}.
#' @return List (or logical vector).
#' @references Sibson, R. (1981). A brief description of natural neighbour
#'   interpolation. In V. Barnett (ed.), Interpreting Multivariate Data, 21-36.
#'   Wiley.
#'
#'   Belikov, V. V. et al. (1997). The non-Sibsonian interpolation: a new method
#'   of interpolation of the values of a function on an arbitrary set of points.
#'   Computational Mathematics and Mathematical Physics 37, 9-15.
#'
#'   Watson, D. F. (1992). Contouring: A Guide to the Analysis and Display of
#'   Spatial Data. Pergamon.
#' @examples
#' sq <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
#' NaturalNeighbourWeights(sq, c(0.5, 0.5))$weights
#' P <- rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(1, 3))
#' NnInterpolate(P, 1 + 2 * P[, 1] - P[, 2], rbind(c(1, 1), c(5, 5)))$estimates
#' @export
NaturalNeighbourWeights <- function(points, x, method = "sibson") {
  w <- .nn_weights(as.matrix(points), as.numeric(x), method)
  list(weights = w, neighbours = sort(as.integer(names(w))))
}

#' @rdname NaturalNeighbourWeights
#' @export
NnInterpolate <- function(points, values, xs, method = "sibson", outside = "nan") {
  P <- as.matrix(points)
  z <- as.numeric(values)
  X <- matrix(as.numeric(as.matrix(xs)), ncol = 2)
  H <- .nn_hull(P)
  res <- t(vapply(seq_len(nrow(X)), function(k) {
    x <- X[k, ]
    if (outside == "nan" && !.nn_in_hull(H, x)) return(rep(NaN, 4))
    w <- .nn_weights(P, x, method)
    zi <- z[as.integer(names(w))]
    f <- sum(w * zi)
    c(f, sum(w * (zi - f)^2), min(zi), max(zi))
  }, numeric(4)))
  list(estimates = res[, 1], variance = res[, 2], lower = res[, 3], upper = res[, 4])
}

#' @rdname NaturalNeighbourWeights
#' @export
NnGradientInterpolate <- function(points, values, xs, method = "sibson") {
  P <- as.matrix(points)
  z <- as.numeric(values)
  X <- matrix(as.numeric(as.matrix(xs)), ncol = 2)
  G <- t(vapply(seq_len(nrow(P)), function(i) {
    w <- .nn_weights(P[-i, , drop = FALSE], P[i, ], "sibson")
    nb <- seq_len(nrow(P))[-i][as.integer(names(w))]
    if (length(nb) < 2) return(c(0, 0))
    d <- sweep(P[nb, , drop = FALSE], 2, P[i, ])
    wt <- 1 / rowSums(d^2)
    A <- crossprod(d * wt, d)
    as.numeric(solve(A, crossprod(d * wt, z[nb] - z[i])))
  }, numeric(2)))
  H <- .nn_hull(P)
  est <- vapply(seq_len(nrow(X)), function(k) {
    x <- X[k, ]
    if (!.nn_in_hull(H, x)) return(NaN)
    w <- .nn_weights(P, x, method)
    ix <- as.integer(names(w))
    sum(w * (z[ix] + G[ix, 1] * (x[1] - P[ix, 1]) + G[ix, 2] * (x[2] - P[ix, 2])))
  }, 0)
  list(estimates = est, gradients = G)
}

#' @rdname NaturalNeighbourWeights
#' @export
NnCrossValidation <- function(points, values, method = "sibson") {
  P <- as.matrix(points)
  z <- as.numeric(values)
  pred <- vapply(seq_len(nrow(P)), function(i) {
    NnInterpolate(P[-i, , drop = FALSE], z[-i], P[i, , drop = FALSE], method = method)$estimates
  }, 0)
  res <- pred - z
  ok <- res[!is.nan(res)]
  list(predictions = pred, residuals = res, rmse = if (length(ok)) sqrt(mean(ok^2)) else NaN,
       mean_error = if (length(ok)) mean(ok) else NaN, n_predicted = length(ok))
}

#' @rdname NaturalNeighbourWeights
#' @export
NnInDomain <- function(points, xs) {
  H <- .nn_hull(as.matrix(points))
  X <- matrix(as.numeric(as.matrix(xs)), ncol = 2)
  vapply(seq_len(nrow(X)), function(k) .nn_in_hull(H, X[k, ]), TRUE)
}
