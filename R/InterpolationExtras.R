.ix_natural <- function(x, y, t) {
  n <- length(x)
  h <- diff(x)
  m <- numeric(n)
  if (n > 2) {
    k <- n - 2
    a <- h[1:k]
    b <- 2 * (h[1:k] + h[2:(k + 1)])
    cc <- h[2:(k + 1)]
    d <- 6 * ((y[3:n] - y[2:(n - 1)]) / h[2:(k + 1)] - (y[2:(n - 1)] - y[1:k]) / h[1:k])
    for (i in seq_len(k)[-1]) {
      w <- a[i] / b[i - 1]
      b[i] <- b[i] - w * cc[i - 1]
      d[i] <- d[i] - w * d[i - 1]
    }
    sol <- numeric(k)
    sol[k] <- d[k] / b[k]
    for (i in rev(seq_len(k - 1))) sol[i] <- (d[i] - cc[i] * sol[i + 1]) / b[i]
    m <- c(0, sol, 0)
  }
  vapply(t, function(v) {
    if (v < x[1]) return(y[1] + ((y[2] - y[1]) / h[1] - h[1] * m[2] / 6) * (v - x[1]))
    if (v > x[n]) return(y[n] + ((y[n] - y[n - 1]) / h[n - 1] + h[n - 1] * m[n - 1] / 6) * (v - x[n]))
    i <- min(n - 1, max(which(x[-n] <= v)))
    A <- (x[i + 1] - v) / h[i]
    B <- (v - x[i]) / h[i]
    A * y[i] + B * y[i + 1] + ((A^3 - A) * m[i] + (B^3 - B) * m[i + 1]) * h[i]^2 / 6
  }, numeric(1))
}

#' Spatial interpolation extras
#'
#' \code{BicubicSpline}: tensor-product natural bicubic spline.
#' \code{BarrierIdw}: IDW with line barriers (visibility exclusion or detour
#' path distance). \code{StewartPotential}: population potential. Identical
#' to the Python arm \code{morie.fn.interpx}.
#'
#' @param x,y Grid coordinates (increasing).
#' @param z Values, one row per y and one column per x.
#' @param xout,yout Prediction coordinates.
#' @param coords Data or unit coordinates (two-column).
#' @param values Data values.
#' @param targets Prediction locations (two-column).
#' @param barriers List of segments, each a 2 by 2 matrix (rows are end points).
#' @param power IDW power.
#' @param method \code{"visibility"} or \code{"path"}.
#' @param masses Unit masses.
#' @param beta Distance exponent.
#' @param self_distance Distance used for the own mass (NULL excludes it).
#' @return A numeric vector.
#' @references de Boor, C. (1962). Bicubic spline interpolation. Journal of
#'   Mathematics and Physics 41, 212-218.
#'
#'   Stewart, J. Q. (1947). Empirical mathematical rules concerning the
#'   distribution and equilibrium of population. Geographical Review 37,
#'   461-485.
#' @examples
#' BicubicSpline(0:2, 0:1, rbind(c(0, 1, 4), c(1, 2, 5)), 1, 0.5)
#' StewartPotential(rbind(c(0, 0), c(3, 4)), c(100, 50))
#' @export
BicubicSpline <- function(x, y, z, xout, yout) {
  z <- as.matrix(z)
  vapply(seq_along(xout), function(k) {
    col <- vapply(seq_len(nrow(z)), function(j) .ix_natural(x, z[j, ], xout[k]), numeric(1))
    .ix_natural(y, col, yout[k])
  }, numeric(1))
}

.ix_orient <- function(p, q, r) (q[1] - p[1]) * (r[2] - p[2]) - (q[2] - p[2]) * (r[1] - p[1])

.ix_visible <- function(p, q, barriers) {
  for (s in barriers) {
    a <- s[1, ]
    b <- s[2, ]
    d1 <- .ix_orient(a, b, p)
    d2 <- .ix_orient(a, b, q)
    d3 <- .ix_orient(p, q, a)
    d4 <- .ix_orient(p, q, b)
    if (((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))) return(FALSE)
  }
  TRUE
}

#' @rdname BicubicSpline
#' @export
BarrierIdw <- function(coords, values, targets, barriers, power = 2, method = "visibility") {
  P <- as.matrix(coords)
  Tg <- as.matrix(targets)
  ends <- do.call(rbind, lapply(barriers, as.matrix))
  apply(Tg, 1, function(t) {
    if (method == "visibility") {
      d <- apply(P, 1, function(p) if (.ix_visible(t, p, barriers)) sqrt(sum((t - p)^2)) else Inf)
    } else if (method == "path") {
      nodes <- rbind(t, ends, P)
      k <- nrow(nodes)
      dist <- rep(Inf, k)
      dist[1] <- 0
      done <- rep(FALSE, k)
      repeat {
        cand <- which(!done & is.finite(dist))
        if (!length(cand)) break
        u <- cand[which.min(dist[cand])]
        done[u] <- TRUE
        for (v in seq_len(k)) {
          if (v != u && !done[v] && .ix_visible(nodes[u, ], nodes[v, ], barriers)) {
            nd <- dist[u] + sqrt(sum((nodes[u, ] - nodes[v, ])^2))
            if (nd < dist[v]) dist[v] <- nd
          }
        }
      }
      d <- dist[(1 + nrow(ends) + 1):k]
    } else {
      stop("method must be 'visibility' or 'path'")
    }
    if (any(d == 0)) return(values[which(d == 0)[1]])
    w <- ifelse(is.finite(d), d^-power, 0)
    if (sum(w) > 0) sum(w * values) / sum(w) else NaN
  })
}

#' @rdname BicubicSpline
#' @export
StewartPotential <- function(coords, masses, beta = 1, self_distance = NULL) {
  D <- as.matrix(stats::dist(coords))
  W <- D^-beta
  diag(W) <- if (is.null(self_distance)) 0 else self_distance^-beta
  as.vector(W %*% masses)
}
