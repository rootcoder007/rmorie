# SPDX-License-Identifier: AGPL-3.0-or-later

#' F function -- empty-space (point-to-nearest-event) distance function
#'
#' \code{F(y) = P(distance from an arbitrary location to the nearest
#' event <= y)}. The location distribution is uniform over the window; a
#' \emph{deterministic} 20-by-20 lattice of cell centres is used as the
#' quadrature sample so that both language arms land on identical
#' numbers rather than merely the same distribution:
#' \code{u_ab = (x0 + (a - 0.5) dx, y0 + (b - 0.5) dy)},
#' \code{dx = (x1 - x0)/20}, \code{Fhat(y) = #(min_i ||u - s_i|| <= y)/m}
#' with \code{m = 400}.
#'
#' Reduced-sample (border) correction restricts to test locations whose
#' distance \code{b_u} to the boundary exceeds \code{y}:
#' \code{Fhat_b(y) = #{u: dist(u) <= y and b_u > y} / #{u: b_u > y}}.
#'
#' Under complete spatial randomness F and G coincide,
#' \code{F(y) = 1 - exp(-lambda pi y^2)}; F above and G below that curve
#' indicates regularity, the reverse indicates clustering.
#'
#' @param points Event coordinates, an n-by-2 matrix.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param r Distances at which to evaluate F.
#' @return Named list: r, f, f_border, csr, m, lambda_hat, n, method.
#' @references Schabenberger, O. and Gotway, C. A. (2005). Statistical
#'   Methods for Spatial Data Analysis, Sec. 3.3.4, pp. 97-98.
#'   Ripley, B. D. (1976). Journal of Applied Probability 13(2), 255-266.
#'   \doi{10.2307/3212829}.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
RipF <- function(points, window, r) {
  grid <- 20L
  P <- as.matrix(points)
  if (ncol(P) != 2L) stop("`points` must have shape (n, 2)")
  n <- nrow(P)
  if (n < 1L) stop("`points` needs at least 1 event")
  w <- as.numeric(window)
  if (length(w) != 4L) stop("`window` must be (xmin, xmax, ymin, ymax)")
  x0 <- w[1]; x1 <- w[2]; y0 <- w[3]; y1 <- w[4]
  if (!(x1 > x0 && y1 > y0))
    stop("`window` must have xmax > xmin and ymax > ymin")
  rs <- as.numeric(r)
  if (min(rs) < 0) stop("`r` must be non-negative")

  px <- as.numeric(P[, 1]); py <- as.numeric(P[, 2])
  if (any(px < x0 | px > x1 | py < y0 | py > y1))
    stop("every point must lie inside `window`")

  dx <- (x1 - x0) / grid
  dy <- (y1 - y0) / grid
  m <- grid * grid
  dmin <- numeric(m); bmin <- numeric(m)
  idx <- 0L
  for (a in seq_len(grid)) {
    ux <- x0 + (a - 0.5) * dx
    for (b in seq_len(grid)) {
      uy <- y0 + (b - 0.5) * dy
      idx <- idx + 1L
      best <- Inf
      for (i in seq_len(n)) {
        dd <- sqrt((ux - px[i])^2 + (uy - py[i])^2)
        if (dd < best) best <- dd
      }
      dmin[idx] <- best
      bmin[idx] <- min(ux - x0, x1 - ux, uy - y0, y1 - uy)
    }
  }

  lam <- n / ((x1 - x0) * (y1 - y0))
  nr <- length(rs)
  f <- numeric(nr); fb <- numeric(nr); csr <- numeric(nr)
  for (t in seq_len(nr)) {
    h <- rs[t]
    f[t] <- sum(dmin <= h) / m
    mm <- sum(bmin > h)
    fb[t] <- if (mm > 0L) sum(bmin > h & dmin <= h) / mm else NaN
    csr[t] <- 1 - exp(-lam * pi * h * h)
  }

  list(
    r = rs, f = f, f_border = fb, csr = csr, m = m,
    lambda_hat = lam, n = n,
    method = "F function (empty-space distances, 20x20 lattice)"
  )
}

#' @rdname RipF
#' @keywords internal
#' @export
morie_ripley_f_function <- RipF
