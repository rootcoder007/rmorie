# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: proportion of the circle of radius `rad` centred at (x, y)
# that lies inside the rectangle [x0, x1] x [y0, y1] -- Ripley's edge
# correction weight w(s_i, s_j) (Schabenberger & Gotway 2005, p. 102).
# Two opposite sides can never both be crossed by the same angle, so the
# only overlaps between the four "outside" arcs are the four corners and
# there are no triple intersections; inclusion-exclusion is exact.
#' Internal: proportion of the circle of radius `rad` centred at (x, y)
#'
#' that lies inside the rectangle \[x0, x1\] x \[y0, y1\] -- Ripley\'s edge
#' correction weight w(s_i, s_j) (Schabenberger & Gotway 2005, p. 102).
#' Two opposite sides can never both be crossed by the same angle, so
#' the only overlaps between the four "outside" arcs are the four
#' corners and there are no triple intersections; inclusion-exclusion is
#' exact.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param y Numeric; combined arithmetically in the body.
#' @param rad Numeric; combined arithmetically in the body.
#' @param x0 Numeric; combined arithmetically in the body.
#' @param x1 Numeric; combined arithmetically in the body.
#' @param y0 Numeric; combined arithmetically in the body.
#' @param y1 Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.ripk_weight <- function(x, y, rad, x0, x1, y0, y1) {
  if (rad <= 0) return(1)
  d <- c(x - x0, x1 - x, y - y0, y1 - y)
  if (min(d) < 0) stop("point lies outside `window`")
  a <- acos(pmin(d / rad, 1))
  outside <- 2 * sum(a)
  # (left, bottom), (left, top), (right, bottom), (right, top)
  for (p in list(c(1L, 3L), c(1L, 4L), c(2L, 3L), c(2L, 4L))) {
    v <- a[p[1]] + a[p[2]] - pi / 2
    if (v > 0) outside <- outside - v
  }
  w <- 1 - outside / (2 * pi)
  if (w > 1e-12) w else 1e-12
}

#' Ripley's K function for a mapped point pattern in a rectangle
#'
#' \code{lambda K(h)} is the expected number of further events within
#' distance \code{h} of an arbitrary event. Two estimators are returned.
#'
#' Isotropic (Ripley 1976; Schabenberger & Gotway 2005, p. 102):
#' \code{Ehat(h) = (1/n) sum_i sum_{j != i} w(s_i,s_j)^-1 I(h_ij <= h)},
#' \code{Khat(h) = lambdahat^-1 Ehat(h)} with \code{lambdahat = n/area}.
#'
#' Reduced-sample (border) correction, same section. The book prints the
#' numerator indicator as \code{d_j > h}; that is a misprint -- the
#' reduced sample conditions on the \emph{centre} event being at least
#' \code{h} from the boundary, so \code{d_i > h} is used here, which is
#' the standard and unbiased form.
#'
#' Under complete spatial randomness \code{K(h) = pi h^2}; the Besag
#' transform \code{L(h) = sqrt(K(h)/pi)} is returned alongside.
#'
#' @param points Event coordinates, an n-by-2 matrix, all inside window.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param r Distances at which to evaluate K.
#' @return Named list: r, k, k_border, l, csr, lambda_hat, area, n, method.
#' @references Ripley, B. D. (1976). The second-order analysis of
#'   stationary point processes. Journal of Applied Probability 13(2),
#'   255-266. \doi{10.2307/3212829}.
#'   Schabenberger, O. and Gotway, C. A. (2005). Statistical Methods for
#'   Spatial Data Analysis, Sec. 3.4.1-3.4.2, pp. 101-103.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
Ripk <- function(points, window, r) {
  P <- as.matrix(points)
  if (ncol(P) != 2L) stop("`points` must have shape (n, 2)")
  n <- nrow(P)
  if (n < 2L) stop("`points` needs at least 2 events")
  w <- as.numeric(window)
  if (length(w) != 4L) stop("`window` must be (xmin, xmax, ymin, ymax)")
  x0 <- w[1]; x1 <- w[2]; y0 <- w[3]; y1 <- w[4]
  if (!(x1 > x0 && y1 > y0))
    stop("`window` must have xmax > xmin and ymax > ymin")
  rs <- as.numeric(r)
  if (min(rs) < 0) stop("`r` must be non-negative")
  area <- (x1 - x0) * (y1 - y0)
  lam <- n / area

  px <- as.numeric(P[, 1]); py <- as.numeric(P[, 2])
  if (any(px < x0 | px > x1 | py < y0 | py > y1))
    stop("every point must lie inside `window`")
  bdist <- pmin(px - x0, x1 - px, py - y0, y1 - py)

  d <- matrix(0, n, n)
  wt <- matrix(1, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      dij <- sqrt((px[i] - px[j])^2 + (py[i] - py[j])^2)
      d[i, j] <- dij
      wt[i, j] <- .ripk_weight(px[i], py[i], dij, x0, x1, y0, y1)
    }
  }

  nr <- length(rs)
  kiso <- numeric(nr); kbor <- numeric(nr)
  lv <- numeric(nr); csr <- numeric(nr)
  for (t in seq_len(nr)) {
    h <- rs[t]
    acc <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i != j && d[i, j] <= h) acc <- acc + 1 / wt[i, j]
      }
    }
    kh <- area * acc / (n * n)
    kiso[t] <- kh
    lv[t] <- if (kh > 0) sqrt(kh / pi) else 0
    csr[t] <- pi * h * h
    m <- 0L; cnt <- 0
    for (i in seq_len(n)) {
      if (bdist[i] > h) {
        m <- m + 1L
        for (j in seq_len(n)) {
          if (i != j && d[i, j] <= h) cnt <- cnt + 1
        }
      }
    }
    kbor[t] <- if (m > 0L) cnt / (lam * m) else NaN
  }

  list(
    r = rs, k = kiso, k_border = kbor, l = lv, csr = csr,
    lambda_hat = lam, area = area, n = n,
    method = "Ripley's K function (isotropic + border correction)"
  )
}

#' @rdname Ripk
#' @keywords internal
#' @export
morie_ripley_k_function <- Ripk
