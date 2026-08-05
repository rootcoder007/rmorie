# SPDX-License-Identifier: AGPL-3.0-or-later

#' G function -- nearest-neighbour event-to-event distance distribution.
#'
#' \code{G(y) = P(nearest-neighbour distance <= y)}. With \code{h_i} the
#' distance from event \code{i} to the nearest other event, the raw
#' empirical estimate is \code{Ghat(y) = #(h_i <= y) / n}
#' (Schabenberger & Gotway 2005, Sec. 3.3.4, p. 98).
#'
#' That estimator is biased downwards near the boundary, because an
#' event's true nearest neighbour may lie outside the window. The
#' reduced-sample (border) correction restricts to events whose distance
#' \code{b_i} to the boundary already exceeds \code{y}, for which the
#' observed nearest neighbour is provably the true one:
#' \code{Ghat_b(y) = #{i: h_i <= y and b_i > y} / #{i: b_i > y}}.
#'
#' Under complete spatial randomness with intensity \code{lambda},
#' \code{G(y) = 1 - exp(-lambda pi y^2)}, returned as \code{csr}.
#'
#' @param points Event coordinates, an n-by-2 matrix.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param r Distances at which to evaluate G.
#' @return Named list: r, g, g_border, nn, csr, mean_nn, lambda_hat, n, method.
#' @references Schabenberger, O. and Gotway, C. A. (2005). Statistical
#'   Methods for Spatial Data Analysis, Sec. 3.3.4, p. 98.
#'   Ripley, B. D. (1976). Journal of Applied Probability 13(2), 255-266.
#'   \doi{10.2307/3212829}.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
RipG <- function(points, window, r) {
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

  px <- as.numeric(P[, 1]); py <- as.numeric(P[, 2])
  if (any(px < x0 | px > x1 | py < y0 | py > y1))
    stop("every point must lie inside `window`")

  nn <- numeric(n)
  for (i in seq_len(n)) {
    best <- Inf
    for (j in seq_len(n)) {
      if (i == j) next
      dij <- sqrt((px[i] - px[j])^2 + (py[i] - py[j])^2)
      if (dij < best) best <- dij
    }
    nn[i] <- best
  }
  bdist <- pmin(px - x0, x1 - px, py - y0, y1 - py)

  lam <- n / ((x1 - x0) * (y1 - y0))
  nr <- length(rs)
  g <- numeric(nr); gb <- numeric(nr); csr <- numeric(nr)
  for (t in seq_len(nr)) {
    h <- rs[t]
    g[t] <- sum(nn <= h) / n
    m <- sum(bdist > h)
    gb[t] <- if (m > 0L) sum(bdist > h & nn <= h) / m else NaN
    csr[t] <- 1 - exp(-lam * pi * h * h)
  }

  list(
    r = rs, g = g, g_border = gb, nn = nn, csr = csr,
    mean_nn = sum(nn) / n, lambda_hat = lam, n = n,
    method = "G function (nearest-neighbour distances, border corrected)"
  )
}

#' @rdname RipG
#' @keywords internal
#' @export
morie_ripley_g_function <- RipG
