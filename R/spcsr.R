# SPDX-License-Identifier: AGPL-3.0-or-later
#' Complete spatial randomness, and a quadrat check of it
#'
#' CSR is the homogeneous Poisson process: constant intensity and
#' independent events, so neither attraction nor inhibition. It is the
#' null every point-pattern test in Ch 3 is stated against.
#'
#' Two consequences are reported, both of which a departure breaks. The
#' quadrat counts have variance equal to their mean, so the index of
#' dispersion s^2 / xbar is about 1; clustering pushes it ABOVE 1 and
#' regularity below. And the mean nearest-neighbour distance is
#' 1 / (2 sqrt(lambda)), so the Clark-Evans ratio of observed to expected
#' is about 1 under CSR, below 1 when clustered and above when regular.
#'
#' @param points Event coordinates (n by 2).
#' @param region c(xmin, ymin, xmax, ymax) or vertices; bounding box of
#'   `points` when NULL.
#' @return Named list: index_of_dispersion, quadrat_counts, n_quadrats,
#'   mean_nn, expected_nn, clark_evans, lambda_est, area.
#' @references Schabenberger & Gotway (2005), Ch 3, Secs 3.2-3.3.
#' @examples
#' spcsr(matrix(runif(400), 200, 2) * 10, region = c(0, 0, 10, 10))$clark_evans
#' @export
spcsr <- function(points, region = NULL) {
  p <- as.matrix(points)
  reg <- .sp_region(region, p)
  area <- (reg[3] - reg[1]) * (reg[4] - reg[2])
  lam <- nrow(p) / area
  k <- max(2, as.integer(sqrt(nrow(p) / 5)))
  xe <- seq(reg[1], reg[3], length.out = k + 1)
  ye <- seq(reg[2], reg[4], length.out = k + 1)
  xi <- pmin(pmax(findInterval(p[, 1], xe, rightmost.closed = TRUE), 1), k)
  yi <- pmin(pmax(findInterval(p[, 2], ye, rightmost.closed = TRUE), 1), k)
  counts <- as.numeric(table(factor((xi - 1) * k + yi, levels = seq_len(k * k))))
  mean_c <- mean(counts)
  iod <- if (mean_c > 0) stats::var(counts) / mean_c else NA_real_
  nn <- .sp_nn(p)
  mean_nn <- mean(nn)
  expected_nn <- if (lam > 0) 1 / (2 * sqrt(lam)) else NA_real_
  list(index_of_dispersion = iod, quadrat_counts = counts,
       n_quadrats = length(counts), mean_nn = mean_nn,
       expected_nn = expected_nn, clark_evans = mean_nn / expected_nn,
       lambda_est = lam, area = area)
}
