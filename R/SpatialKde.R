#' Bivariate Gaussian kernel density of a point pattern
#'
#' Evaluates the kernel sum
#' \eqn{f(u, v) = (N h_x h_y)^{-1} \sum_k \phi((u - x_k) / h_x) \phi((v - y_k) / h_y)}
#' on a regular grid, with kernel standard deviations
#' from the normal-reference plug-in rule
#' \eqn{h = 1.06 \min(s, IQR / 1.34) N^{-1/5}} (method "nrd", equal to
#' \code{MASS::kde2d} with its default bandwidth) or Scott's rule
#' \eqn{h = s N^{-1/6}} (method "scott"). Identical to the Python arm
#' \code{morie.fn.ptkde.spatial_kde}.
#'
#' @param data Two-column matrix (or data frame) of point coordinates.
#' @param method "nrd" or "scott" ("default" means "nrd").
#' @param h Kernel standard deviations (scalar or pair); overrides the rule.
#' @param n Grid size per axis (scalar or pair).
#' @param lims Grid limits c(xmin, xmax, ymin, ymax); default the data range.
#' @return List: x, y (grid), z (density matrix, rows along x), bandwidth, n.
#' @references Silverman, B. W. (1986). Density Estimation for Statistics and
#'   Data Analysis. Chapman and Hall.
#'
#'   Scott, D. W. (1992). Multivariate Density Estimation. Wiley.
#'
#'   Venables, W. N. and Ripley, B. D. (2002). Modern Applied Statistics with
#'   S, 4th edn. Springer.
#' @examples
#' pts <- cbind(c(0.1, 0.4, 0.5, 0.9, 0.3), c(0.2, 0.9, 0.4, 0.7, 0.6))
#' SpatialKde(pts, n = 3)$z
#' @export
SpatialKde <- function(data, method = "nrd", h = NULL, n = 25, lims = NULL) {
  .morie_arg(data, "m")
  data <- as.matrix(data)
  x <- as.numeric(data[, 1])
  y <- as.numeric(data[, 2])
  N <- length(x)
  if (N < 2) stop("need at least 2 points")
  if (method == "default") method <- "nrd"
  if (!method %in% c("nrd", "scott")) stop("method must be 'nrd' or 'scott'")
  bw <- function(v) {
    s <- sqrt(sum((v - sum(v) / N)^2) / (N - 1))
    if (method == "scott") return(s * N^(-1 / 6))
    q <- stats::quantile(v, c(0.25, 0.75), names = FALSE)
    1.06 * min(s, (q[2] - q[1]) / 1.34) * N^(-0.2)
  }
  hh <- if (is.null(h)) c(bw(x), bw(y)) else rep(as.numeric(h), length.out = 2)
  if (!all(hh > 0)) stop("bandwidths must be positive")
  nn <- rep(as.integer(n), length.out = 2)
  lm <- if (is.null(lims)) c(range(x), range(y)) else as.numeric(lims)
  grid <- function(a, b, m) {
    if (m == 1) return(a)
    g <- a + (0:(m - 1)) * ((b - a) / (m - 1))
    g[m] <- b
    g
  }
  gx <- grid(lm[1], lm[2], nn[1])
  gy <- grid(lm[3], lm[4], nn[2])
  ax <- stats::dnorm(outer(gx, x, "-") / hh[1])
  ay <- stats::dnorm(outer(gy, y, "-") / hh[2])
  z <- tcrossprod(matrix(ax, ncol = N), matrix(ay, ncol = N)) / (N * hh[1] * hh[2])
  list(x = gx, y = gy, z = z, bandwidth = hh, n = N)
}
