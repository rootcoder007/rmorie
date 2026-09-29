.gpx_score <- function(y, s, k) {
  n <- length(y)
  if (abs(k) < 1e-10) return(c(-n / s + sum(y) / s^2, sum(y^2) / (2 * s^2) - sum(y) / s))
  z <- 1 + k * y / s
  if (min(z) <= 0) stop("parameters outside the support")
  c(-n / s + (1 + 1 / k) * sum(k * y / s^2 / z), sum(log(z)) / k^2 - (1 + 1 / k) * sum(y / s / z))
}

.gpx_jac <- function(y, s, k) {
  n <- length(y)
  a <- 1 + 1 / k
  z <- 1 + k * y / s
  hss <- n / s^2 + a * sum(-2 * k * y / (s^3 * z) + k^2 * y^2 / (s^4 * z^2))
  hsk <- -sum(k * y / (s^2 * z)) / k^2 + a * sum(y / (s^2 * z) - k * y^2 / (s^3 * z^2))
  hkk <- 2 * sum(y / (s * z)) / k^2 - 2 * sum(log(z)) / k^3 + a * sum(y^2 / (s^2 * z^2))
  rbind(c(hss, hsk), c(hsk, hkk))
}

#' Generalized Pareto distribution and its threshold-exceedance fit
#'
#' R arm of \code{morie.fn.gpdD} and \code{gpfit}. \code{GpdDistribution}:
#' CDF, density, quantiles, mean and variance of the GPD
#' \eqn{F(x) = 1 - (1 + \xi x/\sigma)^{-1/\xi}}. \code{Gpfit}: peaks over
#' threshold, the maximum likelihood fit of the excesses over \code{threshold}
#' (default the type-7 90th percentile), started from
#' \code{morie_evt_gpd_mle} and refined by Newton steps on the analytic score and Hessian;
#' standard errors from the observed information.
#'
#' @param sigma Scale, positive.
#' @param xi Shape.
#' @param x Points (\code{GpdDistribution}) or raw observations (\code{Gpfit}).
#' @param p Probabilities.
#' @param threshold Threshold.
#' @return A list (the Python payload).
#' @references Pickands, J. (1975). Statistical inference using extreme order
#'   statistics. Annals of Statistics 3, 119-131.
#'
#'   Coles, S. (2001). An Introduction to Statistical Modeling of Extreme
#'   Values. Springer.
#' @examples
#' GpdDistribution(2, 0.25, x = c(1, 3), p = c(0.5, 0.9))$cdf
#' Gpfit(c(0.2, 1.4, 0.7, 2.9, 0.3, 5.1, 1.1, 0.9, 3.8, 0.5, 2.2, 7.4, 1.8, 0.6, 4.3), 0.5)$shape
#' @export
GpdDistribution <- function(sigma, xi, x = c(0.5, 1, 2, 4), p = c(0.5, 0.9, 0.95, 0.99)) {
  if (sigma <= 0) stop("sigma must be positive")
  if (any(p <= 0 | p >= 1)) stop("probabilities must lie in (0, 1)")
  eps <- 1e-12
  cdf <- vapply(x, function(v) {
    if (v <= 0) return(0)
    if (abs(xi) < eps) return(1 - exp(-v / sigma))
    z <- 1 + xi * v / sigma
    if (z <= 0) 1 else 1 - z^(-1 / xi)
  }, 0)
  pdf <- vapply(x, function(v) {
    if (v < 0) return(0)
    if (abs(xi) < eps) return(exp(-v / sigma) / sigma)
    z <- 1 + xi * v / sigma
    if (z <= 0) 0 else z^(-1 / xi - 1) / sigma
  }, 0)
  q <- if (abs(xi) < eps) -sigma * log(1 - p) else sigma * ((1 - p)^(-xi) - 1) / xi
  mn <- if (xi < 1) sigma / (1 - xi) else Inf
  list(estimate = mn, cdf = cdf, pdf = pdf, quantile = q, mean = mn,
       variance = if (xi < 0.5) sigma^2 / ((1 - xi)^2 * (1 - 2 * xi)) else Inf,
       upper_endpoint = if (xi >= 0) Inf else -sigma / xi, sigma = sigma, xi = xi, n = length(x))
}

#' @rdname GpdDistribution
#' @export
Gpfit <- function(x, threshold = NULL) {
  x <- as.numeric(x)
  if (length(x) < 5) return(list(estimate = NaN, n = length(x)))
  if (is.null(threshold)) threshold <- stats::quantile(x, 0.9, names = FALSE, type = 7)
  y <- x[x > threshold] - threshold
  n <- length(y)
  if (n < 5) return(list(estimate = NaN, n = n))
  f <- morie_evt_gpd_mle(y)
  s <- f$sigma
  k <- f$xi
  for (it in 1:50) {
    g <- .gpx_score(y, s, k)
    st <- solve(.gpx_jac(y, s, k), g)
    if (s - st[1] <= 0) break
    s <- s - st[1]
    k <- k - st[2]
    if (abs(st[1]) < 1e-15 * s && abs(st[2]) < 1e-15) break
  }
  cv <- solve(-.gpx_jac(y, s, k))
  ll <- -n * log(s) - (1 + 1 / k) * sum(log(1 + k * y / s))
  list(scale = s, shape = k, threshold = threshold, n_exceedances = n, se_sigma = sqrt(max(cv[1, 1], 0)),
       se_xi = sqrt(max(cv[2, 2], 0)), loglik = ll, estimate = s, se = sqrt(max(cv[1, 1], 0)))
}
