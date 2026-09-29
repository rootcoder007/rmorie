.ppm_iso_weight <- function(x, y, rad, win) {
  if (rad <= 0) return(1)
  d <- c(x - win[1], win[2] - x, y - win[3], win[4] - y)
  a <- acos(pmin(d / rad, 1))
  outside <- 2 * sum(a)
  for (p in list(c(1, 3), c(1, 4), c(2, 3), c(2, 4))) {
    v <- a[p[1]] + a[p[2]] - pi / 2
    if (v > 0) outside <- outside - v
  }
  max(1 - outside / (2 * pi), 1e-12)
}

.ppm_rmax <- function(n, win) {
  area <- (win[2] - win[1]) * (win[4] - win[3])
  min(sqrt(1000 / (pi * n / area)), min(win[2] - win[1], win[4] - win[3]) / 4)
}

#' Berman-Diggle cross-validated kernel bandwidth
#'
#' Chooses the standard deviation of an isotropic Gaussian kernel intensity
#' estimate by minimising \eqn{M(r) = (1/\lambda - 2K(r))/(\pi r^2) +
#' J(r)/(\pi r^2)^2}, \eqn{J(r) = \int_0^{2r} \phi(t, r) dK(t)},
#' \eqn{\phi(t, r) = 2r^2(acos(y) - y\sqrt{1 - y^2})}, \eqn{y = t/(2r)},
#' with \eqn{\sigma = r/2} (Diggle 1985; Berman and Diggle 1989), as
#' \code{spatstat.explore::bw.diggle} for a rectangular window: isotropic K
#' with \eqn{\lambda^2 = n(n-1)/|W|^2} on \code{nr} radii up to
#' min(shortside/4, sqrt(1000/(pi lambda))) (or 4 hmax), J as the
#' left-point Stieltjes sum, M for r up to rmax/2, and the grid minimiser.
#'
#' @param points Two-column matrix of coordinates.
#' @param window c(xmin, xmax, ymin, ymax).
#' @param nr Number of radii.
#' @param hmax Largest bandwidth considered.
#' @return List with \code{sigma}, \code{h}, \code{criterion}, \code{J},
#'   \code{lambda_hat}, \code{at_boundary}.
#' @references Berman, M. and Diggle, P. (1989). Estimating weighted
#'   integrals of the second-order intensity of a spatial point process.
#'   Journal of the Royal Statistical Society B 51, 81-92.
#'
#'   Diggle, P. J. (1985). A kernel method for smoothing point process data.
#'   Applied Statistics 34, 138-147.
#' @examples
#' P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9,
#'   .95, .85))
#' BandwidthDiggle(P, c(0, 1, 0, 1))[c("sigma", "at_boundary")]
#' @export
BandwidthDiggle <- function(points, window, nr = 512L, hmax = NULL) {
  P <- as.matrix(points)
  n <- nrow(P)
  if (n < 2) stop("need at least two points")
  area <- (window[2] - window[1]) * (window[4] - window[3])
  lam <- n / area
  rmax <- if (is.null(hmax)) .ppm_rmax(n, window) else 4 * hmax
  r <- rmax * (0:(nr - 1)) / (nr - 1)
  D <- as.matrix(stats::dist(P))
  acc <- numeric(0)
  dd <- numeric(0)
  for (i in 1:n) for (j in 1:n) {
    if (i != j && D[i, j] <= rmax) {
      dd <- c(dd, D[i, j])
      acc <- c(acc, 1 / .ppm_iso_weight(P[i, 1], P[i, 2], D[i, j], window))
    }
  }
  o <- order(dd)
  cs <- cumsum(acc[o])
  K <- vapply(r, function(h) {
    k <- sum(dd[o] <= h)
    if (k == 0) 0 else cs[k]
  }, 0) * area / (n * (n - 1))
  dK <- diff(K)
  ok <- which(r <= r[nr] / 2)
  J <- numeric(length(ok))
  for (i in ok[-1]) {
    y <- r[seq_len(nr - 1)] / (2 * r[i])
    use <- seq_len(sum(cumprod(y < 1)))
    J[i] <- 2 * r[i]^2 * sum((acos(y[use]) - y[use] * sqrt(1 - y[use]^2)) * dK[use])
  }
  pr2 <- pi * r[ok]^2
  crit <- (1 / lam - 2 * K[ok]) / pr2 + J / pr2^2
  crit[pr2 == 0] <- NaN
  best <- which.min(crit)
  list(sigma = r[ok][best] / 2, h = r[ok] / 2, criterion = crit, J = J, lambda_hat = lam,
       at_boundary = best %in% c(2L, length(crit)))
}

.ppm_mark_smooth <- function(points, marks, window, f, ef, r, rmax, correction) {
  P <- as.matrix(points)
  m <- as.numeric(marks)
  n <- nrow(P)
  if (length(m) != n || n < 2) stop("points and marks must have the same length >= 2")
  if (!correction %in% c("iso", "trans")) stop("correction must be 'iso' or 'trans'")
  if (is.null(r)) {
    if (is.null(rmax)) rmax <- .ppm_rmax(n, window)
    r <- rmax * (0:512) / 512
  }
  area <- (window[2] - window[1]) * (window[4] - window[3])
  D <- as.matrix(stats::dist(P))
  d <- e <- ff <- numeric(0)
  for (i in 1:n) for (j in 1:n) {
    if (i != j && D[i, j] <= max(r)) {
      d <- c(d, D[i, j])
      e <- c(e, if (correction == "iso") {
        min(1 / .ppm_iso_weight(P[i, 1], P[i, 2], D[i, j], window), 100)
      } else {
        area / ((window[2] - window[1] - abs(P[i, 1] - P[j, 1])) * (window[4] - window[3] - abs(P[i, 2] - P[j, 2])))
      })
      ff <- c(ff, f(m[i], m[j]))
    }
  }
  bw <- stats::bw.nrd0(d)
  k1 <- stats::density(d, weights = e, bw = bw, from = min(r), to = max(r), n = length(r), subdensity = TRUE)$y
  kf <- stats::density(d, weights = ff * e, bw = bw, from = min(r), to = max(r), n = length(r), subdensity = TRUE)$y
  # the ratio is undefined where the smoothed pair density is numerically zero
  list(r = r, est = ifelse(k1 > 1e-6 * max(k1), kf / (ef * k1), NaN), bw = bw)
}

#' Mark correlation function
#'
#' Stoyan's \eqn{k_{mm}(r) = E(m_i m_j | d_{ij} = r) / E(m)^2}, estimated as
#' \code{spatstat.explore::markcorr} with \code{method = "density"}: Gaussian
#' kernel smoothing (\code{stats::density}, \code{bw.nrd0} bandwidth of the
#' pair distances) of the pair contributions \eqn{m_i m_j e_{ij}} over the
#' weights \eqn{e_{ij}}, with Ripley's isotropic (capped at 100) or the
#' translation edge weights. Where the smoothed pair density is below 1e-6 of
#' its maximum the ratio is undefined and returned as NaN.
#'
#' @param points Two-column matrix of coordinates.
#' @param marks Non-negative numeric marks.
#' @param window c(xmin, xmax, ymin, ymax).
#' @param r Distances (default 513 values up to the Kest rmax rule).
#' @param rmax Largest distance for the default r.
#' @param correction \code{"iso"} or \code{"trans"}.
#' @param normalise Divide by \eqn{E(m)^2}.
#' @return List with \code{r}, \code{k}, \code{bw}, \code{theo}.
#' @references Stoyan, D. and Stoyan, H. (1994). Fractals, Random Shapes and
#'   Point Fields. Wiley, Chichester.
#' @examples
#' P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9,
#'   .95, .85))
#' MarkCorrelation(P, c(1, 2, 1.5, 3, .5, 2.5, 1, 2, .8, 1.2), c(0, 1, 0, 1))$k[257]
#' @export
MarkCorrelation <- function(points, marks, window, r = NULL, rmax = NULL, correction = "iso", normalise = TRUE) {
  m <- as.numeric(marks)
  if (normalise && any(m < 0)) stop("negative marks are not permitted when normalise = TRUE")
  ef <- mean(m)^2
  s <- .ppm_mark_smooth(points, m, window, function(a, b) a * b, if (normalise) ef else 1, r, rmax, correction)
  list(r = s$r, k = s$est, bw = s$bw, theo = if (normalise) 1 else ef)
}

#' Mark variogram
#'
#' \eqn{\gamma(r) = E((m_i - m_j)^2/2 | d_{ij} = r)}, as
#' \code{spatstat.explore::markvario}: the smoother of
#' \code{MarkCorrelation} with \eqn{f = (m_1 - m_2)^2/2} and no
#' normalisation; under independent marks it equals the mark variance.
#'
#' @inheritParams MarkCorrelation
#' @return List with \code{r}, \code{gamma}, \code{bw}, \code{theo}.
#' @examples
#' P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9,
#'   .95, .85))
#' MarkVariogram(P, c(1, 2, 1.5, 3, .5, 2.5, 1, 2, .8, 1.2), c(0, 1, 0, 1))$gamma[257]
#' @export
MarkVariogram <- function(points, marks, window, r = NULL, rmax = NULL, correction = "iso") {
  m <- as.numeric(marks)
  s <- .ppm_mark_smooth(points, m, window, function(a, b) 0.5 * (a - b)^2, 1, r, rmax, correction)
  list(r = s$r, gamma = s$est, bw = s$bw, theo = stats::var(m))
}

#' Random-labelling test of mark dependence
#'
#' Compares the mark correlation of the data with those of \code{nsim}
#' random permutations of the marks (Diggle 2003; \code{mad.test} and
#' \code{dclf.test} with \code{simulate = rlabel} in spatstat). The
#' discrepancy is the largest (\code{"mad"}) or integrated squared
#' (\code{"dclf"}) deviation of \eqn{k_{mm}} from 1 over rmin <= r <= rmax (the integral
#' is (rmax - rmin) times the mean squared deviation, as spatstat);
#' p-value (1 + number of simulated T >= observed T) / (nsim + 1). Permutations are
#' Fisher-Yates shuffles from Philox stream s of \code{seed}, identical to
#' the Python arm.
#'
#' @inheritParams MarkCorrelation
#' @param nsim Number of relabellings.
#' @param seed Philox seed.
#' @param statistic \code{"mad"} or \code{"dclf"}.
#' @param rmin Smallest distance in the discrepancy.
#' @return List with \code{statistic}, \code{p_value}, \code{simulated},
#'   \code{nsim}, \code{r}, \code{k}, \code{method}.
#' @references Diggle, P. J. (1986). Displaced amacrine cells in the retina
#'   of a rabbit: analysis of a bivariate spatial point pattern. Journal of
#'   Neuroscience Methods 18, 115-125.
#'
#'   Diggle, P. J. (2003). Statistical Analysis of Spatial Point Patterns,
#'   2nd edn. Arnold, London.
#' @examples
#' P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9,
#'   .95, .85))
#' MarkDependenceTest(P, c(1, 2, 1.5, 3, .5, 2.5, 1, 2, .8, 1.2), c(0, 1, 0, 1), nsim = 19)$p_value
#' @export
MarkDependenceTest <- function(points, marks, window, nsim = 99L, seed = 1L, statistic = "mad", rmin = 0,
                               rmax = NULL, correction = "iso") {
  if (!statistic %in% c("mad", "dclf")) stop("statistic must be 'mad' or 'dclf'")
  m <- as.numeric(marks)
  n <- length(m)
  base <- MarkCorrelation(points, m, window, rmax = rmax, correction = correction)
  r <- base$r
  keep <- which(r >= rmin & !is.nan(base$k))
  if (!length(keep)) stop("no distances in [rmin, rmax] with a defined k_mm")
  span <- r[length(r)] - rmin
  disc <- function(k) {
    dev <- k[keep] - 1
    dev <- dev[!is.nan(dev)]
    if (statistic == "mad") max(abs(dev)) else span * mean(dev^2)
  }
  obs <- disc(base$k)
  sims <- numeric(nsim)
  for (s in seq_len(nsim)) {
    u <- .morie_random_uniform(n, seed = seed, stream = s - 1L)
    p <- m
    for (i in seq(n, 2)) {
      j <- min(floor(u[i] * i), i - 1) + 1
      tmp <- p[i]
      p[i] <- p[j]
      p[j] <- tmp
    }
    sims[s] <- disc(MarkCorrelation(points, p, window, r = r, correction = correction)$k)
  }
  list(statistic = obs, p_value = (1 + sum(sims >= obs)) / (nsim + 1), simulated = sims, nsim = nsim,
       r = r, k = base$k, method = statistic)
}
