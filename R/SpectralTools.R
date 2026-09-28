.spt_freqs <- function(n, d) {
  k <- 0:(n - 1)
  ifelse(k <= (n - 1) %/% 2, k, k - n) / (n * d)
}

#' Gaussian field with a prescribed power spectrum by FFT filtering
#'
#' White noise on the nx x ny grid (Philox stream 0 of \code{seed},
#' row-major) is transformed, multiplied by the amplitude filter
#' \eqn{H(k) = \sqrt{P(|k|)}} and transformed back; the real part is the
#' field. The default spectrum is the power law \eqn{P(|k|) = |k|^{-\beta}}
#' (Peitgen and Saupe 1988) with the zero frequency removed; any
#' \code{spectrum(k)} of the radial frequency designs another filter.
#' Identical to the Python arm.
#'
#' @param nx,ny Grid size.
#' @param beta Power-law exponent (ignored when \code{spectrum} is given).
#' @param dx,dy Grid spacing.
#' @param spectrum Optional function of the radial frequency.
#' @param seed Philox seed.
#' @return List with \code{field} (nx x ny matrix), \code{filter}, \code{kx},
#'   \code{ky}.
#' @references Peitgen, H.-O. and Saupe, D. (eds) (1988). The Science of
#'   Fractal Images. Springer, New York.
#' @examples
#' sum(PowerLawField(8, 8, beta = 2, seed = 3)$field)
#' @export
PowerLawField <- function(nx, ny, beta = 2, dx = 1, dy = 1, spectrum = NULL, seed = 1L) {
  kx <- .spt_freqs(nx, dx)
  ky <- .spt_freqs(ny, dy)
  K <- sqrt(outer(kx^2, ky^2, "+"))
  P <- if (is.null(spectrum)) ifelse(K == 0, 0, K^(-beta)) else matrix(vapply(K, spectrum, 0), nx, ny)
  H <- sqrt(pmax(P, 0))
  w <- matrix(.morie_random_normal(nx * ny, seed = seed, stream = 0), nx, ny, byrow = TRUE)
  f <- Re(stats::fft(stats::fft(w) * H, inverse = TRUE)) / (nx * ny)
  list(field = f, filter = H, kx = kx, ky = ky)
}

#' Map values onto a target marginal distribution by ranks
#'
#' The value of rank r (ties broken by position) among n becomes the type-7
#' sample quantile of \code{target} at probability (r - 1)/(n - 1); with as
#' many targets as values this is the sorted target in the rank order of the
#' values (Journel and Deutsch 1993).
#'
#' @param values Numeric vector.
#' @param target Sample of the target distribution.
#' @return List with \code{transformed} and \code{ranks}.
#' @references Journel, A. G. and Deutsch, C. V. (1993). Entropy and spatial
#'   disorder. Mathematical Geology 25, 329-355.
#' @examples
#' HistogramTransform(c(.3, -1.2, .8, .1), c(10, 40, 20, 30))$transformed
#' @export
HistogramTransform <- function(values, target) {
  v <- as.numeric(values)
  n <- length(v)
  if (n < 1 || length(target) < 1) stop("values and target must be non-empty")
  r <- rank(v, ties.method = "first")
  p <- if (n > 1) (r - 1) / (n - 1) else 0.5
  list(transformed = unname(stats::quantile(as.numeric(target), p, type = 7)), ranks = as.integer(r))
}

#' Two Gaussian fields with a constant spectral coherence
#'
#' \eqn{Z_1} and an independent \eqn{Z_2} come from \code{SpectralGRF}
#' (seeds \code{seed} and \code{seed + 1});
#' \eqn{Y = \gamma Z_1 + \sqrt{1 - \gamma^2} Z_2} has the covariance of
#' \eqn{Z_1} and cross-covariance \eqn{\gamma C(h)} (intrinsic
#' coregionalisation; Wackernagel 2003).
#'
#' @inheritParams SpectralGRF
#' @param coherence Coherence \eqn{\gamma} between -1 and 1.
#' @return List with \code{first}, \code{second}, \code{coherence}.
#' @references Wackernagel, H. (2003). Multivariate Geostatistics, 3rd edn.
#'   Springer, Berlin.
#' @examples
#' g <- as.matrix(expand.grid(0:2, 0:2))
#' r <- CoherentFields(g, coherence = 1, seed = 2)
#' all.equal(r$first, r$second)
#' @export
CoherentFields <- function(coords, cov_model = "exponential", cov_params = list(), coherence = 0.5,
                           n_sims = 1L, seed = 1L) {
  if (abs(coherence) > 1) stop("coherence must lie in [-1, 1]")
  z1 <- SpectralGRF(coords, cov_model, cov_params, n_sims = n_sims, seed = seed)$simulations
  z2 <- SpectralGRF(coords, cov_model, cov_params, n_sims = n_sims, seed = seed + 1L)$simulations
  y <- coherence * z1 + sqrt(max(0, 1 - coherence^2)) * z2
  list(first = if (n_sims == 1) as.vector(z1) else z1, second = if (n_sims == 1) as.vector(y) else y,
       coherence = coherence)
}
