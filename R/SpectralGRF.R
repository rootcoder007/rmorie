.sgrf_corr <- function(h, model, nu) {
  out <- switch(model,
    exponential = exp(-h),
    gaussian = exp(-h^2),
    spherical = ifelse(h < 1, 1 - 1.5 * h + 0.5 * h^3, 0),
    matern = {
      v <- 2^(1 - nu) / gamma(nu) * h^nu * besselK(pmax(h, 1e-300), nu)
      v
    }
  )
  out[h <= 0] <- 1
  out
}

#' Gaussian random field simulation by circulant embedding
#'
#' The covariance \eqn{C(h) = sill\,\rho(|Ah|/range)} (plus a nugget) on a
#' regular grid is embedded in a torus of g times the grid size whose base
#' block holds C at the wrapped lags; its eigenvalues are the 2-D FFT of that
#' block (Wood and Chan 1994; Dietrich and Newsam 1997). The real part of
#' \eqn{FFT(\sqrt{S/N}(e_1 + i e_2))} then has covariance exactly C on the
#' grid. g doubles (2, 4, 8) until no eigenvalue is below -1e-8 of the
#' largest; otherwise negatives are clipped and \code{embedding_exact} is
#' FALSE. Models: exponential, gaussian, spherical and matern (geoR / fields
#' parameterisation); geometric anisotropy by \code{angle} (radians, major
#' axis) and \code{ratio} (minor/major). Normals are Philox streams 2k and
#' 2k + 1 of \code{seed} (row-major over the torus; nugget stream
#' 2 n_sims + k), identical to the Python arm.
#'
#' @param coords Two-column matrix of regular grid points.
#' @param cov_model \code{"exponential"}, \code{"gaussian"},
#'   \code{"matern"} or \code{"spherical"}.
#' @param cov_params List with \code{sill}, \code{range}, \code{nugget},
#'   \code{nu}, \code{angle}, \code{ratio}.
#' @param n_sims Number of realisations.
#' @param seed Philox seed.
#' @return List with \code{simulations} (n_sims x n), \code{eigenvalues},
#'   \code{torus}, \code{embedding_exact}, \code{min_eigenvalue_ratio}.
#' @references Wood, A. T. A. and Chan, G. (1994). Simulation of stationary
#'   Gaussian processes in the unit cube. Journal of Computational and
#'   Graphical Statistics 3, 409-432.
#'
#'   Dietrich, C. R. and Newsam, G. N. (1997). Fast and exact simulation of
#'   stationary Gaussian processes through circulant embedding of the
#'   covariance matrix. SIAM Journal on Scientific Computing 18, 1088-1107.
#' @examples
#' g <- as.matrix(expand.grid(y = 0:2 * 0.5, x = 0:3 * 0.5)[, 2:1])
#' SpectralGRF(g, "matern", list(range = 1, nu = 1.5), seed = 1)$torus
#' @export
SpectralGRF <- function(coords, cov_model = "exponential", cov_params = list(), n_sims = 1L, seed = 42L) {
  P <- as.matrix(coords)
  n <- nrow(P)
  prm <- utils::modifyList(list(sill = 1, range = 1, nugget = 0, nu = 0.5, angle = 0, ratio = 1), cov_params)
  if (!cov_model %in% c("exponential", "gaussian", "matern", "spherical")) stop("unknown cov_model")
  if (prm$range <= 0 || prm$sill < 0 || prm$nugget < 0 || prm$nu <= 0 || prm$ratio <= 0 || prm$ratio > 1) {
    stop("need range > 0, sill >= 0, nugget >= 0, nu > 0, 0 < ratio <= 1")
  }
  ux <- sort(unique(round(P[, 1], 8)))
  uy <- sort(unique(round(P[, 2], 8)))
  nx <- length(ux)
  ny <- length(uy)
  stx <- if (nx > 1) ux[2] - ux[1] else 1
  sty <- if (ny > 1) uy[2] - uy[1] else 1
  if ((nx > 2 && any(abs(diff(ux) - stx) > 1e-6 * max(1, abs(stx)))) ||
      (ny > 2 && any(abs(diff(uy) - sty) > 1e-6 * max(1, abs(sty))))) stop("coords must lie on a regular grid")
  ix <- round((round(P[, 1], 8) - ux[1]) / stx)
  iy <- round((round(P[, 2], 8) - uy[1]) / sty)
  ca <- cos(prm$angle)
  sa <- sin(prm$angle)
  for (grow in c(2, 4, 8)) {
    Nx <- grow * nx
    Ny <- grow * ny
    li <- 0:(Nx - 1)
    li <- ifelse(li <= Nx %/% 2, li, li - Nx) * stx
    lj <- 0:(Ny - 1)
    lj <- ifelse(lj <= Ny %/% 2, lj, lj - Ny) * sty
    dx <- outer(li, rep(1, Ny))
    dy <- outer(rep(1, Nx), lj)
    u <- ca * dx + sa * dy
    v <- (-sa * dx + ca * dy) / prm$ratio
    C <- matrix(prm$sill * .sgrf_corr(sqrt(u^2 + v^2) / prm$range, cov_model, prm$nu), Nx, Ny)
    S <- Re(stats::fft(C))
    exact <- min(S) >= -1e-8 * max(S)
    if (exact) break
  }
  N <- Nx * Ny
  amp <- sqrt(pmax(S, 0) / N)
  sims <- matrix(0, n_sims, n)
  for (k in seq_len(n_sims)) {
    e1 <- matrix(.morie_random_normal(N, seed = seed, stream = 2 * (k - 1)), Nx, Ny, byrow = TRUE)
    e2 <- matrix(.morie_random_normal(N, seed = seed, stream = 2 * (k - 1) + 1), Nx, Ny, byrow = TRUE)
    f <- Re(stats::fft(amp * complex(real = e1, imaginary = e2)))
    vals <- f[cbind(ix + 1, iy + 1)]
    if (prm$nugget > 0) vals <- vals + sqrt(prm$nugget) * .morie_random_normal(n, seed = seed, stream = 2 * n_sims + k - 1)
    sims[k, ] <- vals
  }
  list(simulations = sims, eigenvalues = S, torus = c(Nx, Ny), embedding_exact = exact,
       min_eigenvalue_ratio = min(S) / max(S))
}
