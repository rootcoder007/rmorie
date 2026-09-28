.tb_radial_3d_exp <- function(u, a) {
  Fr <- function(r) (2 / pi) * (atan(r) - r / (1 + r^2))
  vapply(u, function(v) {
    lo <- 0
    hi <- 1
    while (Fr(hi) < v) hi <- hi * 2
    for (it in 1:200) {
      mid <- 0.5 * (lo + hi)
      if (Fr(mid) < v) lo <- mid else hi <- mid
    }
    0.5 * (lo + hi) / a
  }, 0)
}

#' Turning bands simulation of a Gaussian random field
#'
#' \eqn{\sqrt{sill/L}\sum_l z_l(\langle x, u_l\rangle)} over L bands with
#' spectral line processes \eqn{z_l(t) = \sqrt{2/M}\sum_m \cos(r_{lm} t +
#' \phi_{lm})}, frequencies from the radial spectral law of the covariance
#' (Matheron 1973; Mantoglou and Wilson 1982; Emery and Lantuejoul 2006).
#' 2-D: directions \eqn{\pi(l + U)/L}, models exponential, gaussian,
#' matern, geometric anisotropy; 3-D: Fibonacci directions under a uniform
#' random rotation (Shoemake 1992 quaternion), gaussian and exponential. Philox streams: 0 offset, then 3l + 1 radii, 3l + 2 phases,
#' 3l + 3 extra normals; identical to the Python arm.
#'
#' @param coords Two- or three-column matrix of locations.
#' @param cov_model \code{"exponential"}, \code{"gaussian"} or
#'   \code{"matern"} (2-D).
#' @param sill Variance.
#' @param range_ Range.
#' @param nu Matern smoothness.
#' @param n_bands Number of bands.
#' @param n_waves Waves per band.
#' @param angle Anisotropy angle (2-D).
#' @param ratio Anisotropy ratio (2-D).
#' @param seed Philox seed.
#' @return List with \code{field} and \code{directions}.
#' @references Matheron, G. (1973). The intrinsic random functions and their
#'   applications. Advances in Applied Probability 5, 439-468.
#'
#'   Mantoglou, A. and Wilson, J. L. (1982). The turning bands method for
#'   simulation of random fields using line generation by a spectral method.
#'   Water Resources Research 18, 1379-1394.
#'
#'   Emery, X. and Lantuejoul, C. (2006). TBSIM: a computer program for
#'   conditional simulation of three-dimensional Gaussian random fields via
#'   the turning bands method. Computers and Geosciences 32, 1615-1628.
#'
#'   Shoemake, K. (1992). Uniform random rotations. Graphics Gems III,
#'   124-132.
#' @examples
#' TurningBands(rbind(c(0, 0), c(1, .5)), "gaussian", n_bands = 4, n_waves = 3, seed = 2)$field
#' @export
TurningBands <- function(coords, cov_model = "exponential", sill = 1, range_ = 1, nu = 0.5, n_bands = 64L,
                         n_waves = 50L, angle = 0, ratio = 1, seed = 1L) {
  X <- as.matrix(coords)
  d <- ncol(X)
  if (!d %in% 2:3) stop("coords must be 2-D or 3-D")
  if (!cov_model %in% c("exponential", "gaussian", "matern") || (d == 3 && cov_model == "matern")) {
    stop("cov_model must be exponential, gaussian or (2-D only) matern")
  }
  if (range_ <= 0 || sill < 0 || nu <= 0 || ratio <= 0 || ratio > 1 || n_bands < 1 || n_waves < 1) {
    stop("need range_ > 0, sill >= 0, nu > 0, 0 < ratio <= 1 and positive counts")
  }
  if (d == 2 && (angle != 0 || ratio != 1)) {
    X <- cbind(cos(angle) * X[, 1] + sin(angle) * X[, 2], (-sin(angle) * X[, 1] + cos(angle) * X[, 2]) / ratio)
  }
  L <- as.integer(n_bands)
  M <- as.integer(n_waves)
  if (d == 2) {
    off <- .morie_random_uniform(1, seed = seed, stream = 0)
    th <- pi * (0:(L - 1) + off) / L
    dirs <- cbind(cos(th), sin(th))
  } else {
    u <- .morie_random_uniform(3, seed = seed, stream = 0)
    q <- c(sqrt(1 - u[1]) * sin(2 * pi * u[2]), sqrt(1 - u[1]) * cos(2 * pi * u[2]),
           sqrt(u[1]) * sin(2 * pi * u[3]), sqrt(u[1]) * cos(2 * pi * u[3]))
    Rm <- rbind(
      c(1 - 2 * (q[2]^2 + q[3]^2), 2 * (q[1] * q[2] - q[3] * q[4]), 2 * (q[1] * q[3] + q[2] * q[4])),
      c(2 * (q[1] * q[2] + q[3] * q[4]), 1 - 2 * (q[1]^2 + q[3]^2), 2 * (q[2] * q[3] - q[1] * q[4])),
      c(2 * (q[1] * q[3] - q[2] * q[4]), 2 * (q[2] * q[3] + q[1] * q[4]), 1 - 2 * (q[1]^2 + q[2]^2))
    )
    g <- pi * (3 - sqrt(5))
    z <- 1 - ((0:(L - 1)) + 0.5) / L
    rr <- sqrt(pmax(0, 1 - z^2))
    dirs <- cbind(rr * cos(g * 0:(L - 1)), rr * sin(g * 0:(L - 1)), z) %*% t(Rm)
  }
  field <- numeric(nrow(X))
  for (k in seq_len(L)) {
    ur <- .morie_random_uniform(M, seed = seed, stream = 3 * (k - 1) + 1)
    up <- .morie_random_uniform(M, seed = seed, stream = 3 * (k - 1) + 2)
    rad <- if (d == 2) {
      if (cov_model == "gaussian") 2 / range_ * sqrt(-log(ur)) else {
        v <- if (cov_model == "exponential") 0.5 else nu
        sqrt(ur^(-1 / v) - 1) / range_
      }
    } else if (cov_model == "gaussian") {
      ue <- .morie_random_uniform(2 * M, seed = seed, stream = 3 * (k - 1) + 3)
      z3 <- cbind(.morie_normal_quantile(ur), .morie_normal_quantile(ue[seq(1, 2 * M, 2)]),
                  .morie_normal_quantile(ue[seq(2, 2 * M, 2)]))
      sqrt(2) / range_ * sqrt(rowSums(z3^2))
    } else {
      .tb_radial_3d_exp(ur, range_)
    }
    tproj <- as.vector(X %*% dirs[k, ])
    field <- field + sqrt(2 / M) * rowSums(cos(outer(tproj, rad) + matrix(2 * pi * up, length(tproj), M, byrow = TRUE)))
  }
  list(field = sqrt(sill / L) * field, directions = dirs)
}
