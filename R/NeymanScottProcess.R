.ns_h <- function(z) {
  ifelse(z >= 1, 1, 2 + (1 / pi) * ((8 * z^2 - 4) * acos(pmin(z, 1)) - 2 * asin(pmin(z, 1)) +
                                   4 * z * sqrt(pmax(1 - z^2, 0)^3) - 6 * z * sqrt(pmax(1 - z^2, 0))))
}

.ns_g <- function(z) ifelse(z >= 1, 0, (2 / pi) * (acos(pmin(z, 1)) - z * sqrt(pmax(1 - z^2, 0))))

.ns_chunks <- function(mean) max(1, ceiling(mean / 500))

.ns_poisson <- function(mean, us) {
  part <- mean / length(us)
  total <- 0L
  for (u in us) {
    k <- 0L
    p <- exp(-part)
    cc <- p
    while (u > cc && p > 0) {
      k <- k + 1L
      p <- p * part / k
      cc <- cc + p
    }
    total <- total + k
  }
  total
}

#' Neyman-Scott cluster process: moments and simulation
#'
#' Parents form a Poisson process of intensity \code{kappa}; each has a
#' Poisson(\code{mu}) number of offspring displaced by the kernel:
#' \code{"thomas"} (isotropic Gaussian, sd \code{scale}), \code{"cauchy"}
#' (bivariate Cauchy with scale \code{scale}) or \code{"matern"} (uniform in
#' a disc of radius \code{scale}). The intensity is \eqn{\kappa\mu}; the K
#' function and pair correlation are those of
#' \code{spatstat.random::spatstatClusterModelInfo}. With \code{simulate >
#' 0} and a rectangular \code{window} the process is simulated with parents
#' in the window dilated by the radius beyond which an offspring falls with
#' probability 1e-3 (Thomas, Matern exact) or 1e-2 (Cauchy), from the same
#' Philox streams as the Python arm.
#'
#' @param kappa Parent intensity.
#' @param mu Mean number of offspring per parent.
#' @param scale Kernel scale.
#' @param kernel \code{"thomas"}, \code{"cauchy"} or \code{"matern"}.
#' @param r Distances at which to evaluate K and g.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)} for simulation.
#' @param simulate Number of patterns to simulate.
#' @param seed Philox key.
#' @return List with \code{intensity}, \code{K}, \code{pcf}, \code{r},
#'   \code{simulated}.
#' @references Neyman, J. and Scott, E. L. (1958). Statistical approach to
#'   problems of cosmology. Journal of the Royal Statistical Society B 20,
#'   1-43.
#'
#'   Thomas, M. (1949). A generalization of Poisson's binomial limit for use
#'   in ecology. Biometrika 36, 18-25.
#'
#'   Ghorbani, M. (2013). Cauchy cluster process. Metrika 76, 697-706.
#' @examples
#' NeymanScottProcess(10, 5, 0.05, r = 0.1)$K
#' @export
NeymanScottProcess <- function(kappa, mu, scale, kernel = c("thomas", "cauchy", "matern"), r = NULL,
                               window = NULL, simulate = 0L, seed = 0) {
  kernel <- match.arg(kernel)
  s <- scale
  rs <- if (is.null(r)) numeric(0) else as.numeric(r)
  if (kernel == "thomas") {
    K <- pi * rs^2 + (1 - exp(-rs^2 / (4 * s^2))) / kappa
    g <- 1 + exp(-rs^2 / (4 * s^2)) / (4 * pi * kappa * s^2)
    reach <- s * sqrt(2 * log(1000))
  } else if (kernel == "cauchy") {
    e2 <- 4 * s^2
    K <- pi * rs^2 + (1 - 1 / sqrt(1 + rs^2 / e2)) / kappa
    g <- 1 + (1 + rs^2 / e2)^-1.5 / (2 * pi * e2 * kappa)
    reach <- s * sqrt(1e4 - 1)
  } else {
    K <- pi * rs^2 + .ns_h(rs / (2 * s)) / kappa
    g <- 1 + .ns_g(rs / (2 * s)) / (pi * kappa * s^2)
    reach <- s
  }
  sims <- list()
  if (simulate > 0) {
    w <- as.numeric(window)
    X0 <- w[1] - reach
    X1 <- w[2] + reach
    Y0 <- w[3] - reach
    Y1 <- w[4] + reach
    lam_p <- kappa * (X1 - X0) * (Y1 - Y0)
    cm <- .ns_chunks(mu)
    for (k in seq_len(simulate) - 1L) {
      npar <- .ns_poisson(lam_p, .morie_random_uniform(.ns_chunks(lam_p), seed = seed, stream = 4 * k))
      pat <- matrix(0, 0, 2)
      if (npar > 0) {
        pu <- .morie_random_uniform((2 + cm) * npar, seed = seed, stream = 4 * k + 1)
        counts <- vapply(seq_len(npar) - 1L, function(j) .ns_poisson(mu, pu[(2 + cm) * j + 2 + seq_len(cm)]), 0L)
        tot <- sum(counts)
        if (tot > 0) {
          q <- .morie_normal_quantile(.morie_random_uniform(3 * tot, seed = seed, stream = 4 * k + 2))
          v <- .morie_random_uniform(2 * tot, seed = seed, stream = 4 * k + 3)
          px <- rep(X0 + pu[(2 + cm) * (seq_len(npar) - 1) + 1] * (X1 - X0), counts)
          py <- rep(Y0 + pu[(2 + cm) * (seq_len(npar) - 1) + 2] * (Y1 - Y0), counts)
          i <- seq_len(tot) - 1L
          if (kernel == "thomas") {
            dx <- s * q[3 * i + 1]
            dy <- s * q[3 * i + 2]
          } else if (kernel == "cauchy") {
            dx <- s * q[3 * i + 1] / abs(q[3 * i + 3])
            dy <- s * q[3 * i + 2] / abs(q[3 * i + 3])
          } else {
            rad <- s * sqrt(v[2 * i + 1])
            ang <- 2 * pi * v[2 * i + 2]
            dx <- rad * cos(ang)
            dy <- rad * sin(ang)
          }
          x <- px + dx
          y <- py + dy
          keep <- x >= w[1] & x <= w[2] & y >= w[3] & y <= w[4]
          pat <- cbind(x[keep], y[keep])
        }
      }
      sims[[k + 1L]] <- pat
    }
  }
  list(intensity = kappa * mu, K = K, pcf = g, r = rs, simulated = sims)
}
