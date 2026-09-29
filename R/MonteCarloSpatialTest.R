.mcst_moran <- function(z, coords) {
  n <- length(z)
  d <- z - sum(z) / n
  s0 <- 0
  num <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      dist <- sqrt((coords[i, 1] - coords[j, 1])^2 + (coords[i, 2] - coords[j, 2])^2)
      if (!(dist > 0)) stop("coincident locations: inverse-distance weight is infinite")
      w <- 1 / dist
      s0 <- s0 + w
      num <- num + w * d[i] * d[j]
    }
  }
  n / s0 * num / sum(d * d)
}

.mcst_q7 <- function(xs, p) {
  h <- (length(xs) - 1) * p
  lo <- floor(h)
  hi <- min(lo + 1, length(xs) - 1)
  xs[lo + 1] + (h - lo) * (xs[hi + 1] - xs[lo + 1])
}

#' Monte Carlo spatial test by random relabelling
#'
#' Recomputes a spatial statistic on \code{n_sim} random permutations of the
#' values over the fixed locations and ranks the observed value among them
#' (Besag and Diggle 1977). The default statistic is Moran's I with
#' inverse-distance weights; a statistic that ignores the locations is
#' permutation-invariant and can never reject. Permutations are Fisher-Yates
#' shuffles on the package's Philox stream. Identical to the Python arm
#' \code{morie.fn.sgmci.monte_carlo_spatial_test}.
#'
#' @param Z Numeric vector of observed values.
#' @param coords Two-column matrix of coordinates.
#' @param stat_fn Optional function \code{(z, coords)} returning the statistic.
#' @param n_sim Number of permutations.
#' @param seed Philox seed.
#' @return A list with \code{value} and \code{observed} (the statistic),
#'   \code{envelope_lo}, \code{envelope_hi} (2.5 and 97.5 percent points of the
#'   permutation distribution), \code{p_value} (upper tail),
#'   \code{p_two_sided}, \code{n_sim}, \code{sim_mean}, \code{significant}
#'   and \code{statistic}.
#' @references Besag, J. and Diggle, P. J. (1977). Simple Monte Carlo tests for
#'   spatial pattern. Applied Statistics 26, 327-333.
#'
#'   Cliff, A. D. and Ord, J. K. (1981). Spatial Processes: Models and
#'   Applications. Pion.
#' @examples
#' xy <- cbind(c(0, 1, 2, 0, 1, 2), c(0, 0, 0, 1, 1, 1))
#' monte_carlo_spatial_test(c(1, 1.2, 0.9, 2, 2.4, 2.2), xy, n_sim = 99)$p_value
#' @export
monte_carlo_spatial_test <- function(Z, coords, stat_fn = NULL, n_sim = 999, seed = 42) {
  z <- as.numeric(Z)
  C <- matrix(as.numeric(as.matrix(coords)), ncol = ncol(as.matrix(coords)))
  n <- length(z)
  if (nrow(C) != n) stop("Z and coords must have the same number of rows")
  if (n < 3) stop("need at least 3 locations")
  n_sim <- as.integer(n_sim)
  if (n_sim < 1) stop("n_sim must be at least 1")
  stat <- if (is.null(stat_fn)) .mcst_moran else stat_fn
  obs <- stat(z, C)
  u <- .morie_random_uniform(n_sim * (n - 1), seed = seed, stream = 0)
  sims <- numeric(n_sim)
  pos <- 1
  for (b in seq_len(n_sim)) {
    p <- z
    for (i in seq(n - 1, 1)) {
      j <- floor(u[pos] * (i + 1))
      pos <- pos + 1
      tmp <- p[i + 1]
      p[i + 1] <- p[j + 1]
      p[j + 1] <- tmp
    }
    sims[b] <- stat(p, C)
  }
  p_up <- (1 + sum(sims >= obs)) / (n_sim + 1)
  p_two <- min(1, 2 * min(p_up, (1 + sum(sims <= obs)) / (n_sim + 1)))
  ss <- sort(sims)
  lo <- .mcst_q7(ss, 0.025)
  hi <- .mcst_q7(ss, 0.975)
  list(value = obs, observed = obs, envelope_lo = lo, envelope_hi = hi,
       p_value = p_up, p_two_sided = p_two, n_sim = n_sim, sim_mean = sum(sims) / n_sim,
       significant = obs < lo || obs > hi,
       statistic = if (is.null(stat_fn)) "inverse-distance Moran's I" else "user")
}
