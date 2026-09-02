# Approximate Bayesian computation by rejection.
# Sources: Pritchard, J. K., Seielstad, M. T., Perez-Lezaun, A. and
# Feldman, M. W. (1999), Population growth of human Y chromosomes: a
# study of Y chromosome microsatellites, Molecular Biology and
# Evolution 16(12), 1791-1798 (the rejection algorithm with a
# tolerance on summary statistics); Tavare, S., Balding, D. J.,
# Griffiths, R. C. and Donnelly, P. (1997), Inferring coalescence
# times from DNA sequence data, Genetics 145(2), 505-518 (the exact
# rejection predecessor); Sisson, S. A., Fan, Y. and Beaumont, M. A.
# (2018), Handbook of Approximate Bayesian Computation, Ch. 1.
#
# Native implementation mirroring Python morie.fn.abcrej exactly: the
# same uniform prior draws in the same coordinate order from the
# shared generator, the same Euclidean distance on summaries, and the
# same acceptance test d <= eps.

#' ABC rejection sampler
#'
#' Draws parameters from independent uniform priors, simulates summary
#' statistics, and keeps the draw when the Euclidean distance to the
#' observed summaries is at most \code{eps} (Pritchard et al. 1999).
#' As \code{eps} tends to zero the accepted draws tend to the exact
#' posterior and the acceptance rate tends to zero, which is the
#' trade-off the method is built around.
#'
#' @param sim Function \code{(theta, rng)} returning summary
#'   statistics; \code{rng} is the generator environment, so a
#'   stochastic simulator stays reproducible.
#' @param obs Observed summary statistics.
#' @param eps Acceptance tolerance, positive.
#' @param prior List of \code{c(low, high)} pairs, one per parameter.
#' @param n_draws Number of prior draws.
#' @param seed Seed for the generator shared with the Python arm.
#' @return A list with \code{samples}, \code{n_accepted},
#'   \code{acceptance_rate}, \code{distances}, \code{posterior_mean},
#'   \code{eps}, \code{n_draws}, \code{seed}, \code{method}.
#' @references Pritchard, J. K. et al. (1999). Population growth of
#'   human Y chromosomes. Molecular Biology and Evolution, 16(12),
#'   1791-1798.
#' @export
#' @examples
#' prior <- list(c(-2, 2))
#' sim <- function(theta, rng) theta[1]
#' morie_abcrej(sim, obs = 0.5, eps = 0.5, prior, n_draws = 200)
morie_abcrej <- function(sim, obs, eps, prior, n_draws = 1000L, seed = 0) {
  obs <- as.numeric(obs)
  eps <- as.numeric(eps)
  if (eps <= 0) stop("eps must be positive")
  bounds <- lapply(prior, as.numeric)
  if (any(vapply(bounds, function(b) b[2] <= b[1], logical(1)))) {
    stop("each prior pair must satisfy low < high")
  }
  e <- .ghc_rng(seed)
  accepted <- list()
  dists <- numeric(0)
  for (k in seq_len(as.integer(n_draws))) {
    theta <- vapply(bounds, function(b) .ghc_unif(e, 1L, b[1], b[2]), numeric(1))
    s <- as.numeric(sim(theta, e))
    if (length(s) != length(obs)) {
      stop("sim() must return summaries matching obs")
    }
    d <- sqrt(sum((s - obs)^2))
    if (d <= eps) {
      accepted[[length(accepted) + 1L]] <- theta
      dists <- c(dists, d)
    }
  }
  kk <- length(accepted)
  pm <- if (kk > 0L) {
    vapply(seq_along(bounds), function(j) {
      sum(vapply(accepted, function(a) a[j], numeric(1))) / kk
    }, numeric(1))
  } else {
    rep(NaN, length(bounds))
  }
  list(
    samples = accepted, n_accepted = kk,
    acceptance_rate = kk / as.numeric(n_draws), distances = dists,
    posterior_mean = pm, eps = eps, n_draws = as.integer(n_draws),
    seed = as.integer(seed),
    method = "ABC rejection (Pritchard et al. 1999)"
  )
}
