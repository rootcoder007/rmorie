# SPDX-License-Identifier: AGPL-3.0-or-later
#' Genetic algorithm with elitist truncation selection
#'
#' One generation is selection, crossover and mutation.  The randomness
#' is supplied by the van der Corput low-discrepancy sequence rather
#' than a pseudo-random generator, so the run is exactly reproducible
#' and both language arms visit the same individuals.  Elitism makes the
#' best fitness monotone, which the tests assert.
#'
#' Formula: keep the best ceiling(m/2); each child takes a prefix from
#'   one parent and a suffix from another; a mutation is then added.
#'
#' @param f Fitness function to minimise, of a numeric vector.
#' @param population Matrix of candidate vectors, one per row.
#' @param generations Number of generations.
#' @param mutation Mutation amplitude.
#' @return List with \code{estimate}, \code{best}, \code{best_fitness},
#'   \code{best_path}, \code{generations}, \code{n}, \code{method}.
#' @references Holland (1975), Adaptation in Natural and Artificial
#'   Systems, University of Michigan Press.
#' @export
#' @examples
#' set.seed(1)
#' Ga_opt(function(x) -sum(x^2), population = matrix(rnorm(20), 10, 2),
#'        generations = 10)
Ga_opt <- function(f, population, generations = 20, mutation = 0.1) {
  P <- .s03mat(population)
  m <- nrow(P)
  d <- ncol(P)
  if (m < 2L) stop("genetic_algorithm: population needs at least two individuals")
  if (d == 0L) stop("genetic_algorithm: individuals are empty")
  if (!is.function(f)) stop("genetic_algorithm: f must be callable")
  ng <- as.integer(generations)
  if (ng < 1L) stop("genetic_algorithm: generations must be at least 1")
  mu <- as.numeric(mutation)
  h <- (m + 1L) %/% 2L
  counter <- 0L
  best_path <- numeric(0)
  for (gg in seq_len(ng)) {
    fit <- vapply(seq_len(m), function(i) as.numeric(f(P[i, ])), 0)
    ord <- order(fit, seq_len(m))
    keep <- P[ord[seq_len(h)], , drop = FALSE]
    best_path <- c(best_path, fit[ord[1]])
    kids <- matrix(0, m - h, d)
    if (m - h > 0L) for (i in seq_len(m - h) - 1L) {
      a <- keep[i %% h + 1L, ]
      b <- keep[(i + 1L) %% h + 1L, ]
      cp <- if (d > 1L) (i %% (d - 1L)) + 1L else 0L
      child <- ifelse(seq_len(d) <= cp, a, b)
      for (j in seq_len(d)) {
        child[j] <- child[j] + mu * (2 * .s03vdc(counter) - 1)
        counter <- counter + 1L
      }
      kids[i + 1L, ] <- child
    }
    P <- rbind(keep, kids)
  }
  fit <- vapply(seq_len(m), function(i) as.numeric(f(P[i, ])), 0)
  ord <- order(fit, seq_len(m))
  best_path <- c(best_path, fit[ord[1]])
  .t1_result(estimate = fit[ord[1]], best = P[ord[1], ],
             best_fitness = fit[ord[1]], best_path = best_path,
             generations = ng, n = m,
             method = "elitist truncation selection, one-point crossover, van der Corput mutation; Holland (1975)")
}
