# SPDX-License-Identifier: AGPL-3.0-or-later
#' Differential evolution
#'
#' Storn and Price (1997), Differential evolution -- a simple and
#' efficient heuristic for global optimization over continuous spaces,
#' Journal of Global Optimization 11(4), 341-359.  DE/rand/1/bin: v_i =
#' x_r1 + F(x_r2 - x_r3) with r1, r2, r3 distinct and != i; u_(i,j) = v_(i,j)
#' if rand_j <= CR or j = j_rand, else x_(i,j); and x_i <- u_i iff f(u_i)
#' <= f(x_i).  The paper is paywalled; the mutation, the binomial
#' crossover including the forced index, and the greedy selection are
#' quoted in their standard published form.
#'
#' Randomness: donors (three uniforms, drawn without replacement from the
#' population minus i), j_rand and the crossover draws come from the morie
#' Philox stream in the same order as the Python arm, so runs agree exactly.
#'
#' @param f the objective.
#' @param population starting population, one row per individual.
#' @param F the differential weight.
#' @param CR the crossover probability.
#' @param generations number of generations.
#' @param seed Philox seed.
#' @return list: estimate, x, population, fvals, evals, method.
#' @keywords internal
#' @examples
#' Diffevol(function(v) sum(v^2), matrix(c(1, 1, -1, 2, 0.5, -0.5, 2, 0), 4, 2,
#'          byrow = TRUE), 0.8, 0.9, 50)$estimate
#' @export
Diffevol <- function(f, population, F = 0.8, CR = 0.9, generations = 20, seed = 0) {
  P <- .s03mat(population)
  npop <- nrow(P)
  d <- ncol(P)
  if (npop < 4L) stop("DE/rand/1 needs at least 4 individuals", call. = FALSE)
  fv <- numeric(npop)
  for (i in seq_len(npop)) fv[i] <- as.numeric(f(P[i, ]))
  evals <- npop
  per <- 4L + d
  U <- .morie_random_uniform(as.integer(generations) * npop * per, seed = seed, stream = 0)
  pos <- 0L
  for (gen in seq_len(as.integer(generations))) {
    for (i in seq_len(npop)) {
      pool <- setdiff(seq_len(npop), i)
      r <- integer(3)
      for (t in 1:3) {
        k <- min(floor(U[pos + t] * length(pool)), length(pool) - 1) + 1
        r[t] <- pool[k]
        pool <- pool[-k]
      }
      jr <- min(floor(U[pos + 4L] * d), d - 1)
      u <- numeric(d)
      for (j in seq_len(d)) {
        if (U[pos + 4L + j] <= as.numeric(CR) || (j - 1L) == jr) {
          u[j] <- P[r[1], j] + as.numeric(F) * (P[r[2], j] - P[r[3], j])
        } else {
          u[j] <- P[i, j]
        }
      }
      pos <- pos + per
      fu <- as.numeric(f(u))
      evals <- evals + 1L
      if (fu <= fv[i]) {
        P[i, ] <- u
        fv[i] <- fu
      }
    }
  }
  best <- which.min(fv)
  list(estimate = fv[best], x = as.numeric(P[best, ]), population = P, fvals = fv, evals = evals,
       method = "DE/rand/1/bin (Storn and Price 1997), Philox donors and crossover")
}
