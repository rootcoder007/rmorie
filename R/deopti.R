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
#' Determinism: the donor indices are a fixed offset rotation of the
#' population index and the crossover uses van der Corput points, so every
#' structural feature of DE is preserved and the run reproduces exactly.
#'
#' @param f the objective.
#' @param population starting population, one row per individual.
#' @param F the differential weight.
#' @param CR the crossover probability.
#' @param generations number of generations.
#' @return list: estimate, x, population, fvals, evals, method.
#' @keywords internal
#' @examples
#' Diffevol(function(v) sum(v^2), matrix(c(1, 1, -1, 2, 0.5, -0.5, 2, 0), 4, 2,
#'          byrow = TRUE), 0.8, 0.9, 5)$estimate
#' @export
Diffevol <- function(f, population, F = 0.8, CR = 0.9, generations = 20) {
  P <- .s03mat(population)
  npop <- nrow(P)
  d <- ncol(P)
  fv <- numeric(npop)
  for (i in seq_len(npop)) fv[i] <- as.numeric(f(P[i, ]))
  evals <- npop
  step <- 0L
  for (gen in seq_len(as.integer(generations))) {
    for (i in seq_len(npop)) {
      i0 <- i - 1L
      r1 <- ((i0 + 1L) %% npop) + 1L
      r2 <- ((i0 + 2L) %% npop) + 1L
      r3 <- ((i0 + 3L) %% npop) + 1L
      jr <- as.integer(.s03vdc(step, 3L) * d)
      if (jr >= d) jr <- d - 1L
      u <- numeric(d)
      for (j in seq_len(d)) {
        if (.s03vdc(step * d + (j - 1L), 2L) <= as.numeric(CR) || (j - 1L) == jr) {
          u[j] <- P[r1, j] + as.numeric(F) * (P[r2, j] - P[r3, j])
        } else {
          u[j] <- P[i, j]
        }
      }
      fu <- as.numeric(f(u))
      evals <- evals + 1L
      step <- step + 1L
      if (fu <= fv[i]) { P[i, ] <- u
      fv[i] <- fu }
    }
  }
  best <- 1L
  if (npop > 1L) for (i in seq(2L, npop)) if (fv[i] < fv[best]) best <- i
  list(estimate = if (npop) fv[best] else NaN,
       x = if (npop) as.numeric(P[best, ]) else numeric(0),
       population = P, fvals = fv, evals = evals,
       method = "DE/rand/1/bin (Storn and Price 1997) with a deterministic donor and crossover schedule")
}
