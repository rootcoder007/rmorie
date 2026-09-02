# SPDX-License-Identifier: AGPL-3.0-or-later
#' Particle swarm optimisation on a deterministic low-discrepancy stream
#'
#' The Python module \code{morie.fn.pswrm} is the seeded stochastic
#' version of this method. It cannot be checked across languages: its
#' numbers come from Python's native generator stream and this tree's
#' Philox produces a different sequence from the same seed, so numeric
#' parity between the arms is impossible by construction. This is the
#' deterministic sibling -- the same algorithm driven by a van der Corput
#' sequence with a distinct prime base per coordinate, so both language
#' arms visit the same points in the same order.
#'
#' Formula: \code{v <- w v + c1 r1 (p_best - x) + c2 r2 (g_best - x)},
#' \code{x <- clip(x + v, lo, hi)}, with \code{r1}, \code{r2} from the
#' low-discrepancy stream instead of a uniform generator.
#'
#' @param f Objective \code{f(x)}, minimised.
#' @param bounds List or matrix of (lo, hi) pairs, one per dimension;
#'   at most 16 dimensions.
#' @param n_particles Swarm size, at least 1.
#' @param w Inertia coefficient, non-negative.
#' @param c1 Cognitive coefficient, non-negative.
#' @param c2 Social coefficient, non-negative.
#' @param maxiter Iterations, non-negative.
#' @return List with \code{estimate}, \code{value}, \code{x},
#'   \code{n_eval}, \code{n_particles}, \code{maxiter}, \code{d}.
#' @references Kennedy, J. & Eberhart, R. (1995). Particle swarm
#'   optimization. Proceedings of ICNN'95, volume 4, pages 1942-1948.
#'   \doi{10.1109/ICNN.1995.488968}.
#' @export
Psoop <- function(f, bounds, n_particles = 20, w = 0.7, c1 = 1.5, c2 = 1.5,
                  maxiter = 200) {
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53)
  if (!is.function(f)) stop("Psoop: f must be callable")
  bnd <- lapply(bounds, function(b) as.numeric(b))
  d <- length(bnd)
  if (d == 0L) stop("Psoop: bounds is empty")
  if (d > length(primes)) stop("Psoop: at most 16 dimensions are supported")
  for (b in bnd) if (!(b[2] > b[1])) stop("Psoop: every bound needs hi > lo")
  n_particles <- as.integer(n_particles)
  maxiter <- as.integer(maxiter)
  if (n_particles < 1L) stop("Psoop: n_particles must be at least 1")
  if (maxiter < 0L) stop("Psoop: maxiter must be non-negative")
  w <- as.numeric(w)
  c1 <- as.numeric(c1)
  c2 <- as.numeric(c2)
  if (w < 0 || c1 < 0 || c2 < 0) stop("Psoop: coefficients must be non-negative")

  pos <- matrix(0, n_particles, d)
  for (i in seq_len(n_particles)) for (j in seq_len(d))
    pos[i, j] <- bnd[[j]][1] + (bnd[[j]][2] - bnd[[j]][1]) * .s03vdc(i, primes[j])
  vel <- matrix(0, n_particles, d)
  pbest <- pos
  pval <- vapply(seq_len(n_particles), function(i) as.numeric(f(pos[i, ])), 0)
  n_eval <- n_particles
  gi <- which.min(pval)
  gbest <- pbest[gi, ]
  gval <- pval[gi]
  k <- 1L
  for (it in seq_len(maxiter)) {
    for (i in seq_len(n_particles)) {
      for (j in seq_len(d)) {
        r1 <- .s03vdc(k, 2L)
        r2 <- .s03vdc(k, 3L)
        k <- k + 1L
        vel[i, j] <- w * vel[i, j] + c1 * r1 * (pbest[i, j] - pos[i, j]) +
          c2 * r2 * (gbest[j] - pos[i, j])
        v <- pos[i, j] + vel[i, j]
        if (v < bnd[[j]][1]) v <- bnd[[j]][1] else if (v > bnd[[j]][2]) v <- bnd[[j]][2]
        pos[i, j] <- v
      }
      val <- as.numeric(f(pos[i, ]))
      n_eval <- n_eval + 1L
      if (val < pval[i]) {
        pval[i] <- val
        pbest[i, ] <- pos[i, ]
        if (val < gval) { gval <- val
        gbest <- pos[i, ] }
      }
    }
  }
  .t1_result(estimate = gval, value = gval, x = gbest, n_eval = n_eval,
             n_particles = n_particles, maxiter = maxiter, d = d,
             method = "Particle swarm on a van der Corput stream")
}
