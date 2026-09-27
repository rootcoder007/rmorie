#' Monte Carlo test of space-time interaction
#'
#' With the edge-corrected space-time K estimates of \code{SpaceTimeK}, the
#' statistic is \eqn{T = \sum_{s,t} (K_{st}(s, t) - K_s(s) K_t(t))}, zero in
#' expectation when space and time are independent; its null distribution
#' comes from permuting the event times over the fixed locations, as
#' \code{splancs::stmctest} (Diggle et al. 1995). p-value
#' (1 + #{T_sim >= T_obs}) / (nsim + 1). Permutations are Fisher-Yates
#' shuffles from Philox stream k of \code{seed}, identical to the Python
#' arm (splancs uses \code{sample}). Pairs count at distance <= s and <= t in
#' every bin; splancs drops pairs exactly at the largest s or t, so the two
#' differ only when a pair distance ties a top grid value.
#'
#' @param points Two-column matrix of coordinates.
#' @param times Event times.
#' @param s Spatial distances.
#' @param t Temporal distances.
#' @param window c(xmin, xmax, ymin, ymax).
#' @param tlimits c(tmin, tmax).
#' @param nsim Number of permutations.
#' @param seed Philox seed.
#' @return List with \code{statistic}, \code{p_value}, \code{simulated},
#'   \code{nsim}.
#' @references Diggle, P. J., Chetwynd, A. G., Haggkvist, R. and Morris,
#'   S. E. (1995). Second-order analysis of space-time clustering.
#'   Statistical Methods in Medical Research 4, 124-136.
#' @examples
#' P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9, .95, .85))
#' tm <- c(1, 7, 2, 5, 3, 4.5, 6, 9, 8, 8.5)
#' SpaceTimeInteractionTest(P, tm, c(.2, .4), c(2.3, 4.3), c(0, 1, 0, 1), c(0, 10), nsim = 19)$p_value
#' @export
SpaceTimeInteractionTest <- function(points, times, s, t, window, tlimits, nsim = 99L, seed = 1L) {
  tm <- as.numeric(times)
  n <- length(tm)
  stat <- function(tt) sum(SpaceTimeK(points, tt, window, tlimits, s, t)$D)
  obs <- stat(tm)
  sims <- numeric(nsim)
  for (k in seq_len(nsim)) {
    u <- .morie_random_uniform(n, seed = seed, stream = k - 1L)
    p <- tm
    for (i in seq(n, 2)) {
      j <- min(floor(u[i] * i), i - 1) + 1
      tmp <- p[i]
      p[i] <- p[j]
      p[j] <- tmp
    }
    sims[k] <- stat(p)
  }
  list(statistic = obs, p_value = (1 + sum(sims >= obs)) / (nsim + 1), simulated = sims, nsim = nsim)
}
