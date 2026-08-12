# Parallel tempering (replica-exchange Monte Carlo).
# Sources: Hukushima, K. and Nemoto, K. (1996), Exchange Monte Carlo
# method and application to spin glass simulations, Journal of the
# Physical Society of Japan 65(6), 1604-1608; Earl, D. J. and Deem,
# M. W. (2005), Parallel tempering: theory, applications, and new
# perspectives, Physical Chemistry Chemical Physics 7, 3910-3916,
# whose eq. (4) gives the exchange acceptance
#   min(1, exp[(1/T_i - 1/T_j)(E_j - E_i)]).
# Here E = -log p, so the swap exponent is written directly in terms
# of the log densities.
#
# Native implementation mirroring Python morie.fn.ptmcmc exactly: the
# same per-replica proposal scaled by sqrt(T), the same draw ORDER
# (all replicas move, then the ladder is swept bottom-up for swaps),
# and the same generator stream.

#' Parallel tempering (replica exchange)
#'
#' Runs \code{K} Metropolis chains at ascending temperatures and
#' periodically proposes swaps between neighbours, accepting with the
#' Earl-Deem (2005) eq. (4) probability.  Hot replicas cross energy
#' barriers freely and hand good configurations down the ladder, so
#' the cold chain mixes on multimodal targets where a single chain
#' would stay trapped.
#'
#' @param log_p Unnormalised log density of one number.
#' @param temperatures Ascending positive temperatures; the first is
#'   the target.
#' @param x0 Common starting value.
#' @param n_iter Number of sweeps.
#' @param step Proposal scale at temperature 1.
#' @param seed Seed for the generator shared with the Python arm.
#' @param swap_every Sweeps between swap attempts.
#' @return A list with \code{chain} (the cold chain),
#'   \code{chains_last}, \code{accept_rate},
#'   \code{swap_accept_rate}, \code{temperatures}, \code{n_iter},
#'   \code{seed}, \code{method}.
#' @references Earl, D. J. and Deem, M. W. (2005). Parallel
#'   tempering: theory, applications, and new perspectives. Physical
#'   Chemistry Chemical Physics, 7, 3910-3916.
#' @export
morie_ptmcmc <- function(log_p, temperatures, x0, n_iter = 1000L, step = 1,
                         seed = 0, swap_every = 1L) {
  temps <- as.numeric(temperatures)
  K <- length(temps)
  if (K < 2L) stop("need at least two temperatures")
  if (any(temps <= 0) || any(diff(temps) <= 0))
    stop("temperatures must be positive and ascending")
  n_iter <- as.integer(n_iter)
  e <- .ghc_rng(seed)
  x <- rep(as.numeric(x0), K)
  lp <- vapply(x, function(v) log_p(v), numeric(1))
  acc <- integer(K)
  swap_try <- integer(K - 1L)
  swap_acc <- integer(K - 1L)
  cold <- numeric(n_iter)
  for (sweep in seq_len(n_iter)) {
    for (k in seq_len(K)) {
      prop <- x[k] + step * sqrt(temps[k]) * .ghc_norm(e, 1L)
      lpp <- log_p(prop)
      u <- .ghc_unif(e, 1L)
      if (log(u) < (lpp - lp[k]) / temps[k]) {
        x[k] <- prop; lp[k] <- lpp; acc[k] <- acc[k] + 1L
      }
    }
    if (sweep %% as.integer(swap_every) == 0L) {
      for (k in seq_len(K - 1L)) {
        swap_try[k] <- swap_try[k] + 1L
        delta <- (1 / temps[k] - 1 / temps[k + 1L]) * (lp[k + 1L] - lp[k])
        u <- .ghc_unif(e, 1L)
        if (log(u) < min(0, delta)) {
          tmp <- x[k]; x[k] <- x[k + 1L]; x[k + 1L] <- tmp
          tmp <- lp[k]; lp[k] <- lp[k + 1L]; lp[k + 1L] <- tmp
          swap_acc[k] <- swap_acc[k] + 1L
        }
      }
    }
    cold[sweep] <- x[1L]
  }
  list(chain = cold, chains_last = x,
       accept_rate = acc / n_iter,
       swap_accept_rate = vapply(seq_len(K - 1L), function(k)
         if (swap_try[k] > 0L) swap_acc[k] / swap_try[k] else NaN, numeric(1)),
       temperatures = temps, n_iter = n_iter, seed = as.integer(seed),
       method = paste("Parallel tempering (Earl-Deem 2005 eq. 4;",
                      "Hukushima-Nemoto 1996)"))
}
