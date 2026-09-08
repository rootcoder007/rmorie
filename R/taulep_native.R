# Tau-leaping stochastic simulation.
# Source: Gillespie (2001), J. Chem. Phys. 115(4), 1716-1733,
# Eqs. 14-16 (fetched-wave3/Approximate accelerated stochastic
# simulation of chemically reacting systems.pdf).  Mirrors Python
# morie.fn.taulep exactly: Poisson counts via the exponential
# interarrival counting method on the shared SplitMix64 uniform
# stream, consumed draw for draw.

#' .taulep_poisson
#'
#' A step of the taulep_native implementation. Called by \code{morie_taulep}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param lam Passed to \code{<=}.
#' @return The value of \code{repeat}.
#' @export
.taulep_poisson <- function(e, lam) {
  if (lam <= 0) return(0L)
  k <- 0L
  acc <- 0
  repeat {
    u <- .ghc_unif(e, 1)
    while (u <= 0) u <- .ghc_unif(e, 1)
    acc <- acc - log(u)
    if (acc > lam) return(k)
    k <- k + 1L
  }
}

#' Explicit tau-leaping simulation (Gillespie 2001)
#'
#' Basic tau-leap: per leap, each reaction channel fires
#' K_j ~ Poisson(a_j(x) tau) times independently (Eq. 16) and the
#' state advances by sum_j K_j nu_j; negative populations are clamped
#' to zero (tau-too-large symptom, per Gillespie's discussion).
#'
#' @param nu Stoichiometry matrix (M channels x N species).
#' @param propensity Function(x) returning M non-negative rates.
#' @param x0 Initial state vector.
#' @param tau Fixed leap size.
#' @param n_steps Number of leaps.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{path} (matrix, rows = states),
#'   \code{times}, \code{firings}, \code{tau}, \code{n_steps},
#'   \code{seed}, \code{method}.
#' @references Gillespie, D. T. (2001). Approximate accelerated
#'   stochastic simulation of chemically reacting systems. Journal of
#'   Chemical Physics, 115(4), 1716-1733.
#' @export
morie_taulep <- function(nu, propensity, x0, tau, n_steps, seed = 0) {
  nu <- as.matrix(nu)
  m <- nrow(nu)
  x <- as.numeric(x0)
  nsp <- length(x)
  if (ncol(nu) != nsp) stop("each nu row must match the state length")
  tau <- as.numeric(tau)
  if (tau <= 0) stop("tau must be positive")
  n_steps <- as.integer(n_steps)
  e <- .ghc_rng(seed)
  path <- matrix(0, n_steps + 1, nsp)
  path[1, ] <- x
  times <- (0:n_steps) * tau
  fired <- integer(m)
  for (s in seq_len(n_steps)) {
    a <- as.numeric(propensity(x))
    if (length(a) != m || any(a < 0)) {
      stop("propensity must return M non-negative rates")
    }
    for (j in seq_len(m)) {
      kj <- .taulep_poisson(e, a[j] * tau)
      fired[j] <- fired[j] + kj
      if (kj > 0) x <- x + kj * nu[j, ]
    }
    x[x < 0] <- 0
    path[s + 1, ] <- x
  }
  list(path = path, times = times, firings = fired, tau = tau,
       n_steps = n_steps, seed = seed,
       method = "explicit tau-leaping (Gillespie 2001, Eq. 16)")
}
