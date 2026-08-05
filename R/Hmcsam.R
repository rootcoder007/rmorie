# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hamiltonian Monte Carlo
#'
#' The state is augmented with a momentum and the leapfrog integrator is
#' run L times before a Metropolis accept/reject on exp(-Delta H).
#' Leapfrog is exactly reversible and volume preserving, which is what
#' makes the acceptance ratio simply exp(-Delta H); both properties are
#' checked directly in the tests.  Momenta and acceptance uniforms come
#' from deterministic low-discrepancy streams so the chain is
#' reproducible.
#'
#' Formula: p <- p + (eps/2) grad log p(x); x <- x + eps p;
#'   p <- p + (eps/2) grad log p(x); accept with exp(-Delta H).
#'
#' @param log_p Log target density of a numeric vector.
#' @param grad_log_p Its gradient.
#' @param x0 Starting state.
#' @param step_size Leapfrog step size.
#' @param L Leapfrog steps per proposal.
#' @param n_iter Number of proposals.
#' @return List with \code{estimate}, \code{mean}, \code{draws},
#'   \code{accept_rate}, \code{mean_energy_error}, \code{n},
#'   \code{method}.
#' @references Neal (2011), MCMC using Hamiltonian dynamics, in Brooks,
#'   Gelman, Jones and Meng (eds), Handbook of Markov Chain Monte Carlo,
#'   CRC Press, ch. 5. \doi{10.1201/b10905}
#' @export
Hmcsam <- function(log_p, grad_log_p, x0, step_size = 0.1, L = 10, n_iter = 200) {
  x <- .s03vec(x0)
  d <- length(x)
  if (d == 0L) stop("hamiltonian_mc: x0 is empty")
  if (!is.function(log_p) || !is.function(grad_log_p)) stop("hamiltonian_mc: log_p and grad_log_p must be callable")
  eps <- as.numeric(step_size)
  if (eps <= 0) stop("hamiltonian_mc: step_size must be positive")
  steps <- as.integer(L)
  if (steps < 1L) stop("hamiltonian_mc: L must be at least 1")
  it <- as.integer(n_iter)
  if (it < 1L) stop("hamiltonian_mc: n_iter must be at least 1")
  leap <- function(x, p) {
    g <- .s03vec(grad_log_p(x))
    for (s in seq_len(steps)) {
      p <- p + 0.5 * eps * g
      x <- x + eps * p
      g <- .s03vec(grad_log_p(x))
      p <- p + 0.5 * eps * g
    }
    list(x = x, p = p)
  }
  draws <- matrix(0, it, d)
  acc <- 0L; counter <- 1L; dH <- numeric(it)
  for (s in seq_len(it)) {
    p0 <- vapply(seq_len(d) - 1L, function(j) .s03qnorm(.s03vdc(counter + j, 2L)), 0)
    counter <- counter + d
    H0 <- -as.numeric(log_p(x)) + 0.5 * sum(p0 * p0)
    lf <- leap(x, p0)
    H1 <- -as.numeric(log_p(lf$x)) + 0.5 * sum(lf$p * lf$p)
    dH[s] <- H1 - H0
    u <- .s03vdc(counter, 3L)
    counter <- counter + 1L
    accept <- if (H1 <= H0) TRUE else (u < exp(-(H1 - H0)))
    if (accept) { x <- lf$x; acc <- acc + 1L }
    draws[s, ] <- x
  }
  .t1_result(estimate = mean(draws[, 1]), mean = colMeans(draws), draws = draws,
             accept_rate = acc / it, mean_energy_error = mean(dH), n = it,
             method = "leapfrog integration of H = U + K with a Metropolis correction, Neal (2011) ch. 5")
}
