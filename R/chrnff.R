# SPDX-License-Identifier: AGPL-3.0-or-later
#' Chernoff bound on an upper tail
#'
#' Formula: P(X >= a) <= min_\{s>0\} exp(-s a) E\[exp(s X)\]
#'
#' @param mgf The moment generating function s -> E\[exp(s X)\].
#' @param a Tail threshold.
#' @param s_grid Positive values of s searched; a fixed geometric grid over (0.01, 8.7] if omitted.

#' @param mgf See Usage.
#' @param a See Usage.
#' @param s_grid See Usage.
#' @return List with ``bound``, ``s``, ``log_bound``, ``at_boundary``.
#' @references Chernoff (1952), A measure of asymptotic efficiency for tests of a hypothesis based on the sum of observations, Annals of Mathematical Statistics 23:493-507. Not held locally; the exponential Markov bound is stated in this exact form in every standard reference.
#' @export
#' @examples
#' Chernbnd(mgf = function(s) exp(0.5 * s^2), a = 2)
Chernbnd <- function(mgf, a, s_grid = NULL) {
  a <- as.numeric(a)
  grid <- if (is.null(s_grid)) 0.01 * 1.05^(0:140) else .t1_vec(s_grid)
  if (any(grid <= 0)) stop("s must be positive")
  v <- vapply(grid, function(s) exp(-s * a) * as.numeric(mgf(s)), numeric(1))
  v[!is.finite(v)] <- Inf
  if (all(is.infinite(v))) stop("mgf overflowed at every grid point")
  k <- which.min(v)
  .t1_result(bound = v[k], s = grid[k],
             log_bound = if (v[k] > 0) log(v[k]) else -Inf,
             at_boundary = k == 1L || k == length(grid),
             method = "Chernoff bound")
}
