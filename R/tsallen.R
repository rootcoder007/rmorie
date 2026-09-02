# SPDX-License-Identifier: AGPL-3.0-or-later
#' Tsallis entropy of the empirical distribution of a sample
#'
#' Shannon entropy is additive. Tsallis replaces the logarithm with a
#' power, and independent systems then pick up a \code{(1 - q) S_A S_B}
#' cross term -- the point of the generalisation rather than a defect.
#' As \code{q -> 1} the expression tends to Shannon, and that limit is
#' returned exactly at \code{q = 1} rather than dividing by zero.
#'
#' Formula: \code{S_q = (1 / (q - 1)) (1 - sum_x p(x)^q)}.
#'
#' @param y Sample; the empirical pmf over distinct values is used.
#' @param q Entropic index; \code{q = 1} gives Shannon entropy in nats.
#' @return List with \code{estimate}, \code{n_categories}, \code{n}, \code{q}.
#' @references Tsallis, C. (1988). Possible generalization of
#'   Boltzmann-Gibbs statistics. J Stat Phys 52:479-487, equation (1).
#' @export
#' @examples
#' Tsallen(y = c(1, 2, 3, 4, 5, 6, 7, 8), q = 0.5)
Tsallen <- function(y, q) {
  v <- as.numeric(unlist(y)); n <- length(v)
  tb <- table(v)
  p <- as.numeric(tb) / n
  q <- as.numeric(q)
  s <- if (q == 1) -sum(p[p > 0] * log(p[p > 0])) else (1 - sum(p^q)) / (q - 1)
  .t1_result(estimate = s, n_categories = length(p), n = n, q = q,
             method = "Tsallis q-entropy of the empirical pmf")
}
