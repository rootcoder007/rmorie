# SPDX-License-Identifier: AGPL-3.0-or-later
#' Horvitz-Thompson variance of the estimated total
#'
#' The diagonal term uses pi_ii = pi_i, so it contributes
#' (1 - pi_i) y_i^2 / pi_i^2.  For simple random sampling without
#' replacement the whole expression collapses to N^2 (1 - n/N) S^2 / n,
#' the closed form the tests check against.
#'
#' Formula: V = sum_i sum_j (pi_ij - pi_i pi_j)(y_i/pi_i)(y_j/pi_j)/pi_ij.
#'
#' @param y Observed values for the sampled units.
#' @param pi First-order inclusion probabilities in (0, 1].
#' @param pi_ij Matrix of joint inclusion probabilities.
#' @return List with \code{estimate}, \code{variance}, \code{total},
#'   \code{se}, \code{n}, \code{method}.
#' @references Horvitz and Thompson (1952), A generalization of sampling
#'   without replacement from a finite universe, JASA 47(260):663-685.
#'   \doi{10.1080/01621459.1952.10483446}
#' @export
Htvar1 <- function(y, pi, pi_ij) {
  v <- .s03vec(y); p <- .s03vec(pi); P <- .s03mat(pi_ij)
  n <- length(v)
  if (n == 0L) stop("ht_variance: y is empty")
  if (length(p) != n) stop("ht_variance: y and pi have different lengths")
  if (nrow(P) != n || ncol(P) != n) stop("ht_variance: pi_ij must be n x n")
  if (any(p <= 0 | p > 1)) stop("ht_variance: inclusion probabilities must lie in (0, 1]")
  var <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    pij <- if (i == j) p[i] else P[i, j]
    if (pij <= 0) stop("ht_variance: joint inclusion probability is not positive")
    var <- var + (pij - p[i] * p[j]) * (v[i] / p[i]) * (v[j] / p[j]) / pij
  }
  .t1_result(estimate = var, variance = var, total = sum(v / p),
             se = if (var >= 0) sqrt(var) else NaN, n = n,
             method = "sum_ij (pi_ij - pi_i pi_j)(y_i/pi_i)(y_j/pi_j)/pi_ij, Horvitz & Thompson (1952)")
}
