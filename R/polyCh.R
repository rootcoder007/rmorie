# SPDX-License-Identifier: AGPL-3.0-or-later
#' Chebyshev polynomial basis of the first kind
#'
#' Formula: T_0 = 1, T_1 = x, T_\{n+1\}(x) = 2 x T_n(x) - T_\{n-1\}(x); equivalently T_n(x) = cos(n arccos x)
#'
#' @param x Points at which the basis is evaluated.
#' @param K Highest degree; the basis has K+1 columns.

#' @param x See Usage.
#' @param K See Usage.
#' @return List with ``basis`` (n by K+1), ``degree``, ``trig`` (cos form where |x| <= 1), ``n``.
#' @references Chebyshev (1853). The original is not held locally; the recurrence T_\{n+1\} = 2 x T_n - T_\{n-1\} with T_0 = 1, T_1 = x and the identity T_n(cos t) = cos(n t) are the standard published definitions.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Chebbasis(V)
Chebbasis <- function(x, K = 5) {
  x <- .t1_vec(x); K <- as.integer(K)
  if (K < 0) stop("K must be non-negative")
  out <- matrix(0, length(x), K + 1L)
  out[, 1] <- 1
  if (K >= 1) out[, 2] <- x
  if (K >= 2) for (n in 2:K) out[, n + 1L] <- 2 * x * out[, n] - out[, n - 1L]
  trig <- matrix(NA_real_, length(x), K + 1L)
  ok <- abs(x) <= 1
  if (any(ok)) for (n in 0:K) trig[ok, n + 1L] <- cos(n * acos(x[ok]))
  .t1_result(basis = out, degree = K, trig = trig, n = length(x),
             method = "Chebyshev polynomial basis (first kind)")
}
