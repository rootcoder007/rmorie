# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hermite polynomial basis
#'
#' The physicists polynomials by their three-term recurrence H_0 = 1,
#' H_1 = 2x, H_(n+1) = 2 x H_n - 2 n H_(n-1), equivalent to the Rodrigues
#' formula H_n(x) = (-1)^n exp(x^2) d^n/dx^n exp(-x^2); the probabilists
#' family is also available.  Source consulted: Hermite (1864), Sur un nouveau
#' developpement en serie des fonctions, Comptes Rendus de l Academie des
#' Sciences 58, 93-100 and 266-273.
#'
#' @param x numeric evaluation points.
#' @param K highest degree; the basis has K + 1 columns.
#' @param kind one of physicist, probabilist.
#' @return list: estimate, basis, top, K, kind, n, method.
#' @keywords internal
#' @examples
#' hermitS(c(2), K = 3)$basis
#' @export
hermitS <- function(x, K = 3L, kind = "physicist") {
  xs <- as.numeric(x)
  n <- length(xs)
  kk <- as.integer(K)
  basis <- matrix(0, n, kk + 1L)
  basis[, 1] <- 1
  if (kk >= 1) basis[, 2] <- if (kind == "probabilist") xs else 2 * xs
  if (kk >= 2) for (m in seq_len(kk - 1)) {
    basis[, m + 2] <- if (kind == "probabilist")
      xs * basis[, m + 1] - m * basis[, m]
    else 2 * xs * basis[, m + 1] - 2 * m * basis[, m]
  }
  top <- basis[, kk + 1L]
  list(estimate = mean(top), basis = basis, top = as.numeric(top[n]),
       K = kk, kind = kind, n = as.integer(n),
       method = "Hermite polynomial basis (Hermite 1864)")
}

# CANONICAL TEST
# b <- hermitS(c(2), K = 3)$basis
# stopifnot(abs(b[1, 1] - 1) < 1e-12, abs(b[1, 2] - 4) < 1e-12,
#           abs(b[1, 3] - 14) < 1e-12, abs(b[1, 4] - 40) < 1e-12)

#' @rdname hermitS
#' @keywords internal
#' @export
morie_hermite_basis <- hermitS
