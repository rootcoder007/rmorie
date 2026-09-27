# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local Geary's c per location
#'
#' \eqn{c_i = \sum_j w_{ij} (z_i - z_j)^2} with z the standardised x
#' (Anselin 1995), as \code{spdep::localC}. With several variables (x a
#' matrix, one variable per column) this is the multivariate local Geary of
#' Anselin (2019), \eqn{c_i = (1/k) \sum_v \sum_j w_{ij} (z_{iv} - z_{jv})^2},
#' as \code{spdep::localC} on a list of variables.
#'
#' @param x Values at the n locations: a vector, or an n x k matrix.
#' @param W Spatial weights matrix.
#' @param scale Standardise each variable (sample standard deviation) first.
#' @return List with \code{local}, \code{global_c}, \code{z} (a matrix when
#'   k > 1), \code{n}, \code{n_variables}, \code{method}.
#' @references Anselin, L. (1995). Local indicators of spatial association
#'   - LISA. Geographical Analysis 27, 93-115.
#'
#'   Anselin, L. (2019). A local indicator of multivariate spatial
#'   association: extending Geary's c. Geographical Analysis 51, 133-150.
#' @export
#' @examples
#' W <- matrix(0, 4, 4); W[cbind(1:3, 2:4)] <- 1; W <- W + t(W)
#' Localgeary(c(1, 2, 4, 8), W)$local
#' Localgeary(cbind(c(1, 2, 4, 8), c(3, 1, 2, 5)), W)$local
Localgeary <- function(x, W, scale = TRUE) {
  X <- as.matrix(x)
  W <- as.matrix(W)
  n <- nrow(X)
  Z <- X
  for (v in seq_len(ncol(X))) {
    if (isTRUE(scale)) {
      s <- stats::sd(X[, v])
      if (s <= 0) stop("x has zero variance")
      Z[, v] <- (X[, v] - mean(X[, v])) / s
    }
  }
  loc <- numeric(n)
  for (v in seq_len(ncol(Z))) {
    z <- Z[, v]
    loc <- loc + vapply(seq_len(n), function(i) sum(W[i, ] * (z[i] - z)^2), numeric(1))
  }
  loc <- loc / ncol(Z)
  s0 <- sum(W)
  .t1_result(local = loc, global_c = if (s0 != 0) sum(loc) / (2 * s0) else NA_real_,
             z = if (ncol(Z) == 1) as.vector(Z) else unname(Z), n = n, n_variables = ncol(Z),
             method = if (ncol(Z) == 1) "Local Geary's c" else "Multivariate local Geary's c")
}
