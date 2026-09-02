# SPDX-License-Identifier: AGPL-3.0-or-later
#' Katz centrality
#'
#' x = sum_\{k>=1\} alpha^k A^k 1 = ((I - alpha A)^-1 - I) 1, convergent for
#' alpha < 1/rho(A); alpha defaults to half that radius.  Source consulted:
#' Katz (1953), Psychometrika 18(1), 39-43.
#'
#' @param A adjacency matrix.
#' @param alpha attenuation factor.
#' @param beta exogenous status added to every node.
#' @return list: estimate, centrality, normalized, alpha, radius, n, method.
#' @keywords internal
#' @examples
#' sgtkem(matrix(c(0,1,0,1,0,1,0,1,0), 3, 3), alpha = 0.1)$centrality
#' @export
sgtkem <- function(A, alpha = NULL, beta = 1) {
  m <- as.matrix(A); dimnames(m) <- NULL
  n <- nrow(m)
  sym <- 0.5 * (m + t(m))
  rho <- max(abs(eigen(sym, symmetric = TRUE, only.values = TRUE)$values))
  a <- if (is.null(alpha)) 0.5 / rho else as.numeric(alpha)
  if (rho > 0 && a * rho >= 1) stop("alpha must be below 1 / spectral radius")
  one <- rep(as.numeric(beta), n)
  x <- as.numeric(solve(diag(n) - a * m, one)) - one
  tot <- sum(abs(x))
  list(estimate = max(x), centrality = x,
       normalized = if (tot > 0) x / tot else x, alpha = a, radius = rho,
       n = n, method = "Katz centrality (Katz 1953)")
}

# CANONICAL TEST
# r <- sgtkem(matrix(c(0,1,0,1,0,1,0,1,0),3,3), alpha = 0.1)
# stopifnot(length(r$centrality) == 3L)

#' @rdname sgtkem
#' @keywords internal
#' @export
morie_sgtkem <- sgtkem
