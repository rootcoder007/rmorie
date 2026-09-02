# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intrinsic conditional autoregressive prior
#'
#' The full conditionals correspond to the joint precision
#' Q = (D - W)/tau^2 with D the diagonal of neighbour counts.  Q has the
#' constant vector in its null space, so the prior is improper (rank
#' n - 1) and needs a sum-to-zero constraint; the smallest eigenvalue
#' being exactly zero is the check the tests apply.
#'
#' Formula: u_i | u_-i ~ N(mean of the n_i neighbours, tau^2 / n_i).
#'
#' @param adjacency Symmetric 0/1 neighbour matrix with zero diagonal.
#' @param tau Positive conditional scale.
#' @param u Optional field values at which to evaluate the conditionals.
#' @return List with \code{estimate}, \code{precision},
#'   \code{conditional_mean}, \code{conditional_var},
#'   \code{pairwise_quadratic}, \code{log_density_kernel},
#'   \code{smallest_eigenvalue}, \code{mean_u}, \code{n}, \code{method}.
#' @references Besag (1974), Spatial interaction and the statistical
#'   analysis of lattice systems, JRSS B 36(2):192-236,
#'   \doi{10.1111/j.2517-6161.1974.tb00999.x}; Besag, York and Mollie
#'   (1991), Annals of the Institute of Statistical Mathematics
#'   43(1):1-20. \doi{10.1007/BF00116466}
#' @export
#' @examples
#' Icarbm(matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE))
Icarbm <- function(adjacency, tau = 1, u = NULL) {
  W <- .s03mat(adjacency)
  n <- nrow(W)
  if (n == 0L) stop("icar_prior: adjacency is empty")
  if (ncol(W) != n) stop("icar_prior: adjacency must be square")
  if (!isTRUE(all.equal(W, t(W), check.attributes = FALSE))) stop("icar_prior: adjacency must be symmetric")
  if (any(diag(W) != 0)) stop("icar_prior: adjacency must have a zero diagonal")
  t <- as.numeric(tau)
  if (t <= 0) stop("icar_prior: tau must be positive")
  deg <- rowSums(W)
  if (any(deg <= 0)) stop("icar_prior: every unit needs at least one neighbour")
  Q <- (diag(deg, n) - W) / (t * t)
  cvar <- t * t / deg
  if (is.null(u)) {
    cmean <- rep(0, n); quad <- NaN; centred <- NaN
  } else {
    uv <- .s03vec(u)
    if (length(uv) != n) stop("icar_prior: u and adjacency have different lengths")
    cmean <- as.numeric(W %*% uv) / deg
    quad <- 0
    for (i in seq_len(n)) if (i < n) for (j in seq(i + 1L, n))
      if (W[i, j] != 0) quad <- quad + W[i, j] * (uv[i] - uv[j])^2
    quad <- quad / (t * t)
    centred <- mean(uv)
  }
  vals <- .s03jacobi(Q)$values
  .t1_result(estimate = vals[1], precision = Q, conditional_mean = cmean,
             conditional_var = cvar, pairwise_quadratic = quad,
             log_density_kernel = if (is.nan(quad)) NaN else -0.5 * quad,
             smallest_eigenvalue = vals[1], mean_u = centred, n = n,
             method = "u_i | u_-i ~ N(mean of neighbours, tau^2/n_i); Q = (D - W)/tau^2, Besag (1974)")
}
