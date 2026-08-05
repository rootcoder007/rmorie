# SPDX-License-Identifier: AGPL-3.0-or-later
#' Barycentre whose support is found rather than fixed in advance
#'
#' A fixed grid forces the barycentre onto points that may all be far from
#' the data, which in more than two dimensions is fatal. Letting the atoms
#' move turns the problem into an alternating scheme -- solve the
#' transports, then move each atom to the weighted average of the mass it
#' received -- which is Lloyd's algorithm with a transport plan in place
#' of the nearest-point assignment.
#'
#' Formula: alternate \code{T_k = argmin <T, C(Y, X_k)>} and
#' \code{Y <- n sum_k w_k T_k X_k} -- Cuturi and Doucet (2014) Section 4.
#' Uniform weights are kept on the support throughout.
#'
#' @param X_list List of input point clouds, each n_k by d.
#' @param weights Barycentric weights; rescaled to sum to one.
#' @param n_supp Number of support atoms.
#' @param max_iter Alternations.
#' @return List with \code{Y}, \code{weights_y}, \code{cost},
#'   \code{n_supp}, \code{d}, \code{K}, \code{iters}.
#' @references Cuturi, M. and Doucet, A. (2014). Proceedings of Machine
#'   Learning Research 32:685-693 (ICML).
#' @export
Otbarfree <- function(X_list, weights, n_supp, max_iter = 20) {
  clouds <- lapply(X_list, as.matrix)
  K <- length(clouds)
  if (K == 0L) stop("no input clouds")
  d <- ncol(clouds[[1]])
  for (Xk in clouds) if (ncol(Xk) != d)
    stop("all clouds must share a dimension")
  w <- .ot_hist(weights, normalise = TRUE)
  if (length(w) != K) stop("one weight per cloud is required")
  ns <- as.integer(n_supp)
  pool <- do.call(rbind, clouds)
  if (ns < 1L || ns > nrow(pool))
    stop("n_supp must lie between 1 and the pooled size")
  step <- nrow(pool) / ns
  Y <- pool[floor((seq_len(ns) - 1L) * step) + 1L, , drop = FALSE]
  it <- as.integer(max_iter); cost <- 0
  a <- rep(1 / ns, ns)
  for (t in seq_len(it)) {
    Z <- matrix(0, ns, d); cost <- 0
    for (k in seq_len(K)) {
      Xk <- clouds[[k]]; mk <- nrow(Xk)
      C <- .ot_costmat(Y, Xk, 2)
      r <- .ot_emd(a, rep(1 / mk, mk), C)
      cost <- cost + w[k] * r$cost
      Z <- Z + w[k] * ns * (r$T %*% Xk)
    }
    Y <- Z
  }
  .t1_result(Y = Y, weights_y = rep(1 / ns, ns), cost = cost,
             n_supp = ns, d = d, K = K, iters = it,
             method = "Free-support Wasserstein barycenter")
}
