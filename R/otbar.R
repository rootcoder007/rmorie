# SPDX-License-Identifier: AGPL-3.0-or-later
#' Average measures the way transport says they should be averaged
#'
#' The Euclidean average of two shifted bumps is two bumps; the
#' Wasserstein average is one bump in between. The barycentre respects the
#' geometry of the ground space rather than the vector-space structure of
#' the histograms. Fixing the support turns the problem into a coupled set
#' of Sinkhorn problems sharing a common row scaling.
#'
#' Formula: \eqn{rgmin_nu sum_k w_k OT_eps(mu_k, nu)}, solved by
# prime \code{u_k = nu/(K_k v_k)}, \code{nu = prod_k (K_k v_k)^{w_k}},
#' \eqn{v_k = mu_k/(K_k prime u_k)} -- Benamou et al. (2015) Section 3.2;
#' Peyre and Cuturi (2019) eq. (9.11), (9.15)
#'
#' @param A Input histograms, n by K, one per column.
#' @param C_list List of K cost matrices, each n by n.
#' @param weights Barycentric weights; rescaled to sum to one.
#' @param epsilon Entropic strength, positive.
#' @param max_iter Sweeps.
#' @return List with \code{bary}, \code{mass}, \code{n}, \code{K},
#'   \code{iters}.
#' @references Benamou, J.-D. et al. (2015). SIAM Journal on Scientific
#'   Computing 37(2):A1111-A1138. \doi{10.1137/141000439}. Cuturi, M. and
#'   Doucet, A. (2014). Proceedings of Machine Learning Research
#'   32:685-693 (ICML).
#' @export
Otbar <- function(A, C_list, weights, epsilon, max_iter = 200) {
  Am <- as.matrix(A)
  n <- nrow(Am)
  K <- ncol(Am)
  Cs <- lapply(C_list, as.matrix)
  if (length(Cs) != K)
    stop("one cost matrix per input histogram is required")
  for (cm in Cs) if (nrow(cm) != n || ncol(cm) != n)
    stop("each cost matrix must be n by n")
  w <- .ot_hist(weights, normalise = TRUE)
  if (length(w) != K) stop("one weight per input histogram is required")
  eps <- as.numeric(epsilon)
  if (eps <= 0) stop("epsilon must be positive")
  Ks <- lapply(Cs, function(cm) exp(-cm / eps))
  v <- lapply(seq_len(K), function(k) rep(1, n))
  bary <- rep(1 / n, n)
  it <- as.integer(max_iter)
  for (t in seq_len(it)) {
    Kv <- lapply(seq_len(K), function(k) as.numeric(Ks[[k]] %*% v[[k]]))
    lg <- rep(0, n)
    for (k in seq_len(K)) lg <- lg + w[k] * log(Kv[[k]])
    bary <- exp(lg)
    bary[!is.finite(bary)] <- 0
    u <- lapply(seq_len(K), function(k)
      ifelse(Kv[[k]] > 0, bary / Kv[[k]], 0))
    for (k in seq_len(K)) {
      s <- as.numeric(crossprod(Ks[[k]], u[[k]]))
      v[[k]] <- ifelse(s > 0, Am[, k] / s, 0)
    }
  }
  .t1_result(bary = bary, mass = sum(bary), n = n, K = K, iters = it,
             method = "Entropic Wasserstein barycenter, fixed support")
}
