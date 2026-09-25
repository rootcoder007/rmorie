# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for a path-specific effect through a chosen subset of mediators
#'
#' A path-specific effect sets the treatment to different values on
#' different edges: mediators marked in \code{path} see \code{A = 1}, all
#' other mediators see \code{A = 0}, and the outcome node sees
#' \code{A = 1}; the contrast is against the all-zero regime.  This is
#' identified only when there is no recanting witness, which the caller
#' asserts by supplying \code{path} -- the function cannot check the
#' graph for it.
#'
#' Mediators are taken in column order as a causal chain, each modelled
#' linearly on treatment, covariates and the mediators before it.  The
#' counterfactual mediator values are generated recursively at the
#' path-assigned treatment values, so an upstream counterfactual feeds
#' the downstream model.  With every model linear the plug-in reduces to
#' the product of coefficients along the selected paths, and its
#' influence curve is the delta method on the stacked least-squares
#' influence functions.  No fluctuation is applied: the point-treatment
#' clever covariate \code{D/g - (1 - D)/(1 - g)} solves the score of the
#' total effect, not the path-specific one (\code{eps} is 0).
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param M_chain Mediators in causal order.
#' @param X Baseline covariates.
#' @param path 1 if the treatment may act through that mediator.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{n_path}, \code{n}.
#' @references Miles, C. H. et al. (2017). JASA 112(520):1443-1452;
#'   Avin, C., Shpitser, I. & Pearl, J. (2005). IJCAI-05, 357-363.
#' @export
Tmlpse <- function(y, D, M_chain, X, path) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  pv <- as.numeric(path)
  n <- length(yv)
  if (n == 0L || length(Dv) != n)
    stop("Tmlpse: y and D must share one length")
  Mm <- as.matrix(M_chain)
  Xm <- as.matrix(X)
  if (nrow(Mm) != n || nrow(Xm) != n)
    stop("Tmlpse: M_chain and X must have one row per subject")
  K <- ncol(Mm)
  if (length(pv) != K) stop("Tmlpse: path must have one entry per mediator")
  W <- cbind(1, Xm)
  p1 <- ncol(W)
  asg <- ifelse(pv > 0.5, 1, 0)
  ols_if <- function(des, t) {
    f <- .s4_ols(des, t)
    list(beta = f$beta, inf = n * (des %*% f$xtxinv) * f$resid)
  }
  mb <- vector("list", K)
  mif <- vector("list", K)
  for (k in seq_len(K)) {
    des <- if (k > 1L) cbind(Dv, W, Mm[, seq_len(k - 1L), drop = FALSE]) else cbind(Dv, W)
    f <- ols_if(des, Mm[, k])
    mb[[k]] <- f$beta
    mif[[k]] <- f$inf
  }
  fq <- ols_if(cbind(Dv, W, Mm), yv)
  qb <- fq$beta
  ## under the linear models the covariate terms cancel and the
  ## counterfactual mediator gap is the same for every subject:
  ## delta_k = asg_k a_k + sum_{j<k} c_kj delta_j
  delta <- numeric(K)
  for (k in seq_len(K)) {
    delta[k] <- asg[k] * mb[[k]][1L] +
      (if (k > 1L) sum(mb[[k]][1L + p1 + seq_len(k - 1L)] * delta[seq_len(k - 1L)]) else 0)
  }
  psi <- qb[1L] + sum(qb[1L + p1 + seq_len(K)] * delta)
  lam <- numeric(K)
  for (k in rev(seq_len(K))) {
    lam[k] <- qb[1L + p1 + k] +
      (if (k < K) sum(vapply((k + 1L):K, function(l) lam[l] * mb[[l]][1L + p1 + k], 0)) else 0)
  }
  ic <- fq$inf[, 1L] + as.numeric(fq$inf[, 1L + p1 + seq_len(K), drop = FALSE] %*% delta)
  for (k in seq_len(K)) {
    ic <- ic + lam[k] * asg[k] * mif[[k]][, 1L]
    if (k > 1L)
      ic <- ic + lam[k] * as.numeric(mif[[k]][, 1L + p1 + seq_len(k - 1L), drop = FALSE] %*% delta[seq_len(k - 1L)])
  }
  eps <- 0
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, n_path = sum(pv), n = n,
             method = "Path-specific effect under linear structural models, delta-method influence curve")
}
