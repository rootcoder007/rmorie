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
#' the downstream model.  The outcome regression is targeted with
#' \code{H = D/g - (1 - D)/(1 - g)} and the path-specific mean is the
#' plug-in through the targeted Q.
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
  yv <- as.numeric(y); Dv <- as.numeric(D); pv <- as.numeric(path)
  n <- length(yv)
  if (n == 0L || length(Dv) != n)
    stop("Tmlpse: y and D must share one length")
  Mm <- as.matrix(M_chain); Xm <- as.matrix(X)
  if (nrow(Mm) != n || nrow(Xm) != n)
    stop("Tmlpse: M_chain and X must have one row per subject")
  K <- ncol(Mm)
  if (length(pv) != K) stop("Tmlpse: path must have one entry per mediator")
  W <- cbind(1, Xm)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  mb <- vector("list", K)
  for (k in seq_len(K)) {
    des <- if (k > 1L) cbind(Dv, W, Mm[, seq_len(k - 1L), drop = FALSE]) else cbind(Dv, W)
    mb[[k]] <- .s4_ols(des, Mm[, k])$beta
  }
  gen <- function(assign) {
    out <- matrix(0, n, K)
    for (k in seq_len(K)) {
      row <- if (k > 1L) cbind(assign[k], W, out[, seq_len(k - 1L), drop = FALSE])
             else cbind(assign[k], W)
      out[, k] <- as.numeric(row %*% mb[[k]])
    }
    out
  }
  Mstar <- gen(ifelse(pv > 0.5, 1, 0))
  Mnull <- gen(rep(0, K))
  qdes <- cbind(Dv, W, Mm)
  qb <- .s4_ols(qdes, yv)$beta
  Qobs <- as.numeric(qdes %*% qb)
  H <- Dv / g - (1 - Dv) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1 <- as.numeric(cbind(1, W, Mstar) %*% qb) + eps / g
  Q0 <- as.numeric(cbind(0, W, Mnull) %*% qb) - eps / (1 - g)
  psi <- sum(Q1 - Q0) / n
  ic <- H * (yv - Qobs - eps * H) + Q1 - Q0 - psi
  se <- if (n > 1L) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, eps = eps, n_path = sum(pv), n = n,
             method = "TMLE for a path-specific effect through a chosen mediator subset")
}
