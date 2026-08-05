# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wild cluster bootstrap-t for clustered errors
#'
#' Cameron, A. C., Gelbach, J. B. and Miller, D. L. (2008), "Bootstrap-Based
#' Improvements for Inference with Clustered Errors", The Review of Economics
#' and Statistics 90(3), 414-427.  Read from the NBER Technical Working Paper
#' 344 text of the same paper; the two load-bearing passages are section 3.2,
#' which states the scheme as "u*_g = u_g with probability 0.5 and
#' u*_g = -u_g with probability 0.5, with this assignment AT THE CLUSTER
#' LEVEL", naming the +/-1 multipliers Rademacher weights, and the CRVE
#' finite-sample factor c = [G/(G-1)][(N-1)/(N-k)] with
#' u_tilde_g = sqrt(c) u_hat_g.
#'
#' So the whole cluster's residual vector is flipped by one shared sign,
#' which is what preserves the within-cluster correlation the CRVE is there to
#' handle.  Flipping observation by observation would silently destroy it and
#' would still produce plausible-looking numbers.
#'
#' The Wald statistic is bootstrapped, not the coefficient (the paper's
#' "bootstrap-t"), because only the studentised version gets the asymptotic
#' refinement that makes this worth doing with few clusters:
#' w*_b = (beta*_b - beta_hat)/se*_b with se*_b a cluster-robust standard
#' error recomputed on each pseudo-sample, and the two-sided p-value is the
#' fraction of |w*_b| at least |w| where w = (beta_hat - beta_0)/se.
#'
#' Anchor: with G = N singleton clusters the cluster sign is an
#' observation-level Rademacher draw, so the bootstrap variance target
#' collapses to the HC0 sandwich; and in general Var*(beta*) is exactly the
#' UNCORRECTED clustered sandwich because Var(v_g) = 1.  vcov_cluster0
#' reports that target directly and vcov_cluster applies the paper's c.
#'
#' @param X the n x p design.
#' @param y the n responses.
#' @param cluster cluster label per observation.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param coef zero-based index of the coefficient under test.
#' @param beta0 null value for that coefficient.
#' @param alpha test level; reject is the decision at this level.
#' @return list: beta_b, w_b, beta_hat, se_cluster, w, p_value, reject,
#'   vcov_cluster, vcov_cluster0, G, n, p, B, estimate, method.
#' @keywords internal
#' @examples
#' X <- cbind(1, rep(0:1, each = 6)); y <- 1 + 0.3 * X[, 2] + (1:12) / 20
#' Btwldcl(X, y, rep(1:4, each = 3), B = 20)$p_value
#' @export
Btwldcl <- function(X, y, cluster, B = 200, seed = 1, coef = 1, beta0 = 0,
                    alpha = 0.05) {
  Xm <- .s03mat(X); yy <- .s03vec(y)
  n <- nrow(Xm); p <- ncol(Xm)
  groups <- as.character(cluster)
  if (n != length(yy) || n != length(groups))
    stop("boot_wild_cluster: X, y and cluster have different lengths")
  if (n <= p) stop("boot_wild_cluster: need more rows than columns")
  if (as.integer(B) < 2L) stop("boot_wild_cluster: need at least two replicates")
  j0 <- as.integer(coef)
  if (!(j0 >= 0L && j0 < p)) stop("boot_wild_cluster: coef out of range")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_wild_cluster: alpha must lie strictly between 0 and 1")
  bh <- .s03lstsq(Xm, yy)
  fit <- as.numeric(Xm %*% bh)
  res <- yy - fit
  keys <- unique(groups)
  G <- length(keys)
  vc <- .btwldcl_crve(Xm, res, groups, keys, n, p, TRUE)
  vc0 <- .btwldcl_crve(Xm, res, groups, keys, n, p, FALSE)
  se <- sqrt(vc[j0 + 1L])
  w <- if (se > 0) (bh[j0 + 1L] - as.numeric(beta0)) / se else NaN
  gid <- match(groups, keys)
  g <- .t1_lcg(seed)
  reps <- vector("list", as.integer(B)); ws <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B))) {
    v <- vapply(seq_len(G), function(k) if (g$unif() < 0.5) 1 else -1, 0)
    ys <- fit + res * v[gid]
    bb <- .s03lstsq(Xm, ys)
    rb <- ys - as.numeric(Xm %*% bb)
    vb <- .btwldcl_crve(Xm, rb, groups, keys, n, p, TRUE)
    sb <- if (vb[j0 + 1L] > 0) sqrt(vb[j0 + 1L]) else NaN
    reps[[b]] <- bb
    ws[b] <- if (!is.na(sb) && sb > 0) (bb[j0 + 1L] - bh[j0 + 1L]) / sb else NaN
  }
  good <- ws[!is.na(ws)]
  pv <- if (length(good)) (sum(abs(good) >= abs(w)) + 1) / (length(good) + 1) else NaN
  list(beta_b = reps, w_b = ws, beta_hat = bh, se_cluster = se, w = w,
       p_value = pv, reject = if (!is.na(pv) && pv < a) 1 else 0,
       vcov_cluster = vc, vcov_cluster0 = vc0, G = G, n = n, p = p,
       B = as.integer(B), estimate = bh[j0 + 1L],
       method = "Cameron, Gelbach and Miller (2008) Rev. Econ. Statist. 90(3):414-427")
}

#' @noRd
.btwldcl_crve <- function(Xm, res, groups, keys, n, p, corrected) {
  XtXinv <- .btres_xtxinv(Xm, p)
  G <- length(keys)
  meat <- matrix(0, p, p)
  for (k in keys) {
    sel <- groups == k
    sc <- as.numeric(crossprod(Xm[sel, , drop = FALSE], res[sel]))
    meat <- meat + (sc %o% sc)
  }
  cc <- 1
  if (isTRUE(corrected)) {
    if (G < 2L) stop("boot_wild_cluster: need at least two clusters")
    cc <- (G / (G - 1)) * ((n - 1) / (n - p))
  }
  diag(XtXinv %*% (meat * cc) %*% XtXinv)
}
