# R arm of funCA -- functional canonical analysis for two square-integrable
# processes, restricted to the leading functional principal components.
#
# He, G., Muller, H.-G. & Wang, J.-L. (2003) "Functional canonical analysis
# for square integrable stochastic processes", Journal of Multivariate
# Analysis 85(1), 54-77, doi:10.1016/S0047-259X(02)00056-8.
#
# Mirrors src/morie/fn/funCA.py. Unrestricted functional CCA is ill-posed --
# the supremum is 1 for almost any pair of processes -- so the weight
# functions are confined to the span of the leading FPCs of each process and
# the truncation (p, q) is reported with the correlations.

.funCA_EPS <- 1e-12

.funCA_grid_weights <- function(n_t) {
  if (n_t < 2L) return(1.0)
  h <- 1.0 / (n_t - 1L)
  w <- rep(h, n_t); w[1L] <- 0.5 * h; w[n_t] <- 0.5 * h
  w
}

.funCA_fpca <- function(C, w, n_keep) {
  T <- length(w)
  rw <- sqrt(w)
  Cw <- outer(rw, rw) * C
  ev <- eigen(Cw, symmetric = TRUE)
  ord <- order(ev$values, decreasing = TRUE)
  lam <- pmax(ev$values[ord], 0.0)
  U <- ev$vectors[, ord, drop = FALSE]
  denom <- ifelse(rw > .funCA_EPS, rw, 1.0)
  phi <- U / denom
  # sign is arbitrary; pin it so the reported weights are reproducible
  for (j in seq_len(ncol(phi))) {
    top <- which.max(abs(phi[, j]))
    if (phi[top, j] < 0) phi[, j] <- -phi[, j]
  }
  list(lam = lam[seq_len(n_keep)],
       phi = phi[, seq_len(n_keep), drop = FALSE], all = lam)
}

.funCA_sym_inv_sqrt <- function(M) {
  ev <- eigen(M, symmetric = TRUE)
  d <- ev$values
  V <- ev$vectors
  inv <- ifelse(d > .funCA_EPS, 1.0 / sqrt(pmax(d, .funCA_EPS)), 0.0)
  V %*% diag(inv, nrow = length(inv)) %*% t(V)
}

#' morie_funCA_functional_cca
#'
#' Part of the funCA_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param Y See Usage.
#' @param p Defaults to \code{NULL}.
#' @param q Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{correlations}, \code{weights_x}, \code{weights_y}, \code{variates_x}, \code{variates_y}, \code{p}, \code{q}, \code{explained_x}, \code{explained_y}, \code{eigenvalues_x}, \code{eigenvalues_y}, \code{n}, \code{method}, \code{note}.
#' @export
morie_funCA_functional_cca <- function(X, Y, p = NULL, q = NULL) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  Ym <- as.matrix(Y); storage.mode(Ym) <- "double"
  n <- nrow(Xm)
  if (n == 0L || nrow(Ym) != n)
    stop("funCA: X and Y must hold the same number of curves")
  if (n < 3L)
    stop("funCA: canonical analysis needs at least three paired curves")
  T <- ncol(Xm); S <- ncol(Ym)
  wx <- .funCA_grid_weights(T); wy <- .funCA_grid_weights(S)

  xbar <- colSums(Xm) / n; ybar <- colSums(Ym) / n
  Xc <- sweep(Xm, 2L, xbar, "-"); Yc <- sweep(Ym, 2L, ybar, "-")
  Cx <- crossprod(Xc) / n
  Cy <- crossprod(Yc) / n

  pick <- function(lam_all, want, cap) {
    tot <- sum(lam_all)
    if (tot <= .funCA_EPS) stop("funCA: a process carries no variation")
    # never form a component the data cannot support: a direction with a
    # numerically zero eigenvalue is noise, and the arms would disagree on
    # that noise at around 1e-9
    rank <- sum(lam_all > .funCA_EPS * tot)
    cap <- max(1L, min(cap, rank))
    if (!is.null(want)) return(max(1L, min(as.integer(want), cap)))
    run <- 0.0; kk <- cap
    for (j in seq_along(lam_all)) {
      run <- run + lam_all[j] / tot
      if (run >= 0.95) { kk <- j; break }
    }
    max(1L, min(kk, cap))
  }

  ax <- .funCA_fpca(Cx, wx, T)
  ay <- .funCA_fpca(Cy, wy, S)
  pp <- pick(ax$all, p, min(T, n - 1L))
  qq <- pick(ay$all, q, min(S, n - 1L))
  fx <- .funCA_fpca(Cx, wx, pp)
  fy <- .funCA_fpca(Cy, wy, qq)

  xi <- Xc %*% (fx$phi * wx)
  eta <- Yc %*% (fy$phi * wy)

  Sxx <- crossprod(xi) / n
  Syy <- crossprod(eta) / n
  Sxy <- crossprod(xi, eta) / n
  diag(Sxx) <- diag(Sxx) + .funCA_EPS
  diag(Syy) <- diag(Syy) + .funCA_EPS

  Rx <- .funCA_sym_inv_sqrt(Sxx)
  Ry <- .funCA_sym_inv_sqrt(Syy)
  M <- Rx %*% Sxy %*% Ry

  MMt <- M %*% t(M)
  ev <- eigen(MMt, symmetric = TRUE)
  ord <- order(ev$values, decreasing = TRUE)
  r <- min(pp, qq)
  corrs <- pmin(1.0, sqrt(pmax(ev$values[ord][seq_len(r)], 0.0)))

  weights_x <- list(); weights_y <- list()
  var_x <- list(); var_y <- list()
  for (j in seq_len(r)) {
    u <- ev$vectors[, ord[j]]
    a_coef <- as.numeric(Rx %*% u)
    wxj <- as.numeric(fx$phi %*% a_coef)
    Mtu <- as.numeric(t(M) %*% u)
    nrm <- sqrt(sum(Mtu ^ 2))
    v_coef <- if (nrm > .funCA_EPS) Mtu / nrm else rep(0.0, length(Mtu))
    b_coef <- as.numeric(Ry %*% v_coef)
    wyj <- as.numeric(fy$phi %*% b_coef)
    top <- which.max(abs(wxj))
    if (wxj[top] < 0) { wxj <- -wxj; wyj <- -wyj }
    weights_x[[j]] <- wxj
    weights_y[[j]] <- wyj
    var_x[[j]] <- as.numeric(Xc %*% (wxj * wx))
    var_y[[j]] <- as.numeric(Yc %*% (wyj * wy))
  }

  tx <- sum(ax$all); ty <- sum(ay$all)
  list(
    estimate = corrs,
    correlations = corrs,
    weights_x = weights_x,
    weights_y = weights_y,
    variates_x = var_x,
    variates_y = var_y,
    p = as.integer(pp),
    q = as.integer(qq),
    explained_x = if (tx > .funCA_EPS) sum(fx$lam) / tx else 0.0,
    explained_y = if (ty > .funCA_EPS) sum(fy$lam) / ty else 0.0,
    eigenvalues_x = fx$lam,
    eigenvalues_y = fy$lam,
    n = as.integer(n),
    method = paste0("functional canonical analysis restricted to the ",
                    "leading functional principal components (He, Muller ",
                    "& Wang 2003)"),
    note = paste0("unrestricted functional CCA is ill-posed -- the ",
                  "supremum is 1 for almost any pair of processes -- so ",
                  "the correlations are only interpretable against the ",
                  "truncation p, q that produced them")
  )
}

.funCA_cheatsheet <- function() {
  paste0("funCA: morie_funCA_functional_cca(X, Y, p, q) -> canonical ",
         "correlations between two sets of curves, restricted to the ",
         "leading FPCs (He, Muller & Wang 2003, J. Multivar. Anal. 85(1), ",
         "54-77)")
}

morie_funCA <- morie_funCA_functional_cca
