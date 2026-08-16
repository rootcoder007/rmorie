# Network psychometrics via the graphical lasso.
# Source: Hastie, Tibshirani & Friedman (2009), ESL 2nd ed.,
# Sec. 17.3.2, Algorithm 17.2, Eqs. 17.22-17.27 (local PDF:
# WD_BLACK/library/pdf/BookAdvanced_elementsofstatisticallearning.pdf);
# Friedman, Hastie & Tibshirani (2008), Biostatistics 9, 432-441;
# Epskamp, Borsboom & Fried (2018), Behav. Res. Methods 50, 195-212.
# Mirrors Python morie.fn.netcms exactly (same sweep order, same
# warm-started coordinate descent).

#' .netcms_soft
#'
#' A step of the netcms_native implementation. Called by \code{.netcms_lasso}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; passed to \code{abs}.
#' @param t Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.netcms_soft <- function(x, t) sign(x) * pmax(abs(x) - t, 0)

#' .netcms_lasso
#'
#' A step of the netcms_native implementation. Called by \code{morie_netcms}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param V A matrix; indexed by row and column.
#' @param s12 A vector; its length is taken and its elements indexed.
#' @param lam Passed to \code{.netcms_soft}.
#' @param beta A vector; indexed elementwise.
#' @param tol Passed to \code{<}.
#' @param maxit A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.netcms_lasso <- function(V, s12, lam, beta, tol, maxit) {
  p1 <- length(s12)
  for (it in seq_len(maxit)) {
    delta <- 0
    for (j in seq_len(p1)) {
      r <- s12[j] - sum(V[, j] * beta) + V[j, j] * beta[j]
      new <- .netcms_soft(r, lam) / V[j, j]
      delta <- max(delta, abs(new - beta[j]))
      beta[j] <- new
    }
    if (delta < tol) break
  }
  beta
}

#' Gaussian graphical model by the graphical lasso
#'
#' ESL Algorithm 17.2: W = S + lambda I with fixed diagonal; cycle
#' over variables solving the modified lasso
#' W11 beta - s12 + lambda Sign(beta) = 0 by coordinate descent
#' (Eq. 17.26, soft threshold Eq. 17.27), update w12 = W11 beta; then
#' theta_22 = 1/(w22 - w12' beta), theta_12 = -beta theta_22.  The
#' nonzero pattern of Theta is the conditional-independence
#' (partial-correlation) network of network psychometrics.
#'
#' @param data Optional data matrix (sample covariance used as S).
#' @param S Optional covariance matrix (overrides data).
#' @param lam L1 penalty lambda >= 0.
#' @param tol,maxit Coordinate-descent controls.
#' @return A list with elements \code{precision},
#'   \code{covariance_fit}, \code{adjacency},
#'   \code{partial_correlations}, \code{n_edges}, \code{lam},
#'   \code{method}.
#' @references Hastie, T., Tibshirani, R. and Friedman, J. (2009).
#'   The Elements of Statistical Learning, 2nd ed. Springer,
#'   Sec. 17.3.2.  Friedman, J., Hastie, T. and Tibshirani, R.
#'   (2008). Biostatistics, 9, 432-441.  Epskamp, S., Borsboom, D.
#'   and Fried, E. I. (2018). Behavior Research Methods, 50, 195-212.
#' @export
morie_netcms <- function(data = NULL, S = NULL, lam = 0.1,
                         tol = 1e-8, maxit = 500) {
  if (is.null(S)) {
    if (is.null(data)) stop("provide data or S")
    X <- as.matrix(data)
    n <- nrow(X)
    mu <- colMeans(X)
    Xc <- sweep(X, 2, mu)
    S <- t(Xc) %*% Xc / n
  } else {
    S <- as.matrix(S)
  }
  p <- ncol(S)
  lam <- as.numeric(lam)
  if (lam < 0) stop("lam must be non-negative")
  W <- S + diag(lam, p)
  betas <- vector("list", p)
  for (j in seq_len(p)) betas[[j]] <- numeric(p - 1)
  for (cycle in seq_len(maxit)) {
    W_old <- W
    for (j in seq_len(p)) {
      idx <- setdiff(seq_len(p), j)
      V <- W[idx, idx, drop = FALSE]
      s12 <- S[idx, j]
      beta <- .netcms_lasso(V, s12, lam, betas[[j]], tol, maxit)
      betas[[j]] <- beta
      w12 <- as.numeric(V %*% beta)
      W[idx, j] <- w12
      W[j, idx] <- w12
    }
    if (max(abs(W - W_old)) < tol) break
  }
  Theta <- matrix(0, p, p)
  for (j in seq_len(p)) {
    idx <- setdiff(seq_len(p), j)
    beta <- betas[[j]]
    t22 <- 1 / (W[j, j] - sum(W[idx, j] * beta))
    Theta[j, j] <- t22
    Theta[idx, j] <- -beta * t22
  }
  Theta <- (Theta + t(Theta)) / 2
  adj <- (abs(Theta) > 1e-10) * 1
  diag(adj) <- 0
  pcor <- -Theta / sqrt(outer(diag(Theta), diag(Theta)))
  diag(pcor) <- 1
  list(precision = Theta,
       covariance_fit = W,
       adjacency = adj,
       partial_correlations = pcor,
       n_edges = sum(adj[upper.tri(adj)]),
       lam = lam,
       method = "graphical lasso (ESL Alg. 17.2; Friedman 2008)")
}
