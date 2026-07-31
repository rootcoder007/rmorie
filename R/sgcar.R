# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conditional autoregressive (CAR) model.
#'
#' Fits a Gaussian CAR by profiling the log-likelihood over the spatial
#' dependence parameter rho. With precision Q = D - rho W, where
#' D = diag(W 1):
#'
#'   beta(rho)  = (X' Q X)^-1 X' Q Z
#'   tau2(rho)  = r' Q r / n,  r = Z - X beta
#'   l(rho)     = 1/2 log|Q| - n/2 log(tau2) - n/2
#'
#' rho is searched on a grid over (0, 1); candidates that give a
#' non-positive tau2 or a non-positive-definite Q are skipped, and if no
#' candidate is admissible the fit falls back to OLS with rho = 0.
#'
#' @param Z Response, length n.
#' @param W Adjacency weights (n by n).
#' @param X Covariates (n by p); an intercept when NULL.
#' @return Named list: name, statistic (estimated rho), p_value (NULL),
#'   extra with `beta` and `tau2`.
#' @references Schabenberger & Gotway (2005), Sec 6.2.2.2, eqs
#'   (6.43)-(6.45), pp. 338-339; Besag (1974).
#' @examples
#' n <- 20
#' W <- matrix(0, n, n); W[cbind(1:(n - 1), 2:n)] <- 1; W <- W + t(W)
#' sgcar(rnorm(n), W)
#' @export
sgcar <- function(Z, W, X = NULL) {
  Z <- as.numeric(Z)
  W <- as.matrix(W)
  n <- length(Z)
  if (nrow(W) != n || ncol(W) != n) {
    stop("`W` must be ", n, " by ", n, " to match `Z`")
  }
  X <- if (is.null(X)) matrix(1, n, 1) else as.matrix(X)
  if (nrow(X) != n) stop("`X` must have one row per element of `Z`")

  D <- diag(rowSums(W), nrow = n)
  best_ll <- -Inf
  best_rho <- 0
  best_beta <- NULL
  best_tau2 <- 1

  for (rho in seq(0.01, 0.99, length.out = 30)) {
    Q <- D - rho * W
    # singularity guard: skip this rho rather than erroring out
    if (inherits(try(solve(Q), silent = TRUE), "try-error")) next
    XtQX <- crossprod(X, Q %*% X)
    beta <- try(solve(XtQX, crossprod(X, Q %*% Z)), silent = TRUE)
    if (inherits(beta, "try-error")) next
    beta <- as.numeric(beta)
    resid <- Z - as.numeric(X %*% beta)
    tau2 <- as.numeric(crossprod(resid, Q %*% resid) / n)
    if (tau2 <= 0) next
    dt <- determinant(Q, logarithm = TRUE)
    if (dt$sign <= 0) next
    ll <- 0.5 * as.numeric(dt$modulus) - 0.5 * n * log(tau2) - 0.5 * n
    if (ll > best_ll) {
      best_ll <- ll; best_rho <- rho; best_beta <- beta; best_tau2 <- tau2
    }
  }

  if (is.null(best_beta)) {
    best_beta <- as.numeric(qr.solve(X, Z))
  }
  list(name = "conditional_autoregressive", statistic = best_rho,
       p_value = NULL, extra = list(beta = best_beta, tau2 = best_tau2))
}
