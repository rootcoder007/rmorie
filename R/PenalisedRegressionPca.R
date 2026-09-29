#' Lasso, elastic net and principal component front ends
#'
#' R arm of \code{morie.fn.elnetr}, \code{lasr} and \code{pcaprx}.
#' \code{Elnetr} minimises \eqn{(1/(2n))||y - b_0 - Xb||^2 + \alpha\rho||b||_1 +
#' \alpha(1-\rho)||b||^2/2} by cyclic coordinate descent on centred columns
#' (soft-threshold updates, sweeps stop when the largest coefficient change is
#' below 1e-8); \code{Lasr} is the case \eqn{\rho = 1}. \code{Pcaprx}
#' eigendecomposes the sample covariance (or correlation, when
#' \code{standardize = TRUE}) matrix, orders components by decreasing eigenvalue
#' and signs each so its largest-magnitude loading is positive.
#'
#' @param X Numeric matrix (n by p).
#' @param y Response vector.
#' @param alpha Overall penalty.
#' @param l1_ratio L1 share of the penalty (1 = lasso, 0 = ridge).
#' @param fit_intercept Centre and fit an unpenalised intercept.
#' @param max_iter Maximum number of coordinate-descent sweeps.
#' @param n_components Components retained (all when \code{NULL}).
#' @param standardize Scale columns by their sample standard deviation first.
#' @return A named list (the Python result's payload).
#' @references Tibshirani, R. (1996). Regression shrinkage and selection via the lasso.
#'   Journal of the Royal Statistical Society B 58(1), 267-288.
#'
#'   Zou, H. and Hastie, T. (2005). Regularization and variable selection via the elastic net.
#'   Journal of the Royal Statistical Society B 67(2), 301-320.
#'
#'   Friedman, J., Hastie, T. and Tibshirani, R. (2010). Regularization paths for generalized
#'   linear models via coordinate descent. Journal of Statistical Software 33(1), 1-22.
#'
#'   Jolliffe, I. T. (2002). Principal Component Analysis, 2nd ed. Springer.
#' @examples
#' X <- cbind(1:6, c(0.5, -1, 0.2, 1.5, -0.3, 0.8))
#' y <- c(1.1, 2.3, 2.8, 4.4, 4.9, 6.2)
#' Elnetr(X, y, alpha = 0.1, l1_ratio = 0.5)$coef
#' Lasr(X, y, alpha = 0.2)$coef
#' Pcaprx(cbind(X, y))$explained_variance
#' @export
Elnetr <- function(X, y, alpha = 1, l1_ratio = 0.5, fit_intercept = TRUE, max_iter = 10000) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X)
  d <- ncol(X)
  if (fit_intercept) {
    xm <- colMeans(X)
    ym <- mean(y)
    Xc <- sweep(X, 2, xm)
    yc <- y - ym
  } else {
    xm <- rep(0, d)
    ym <- 0
    Xc <- X
    yc <- y
  }
  col_ss <- colSums(Xc^2)
  b <- rep(0, d)
  resid <- yc
  l1 <- alpha * l1_ratio * n
  l2 <- alpha * (1 - l1_ratio) * n
  for (sweep_i in seq_len(max_iter)) {
    delta <- 0
    for (j in seq_len(d)) {
      if (col_ss[j] == 0) next
      rho <- sum(Xc[, j] * resid) + b[j] * col_ss[j]
      new <- if (rho > l1) (rho - l1) / (col_ss[j] + l2) else if (rho < -l1) (rho + l1) / (col_ss[j] + l2) else 0
      if (new != b[j]) {
        diff <- new - b[j]
        resid <- resid - diff * Xc[, j]
        delta <- max(delta, abs(diff))
        b[j] <- new
      }
    }
    if (delta < 1e-8) break
  }
  intercept <- if (fit_intercept) ym - sum(b * xm) else 0
  fitted <- intercept + drop(X %*% b)
  r2 <- 1 - sum((y - fitted)^2) / sum((y - mean(y))^2)
  list(coef = b, intercept = intercept, r2 = r2, alpha = alpha, l1_ratio = l1_ratio,
       nonzero = sum(abs(b) > 1e-10))
}

#' @rdname Elnetr
#' @export
Lasr <- function(X, y, alpha = 1, fit_intercept = TRUE, max_iter = 10000) {
  r <- Elnetr(X, y, alpha = alpha, l1_ratio = 1, fit_intercept = fit_intercept, max_iter = max_iter)
  r$l1_ratio <- NULL
  r
}

#' @rdname Elnetr
#' @export
Pcaprx <- function(X, n_components = NULL, standardize = TRUE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  if (standardize) {
    sd <- apply(X, 2, stats::sd)
    sd[sd == 0] <- 1
    X <- sweep(sweep(X, 2, colMeans(X)), 2, sd, "/")
  }
  Xc <- sweep(X, 2, colMeans(X))
  e <- eigen(crossprod(Xc) / (n - 1), symmetric = TRUE)
  k <- if (is.null(n_components)) p else n_components
  ord <- order(-e$values)[seq_len(k)]
  V <- e$vectors[, ord, drop = FALSE]
  for (c in seq_len(k)) if (V[which.max(abs(V[, c])), c] < 0) V[, c] <- -V[, c]
  ev <- e$values[ord]
  ratio <- ev / sum(e$values)
  cum <- cumsum(ratio)
  first_at <- function(level) if (any(cum >= level)) which(cum >= level)[1] else length(cum)
  list(components = t(V), scores = Xc %*% V, explained_variance = ev, explained_variance_ratio = ratio,
       n_for_80 = first_at(0.8), n_for_95 = first_at(0.95))
}
