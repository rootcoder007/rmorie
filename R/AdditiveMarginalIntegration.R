#' Additive model by marginal integration
#'
#' Linton and Nielsen (1995): the pilot is the full-dimensional
#' Nadaraya-Watson estimate \eqn{\hat g} with a product Gaussian kernel, and
#' component \eqn{j} averages it over the observed values of the other
#' covariates, \eqn{\hat m_j(t) = n^{-1} \sum_k \hat g(t, X_{k,-j}) - \bar Y},
#' evaluated on an equally spaced grid. Fitted values interpolate each
#' component linearly. Identical to the Python arm \code{morie.fn.admod.admod}.
#'
#' @param Y Numeric outcome vector.
#' @param X Covariate matrix (a vector is one covariate).
#' @param bandwidth Kernel bandwidth used in every direction; default
#'   \eqn{1.06 \bar s n^{-1/(4+p)}} with \eqn{\bar s} the mean of the column
#'   standard deviations (divisor n).
#' @param grid_size Grid points per component.
#' @return A list with \code{intercept}, \code{components} (each a list with
#'   \code{x_grid} and \code{m_hat}), \code{residuals}, \code{bandwidth},
#'   \code{n}, \code{p} and \code{method}.
#' @references Linton, O. and Nielsen, J. P. (1995). A kernel method of
#'   estimating structured nonparametric regression based on marginal
#'   integration. Biometrika 82, 93-100.
#' @examples
#' X <- cbind(c(0, 1, 2, 3, 4, 5), c(1, 0, 2, 1, 3, 2))
#' admod(c(1, 1.5, 3.2, 3.9, 6.1, 6), X, bandwidth = 1, grid_size = 3)$components[[1]]$m_hat
#' @export
admod <- function(Y, X, bandwidth = NULL, grid_size = 50) {
  y <- as.numeric(Y)
  X <- if (is.null(dim(X))) matrix(as.numeric(X), ncol = 1) else as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n) stop(sprintf("Y length %d != X rows %d.", length(y), n))
  if (is.null(bandwidth)) {
    sds <- apply(X, 2, function(v) sqrt(sum((v - sum(v) / n)^2) / n))
    bandwidth <- 1.06 * mean(sds) * n^(-1 / (4 + p))
  }
  h <- bandwidth
  if (!(h > 0)) stop("bandwidth must be positive")
  mu <- sum(y) / n
  comps <- vector("list", p)
  for (j in seq_len(p)) {
    S <- matrix(0, n, n)
    for (d in setdiff(seq_len(p), j)) S <- S + (outer(X[, d], X[, d], "-") / h)^2
    W <- exp(-0.5 * S)
    col <- X[, j]
    grid <- if (grid_size > 1) {
      min(col) + (max(col) - min(col)) * (0:(grid_size - 1)) / (grid_size - 1)
    } else {
      min(col)
    }
    mh <- vapply(grid, function(t) {
      A <- exp(-0.5 * ((t - col) / h)^2)
      num <- as.numeric(W %*% (A * y))
      den <- as.numeric(W %*% A)
      g <- ifelse(den > 1e-300, num / den, mu)
      sum(g) / n - mu
    }, 0)
    comps[[j]] <- list(x_grid = grid, m_hat = mh)
  }
  fit <- rep(mu, n)
  for (j in seq_len(p)) {
    fit <- fit + stats::approx(comps[[j]]$x_grid, comps[[j]]$m_hat, xout = X[, j], rule = 2, ties = "ordered")$y
  }
  list(intercept = mu, components = comps, residuals = y - fit, bandwidth = h, n = n, p = p,
       method = "AdditiveModel_MarginalIntegration")
}
