#' Spatial lag of X (SLX) model diagnostics
#'
#' Front ends for the SLX model \eqn{y = X\beta + WX_*\theta + e} fitted by
#' OLS on \eqn{Z = (X, WX_*)} (\eqn{X_*} the non-constant columns); R arm of
#' the Python modules \code{morie.fn.slx*}. \code{Slxwx}: \eqn{WX_*} with
#' column means and correlations. \code{Slxflt}: the local deviation
#' \eqn{(I - W)X}. \code{Slxres}: Moran's I of residuals, exact test when
#' the design \eqn{Z} is given. \code{Slxboot}: residual-bootstrap
#' percentile intervals for \eqn{\theta} (Philox resampling, type-7
#' quantiles).
#'
#' @param X Design matrix including any intercept column (for
#'   \code{Slxres}, optionally the SLX design).
#' @param W Spatial weights matrix.
#' @param resid Model residuals.
#' @param y Response.
#' @param B Bootstrap replicates.
#' @param seed Philox seed.
#' @param level Confidence level.
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result's \code{extra}.
#' @references Halleck Vega, S. and Elhorst, J. P. (2015). The SLX model.
#'   Journal of Regional Science 55, 339-363.
#'
#'   Efron, B. and Tibshirani, R. J. (1993). An Introduction to the
#'   Bootstrap. Chapman and Hall.
#' @examples
#' W <- matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3)
#' X <- cbind(1, c(0.2, 0.9, 0.4))
#' Slxwx(X, W)$means
#' Slxflt(X, W)$filtered
#' @export
Slxwx <- function(X, W) {
  Sdmwx(X, W)
}

#' @rdname Slxwx
#' @export
Slxflt <- function(X, W) {
  X <- as.matrix(X)
  list(statistic = ncol(X), filtered = unname(X - as.matrix(W) %*% X))
}

#' @rdname Slxwx
#' @export
Slxres <- function(resid, W, X = NULL) {
  .sxd_resmoran(resid, W, X)
}

#' @rdname Slxwx
#' @export
Slxboot <- function(y, X, W, B = 99, seed = 0, level = 0.95) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  d <- .sd_durbin(X, W)
  Z <- d$Z
  n <- length(y)
  p <- ncol(X)
  ZtZ <- crossprod(Z)
  ols <- function(v) as.vector(solve(ZtZ, crossprod(Z, v)))
  g <- ols(y)
  fit <- as.vector(Z %*% g)
  e <- y - fit
  e <- e - sum(e) / n
  u <- .morie_random_uniform(B * n, seed = seed)
  draws <- matrix(0, B, length(d$lag))
  for (b in seq_len(B)) {
    idx <- pmin(floor(u[(b - 1) * n + seq_len(n)] * n), n - 1) + 1
    draws[b, ] <- ols(fit + e[idx])[-seq_len(p)]
  }
  a <- (1 - level) / 2
  theta <- g[-seq_len(p)]
  list(statistic = theta[1], theta = theta,
       ci_lower = apply(draws, 2, .sxd_q7, q = a), ci_upper = apply(draws, 2, .sxd_q7, q = 1 - a),
       se_boot = apply(draws, 2, stats::sd), lagged_columns = d$lag - 1L, draws = draws)
}
