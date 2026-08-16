# Regression quantiles (exact, basis enumeration).
# Source: Koenker & Bassett (1978), Econometrica 46(1), 33-50,
# Sec. 2 (defining minimization) and Theorem 3.1 (basic solutions
# b* = X(h)^-1 y(h))
# (fetched-wave3/Koenker-RegressionQuantiles-1978.pdf); Koenker
# (2005), Quantile Regression, CUP (delivered).  Mirrors Python
# morie.fn.quanrg exactly.

#' .quanrg_loss
#'
#' A step of the quanrg_native implementation. Called by \code{morie_quanrg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param res Numeric; combined arithmetically in the body.
#' @param theta Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.quanrg_loss <- function(res, theta) {
  sum(ifelse(res >= 0, theta * res, (theta - 1) * res))
}

#' Exact regression quantile (Koenker-Bassett)
#'
#' Enumerates the basic solutions of Theorem 3.1 (exact fits through
#' K observations) and returns the minimizer of the asymmetric
#' absolute loss; exact for K <= 3 coefficients.
#'
#' @param y Numeric responses.
#' @param X Optional regressor matrix WITHOUT intercept (added
#'   automatically); default location model = sample quantile.
#' @param theta Quantile level in (0, 1).
#' @return A list with elements \code{coefficients},
#'   \code{objective}, \code{basis} (1-based row indices),
#'   \code{n_bases_checked}, \code{theta}, \code{method}.
#' @references Koenker, R. and Bassett, G. (1978). Regression
#'   quantiles. Econometrica, 46(1), 33-50.  Koenker, R. (2005).
#'   Quantile Regression. Cambridge University Press.
#' @export
morie_quanrg <- function(y, X = NULL, theta = 0.5) {
  yv <- as.numeric(y)
  n <- length(yv)
  Xm <- if (is.null(X)) matrix(1, n, 1) else cbind(1, as.matrix(X))
  if (nrow(Xm) != n) stop("X must have one row per observation")
  k <- ncol(Xm)
  theta <- as.numeric(theta)
  if (theta <= 0 || theta >= 1) stop("theta must be in (0, 1)")
  if (k > 3) stop("basis enumeration supports at most 3 coefficients")
  if (n < k + 1) stop("need more observations than coefficients")
  combs <- utils::combn(n, k)
  best_obj <- Inf
  best_beta <- NULL
  best_h <- NULL
  checked <- 0L
  for (ci in seq_len(ncol(combs))) {
    h <- combs[, ci]
    A <- Xm[h, , drop = FALSE]
    beta <- tryCatch({
      piv <- abs(det(A))
      if (piv < 1e-12) NULL else solve(A, yv[h])
    }, error = function(e) NULL)
    if (is.null(beta)) next
    checked <- checked + 1L
    res <- yv - as.numeric(Xm %*% beta)
    obj <- .quanrg_loss(res, theta)
    if (obj < best_obj - 1e-15) {
      best_obj <- obj
      best_beta <- as.numeric(beta)
      best_h <- h
    }
  }
  if (is.null(best_beta)) stop("design matrix has no nonsingular basis")
  list(coefficients = best_beta, objective = best_obj,
       basis = best_h, n_bases_checked = checked, theta = theta,
       method = "exact regression quantile (K&B 1978 Thm 3.1 bases)")
}
