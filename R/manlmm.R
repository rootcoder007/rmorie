# SPDX-License-Identifier: AGPL-3.0-or-later
#' Borrow strength across a whole network of pairwise comparisons
#'
#' Two treatments never compared head to head still have a comparison
#' implied by the trials each has against a common third: the consistency
#' assumption turns a set of unconnected pairwise meta-analyses into one
#' model with \code{T - 1} free parameters. What is bought is a comparison
#' that no trial made; what is risked is that the assumption is false,
#' which is why the residual heterogeneity is reported alongside.
#'
#' Formula: \code{y_i = d_{t2(i)} - d_{t1(i)} + u_i + e_i} with \code{d}
#' of the reference treatment fixed at zero, \code{Var(u) = tau^2} common
#' across contrasts, weights \code{1/(v_i + tau^2)} -- Salanti et al.
#' (2008) Section 3.
#'
#' @param yi Contrast estimates: the comparator against the baseline of
#'   the same row of \code{design}.
#' @param vi Their sampling variances, strictly positive.
#' @param design Baseline and comparator treatment labels, n by 2.
#' @return List with \code{theta}, \code{se_theta}, \code{ranks},
#'   \code{tau2}, \code{QE}, \code{treatments}, \code{n}, \code{T}.
#' @references Salanti, G., Higgins, J. P. T., Ades, A. E. and Ioannidis,
#'   J. P. A. (2008). Statistical Methods in Medical Research
#'   17(3):279-301. \doi{10.1177/0962280207080643}.
#' @export
Manlmm <- function(yi, vi, design) {
  y <- as.numeric(yi); v <- as.numeric(vi); n <- length(y)
  if (n == 0L) stop("no studies")
  if (length(v) != n) stop("yi and vi must have equal length")
  if (any(v <= 0)) stop("sampling variances must be strictly positive")
  nd <- .ma_net_design(design)
  X <- nd$X; treats <- nd$treats; T <- nd$T
  if (nrow(X) != n) stop("design must have one row per study")
  p <- T - 1L
  w0 <- 1 / v
  f0 <- .ma_wls(X, y, w0)
  resid <- y - as.numeric(X %*% f0$beta)
  QE <- sum(w0 * resid^2)
  df <- n - p
  denom <- 0
  if (df > 0) {
    A2 <- crossprod(X * w0^2, X)
    denom <- sum(w0) - sum(diag(f0$cov %*% A2))
  }
  tau2 <- if (denom > 0) max(0, (QE - df) / denom) else 0
  f1 <- .ma_wls(X, y, 1 / (v + tau2))
  theta <- c(0, f1$beta)
  dg <- diag(f1$cov)
  se <- c(0, ifelse(dg > 0, sqrt(pmax(dg, 0)), NA_real_))
  ranks <- rank(theta, ties.method = "first")
  .t1_result(theta = theta, se_theta = se, ranks = ranks, tau2 = tau2,
             QE = QE, treatments = treats, n = n, T = T,
             method = "Network meta-analysis on contrasts")
}
