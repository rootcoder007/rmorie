# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nakagawa-Schielzeth marginal R-squared for a random-intercept LMM
#'
#' Formula (Nakagawa & Schielzeth 2013, eq. 26):
#' \code{R2_marginal = sigma2_f / (sigma2_f + sigma2_l + sigma2_e)},
#' where \code{sigma2_f} is the variance of the fixed-effect fitted
#' values, \code{sigma2_l} the between-cluster variance and
#' \code{sigma2_e} the residual variance. The conditional R-squared
#' adds \code{sigma2_l} to the numerator.
#'
#' Determinism: the variance components are the one-way random-effects
#' ANOVA moment estimators on the OLS residuals, not an iterative REML
#' fit, so both language arms land on the same numbers exactly.
#' \code{sigma2_e = MSW}, \code{sigma2_l = max(0, (MSB - MSW) / n0)},
#' \code{n0 = (N - sum n_i^2 / N) / (k - 1)}.
#'
#' @param y Response, length N.
#' @param X Fixed-effects design WITHOUT an intercept column.
#' @param Z Ignored; a random-intercept model has \code{Z} equal to the
#'   cluster indicator, which \code{cluster} already supplies.
#' @param cluster Grouping label per observation.
#' @return List with \code{estimate}, \code{r2_marginal},
#'   \code{r2_conditional}, \code{sigma2_f}, \code{sigma2_l},
#'   \code{sigma2_e}, \code{icc}, \code{n}, \code{n_clusters}.
#' @references Nakagawa, S. & Schielzeth, H. (2013). A general and
#'   simple method for obtaining R^2 from generalized linear
#'   mixed-effects models. Methods in Ecology and Evolution, 4(2),
#'   133-142. doi:10.1111/j.2041-210x.2012.00261.x
#' @export
Niccgg <- function(y, X, Z = NULL, cluster = NULL) {
  yv <- as.numeric(y)
  N <- length(yv)
  if (N == 0L) stop("Niccgg: y is empty")
  Xm <- .t1_cbind1(X)
  if (nrow(Xm) != N) stop("Niccgg: X and y have different lengths")
  if (is.null(cluster)) stop("Niccgg: cluster labels are required")
  lab <- as.character(cluster)
  if (length(lab) != N) stop("Niccgg: cluster and y have different lengths")
  fit <- .t1_lstsq(Xm, yv)
  s2f <- stats::var(fit$fitted)
  resid <- fit$resid
  groups <- unique(lab)
  k <- length(groups)
  if (k < 2L) stop("Niccgg: need at least two clusters")
  if (k >= N) stop("Niccgg: need more observations than clusters")
  j <- match(lab, groups)
  ni <- as.numeric(tabulate(j, nbins = k))
  si <- as.numeric(tapply(resid, factor(j, levels = seq_len(k)), sum))
  gm <- sum(resid) / N
  msb <- sum(ni * (si / ni - gm)^2) / (k - 1)
  msw <- sum((resid - (si / ni)[j])^2) / (N - k)
  n0 <- (N - sum(ni * ni) / N) / (k - 1)
  s2l <- (msb - msw) / n0
  if (s2l < 0) s2l <- 0
  s2e <- msw
  tot <- s2f + s2l + s2e
  if (tot <= 0) stop("Niccgg: total variance is zero")
  .t1_result(estimate = s2f / tot, r2_marginal = s2f / tot,
             r2_conditional = (s2f + s2l) / tot,
             sigma2_f = s2f, sigma2_l = s2l, sigma2_e = s2e,
             icc = if ((s2l + s2e) > 0) s2l / (s2l + s2e) else NaN,
             n = N, n_clusters = k,
             method = "Nakagawa-Schielzeth marginal R^2 (random intercept)")
}
