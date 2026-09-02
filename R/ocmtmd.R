# SPDX-License-Identifier: AGPL-3.0-or-later
#' Outcome-model diagnostic for a marginal structural model
#'
#' A marginal structural model is only as good as the outcome model
#' behind it, and the failure it hides is systematic: if \code{Q} is
#' misspecified in a way that correlates with treatment, the residuals
#' carry treatment signal the model should have absorbed. The
#' diagnostic regresses the standardized residuals on the treatment,
#' \code{r = (y - Q) / sd(y - Q)}, \code{r ~ b0 + b1 A}, so \code{b1}
#' near zero with a small t-statistic is the pass condition.
#'
#' The second failure mode is the design rather than the model: without
#' overlap there is no data at some treatment levels and any weight is
#' an extrapolation. Crump et al. give the subsample worth analysing,
#' \code{alpha <= e(x) <= 1 - alpha}, with \code{alpha} the smallest
#' value satisfying \code{1 / (alpha (1 - alpha)) <=
#' 2 E\[1 / (e (1 - e)); alpha <= e <= 1 - alpha\]}, solved here by a
#' deterministic ascending scan over the observed propensities. The
#' propensity is fitted by logistic regression of \code{A} on \code{H}.
#'
#' @param y Observed outcome.
#' @param A Binary treatment, 0/1.
#' @param H Covariate history, no intercept column; one is added.
#' @param Q Fitted values of the outcome model; if \code{NULL}, the
#'   outcome is regressed on \code{\[1, A, H\]} by least squares.
#' @return List with \code{estimate}, \code{b0}, \code{b1},
#'   \code{t_stat}, \code{resid_sd}, \code{mean_resid_treated},
#'   \code{mean_resid_control}, \code{alpha_crump}, \code{n_kept},
#'   \code{min_ps}, \code{max_ps}, \code{n}.
#' @references Crump, R. K., Hotz, V. J., Imbens, G. W. & Mitnik, O. A.
#'   (2009). Dealing with limited overlap in estimation of average
#'   treatment effects. Biometrika, 96(1), 187-199.
#'   doi:10.1093/biomet/asn055 Robins, J. M., Hernan, M. A. &
#'   Brumback, B. (2000). Marginal structural models and causal
#'   inference in epidemiology. Epidemiology, 11(5), 550-560.
#' @export
Ocmtmd <- function(y, A, H, Q = NULL) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("Ocmtmd: y is empty")
  Av <- as.numeric(A)
  if (length(Av) != n) stop("Ocmtmd: y and A have different lengths")
  if (!all(Av %in% c(0, 1))) stop("Ocmtmd: A must be binary 0/1")
  Hm <- as.matrix(H)
  if (nrow(Hm) != n) stop("Ocmtmd: H and y have different lengths")
  Qv <- if (is.null(Q)) .t1_lstsq(cbind(1, Av, Hm), yv)$fitted else as.numeric(Q)
  if (length(Qv) != n) stop("Ocmtmd: Q and y have different lengths")
  e <- yv - Qv
  sd <- stats::sd(e)
  if (is.na(sd) || sd <= 0) stop("Ocmtmd: residuals have zero spread")
  r <- e / sd
  fit <- .t1_lstsq(cbind(1, Av), r)
  s2 <- if (n > 2L) sum(fit$resid^2) / (n - 2) else NaN
  seb <- if (n > 2L && fit$xtxinv[2, 2] > 0) sqrt(s2 * fit$xtxinv[2, 2]) else NaN
  tstat <- if (!is.na(seb) && seb > 0) fit$beta[2] / seb else NaN
  nt <- sum(Av == 1)
  mrt <- if (nt > 0) sum(r[Av == 1]) / nt else NaN
  mrc <- if (n - nt > 0) sum(r[Av == 0]) / (n - nt) else NaN

  Hd <- cbind(1, Hm)
  gam <- .s03logit(Hd, Av)
  ps <- vapply(as.numeric(Hd %*% gam), .s03sigmoid, 0)
  cand <- sort(unique(pmin(ps, 1 - ps)))
  alpha <- 0
  for (a in cand) {
    if (a >= 0.5) next
    keep <- ps[ps >= a & ps <= 1 - a]
    if (!length(keep)) next
    rhs <- 2 * sum(1 / (keep * (1 - keep))) / length(keep)
    if (1 / (a * (1 - a)) <= rhs) { alpha <- a; break }
  }
  nkeep <- sum(ps >= alpha & ps <= 1 - alpha)
  .t1_result(estimate = fit$beta[2], b0 = fit$beta[1], b1 = fit$beta[2],
             t_stat = tstat, resid_sd = sd, mean_resid_treated = mrt,
             mean_resid_control = mrc, alpha_crump = alpha, n_kept = nkeep,
             min_ps = min(ps), max_ps = max(ps), n = n,
             method = "MSM outcome-model residual diagnostic with Crump overlap")
}
