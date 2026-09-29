#' E-values and the joint-significance mediation test
#'
#' R arm of \code{morie.fn.evalu}, \code{evaltw} and \code{jntmed}.
#' \code{Evalu}: the E-value \eqn{RR^* + \sqrt{RR^*(RR^* - 1)}},
#' \eqn{RR^* = \max(RR, 1/RR)}, of an observed risk ratio and of the confidence
#' limit closer to the null (1 when the interval covers 1). \code{Evaltw} is the
#' same with the three numbers given positionally. \code{Jntmed}: fits
#' \eqn{M = i_1 + aX} and \eqn{Y = i_2 + c'X + bM} by least squares and declares
#' mediation when both \eqn{a} and \eqn{b} are significant; the p-value is
#' \eqn{\max(p_a, p_b)}.
#'
#' @param RR,estimate Observed risk ratio (or a rare-outcome odds or hazard ratio).
#' @param ci_lower,ci_upper Confidence limits on the same scale (optional for \code{Evalu}).
#' @param rare_outcome Logical, recorded in the result (no transform is applied).
#' @param x,m,y Treatment, mediator and outcome vectors.
#' @param alpha Per-path significance level.
#' @return A named list (the Python result's fields).
#' @references VanderWeele, T. J. and Ding, P. (2017). Sensitivity analysis in observational
#'   research: introducing the E-value. Annals of Internal Medicine 167(4), 268-274.
#'
#'   MacKinnon, D. P., Lockwood, C. M., Hoffman, J. M., West, S. G. and Sheets, V. (2002).
#'   A comparison of methods to test mediation and other intervening variable effects.
#'   Psychological Methods 7(1), 83-104.
#' @examples
#' Evalu(2, ci_lower = 1.3, ci_upper = 3.1)$evalue_ci
#' Evaltw(0.5, 0.3, 0.8)$evalue
#' Jntmed(0:7, c(0.3, 1.1, 1.6, 3.4, 3.9, 5.2, 6.4, 6.8), c(1, 1.9, 3.1, 4.4, 4.8, 6.9, 7.2, 8.1))$p_value
#' @export
Evalu <- function(RR, ci_lower = NULL, ci_upper = NULL, rare_outcome = TRUE) {
  rr <- as.numeric(RR)
  if (rr <= 0) stop("RR must be positive, got ", rr)
  e <- function(r) {
    rs <- max(r, 1 / r)
    rs + sqrt(rs * (rs - 1))
  }
  e_ci <- NULL
  if (!is.null(ci_lower) || !is.null(ci_upper)) {
    if (is.null(ci_lower) || is.null(ci_upper)) stop("Supply both confidence limits or neither.")
    if (!(ci_lower > 0 && ci_lower <= ci_upper)) stop("Need 0 < ci_lower <= ci_upper")
    e_ci <- if (ci_lower <= 1 && ci_upper >= 1) 1 else e(if (ci_lower > 1) ci_lower else ci_upper)
  }
  ep <- e(rr)
  list(evalue = ep, estimate = ep, evalue_ci = e_ci, rr = rr, rr_star = max(rr, 1 / rr),
       rare_outcome = isTRUE(rare_outcome), method = "E-value (VanderWeele & Ding 2017)")
}

#' @rdname Evalu
#' @export
Evaltw <- function(estimate, ci_lower, ci_upper) {
  Evalu(estimate, ci_lower = ci_lower, ci_upper = ci_upper)
}

#' @rdname Evalu
#' @export
Jntmed <- function(x, m, y, alpha = 0.05) {
  x <- as.numeric(x)
  m <- as.numeric(m)
  y <- as.numeric(y)
  n <- length(x)
  if (length(m) != n || length(y) != n) stop("x, m and y must share a length")
  if (n < 4) stop("Need at least 4 observations, got ", n)
  if (!(alpha > 0 && alpha < 1)) stop("alpha must lie in (0, 1)")
  ols_t <- function(D, resp, col) {
    xtx_inv <- solve(crossprod(D))
    beta <- drop(xtx_inv %*% crossprod(D, resp))
    r <- resp - drop(D %*% beta)
    dof <- n - ncol(D)
    tt <- beta[col] / sqrt(sum(r^2) / dof * xtx_inv[col, col])
    unname(c(beta[col], 2 * stats::pt(abs(tt), dof, lower.tail = FALSE)))
  }
  pa <- ols_t(cbind(1, x), m, 2)
  pb <- ols_t(cbind(1, x, m), y, 3)
  p <- max(pa[2], pb[2])
  list(significant = p < alpha, p_value = p, a = pa[1], b = pb[1], p_a = pa[2], p_b = pb[2],
       indirect = pa[1] * pb[1], n = n, alpha = alpha,
       method = "Joint significance (max-p) mediation test (MacKinnon et al. 2002)")
}
