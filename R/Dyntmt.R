# SPDX-License-Identifier: AGPL-3.0-or-later
#' Value of a dynamic treatment regime under a marginal structural model
#'
#' \code{V(d) = E\[Y(d_bar(H))\]} is the mean counterfactual outcome had
#' every subject been treated according to the rule \code{d} applied to
#' their own evolving history.  It is estimated here by the
#' inverse-probability weighted (Hajek) form: \code{C_i} indicates that
#' the observed treatment history follows the rule at every time,
#' \code{SW_i = prod_t P(D_it = d_it) / P(D_it = d_it | H_it)} are
#' stabilised weights with the marginal treatment frequency in the
#' numerator and a pooled logistic fit on the history in the
#' denominator, and
#' \code{V = sum_i C_i SW_i Y_i / sum_i C_i SW_i}.  When treatment is
#' independent of the history the propensity fit returns the marginal
#' frequency itself, every stabilised weight is exactly one, and \code{V}
#' reduces to the plain mean of Y over the subjects who follow the rule.
#' It is NOT true in general that the weights collapse merely because
#' everyone follows.
#'
#' @param y End-of-follow-up outcome, one entry per subject.
#' @param D_history Observed binary treatment, subjects by time points.
#' @param H_history Time-varying history, subjects by time points.
#' @param regime_fn The rule: a function applied elementwise to
#'   \code{H_history}, or an n x T array of prescribed treatments, or
#'   \code{NULL} for the threshold rule "treat once the history exceeds
#'   zero".
#' @return List with \code{estimate}, \code{value}, \code{n_follow},
#'   \code{sum_weights}, \code{mean_weight}, \code{max_weight},
#'   \code{ess}, \code{naive_mean}, \code{n_time}, \code{n}.
#' @references Robins, J. M., Orellana, L. and Rotnitzky, A. (2008).
#'   Estimation and extrapolation of optimal treatment and testing
#'   strategies. Statistics in Medicine 27(23), 4678-4721.  Petersen,
#'   M., Schwab, J., Gruber, S., Blaser, N., Schomaker, M. and van der
#'   Laan, M. (2014). Journal of Causal Inference 2(2), 147-185.
#' @export
Dyntmt <- function(y, D_history, H_history, regime_fn = NULL) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("Dyntmt: empty input, y has no observations")
  D <- .s03mat(D_history); H <- .s03mat(H_history)
  if (nrow(D) != n || nrow(H) != n)
    stop("Dyntmt: D_history and H_history must have one row per subject")
  Tt <- ncol(D)
  if (Tt == 0L) stop("Dyntmt: D_history has no time points")
  if (ncol(H) != Tt) stop("Dyntmt: ragged history, every row must have T entries")
  if (any(D != 0 & D != 1)) stop("Dyntmt: D_history must be binary 0/1")
  pres <- if (is.null(regime_fn)) {
    ifelse(H > 0, 1, 0)
  } else if (is.function(regime_fn)) {
    matrix(vapply(as.vector(H), function(v) if (regime_fn(v)) 1 else 0, 0),
           n, Tt)
  } else {
    R <- .s03mat(regime_fn)
    if (nrow(R) != n || ncol(R) != Tt)
      stop("Dyntmt: regime_fn as an array must be n x T")
    ifelse(R > 0.5, 1, 0)
  }
  follow <- as.numeric(rowSums(D != pres) == 0)
  w <- rep(1, n)
  for (t in seq_len(Tt)) {
    dt <- D[, t]
    marg <- .s03mean(dt)
    if (marg <= 0 || marg >= 1) next
    Z <- cbind(1, H[, t])
    gam <- .s03logit(Z, dt, 60L)
    p <- pmin(pmax(vapply(.s03matvec(Z, gam), .s03sigmoid, 0), 1e-12), 1 - 1e-12)
    num <- ifelse(pres[, t] > 0.5, marg, 1 - marg)
    den <- ifelse(pres[, t] > 0.5, p, 1 - p)
    w <- w * num / den
  }
  ww <- follow * w
  den <- sum(ww)
  if (den <= 0)
    stop("Dyntmt: no subject follows the regime, V(d) is not identified")
  s2 <- sum(ww * ww)
  .t1_result(estimate = sum(ww * yv) / den, value = sum(ww * yv) / den,
             n_follow = sum(follow), sum_weights = den,
             mean_weight = den / n, max_weight = max(ww),
             ess = if (s2 > 0) den * den / s2 else 0,
             naive_mean = .s03mean(yv), n_time = Tt, n = n,
             method = "Dynamic-regime MSM (regime depends on history)")
}
