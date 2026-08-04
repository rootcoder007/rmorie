# SPDX-License-Identifier: AGPL-3.0-or-later
#' Predictive criterion computed from the posterior, pointwise
#'
#' AIC and DIC both need a point estimate of the parameters; WAIC does
#' not, which makes it usable for singular models. The effective
#' parameter count falls out as the posterior variance of the pointwise
#' log likelihood, and a large variance for some observation is a warning
#' that the posterior is being stretched by that point.
#'
#' Formula: \code{lppd = sum_i log mean_s exp(ll_is)},
#' \code{p_WAIC = sum_i Var_s(ll_is)}, \code{WAIC = -2(lppd - p_WAIC)}.
#'
#' @param log_lik Pointwise log likelihood, draws by observations.
#' @return List with \code{estimate}, \code{lppd}, \code{p_waic},
#'   \code{elpd}, \code{n_high_var}, \code{S}, \code{n}.
#' @references Watanabe, S. (2010). JMLR 11:3571-3594; Vehtari, Gelman &
#'   Gabry (2017) Statist Comput 27:1413-1432.
#' @export
Waicd <- function(log_lik) {
  L <- as.matrix(log_lik); Sn <- nrow(L); n <- ncol(L)
  lppd <- 0; pw <- 0; high <- 0L
  for (i in seq_len(n)) {
    col <- L[, i]
    m <- max(col)
    lppd <- lppd + m + log(sum(exp(col - m)) / Sn)
    v <- sum((col - mean(col))^2) / (Sn - 1)
    pw <- pw + v
    if (v > 0.4) high <- high + 1L
  }
  .t1_result(estimate = -2 * (lppd - pw), lppd = lppd, p_waic = pw,
             elpd = lppd - pw, n_high_var = high, S = Sn, n = n,
             method = "WAIC with effective parameter count")
}
