# SPDX-License-Identifier: AGPL-3.0-or-later
#' WAIC and PSIS-LOO computed from the same draws, so they can disagree
#'
#' They usually agree, and the informative case is when they do not: WAIC
#' has no diagnostic of its own, while PSIS-LOO reports a Pareto shape
#' per observation and can say which point breaks the approximation. A
#' criterion without its diagnostic is a number with no error bar.
#'
#' Formula: \code{WAIC = -2(lppd - p_WAIC)}; LOO weights
#' \code{w_is = 1/exp(ll_is)} smoothed by a generalised Pareto on the
#' largest \code{min(0.2 S, 3 sqrt(S))}.
#'
#' @param log_lik_samples Pointwise log likelihood, draws by observations.
#' @return List with \code{estimate}, \code{looic}, \code{elpd_loo},
#'   \code{p_loo}, \code{k_max}, \code{S}, \code{n}.
#' @references Watanabe, S. (2013). JMLR 14:867-897; Vehtari, Gelman &
#'   Gabry (2017) Statist Comput 27:1413-1432.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Infcrt(V)
Infcrt <- function(log_lik_samples) {
  L <- as.matrix(log_lik_samples); Sn <- nrow(L); n <- ncol(L)
  lppd <- 0; pw <- 0; elpd_loo <- 0; ks <- numeric(n)
  for (i in seq_len(n)) {
    col <- L[, i]
    m <- max(col)
    lppd <- lppd + m + log(sum(exp(col - m)) / Sn)
    pw <- pw + sum((col - mean(col))^2) / (Sn - 1)
    ps <- .s4_psis(-col)
    ks[i] <- ps$k
    sm <- ps$lw
    mm <- max(sm)
    elpd_loo <- elpd_loo + log(sum(exp(sm - mm + col))) - log(sum(exp(sm - mm)))
  }
  .t1_result(estimate = -2 * (lppd - pw), looic = -2 * elpd_loo,
             elpd_loo = elpd_loo, p_loo = lppd - elpd_loo, k_max = max(ks),
             S = Sn, n = n, method = "WAIC with a PSIS-LOO cross-check")
}
