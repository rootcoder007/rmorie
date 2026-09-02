# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effect of a treatment rule read off from each time point onwards
#'
#' A standard marginal structural model answers one question at baseline.
#' The history-adjusted version answers it again at every time point,
#' conditioning on what was known then -- the decision at month six is
#' made with six months of information, not none.
#'
#' Formula: \code{E\[Y(d) | H_t\] = g0 + g1 t}, fitted with weights
#' \code{prod_s 1/P(A_s|H_s)} over units consistent with the regime.
#'
#' @param y Outcome.
#' @param treatment_history Treatment received at each time, n by T.
#' @param covariate_history Time-varying covariate, n by T.
#' @param time Time points.
#' @param regime Treatment the rule prescribes at each time.
#' @return List with \code{estimate}, \code{intercept}, \code{by_time},
#'   \code{n_consistent}, \code{n}.
#' @references van der Laan, M. J. & Petersen, M. L. (2007). IJB 3(1):3;
#'   van der Laan, Petersen & Joffe (2005) IJB 1(1):4.
#' @export
#' @examples
#' Hmstrn(y = c(1, 2, 3, 4, 5, 6, 7, 8), treatment_history = c(1, 2, 3, 4, 5, 6, 7, 8), covariate_history = c(1, 2, 3, 4, 5, 6, 7, 8), time = c(1, 2, 3, 4, 5, 6, 7, 8), regime = c(1, 2, 3, 4, 5, 6, 7, 8))
Hmstrn <- function(y, treatment_history, covariate_history, time, regime) {
  yv <- as.numeric(y); A <- as.matrix(treatment_history)
  L <- as.matrix(covariate_history)
  tv <- as.numeric(time); d <- as.numeric(regime)
  n <- nrow(A); T_ <- ncol(A)
  means <- numeric(0); times <- numeric(0)
  for (t in seq_len(T_)) {
    ok <- apply(abs(A[, seq_len(t), drop = FALSE] -
                    matrix(d[seq_len(t)], n, t, byrow = TRUE)) < 0.5, 1, all)
    idx <- which(ok)
    if (length(idx) == 0L) next
    des <- cbind(1, L[idx, t])
    gb <- .s4_glmbin(des, A[idx, t])
    g <- .s4_clip(.s4_expit(as.numeric(des %*% gb)), 0.025, 0.975)
    w <- ifelse(A[idx, t] > 0.5, 1 / g, 1 / (1 - g))
    means <- c(means, sum(w * yv[idx]) / sum(w))
    times <- c(times, tv[t])
  }
  beta <- .s4_ols(cbind(1, times), means)$beta
  ok0 <- apply(abs(A - matrix(d, n, T_, byrow = TRUE)) < 0.5, 1, all)
  .t1_result(estimate = beta[2], intercept = beta[1], by_time = means,
             n_consistent = sum(ok0), n = n,
             method = "History-adjusted marginal structural model")
}
