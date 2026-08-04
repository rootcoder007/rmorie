# SPDX-License-Identifier: AGPL-3.0-or-later
#' Warped Gaussian process
#'
#' Snelson, Rasmussen and Ghahramani (2004), Warped Gaussian processes,
#' NIPS 16, 337-344: a monotone warping t = f(y) is applied, a GP is
#' fitted in the warped space, and the log marginal likelihood picks up
#' the Jacobian, log p(y | X) = log p_GP(f(y) | X) + sum_i log f'(y_i).
#' The predictive MEDIAN in the original space is the inverse warp of the
#' warped-space mean, because a monotone map preserves quantiles; the
#' predictive mean is not, and is not claimed here.  The proceedings were
#' not retrievable; both statements are quoted in their standard published
#' form.  The identity warp reduces the model exactly to an ordinary GP.
#'
#' @param X,y training data.
#' @param X_test test inputs.
#' @param warp "identity", "log" or "sqrt".
#' @param lam,gamma GP hyperparameters.
#' @return list: estimate, median, warped_mean, var, log_jacobian, warp,
#'   method.
#' @keywords internal
#' @examples
#' Warpedgp(matrix(c(0, 1, 2), 3, 1), c(1, 2, 4), warp = "log")$median
#' @export
Warpedgp <- function(X, y, X_test = NULL, warp = "identity", lam = 1e-2,
                     gamma = 1) {
  yv <- .s03vec(y)
  if (identical(warp, "log")) {
    fwd <- log; inv <- exp; der <- function(v) 1 / v
  } else if (identical(warp, "sqrt")) {
    fwd <- sqrt; inv <- function(t) t * t; der <- function(v) 0.5 / sqrt(v)
  } else {
    fwd <- function(v) v; inv <- function(t) t; der <- function(v) 1
  }
  t <- vapply(yv, fwd, 0)
  fit <- Krrdual(X, t, X_test, lam, gamma)
  med <- vapply(fit$pred, inv, 0)
  lj <- 0
  for (v in yv) {
    d <- der(v)
    lj <- lj + (if (d > 0) log(d) else -Inf)
  }
  list(estimate = if (length(med)) med[1] else NaN, median = med,
       warped_mean = fit$pred, var = fit$var, log_jacobian = lj,
       warp = as.character(warp),
       method = "Warped GP: fit in the warped space, invert for the predictive median (Snelson et al. 2004)")
}
