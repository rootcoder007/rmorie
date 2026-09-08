# SPDX-License-Identifier: AGPL-3.0-or-later
#' Oster bound on bias from omitted variables
#'
#' Coefficient stability alone is not evidence: a coefficient that
#' barely moves when controls are added is uninformative unless the
#' controls also moved the R-squared. Oster's adjustment ties the two
#' together through \code{delta}, the ratio of selection on
#' unobservables to selection on observables.
#'
#' Formula: \code{beta* = beta_long - delta (beta_short - beta_long)
#' (R_max - R_long) / (R_long - R_short)}. Also returned is
#' \code{delta_star = beta_long (R_long - R_short) /
#' ((beta_short - beta_long) (R_max - R_long))}, the value of
#' \code{delta} at which \code{beta*} is exactly zero.
#'
#' @param beta_short Treatment coefficient without controls.
#' @param beta_long Treatment coefficient with controls.
#' @param R_short R-squared without controls.
#' @param R_long R-squared with controls; must exceed \code{R_short}.
#' @param R_max R-squared of the hypothetical regression on treatment
#'   plus all observed and unobserved controls.
#' @param delta Proportional-selection coefficient.
#' @return List with \code{estimate}, \code{beta_star}, \code{bias},
#'   \code{delta_star}, \code{bound_lower}, \code{bound_upper},
#'   \code{sign_stable}.
#' @references Oster, E. (2019). Unobservable selection and coefficient
#'   stability: theory and evidence. Journal of Business & Economic
#'   Statistics, 37(2), 187-204. doi:10.1080/07350015.2016.1227711
#' @export
Ovbnk <- function(beta_short, beta_long, R_short, R_long,
                  R_max = 1, delta = 1) {
  bs <- as.numeric(beta_short)
  bl <- as.numeric(beta_long)
  rs <- as.numeric(R_short)
  rl <- as.numeric(R_long)
  rm_ <- as.numeric(R_max)
  d <- as.numeric(delta)
  if (!(rl > rs)) stop("Ovbnk: R_long must exceed R_short")
  if (rm_ < rl) stop("Ovbnk: R_max must be at least R_long")
  if (!(rs >= 0 && rs <= 1 && rl >= 0 && rl <= 1 && rm_ >= 0 && rm_ <= 1))
    stop("Ovbnk: R-squared values must lie in [0, 1]")
  scale <- (rm_ - rl) / (rl - rs)
  bias <- d * (bs - bl) * scale
  beta_star <- bl - bias
  denom <- (bs - bl) * (rm_ - rl)
  delta_star <- if (denom != 0) bl * (rl - rs) / denom else Inf
  lo <- min(beta_star, bl)
  hi <- max(beta_star, bl)
  .t1_result(estimate = beta_star, beta_star = beta_star, bias = bias,
             delta_star = delta_star, bound_lower = lo, bound_upper = hi,
             sign_stable = if (lo * hi > 0) 1 else 0,
             method = "Oster (2019) proportional-selection bias bound")
}
