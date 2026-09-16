# R arm of morie/fn/krigun.py -- universal kriging with a polynomial trend.
#
# The Python body was a placeholder: it averaged `coords` and used neither
# `values`, `s_predict` nor `trend_order`. There was no R arm at all.
#
#   Z(s) = mu(s) + delta(s),  mu(s) = sum_k beta_k f_k(s)
#
# with delta zero-mean second-order stationary. The predictor solves the
# augmented system that enforces unbiasedness on every trend basis
# function, so the trend coefficients never have to be estimated
# separately. Order 0 makes the basis a single column of ones, which is
# exactly the ordinary kriging constraint sum_i lambda_i = 1.
#
# Cressie (1993) sec. 3.4.5; Schabenberger & Gotway (2005) ch. 5.

#' R arm of morie/fn/krigun.py -- universal kriging with a polynomial trend
#'
#' R arm of morie/fn/krigun.py -- universal kriging with a polynomial trend. The Python body was a placeholder: it averaged `coords` and used neither `values`, `s_predict` nor `trend_order`. There was no R arm at all.   Z(s) = mu(s) + delta(s),  mu(s) = sum_k beta_k f_k(s) with delta zero-mean second-order stationary. The predictor solves the augmented system that enforces unbiasedness on every trend basis function, so the trend coefficients never have to be estimated separately. Order 0 makes the basis a single column of ones, which is exactly the ordinary kriging constraint sum_i lambda_i = 1. ' @noRd
#'
#' @param coords Argument `coords`; see Usage.
#' @param values Argument `values`; see Usage.
#' @param s_predict Argument `s_predict`; see Usage.
#' @param trend_order Argument `trend_order`; see Usage.
#' @param model Argument `model`; see Usage.
#' @param nugget Argument `nugget`; see Usage.
#' @param sill Argument `sill`; see Usage.
#' @param range_ Argument `range_`; see Usage.
#' @return A list with components `estimate`, `se`, `n`, `trend_order`, `method`.
#' @examples
#' rmorie:::morie_krigun(coords = c(1, 2, 3, 4, 5, 6, 7, 8), values = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   s_predict = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
morie_krigun <- function(coords, values, s_predict, trend_order = 1,
                                    model = "exponential", nugget = 0,
                                    sill = 1, range_ = 1) {
  res <- ukrig(values, coords, s_predict, model, nugget, sill, range_,
               trend_order)
  list(estimate = as.numeric(res$estimate), se = as.numeric(res$se),
       n = as.integer(res$n), trend_order = as.integer(trend_order),
       method = sprintf("Universal kriging with a polynomial trend of order %d",
                        as.integer(trend_order)))
}

#' @noRd
Krigun <- morie_krigun
