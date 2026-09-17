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

#' Universal kriging with a polynomial trend
#'
#' Predicts a spatial field at new locations under the model
#' `Z(s) = mu(s) + delta(s)`, where the trend `mu(s)` is a polynomial in the
#' coordinates of order `trend_order` and `delta(s)` is zero-mean and
#' second-order stationary. The predictor solves the augmented kriging
#' system that enforces unbiasedness on every trend basis function, so the
#' trend coefficients are never estimated separately. Order 0 reduces the
#' basis to a single column of ones, which is ordinary kriging with the
#' constraint that the weights sum to one.
#'
#' @param coords Numeric vector (one dimension) or matrix with one row per
#'   observation giving the locations of `values`.
#' @param values Numeric vector of observations, one per row of `coords`.
#' @param s_predict Locations to predict at, in the same layout as `coords`.
#' @param trend_order Polynomial order of the trend; 0 gives ordinary kriging.
#' @param model Covariance model: `"exponential"`, `"gaussian"` or
#'   `"spherical"`.
#' @param nugget Nugget variance added at zero distance.
#' @param sill Partial sill (the covariance at zero distance beyond the
#'   nugget).
#' @param range_ Range parameter of the covariance model.
#' @return A list with `estimate` and `se` (one per prediction location),
#'   `n` (number of observations), `trend_order` and a `method` label.
#' @references Cressie, N. (1993). *Statistics for Spatial Data*, revised
#'   edition, section 3.4.5. Wiley.
#'
#'   Schabenberger, O. and Gotway, C. A. (2005). *Statistical Methods for
#'   Spatial Data Analysis*, chapter 5. Chapman and Hall/CRC.
#' @examples
#' coords <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' values <- c(1.1, 2.3, 2.9, 4.2, 4.8, 6.1, 7.2, 7.9)
#' fit <- rmorie:::morie_krigun(coords, values, s_predict = c(2.5, 9),
#'                              trend_order = 1, range_ = 2)
#' fit$estimate
#' fit$se
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
