# SPDX-License-Identifier: AGPL-3.0-or-later

#' Empirical spatio-temporal semivariogram
#'
#' For a stationary spatio-temporal process the semivariogram relates to the
#' covariance function exactly as in the purely spatial case:
#' \eqn{\gamma(h,k) = C(0,0) - C(h,k)}. The estimator is the spatio-temporal
#' Matheron, eq (9.18),
#' \eqn{\hat\gamma(h,k) = \frac{1}{2|N(h,k)|} \sum_{N(h,k)}
#' \{Z(s_i,t_i) - Z(s_j,t_j)\}^2}, where \eqn{N(h,k)} is the set of pairs
#' within spatial distance \eqn{h} AND time lag \eqn{k} of each other.
#'
#' Two things the text insists on. The lag tolerances in space and time must
#' be chosen separately, because data are generally irregular in both and a
#' single tolerance cannot give enough pairs at each spatio-temporal lag;
#' `n_space_bins` and `n_time_bins` are independent for that reason, and the
#' pair counts are returned so thin cells are visible rather than inferred.
#'
#' And (9.18) is a joint estimator, not a conditional one. Supplying `at_time`
#' additionally returns the conditional spatial semivariogram of eq (9.19),
#' which is what a two-stage analysis uses. They are different quantities;
#' Sec. 9.1 sets out why the two-stage route is weaker.
#'
#' Cells containing no pairs are returned as `NA` with a count of zero, never
#' filled. An unestimated semivariogram and a zero one are different claims.
#'
#' @param coords Matrix of coordinates, one row per observation.
#' @param times Numeric vector of observation times.
#' @param z Numeric vector of observed values.
#' @param n_space_bins,n_time_bins Numbers of spatial and temporal lag
#'   classes -- the two tolerances.
#' @param max_dist,max_time Largest lags retained; default to half the
#'   observed maxima.
#' @param at_time Optional time at which to also compute eq (9.19).
#' @param model_fn Optional \eqn{\gamma(h, k; \theta)}. When supplied the
#'   weighted least squares criterion of Sec. 9.4 is evaluated against the
#'   empirical surface.
#' @return A list with `st_variogram`, `counts`, `space_lags`, `time_lags`,
#'   `n_pairs`, `n_cells_estimated`, and optionally `conditional`,
#'   `wls_objective` and `fitted`.
#' @references Schabenberger Ch 9, Sec 9.4, eqs (9.18)-(9.19)
#' @export
#' @examples
#' spstvg(coords = c(1, 2, 3, 4, 5, 6, 7, 8), times = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1,
#' 2, 3, 4, 5, 6, 7, 8))
spstvg <- function(coords, times, z, n_space_bins = 10L, n_time_bins = 5L,
                   max_dist = NULL, max_time = NULL, at_time = NULL,
                   model_fn = NULL) {
  emp <- .schab_st_empirical_semivariogram(coords, times, z,
                                           n_space_bins = n_space_bins,
                                           n_time_bins = n_time_bins,
                                           max_dist = max_dist,
                                           max_time = max_time)
  counts <- emp$counts
  filled <- sum(counts > 0L)
  out <- list(st_variogram = emp$gamma, counts = counts,
              space_lags = emp$space_lags, time_lags = emp$time_lags,
              space_edges = emp$space_edges, time_edges = emp$time_edges,
              n_pairs = sum(counts), n_cells = length(counts),
              n_cells_estimated = filled)
  if (filled < length(counts)) {
    out$warning <- sprintf(paste("%d of %d lag cells contain no pairs and are",
                                 "NA; widen the tolerances or reduce the bin",
                                 "counts"),
                           length(counts) - filled, length(counts))
  }
  if (!is.null(at_time)) {
    out$conditional <- .schab_st_conditional_semivariogram(
      coords, times, z, at_time = at_time, n_bins = n_space_bins,
      max_dist = max_dist)
    out$conditional_note <- paste(
      "eq (9.19) is the CONDITIONAL spatial semivariogram at one time, used",
      "by two-stage analyses; it is not comparable with the joint estimator")
  }
  if (!is.null(model_fn)) {
    out$wls_objective <- .schab_st_wls_objective(emp, model_fn)
    hh <- rep(emp$space_lags, times = length(emp$time_lags))
    kk <- rep(emp$time_lags, each = length(emp$space_lags))
    out$fitted <- matrix(as.numeric(model_fn(hh, kk)),
                         nrow = length(emp$space_lags))
  }
  out
}
