# SPDX-License-Identifier: AGPL-3.0-or-later

#' Separable spatio-temporal covariance functions
#'
#' A separable covariance decomposes
#' \eqn{Cov\[Z(s,t), Z(s+h,t+k)\]} into a purely spatial and a purely temporal
#' component, combined by multiplication or addition:
#' \eqn{C(h,k) = C_s(h) C_t(k)} (product),
#' \eqn{C(h,k) = C_s(h) + C_t(k)} (sum), or
#' \eqn{C_s C_t + C_s + C_t} (product-sum).
#'
#' The first two are valid whenever the components are, by the two elementary
#' properties quoted at the head of Sec. 9.2: a non-negative combination of
#' valid covariance functions is valid, and so is a product. The components
#' normally carry different parameters, which is what accommodates space-time
#' anisotropy.
#'
#' `product_sum` is De Cesare, Myers and Posa (2001). It appears in this
#' section of the book, but the text is explicit that it "is generally
#' nonseparable", so the returned `separable` flag is `FALSE` for it.
#'
#' The drawback Sec. 9.2 identifies is reported rather than hidden: under
#' product separability the spatial covariances at different time lags are
#' proportional, so the spatial dependence has the same shape at every time
#' lag and the two components "do not act upon each other". If that matters,
#' use [spstcn()].
#'
#' @param spatial_h Spatial lag \eqn{||h||}, non-negative.
#' @param temporal_u Temporal lag \eqn{k}. Carried separately from the spatial
#'   lag throughout; the two are never concatenated into one vector.
#' @param cov_spatial,cov_temporal Functions giving \eqn{C_s(h)} and
#'   \eqn{C_t(k)}. Each must be a valid covariance function in its own domain.
#' @param form One of "product", "sum", "product_sum".
#' @param coords,times Optional design. When supplied, eq (9.5) is checked
#'   numerically on it and the minimum eigenvalue is reported. Construction
#'   alone is not proof of validity: Sec. 9.3 records that Gneiting (2002)
#'   found published covariance functions in Cressie and Huang (1999) to be
#'   invalid.
#' @return A list with `st_covariance`, `form`, `separable`, `spatial_only`,
#'   `temporal_only`, `sill`, and when a design is supplied `valid` and
#'   `min_eigenvalue`.
#' @references Schabenberger Ch 9, Sec 9.2, eqs (9.5)-(9.6)
#' @export
spstcv <- function(spatial_h, temporal_u, cov_spatial, cov_temporal,
                   form = "product", coords = NULL, times = NULL) {
  cvals <- .schab_st_separable_covariance(spatial_h, temporal_u, cov_spatial,
                                          cov_temporal, form = form)
  zero_h <- rep(0, length(as.numeric(spatial_h)))
  zero_k <- rep(0, length(as.numeric(temporal_u)))
  out <- list(
    st_covariance = cvals,
    form = form,
    separable = .schab_st_is_separable(form),
    # C(h, 0) and C(0, k), which Sec. 9.2 names as the spatial and temporal
    # covariance functions of the process
    spatial_only = .schab_st_separable_covariance(spatial_h, zero_h,
                                                  cov_spatial, cov_temporal,
                                                  form = form),
    temporal_only = .schab_st_separable_covariance(zero_k, temporal_u,
                                                   cov_spatial, cov_temporal,
                                                   form = form),
    sill = .schab_st_separable_covariance(0, 0, cov_spatial, cov_temporal,
                                          form = form)[1])
  if (!is.null(coords) && !is.null(times)) {
    v <- .schab_st_is_valid_covariance(
      coords, times,
      function(d, u) .schab_st_separable_covariance(d, u, cov_spatial,
                                                    cov_temporal, form = form))
    out$valid <- v$valid
    out$min_eigenvalue <- v$min_eigenvalue
    if (!isTRUE(v$valid)) {
      out$warning <- paste("eq (9.5) fails on this design: the construction",
                           "does not yield a valid covariance function here")
    }
  }
  out
}
