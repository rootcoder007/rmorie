# SPDX-License-Identifier: AGPL-3.0-or-later

#' Spatio-temporal point process intensity
#'
#' The first-order intensity is eq (9.20),
#' \eqn{\lambda(s,t) = \lim E\[N(ds, dt)\] / (|ds| |dt|)}, where
#' \eqn{N(ds, dt)} counts events in an infinitesimal cylinder with base
#' \eqn{ds} and height \eqn{dt} (Dorai-Raj, 2001). The cylinder, rather than a
#' ball in \eqn{R^3}, is the same refusal to treat time as a third spatial
#' coordinate that runs through the chapter. Under first-order stationarity in
#' space and time the intensity does not depend on \eqn{s} or \eqn{t} and is
#' estimated by \eqn{N / (|A| |T|)}.
#'
#' The marginals of eqs (9.21) and (9.22) are also returned, estimated by
#' binning: a cell's marginal spatial intensity is its count over the cell
#' area, already integrated across all of \eqn{T}; a bin's marginal temporal
#' intensity is its count over the bin width, already integrated across all of
#' \eqn{D}. Sec. 9.5.3 gives the consistency checks: under first-order
#' stationarity in time \eqn{\lambda(s,\cdot) = |T| \lambda^{**}(s)}, and in
#' space \eqn{\lambda(\cdot,t) = |A| \lambda^{*}(t)}.
#'
#' The benchmark is the completely spatio-temporally random (CSTR) process:
#' Poisson in both space and time, so
#' \eqn{N(A,T) \sim Poisson(\lambda |A \times T|)}, \eqn{\lambda(s,t) = \lambda}
#' and \eqn{\lambda_2 = \lambda^2}. The text's own assessment is worth
#' carrying: "If the CSR process is an unattainable standard for spatial point
#' processes, then the CSTR process is even more so." It is the initial
#' benchmark to test against, not a model of anything.
#'
#' Chapter 9 supplies that benchmark but no test. The test reported here is
#' the book's own quadrat statistic for CSR, Sec. 3.3 eq (3.3), in the form
#' \eqn{X^2 = (rc-1)s^2/\bar n} with \eqn{s^2} the sample variance and
#' reference \eqn{\chi^2_{rc-1}}, extended from quadrats in \eqn{D} to cells in
#' \eqn{D \times T} by the analogy Sec. 9.5.3 itself draws. The index is
#' returned alongside the p-value, because with few cells the test has little
#' power and a non-rejection is not evidence of randomness.
#'
#' `process_type` records which of the Sec. 9.5.1 types the data are taken to
#' be. It does not change the arithmetic; it is carried into the result
#' because the same numbers mean different things for each, and because the
#' text notes two of them can be indistinguishable from the data alone.
#'
#' @param points Matrix of event coordinates, one row per event.
#' @param region Numeric vector `(xmin, xmax, ymin, ymax)`.
#' @param time_interval Numeric vector `(t0, t1)`.
#' @param times Numeric vector of event times. Required.
#' @param process_type Optional; one of "earthquake", "explosion",
#'   "birth_death", "sampled_in_time".
#' @param n_space_bins,n_time_bins Grid for the marginals and the CSTR test.
#' @return A list with `intensity`, `n`, `area`, `duration`, `volume`,
#'   `marginal_spatial`, `marginal_temporal`, `cstr`, `index_of_dispersion`,
#'   `df`, `p_value`, `cell_counts` and `process_type`.
#' @references Schabenberger Ch 9, Sec 9.5, eqs (9.20)-(9.23); Sec 3.3 eq (3.3)
#' @export
spstp <- function(points, region, time_interval, times = NULL,
                  process_type = NULL, n_space_bins = 3L, n_time_bins = 3L) {
  if (is.null(times)) {
    stop(paste("`times` is required: a spatio-temporal point process needs an",
               "event time for every event"), call. = FALSE)
  }
  types <- c("earthquake", "explosion", "birth_death", "sampled_in_time")
  if (!is.null(process_type) && !process_type %in% types) {
    stop(sprintf("`process_type` must be one of %s",
                 paste(types, collapse = ", ")), call. = FALSE)
  }
  lam <- .schab_st_intensity(points, times, region, time_interval)
  marg <- .schab_st_marginal_intensities(points, times, region, time_interval,
                                         n_space_bins = n_space_bins,
                                         n_time_bins = n_time_bins)
  ref <- .schab_cstr_reference(lam$area, lam$duration, lam$intensity)
  test <- .schab_cstr_test(points, times, region, time_interval,
                           n_space_bins = n_space_bins,
                           n_time_bins = n_time_bins)
  out <- list(intensity = lam$intensity, n = lam$n, area = lam$area,
              duration = lam$duration, volume = lam$volume,
              marginal_spatial = marg$marginal_spatial,
              marginal_temporal = marg$marginal_temporal,
              cell_area = marg$cell_area, time_bin_width = marg$bin_width,
              cstr = ref, index_of_dispersion = test$index_of_dispersion,
              df = test$df, p_value = test$p_value,
              cell_counts = test$counts, process_type = process_type)
  n_cells <- length(test$counts)
  if (n_cells < 20L) {
    out$power_note <- sprintf(paste("only %d space-time cells: the dispersion",
                                    "test has little power here, and failing",
                                    "to reject CSTR is not evidence for it"),
                              n_cells)
  }
  if (!is.null(process_type) &&
      process_type %in% c("birth_death", "sampled_in_time")) {
    out$identifiability_note <- paste(
      "a birth-death process observed at fixed times can be indistinguishable",
      "from a pattern sampled in time (Sec. 9.5.1); an event absent at the",
      "next time may be a death or a displacement")
  }
  if (identical(process_type, "earthquake")) {
    out$conditional_note <- paste(
      "for an earthquake process the conditional intensities are not",
      "meaningful and should be replaced by intensities on intervals in time",
      "or areas in space (Rathbun, 1996)")
  }
  out
}
