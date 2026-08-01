# SPDX-License-Identifier: AGPL-3.0-or-later
#' Moving-window kriging with locally re-estimated semivariograms
#'
#' Local kriging restricts the solve to a neighbourhood but keeps ONE global
#' covariance model: "all n data points contribute to the estimation of
#' theta in local kriging" (p. 425). The moving-window method of Haas (1990,
#' 1995) re-estimates the semivariogram *inside each window*, so every
#' prediction location carries its own \eqn{\theta_i}. That distinction is
#' the content of the section and `local_variogram` selects between them.
#'
#' Window size follows p. 426 literally: enlarge a circle about the
#' prediction site until at least `min_sites` sites are inside, then add
#' `step` at a time until every lag class holds at least one pair and the
#' local fit converges. The book's cautions are returned rather than hidden:
#' the local predictor "is no longer best", and changing neighbourhoods can
#' introduce "spurious discontinuities".
#'
#' @param coords Site coordinates, (n, d).
#' @param z Observations, length n.
#' @param window_size Optional fixed radius overriding Haas's adaptive rule;
#'   the sites actually captured per window are reported, and a window
#'   holding fewer than 35 sites draws a warning.
#' @param targets Prediction locations; defaults to the observed sites.
#' @param min_sites,step Haas's rule parameters (35 and 5 in the book).
#' @param n_lags Lag classes for the empirical semivariogram.
#' @param local_variogram `TRUE` for Haas's method, `FALSE` for local
#'   kriging with the global model.
#' @param local_mean Re-estimate the mean inside each window.
#' @return A list with `local_variograms` (per-window sill and range),
#'   `prediction`, `window_sizes`, `converged`, `global_sill`,
#'   `global_range`, `theta_is_global`, `caveats`, `targets`, and for a
#'   fixed window `fixed_window_size`, `fixed_window_counts` and possibly
#'   `warning`.
#' @references Schabenberger Ch 8, Sec 8.3.1, pp. 425-426. Haas (1990),
#'   JASA 85:950-963; Haas (1995), JASA 90:1189-1199.
#' @export
spmwst <- function(coords, z, window_size = NULL, targets = NULL,
                   min_sites = 35L, step = 5L, n_lags = 10L,
                   local_variogram = TRUE, local_mean = FALSE) {
  s <- as.matrix(coords)
  z <- as.numeric(z)
  if (nrow(s) != length(z)) stop("coordinates and observations disagree on n")
  tg <- if (is.null(targets)) s else as.matrix(targets)
  fixed_counts <- NULL
  if (!is.null(window_size)) {
    wsz <- as.numeric(window_size)
    if (wsz <= 0) stop("window_size must be positive")
    dmat <- sqrt(outer(tg[, 1], s[, 1], "-")^2 +
                 outer(tg[, 2], s[, 2], "-")^2)
    fixed_counts <- rowSums(dmat <= wsz)
    min_sites <- max(2L, as.integer(min(fixed_counts)))
  }
  res <- .schab_moving_window_krige(s, z, tg, min_sites = min_sites,
                                    step = step, n_lags = n_lags,
                                    local_mean = local_mean,
                                    local_variogram = local_variogram)
  res$local_variograms <- cbind(res$local_sill, res$local_range)
  res$targets <- tg
  if (!is.null(fixed_counts)) {
    res$fixed_window_size <- as.numeric(window_size)
    res$fixed_window_counts <- fixed_counts
    if (min(fixed_counts) < 35) {
      res$warning <- paste0(
        "the requested window holds as few as ", min(fixed_counts),
        " sites, below the 35 that Sec. 8.3.1 sets as the starting point ",
        "for a reliable local semivariogram")
    }
  }
  res
}
