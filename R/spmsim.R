# SPDX-License-Identifier: AGPL-3.0-or-later

#' Multiscale GWR: one bandwidth per covariate
#'
#' Ordinary GWR gives every covariate the same bandwidth, which asserts that
#' every relationship in the model varies over space at the same rate. MGWR
#' drops that assumption: each \eqn{\beta_k(s)} gets its own bandwidth
#' \eqn{h_k}, so a covariate whose effect is near-constant can take a wide
#' kernel while one that genuinely varies locally takes a narrow one.
#'
#' Estimation is GAM backfitting. Start from an ordinary GWR fit with a single
#' bandwidth. Then sweep the covariates: for each \eqn{j}, form the partial
#' residual \eqn{XB_j + e}, select a bandwidth for a *univariate* GWR of that
#' partial residual on \eqn{x_j} alone, refit, and carry the updated residual
#' into the next covariate of the same sweep. Sweep until the score of change
#' (SOC) falls below `tol`. With `rss_score = FALSE` the score is SOC-f,
#' \eqn{\sqrt{(\sum (XB_{new} - XB)^2 / n) / \sum_i (\sum_j XB_{new,ij})^2}};
#' with `rss_score = TRUE` it is SOC-RSS,
#' \eqn{|rss_{new} - rss| / rss_{new}}.
#'
#' If the bandwidth vector repeats unchanged for `bws_same_times` consecutive
#' sweeps the search is frozen and only the coefficients are refitted, which
#' is what keeps the cost tolerable -- each sweep otherwise runs a full
#' bandwidth search per covariate.
#'
#' A caution the SOC makes necessary. Both scores measure how much the fit
#' *moved*, not how good it is, so a sweep that barely changes anything scores
#' as converged. When the initial single-bandwidth GWR already sits at the
#' wide end of the search interval, the first sweep can leave every covariate
#' there, the score is tiny, and the loop stops after two or three sweeps
#' having found no scale separation at all. Measured over eight seeds of a
#' fixture with two genuinely different scales, this happened twice. It is a
#' property of the criterion, not of this port -- the reference implementation
#' uses the same score and the same default tolerance. `at_search_boundary`
#' flags it: when `TRUE`, treat the bandwidths as a non-result and rerun from
#' a narrower `init_bandwidth`.
#'
#' MGWR postdates Schabenberger & Gotway (2005) and is not in that book; it is
#' included here because the shelf names it.
#'
#' @param x Design matrix, n by p. The intercept, if present, is treated as a
#'   covariate like any other and gets its own bandwidth.
#' @param y Response, length n.
#' @param coords Coordinates, n by 2.
#' @param kernel One of "gaussian", "bisquare", "tricube", "boxcar".
#' @param criterion Passed to each inner univariate bandwidth search; one of
#'   "aicc", "cv", "aic".
#' @param adaptive Logical; use nearest-neighbour bandwidths throughout.
#' @param tol Convergence tolerance on the score of change.
#' @param max_iter Maximum backfitting sweeps.
#' @param rss_score Logical; use SOC-RSS instead of SOC-f.
#' @param bws_same_times Sweeps of unchanged bandwidths that freeze the search.
#' @param init_bandwidth Optional; skip the initial single-bandwidth search.
#' @param standardize Logical, default `TRUE`. Standardize `y` and every
#'   non-constant column of `x` to mean 0 and variance 1 before calibrating.
#'   Fotheringham, Oshan & Li (2024) Sec. 2.3.3.2 states that comparing the
#'   covariate-specific bandwidths to one another requires this, and Sec. 6.3
#'   records that in the authors' own software it is a default that "has to be
#'   actively turned off". Coefficients come back on the standardized scale,
#'   with the centres and scales in the result; `fitted` and `resid` are
#'   converted back to the units of `y`.
#' @return A list with `bandwidths` (one per covariate),
#'   `local_coefficients`, `fitted`, `resid`, `rss`, `bandwidth_gwr`,
#'   `bandwidth_history`, `score_history`, `n_iter`, `converged`,
#'   `at_search_boundary`, `score_type`, and a `warning` when either the
#'   boundary case or non-convergence applies.
#' @references Fotheringham, A. S., Yang, W. & Kang, W. (2017), Annals of the
#'   American Association of Geographers 107(6):1247-1265,
#'   doi:10.1080/24694452.2017.1352480 -- eq (9) SOC-RSS, eq (10) SOC-f, the
#'   back-fitting algorithm of Figure 1, GWR estimates as the initialisation,
#'   and SOC-f below 1e-5 as the termination criterion. Fotheringham, Oshan &
#'   Li (2024), Multiscale Geographically Weighted Regression: Theory and
#'   Practice, 1st ed., CRC Press, doi:10.1201/9781003435464 -- Sec. 2.3.2
#'   eqs (2.38)-(2.39) restate the SOC, Sec. 2.3.3.2 and Sec. 6.3 require
#'   standardization. Oshan, Li, Kang, Wolf & Fotheringham (2019), the mgwr
#'   package, mgwr/search.py, function multi_bw -- the authors' own
#'   implementation. Schabenberger Ch 6, Sec 6.1.3.1, pp. 316-317, for the
#'   single-scale GWR this generalises.
#' @export
#' @examples
#' spmsim(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), coords = c(1, 2, 3, 4, 5, 6, 7, 8))
spmsim <- function(x, y, coords, kernel = "gaussian", criterion = "aicc",
                   adaptive = FALSE, tol = 1e-5, max_iter = 200L,
                   rss_score = FALSE, bws_same_times = 5L,
                   init_bandwidth = NULL, standardize = TRUE) {
  x <- as.matrix(x)
  y <- as.numeric(y)
  res <- .schab_mgwr_backfit(y, x, coords, kernel = kernel,
                             criterion = criterion, adaptive = adaptive,
                             tol = tol, max_iter = max_iter,
                             rss_score = rss_score,
                             bws_same_times = bws_same_times,
                             init_bandwidth = init_bandwidth,
                             standardize = standardize)
  out <- list(
    bandwidths = res$bandwidths,
    local_coefficients = res$params,
    fitted = res$fitted,
    resid = res$resid,
    rss = sum(res$resid^2),
    bandwidth_gwr = res$bandwidth_gwr,
    bandwidth_history = res$bandwidth_history,
    score_history = res$score_history,
    n_iter = res$n_iter,
    converged = res$converged,
    at_search_boundary = res$at_search_boundary,
    standardized = res$standardized,
    y_centre = res$y_centre, y_scale = res$y_scale,
    x_centre = res$x_centre, x_scale = res$x_scale,
    criterion = criterion,
    kernel = kernel,
    score_type = if (rss_score) "SOC-RSS" else "SOC-f",
    n = nrow(x),
    p = ncol(x)
  )
  if (res$at_search_boundary) {
    out$warning <- paste0(
      "every bandwidth sits at the top of the search interval and the ",
      "backfit stopped after ", res$n_iter, " sweep(s): the SOC measures ",
      "how much the fit MOVED, so a first sweep that changes nothing scores ",
      "as converged. No scale separation was found -- rerun from a narrower ",
      "init_bandwidth before reading anything into these bandwidths")
  }
  if (!res$converged) {
    out$warning <- sprintf(
      paste0("backfitting hit max_iter=%d with SOC=%.3e still above ",
             "tol=%.3e; the bandwidths are the last sweep's, not a ",
             "converged optimum"),
      max_iter, res$score_history[res$n_iter], tol)
  }
  out
}
