# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean-square equivalence of the smoothed and ordinary tests (Theorem 5.8)
#'
#' Theorem 5.8: if `f'` exists and is continuous near `-theta`, the bandwidth
#' is `h = c n^(-d)` with `c > 0` and `1/4 < d < 1/2`, the kernel is symmetric,
#' and two limiting moment conditions hold, then the standardised smoothed and
#' ordinary statistics agree in MEAN SQUARE:
#' \deqn{\lim_n E\Big[\frac{S - E(S)}{\sqrt{V(S)}} - \frac{\tilde S - E(\tilde S)}{\sqrt{V(\tilde S)}}\Big]^2 = 0,}{lim E[(S - E S)/sqrt(V S) - (Stilde - E Stilde)/sqrt(V Stilde)]^2 = 0,}
#' and likewise for `W` and `Wtilde`.
#'
#' Mean-square convergence is stronger than convergence in probability and is
#' what licenses the conclusion that Pitman efficiencies coincide. Smoothing
#' costs nothing asymptotically.
#'
#' The bandwidth window `1/4 < d < 1/2` is the operative restriction and is
#' checked here. The lower end is the same `n^(-1/4)` undersmoothing threshold
#' as (3.8) and Theorem 5.7; the upper end, `d < 1/2`, says the bandwidth may
#' not shrink so fast that the smoothing does nothing -- `n h -> Inf` in
#' disguise.
#'
#' @param d The bandwidth exponent in `h = c n^(-d)`.
#' @param c The bandwidth constant; must be positive.
#' @param n Sample size, used to report `h`.
#' @param zstd,zsmooth The two standardised statistics, for the finite-`n`
#'   squared difference.
#' @return Named list with ``ok``, ``d``, ``h``, ``sqdiff``, ``lower``, ``upper``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.8.
#' @examples
#' Smthconv(d = 1/3, n = 1000)
#' @export
Smthconv <- function(d, c = 1, n = NULL, zstd = NULL, zsmooth = NULL) {
  if (c <= 0) stop("the bandwidth constant must be positive.")
  lower <- d > 0.25
  upper <- d < 0.5
  h <- if (is.null(n)) NA_real_ else {
    if (n < 1) stop("sample size must be at least 1.")
    c * n^(-d)
  }
  sqdiff <- if (is.null(zstd) || is.null(zsmooth)) NA_real_ else (zstd - zsmooth)^2
  list(ok = lower && upper, d = d, h = h, sqdiff = sqdiff,
       lower = lower, upper = upper,
       method = "mean-square equivalence of smoothed and ordinary tests (Theorem 5.8)")
}

# CANONICAL TEST
# r <- Smthconv(d = 1/3, n = 1000); stopifnot(r$ok)

#' @rdname Smthconv
#' @keywords internal
#' @export
morie_fauzi_thm5_8_smoothed_convergence <- Smthconv
