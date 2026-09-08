# SPDX-License-Identifier: AGPL-3.0-or-later

#' Equivalence of the boundary-free and empirical CvM statistics (Theorem 5.7)
#'
#' Theorem 5.7: under `H0: F_X = F` on `Omega`,
#' \deqn{|CvM_n - \tilde{CvM}| \to_p 0,}{|CvM_n - CvMtilde| ->_p 0,}
#' with `CvMtilde` built from the boundary-free estimator (5.5).
#'
#' The proof needs something Theorem 5.6 did not: it ASSUMES `h = o(n^-1/4)`.
#' That is not decoration. A supremum difference is controlled by a uniform
#' bound, but the CvM difference is an integral of a SQUARED discrepancy
#' multiplied by `n`, so a bias of order `h^2` contributes `n h^4`, and only
#' `h = o(n^-1/4)` makes that vanish.
#'
#' The same `n^(-1/4)` threshold appears in (3.8) for the quantile Edgeworth
#' expansion and in Theorem 5.9 for the smoothed Wilcoxon test, for the same
#' reason each time: whenever a squared bias is multiplied by `n`,
#' undersmoothing becomes compulsory.
#'
#' Pass `h` so the condition is checked rather than assumed; `bwok` reports it.
#'
#' @param empirical The empirical statistic.
#' @param smoothed The boundary-free statistic.
#' @param tol Tolerance against which the difference is reported.
#' @param h Bandwidth; with `n`, the `h = o(n^-1/4)` condition is checked.
#' @param n Sample size.
#' @return Named list with ``difference``, ``close``, ``tol``, ``bwok``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.7.
#' @examples
#' Bfcvmeq(empirical = 0.20, smoothed = 0.21, h = 0.05, n = 1000)
#' @export
Bfcvmeq <- function(empirical, smoothed, tol = 0.05, h = NULL, n = NULL) {
  if (tol <= 0) stop("tol must be positive.")
  d <- abs(empirical - smoothed)
  bwok <- if (is.null(h) || is.null(n)) NA else (h < n^-0.25)
  list(difference = d, close = (d < tol), tol = tol, bwok = bwok,
       method = "boundary-free vs empirical CvM equivalence (Theorem 5.7)")
}

# CANONICAL TEST
# r <- Bfcvmeq(empirical = 0.20, smoothed = 0.21, h = 0.05, n = 1000)
# stopifnot(r$close, r$bwok)

#' @rdname Bfcvmeq
#' @keywords internal
#' @export
morie_fauzi_thm5_7_bdfree_cvm_equiv <- Bfcvmeq
