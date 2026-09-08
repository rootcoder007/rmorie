# SPDX-License-Identifier: AGPL-3.0-or-later

#' Equivalence of the boundary-free and empirical KS statistics (Theorem 5.6)
#'
#' Theorem 5.6: under `H0: F_X = F` on the support `Omega`,
#' \deqn{|KS_n - \tilde{KS}| \to_p 0,}{|KS_n - KStilde| ->_p 0,}
#' with `KStilde` built from the BOUNDARY-FREE estimator (5.5).
#'
#' The proof is short and worth knowing. Both statistics are suprema, so their
#' difference is bounded by `sup_x |Ftilde_X(x) - F_n(x)|`. Transforming to
#' `Y_i = g^-1(X_i)` that becomes `sup_y |Ftilde_Y(y) - F_{n,Y}(y)|`, known to
#' be `o_p(n^-1/2)` for the ordinary kernel estimator on the whole line. The
#' bijection carries the result back unchanged because
#' `Ftilde_Y(g^-1(x)) = Ftilde_X(x)` and `F_{n,Y}(g^-1(x)) = F_n(x)` EXACTLY --
#' the same change-of-variable identity that made (5.5) work.
#'
#' @param empirical The empirical statistic.
#' @param smoothed The boundary-free statistic.
#' @param tol Tolerance against which the difference is reported.
#' @param h Bandwidth; with `n`, the `h = o(n^-1/4)` condition is checked.
#' @param n Sample size.
#' @return Named list with ``difference``, ``close``, ``tol``, ``bwok``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.6.
#' @examples
#' Bfkseq(empirical = 0.20, smoothed = 0.21, h = 0.05, n = 1000)
#' @export
Bfkseq <- function(empirical, smoothed, tol = 0.05, h = NULL, n = NULL) {
  if (tol <= 0) stop("tol must be positive.")
  d <- abs(empirical - smoothed)
  bwok <- if (is.null(h) || is.null(n)) NA else (h < n^-0.25)
  list(difference = d, close = (d < tol), tol = tol, bwok = bwok,
       method = "boundary-free vs empirical KS equivalence (Theorem 5.6)")
}

# CANONICAL TEST
# r <- Bfkseq(empirical = 0.20, smoothed = 0.21, h = 0.05, n = 1000)
# stopifnot(r$close, r$bwok)

#' @rdname Bfkseq
#' @keywords internal
#' @export
morie_fauzi_thm5_6_bdfree_ks_equiv <- Bfkseq
