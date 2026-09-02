# SPDX-License-Identifier: AGPL-3.0-or-later

#' Moment bound for the H-decomposition projections
#'
#' Eq. (3.12): for `q >= 2`, if `E|nu(X_1,...,X_r)|^q < Inf` there is a
#' constant `C` depending on `nu` and `F` but NOT on `n` with
#' \deqn{E|A_k|^q \le C n^{qk/2} E|\rho_k(X_{i_1},\dots,X_{i_k})|^q,}{E|A_k|^q <= C n^(qk/2) E|rho_k|^q,}
#' where `A_k` is the `k`-th projection of the H-decomposition (3.10)-(3.11).
#'
#' The exponent is the useful part. A naive count would put `choose(n, k) ~ n^k`
#' terms in `A_k`, giving `n^(qk)`; the martingale property (3.9),
#' `E\[rho_k | X_1,...,X_{k-1}\] = 0`, halves the exponent to `n^(qk/2)`. That
#' square root makes the higher projections negligible and lets Lemma 3.1 stop
#' at `k = 3` with an `o_L(n^-1/2)` remainder.
#'
#' `C` is genuinely unspecified in the text -- a generic constant the book says
#' "may change its meaning at different places". It defaults to 1 and the
#' returned bound is labelled `bound_over_c` for that reason; treating it as an
#' absolute number would be a fabrication.
#'
#' @param n Sample size.
#' @param k Projection order, `k >= 1`.
#' @param q Moment order, `q >= 2`.
#' @param rhomom `E|rho_k|^q`.
#' @param c The unspecified constant of (3.12).
#' @return Named list with ``bound_over_c``, ``bound``, ``exponent``, ``naive``, ``method``.
#' @references Fauzi and Maesono (2023), Eqs. (3.9)-(3.13).
#' @examples
#' Hdecmom(n = 100, k = 2, q = 2)
#' @export
Hdecmom <- function(n, k, q, rhomom = 1, c = 1) {
  if (n < 1) stop("sample size must be at least 1.")
  if (k < 1) stop("projection order must be at least 1.")
  if (q < 2) stop("(3.12) is stated for q >= 2.")
  expo <- q * k / 2
  scaled <- n^expo * rhomom
  list(bound_over_c = scaled, bound = c * scaled, exponent = expo,
       naive = q * k, method = "H-decomposition moment bound (Eq. 3.12)")
}

# CANONICAL TEST
# r <- Hdecmom(n = 100, k = 2, q = 2)
# stopifnot(r$exponent == 2, r$naive == 4)

#' @rdname Hdecmom
#' @keywords internal
#' @export
morie_fauzi_moment_ineq_ustat <- Hdecmom
