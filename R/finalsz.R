# SPDX-License-Identifier: AGPL-3.0-or-later

#' Final epidemic size (Kermack-McKendrick)
#'
#' Formula: for the standard SIR model the final size Z = S(0) - S(inf)
#' satisfies the implicit relation
#' \preformatted{
#'   Z = S(0) (1 - exp(-R0 \[Z + I(0)\]))
#' }
#' (Ma & Earn 2006, eq. 4, p.681), which in the limit I(0) -> 0,
#' S(0) -> 1 collapses to the classical Z = 1 - exp(-R0 Z) (their eq. 5).
#' Ma & Earn show the relation holds for arbitrary distributions of the
#' infectious period, so R0 alone determines the final size.  The root is
#' isolated by bisection on \[0, S(0)\], where the residual is
#' non-negative at 0 and strictly negative at S(0).
#'
#' @param R0 Basic reproduction number (>= 0).
#' @param s0 Initial susceptible proportion in (0, 1].
#' @param i0 Initial infectious proportion; default 1 - s0.
#' @param tol Bracket width at which bisection stops.
#' @param max_iter Maximum bisection steps.
#' @return List with \code{estimate}, \code{final_size}, \code{s_inf},
#'   \code{attack_rate}, \code{R0}, \code{s0}, \code{i0},
#'   \code{residual}, \code{iters}, \code{n}, \code{method}.
#' @references Kermack & McKendrick (1927), Proc. R. Soc. Lond. A
#'   115(772):700-721, doi:10.1098/rspa.1927.0118; Ma & Earn (2006),
#'   Bull. Math. Biol. 68(3):679-702, doi:10.1007/s11538-005-9047-7.
#' @export
#' @examples
#' Finalsz(R0 = 5L)
Finalsz <- function(R0, s0 = 1, i0 = NULL, tol = 1e-14, max_iter = 200) {
  R0 <- as.numeric(R0)
  s0 <- as.numeric(s0)
  if (R0 < 0) stop("R0 must be non-negative")
  if (!(s0 > 0 && s0 <= 1)) stop("s0 must lie in (0, 1]")
  i0 <- if (is.null(i0)) 1 - s0 else as.numeric(i0)
  if (i0 < 0 || s0 + i0 > 1) stop("i0 must be non-negative with s0 + i0 <= 1")
  tol <- as.numeric(tol)
  if (tol <= 0) stop("tol must be positive")
  resid <- function(Z) s0 * (1 - exp(-R0 * (Z + i0))) - Z
  lo <- 0
  hi <- s0
  it <- 0L
  if (i0 == 0 && R0 * s0 <= 1) {
    Z <- 0
  } else {
    for (k in seq_len(as.integer(max_iter))) {
      it <- k
      mid <- 0.5 * (lo + hi)
      if (resid(mid) > 0) lo <- mid else hi <- mid
      if (hi - lo < tol) break
    }
    Z <- 0.5 * (lo + hi)
  }
  .t1_result(estimate = Z, final_size = Z, s_inf = s0 - Z,
             attack_rate = Z / s0, R0 = R0, s0 = s0, i0 = i0,
             residual = resid(Z), iters = it, n = 1L,
             method = "Final epidemic size (Kermack-McKendrick)")
}
