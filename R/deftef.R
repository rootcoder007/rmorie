# SPDX-License-Identifier: AGPL-3.0-or-later

#' Design effect DEFF from the two variances
#'
#' Formula: DEFF = Var_design / Var_SRS
#'
#' DEFT = sqrt(DEFF) is the standard-error inflation factor, and
#' n_eff = n / DEFF the effective sample size.  Both arguments may be
#' vectors, in which case the ratio is taken element by element.
#'
#' @param design_var Variance under the realised complex design.
#' @param srs_var Variance under simple random sampling of the same size.
#' @return List with \code{estimate} (DEFF), \code{deff}, \code{deft},
#'   \code{n}, \code{method}.
#' @references Kish (1965), Survey Sampling, Wiley, section 8.2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Deftef(V, V)
Deftef <- function(design_var, srs_var) {
  d <- .s03vec(design_var)
  s <- .s03vec(srs_var)
  if (!length(d) || !length(s)) stop("empty input: both variances are required")
  if (length(d) != length(s) && length(d) != 1L && length(s) != 1L)
    stop("design_var and srs_var must have the same length")
  m <- max(length(d), length(s))
  if (length(d) == 1L) d <- rep(d, m)
  if (length(s) == 1L) s <- rep(s, m)
  if (any(!(s > 0))) stop("srs_var must be strictly positive")
  deff <- d / s
  deft <- ifelse(deff >= 0, sqrt(abs(deff)), NaN)
  .t1_result(estimate = if (m == 1L) deff[1] else sum(deff) / m,
             deff = deff, deft = deft, n = m,
             method = "Kish design effect DEFF = Var_design / Var_SRS")
}
