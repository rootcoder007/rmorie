# SPDX-License-Identifier: AGPL-3.0-or-later
#' Inverse-distance weighting
#'
#' Shepard (1968), A two-dimensional interpolation function for
#' irregularly-spaced data, ACM National Conference 23, 517-524: z(s*) =
#' sum_i w_i z_i / sum_i w_i with w_i = 1 / d(s*, s_i)^p, and the
#' convention that if s* coincides with a datum the interpolated value IS
#' that datum -- Shepard's function is an exact interpolator, so the limit
#' is taken rather than the division attempted.  The 1968 proceedings were
#' not retrievable here; the interpolant and the exactness convention are
#' quoted in their standard published form.  p = 2 is Shepard's own choice.
#' The effective number of contributing points, (sum w)^2 / sum w^2, is
#' returned because it is the honest measure of how local the estimate is.
#'
#' @param coords data locations, one row per point.
#' @param values data values.
#' @param s_predict prediction locations.
#' @param power the exponent p.
#' @return list: estimate, pred, ess, exact, power, method.
#' @keywords internal
#' @examples
#' Idw(matrix(c(0, 0, 1, 0), 2, 2, byrow = TRUE), c(1, 3),
#'     matrix(c(0.5, 0), 1, 2))$pred
#' @export
Idw <- function(coords, values, s_predict = NULL, power = 2) {
  P <- .s03mat(coords); z <- .s03vec(values)
  S <- if (!is.null(s_predict)) .s03mat(s_predict) else P
  p <- as.numeric(power)
  out <- numeric(nrow(S)); ess <- numeric(nrow(S)); exact <- logical(nrow(S))
  for (t in seq_len(nrow(S))) {
    w <- numeric(nrow(P)); hit <- -1L
    for (i in seq_len(nrow(P))) {
      s <- 0
      for (a in seq_len(ncol(P))) { d <- P[i, a] - S[t, a]; s <- s + d * d }
      d <- sqrt(s)
      if (d == 0) hit <- i
      w[i] <- if (d > 0) 1 / (d^p) else 0
    }
    if (hit > 0L) { out[t] <- z[hit]; ess[t] <- 1; exact[t] <- TRUE; next }
    sw <- 0; sw2 <- 0; num <- 0
    for (i in seq_len(nrow(P))) {
      sw <- sw + w[i]; sw2 <- sw2 + w[i] * w[i]; num <- num + w[i] * z[i]
    }
    out[t] <- if (sw > 0) num / sw else NaN
    ess[t] <- if (sw2 > 0) (sw * sw) / sw2 else NaN
    exact[t] <- FALSE
  }
  list(estimate = if (length(out)) out[1] else NaN, pred = out, ess = ess,
       exact = exact, power = p,
       method = "Shepard (1968) inverse-distance weighting, exact at the data")
}
