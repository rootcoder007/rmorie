# SPDX-License-Identifier: AGPL-3.0-or-later

#' DFBETAS scaled change in coefficient j when obs i deleted
#'
#' Formula: DFBETAS_ij = (beta_j - beta_j(-i)) / (s_(i) sqrt((X'X)^-1_jj))
#'
#' The deletion update beta - beta(-i) = (X'X)^-1 x_i e_i / (1 - h_ii)
#' avoids refitting n times.  Cut-off 2/sqrt(n).
#'
#' @param y Response vector, length n.
#' @param X Design matrix with n rows.
#' @param intercept Whether to prepend a column of ones.
#' @return List with \code{estimate} (max |DFBETAS|), \code{dfbetas},
#'   \code{threshold}, \code{n_influential}, \code{n}, \code{p},
#'   \code{method}.
#' @references Belsley, Kuh & Welsch (1980), Regression Diagnostics,
#'   Wiley, ch. 2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dfbetb(V, V)
Dfbetb <- function(y, X, intercept = TRUE) {
  f <- .bkw_influence(y, X, intercept)
  n <- f$n
  p <- f$p
  D <- f$D
  out <- matrix(0, n, p)
  worst <- 0
  n_infl <- 0L
  thr <- 2 / sqrt(n)
  for (i in seq_len(n)) {
    si <- .bkw_sdel(f$sse, f$e[i], f$h[i], n, p)
    flag <- 0L
    for (j in seq_len(p)) {
      num <- 0
      for (a in seq_len(p)) num <- num + f$inv[[j]][a] * D[i, a]
      denom <- si * sqrt(f$inv[[j]][j]) * (1 - f$h[i])
      v <- if (denom != 0) num * f$e[i] / denom else NaN
      out[i, j] <- v
      if (!is.nan(v)) {
        if (abs(v) > worst) worst <- abs(v)
        if (abs(v) > thr) flag <- 1L
      }
    }
    n_infl <- n_infl + flag
  }
  .t1_result(estimate = worst, dfbetas = out, threshold = thr,
             n_influential = n_infl, n = n, p = p,
             method = "DFBETAS scaled change in coefficient when obs i deleted")
}
