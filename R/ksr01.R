# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical process G_n(t) = sqrt(n)(F_n(t) - F(t)) at given points
#'
#' The returned covariance is that of the LIMIT, not of G_n, and is
#' singular at the ends because F(t)(1 - F(t)) vanishes there.
#'
#' Formula: F_n(t) = n^-1 sum_i 1\{x_i <= t\};
#'   G_n(t) = sqrt(n) \[F_n(t) - F(t)\];
#'   cov\[G(s), G(t)\] = F(s ^ t) - F(s) F(t)
#'
#' @param x The sample.
#' @param t Points at which the process is evaluated, non-decreasing.
#' @param F The true cdf at those points.
#' @return List with \code{Fn}, \code{Gn}, \code{cov}, \code{sup_abs},
#'   \code{n}, \code{k}.
#' @references Kosorok (2008), Introduction to Empirical Processes and
#'   Semiparametric Inference, Section 2.1, equation (2.5). Fetched as the
#'   full text of the book.
#' @export
Empproc <- function(x, t, F) {
  x <- .t1_vec(x); t <- .t1_vec(t); F <- .t1_vec(F)
  n <- length(x); k <- length(t)
  if (n < 1L) stop("the sample must be non-empty")
  if (length(F) != k) stop("t and F must have the same length")
  if (any(F < 0 | F > 1)) stop("F must lie in [0, 1]")
  if (is.unsorted(t)) stop("t must be non-decreasing")
  Fn <- vapply(t, function(v) sum(x <= v), 0) / n
  Gn <- sqrt(n) * (Fn - F)
  cov <- outer(F, F, pmin) - outer(F, F)
  .t1_result(Fn = Fn, Gn = Gn, cov = cov, sup_abs = max(abs(Gn)),
             n = n, k = k,
             method = "Empirical process, Kosorok Section 2.1")
}

# NAMESPACE exported both of these names, but only the short function above
# was ever defined, so loading the namespace could not resolve them. Same
# alias pattern as ksr02.R.

#' @rdname Empproc
#' @keywords internal
#' @export
morie_ksr01_kosorok_empirical_process <- Empproc

#' @rdname Empproc
#' @keywords internal
#' @export
morie_kosorok_empirical_process <- Empproc
