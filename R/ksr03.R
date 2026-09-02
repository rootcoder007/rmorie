# SPDX-License-Identifier: AGPL-3.0-or-later
#' Uniform distance sup_t |F_n(t) - F(t)|, with the DKW tail bound.
#'
#' The supremum is attained only at the sample points, and at each one it
#' must be evaluated on BOTH sides.
#'
#' Formula: D_n = max_i max( i/n - F(x_(i)), F(x_(i)) - (i-1)/n );
#'   P(D_n > eps) <= 2 exp(-2 n eps^2)
#'
#' @param x The sample.
#' @param F The true cdf evaluated at the SORTED sample.
#' @return List with \code{statistic}, \code{d_plus}, \code{d_minus},
#'   \code{argmax}, \code{dkw_bound}, \code{n}.
#' @references Kosorok (2008), Introduction to Empirical Processes and
#'   Semiparametric Inference, Section 2.1, equation (2.3). Fetched as the
#'   full text of the book. The sharp constant in the tail bound is
#'   Massart (1990), Annals of Probability 18(3), 1269-1283; it is NOT in
#'   Kosorok and is cited to its own source.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Glivenko(V, V)
Glivenko <- function(x, F) {
  x <- .t1_vec(x); F <- .t1_vec(F); n <- length(x)
  if (n < 1L) stop("the sample must be non-empty")
  if (length(F) != n) stop("x and F must have the same length")
  Fs <- F[order(x)]
  i <- seq_len(n)
  a <- i / n - Fs
  b <- Fs - (i - 1) / n
  dp <- max(a); dm <- max(b)
  arg <- which.max(pmax(a, b))
  D <- max(dp, dm)
  .t1_result(statistic = D, d_plus = dp, d_minus = dm,
             argmax = as.numeric(arg),
             dkw_bound = min(1, 2 * exp(-2 * n * D^2)), n = as.numeric(n),
             method = "Glivenko-Cantelli supremum with the DKW-Massart bound")
}

# NAMESPACE exported both of these names, but only the short function above
# was ever defined, so loading the namespace could not resolve them. Same
# alias pattern as ksr02.R.

#' @rdname Glivenko
#' @keywords internal
#' @export
morie_ksr03_kosorok_glivenko_cantelli <- Glivenko

#' @rdname Glivenko
#' @keywords internal
#' @export
morie_kosorok_glivenko_cantelli <- Glivenko
