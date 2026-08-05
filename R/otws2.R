# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wasserstein distance on the line, by sorting
#'
#' In one dimension the optimal coupling is the monotone one, so the whole
#' linear program collapses to a pair of sorts: the k-th smallest goes to
#' the k-th smallest. This is not an approximation, and it is the reason
#' every sliced method exists.
#'
#' Formula: \code{W_p^p = (1/n) sum_i |x_(i) - y_(i)|^p} for two
#' equal-weight samples -- Bobkov and Ledoux (2019) Section 2; Peyre and
#' Cuturi (2019) Remark 2.30.
#'
#' @param x,y Two samples of equal length.
#' @param p Exponent, positive.
#' @return List with \code{Wp}, \code{Wp_p}, \code{n}, \code{p}.
#' @references Bobkov, S. and Ledoux, M. (2019). Memoirs of the American
#'   Mathematical Society 261(1259). \doi{10.1090/memo/1259}.
#' @export
Otws2 <- function(x, y, p = 2) {
  w <- .ot_wp1d(x, y, as.numeric(p))
  .t1_result(Wp = w, Wp_p = w^as.numeric(p), n = length(as.numeric(x)),
             p = as.numeric(p), method = "Univariate Wasserstein distance")
}
