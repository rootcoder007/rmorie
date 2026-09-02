# SPDX-License-Identifier: AGPL-3.0-or-later
#' Alternate KL projections of a Gibbs kernel onto the two marginals
#'
#' Entropic transport is a Bregman projection: the regularised plan is the
#' KL-projection of \code{K = exp(-C/eps)} onto the transport polytope
#' (eq. 4.7), and the polytope is the intersection of two affine sets, so
#' alternating projections converge to it. Projecting on the rows means
#' rescaling each row to sum to \code{a}; on the columns, to \code{b}.
#'
#' Formula: \code{T <- diag(a / (T 1)) T} then
#' \eqn{T <- T diag(b / (T prime 1))}, repeated -- Benamou et al. (2015)
#' eq. (5)-(6); Peyre and Cuturi (2019) eq. (4.6)-(4.7).
#'
#' @param K Gibbs kernel, n by m, entrywise positive.
#' @param a Row marginal.
#' @param b Column marginal.
#' @param max_iter Number of row/column sweeps; fixed, not tolerance-driven.
#' @return List with \code{T}, \code{iters}, \code{row_err},
#'   \code{col_err}, \code{n}, \code{m}.
#' @references Benamou, J.-D., Carlier, G., Cuturi, M., Nenna, L. and
#'   Peyre, G. (2015). SIAM Journal on Scientific Computing
#'   37(2):A1111-A1138. \doi{10.1137/141000439}.
#' @export
#' @examples
#' Otbreg(K = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
Otbreg <- function(K, a, b, max_iter = 200) {
  Km <- as.matrix(K)
  aa <- .ot_hist(a); bb <- .ot_hist(b)
  n <- nrow(Km); m <- ncol(Km)
  if (length(aa) != n || length(bb) != m)
    stop("kernel does not match the marginals")
  if (any(Km <= 0)) stop("the Gibbs kernel must be entrywise positive")
  T <- Km
  it <- as.integer(max_iter)
  for (k in seq_len(it)) {
    rs <- rowSums(T); f <- ifelse(rs > 0, aa / rs, 0)
    T <- T * f
    cs <- colSums(T); g <- ifelse(cs > 0, bb / cs, 0)
    T <- T * rep(g, each = n)
  }
  .t1_result(T = T, iters = it,
             row_err = max(abs(rowSums(T) - aa)),
             col_err = max(abs(colSums(T) - bb)),
             n = n, m = m,
             method = "Iterative Bregman projections")
}
