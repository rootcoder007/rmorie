# SPDX-License-Identifier: AGPL-3.0-or-later
#' Exact Cheeger constant, with the eigenvalue bounds it satisfies.
#'
#' Computed by exhaustive enumeration of the vertex bipartitions, so it is
#' exact rather than a relaxation. Enumeration is exponential, hence the
#' \code{max_n} guard.
#'
#' Formula: h_G(S) = |E(S, Sbar)| / min(vol S, vol Sbar),
#'   h_G = min_S h_G(S); Cheeger inequality 2 h_G >= lambda_1 > h_G^2 / 2,
#'   and the sharper lambda_1 >= 1 - sqrt(1 - h_G^2)
#'
#' @param W Symmetric non-negative weight matrix, connected.
#' @param max_n Refuse to enumerate beyond this many vertices.
#' @return List with \code{h}, \code{argmin}, \code{cut}, \code{vol_S},
#'   \code{vol_complement}, \code{lambda1}, \code{upper_bound},
#'   \code{lower_bound}, \code{lower_bound_sharp}, \code{n}.
#' @references Chung (1997), Spectral Graph Theory, CBMS 92, Section 2.2,
#'   equations (2.1) and (2.2) for h_G(S) and h_G; Theorem 2.2 for
#'   2 h_G >= lambda_1 > h_G^2 / 2; Theorem 2.3 for
#'   lambda_1 >= 1 - sqrt(1 - h_G^2). Fetched from the author's own copy
#'   of the chapter.
#' @export
Cheeger <- function(W, max_n = 20) {
  W <- as.matrix(W)
  n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  if (n > as.integer(max_n))
    stop("exact enumeration refused above max_n vertices")
  if (n < 2L) stop("the Cheeger constant needs at least two vertices")
  d <- rowSums(W)
  vol <- sum(d)
  best <- NA_real_; arg <- integer(0); cut <- 0; vs <- 0
  for (mask in seq_len(2^(n - 1) - 1)) {
    bits <- as.integer(intToBits(mask))[seq_len(n - 1)]
    S <- c(1L, which(bits == 1L) + 1L)
    volS <- sum(d[S])
    if (volS == 0 || volS == vol) next
    e <- sum(W[S, -S, drop = FALSE])
    h <- e / min(volS, vol - volS)
    if (is.na(best) || h < best) { best <- h; arg <- S; cut <- e; vs <- volS }
  }
  s <- ifelse(d == 0, 0, 1 / sqrt(ifelse(d == 0, 1, d)))
  L <- -W; diag(L) <- d - diag(W)
  vals <- rev(.t1_eigsym(diag(s, n) %*% L %*% diag(s, n))$values)
  nz <- vals[vals > 1e-10]
  lam1 <- if (length(nz)) nz[1] else 0
  sharp <- if (best <= 1) 1 - sqrt(max(0, 1 - best^2)) else 1
  .t1_result(h = best, argmin = arg, cut = cut, vol_S = vs,
             vol_complement = vol - vs, lambda1 = lam1,
             upper_bound = 2 * best, lower_bound = best^2 / 2,
             lower_bound_sharp = sharp, n = n,
             method = "Cheeger constant with Chung Theorems 2.2 and 2.3")
}
