# SPDX-License-Identifier: AGPL-3.0-or-later
#' PERMANOVA pseudo-F from a distance matrix
#'
#' SS_T = (1/N) sum_\{i<j\} d_ij^2; SS_W = sum_\{i<j\} d_ij^2 eps_ij/n_g;
#' F = (SS_A/(a-1)) / (SS_W/(N-a)) with SS_A = SS_T - SS_W.
#'
#' @param X Rows are compositions (positive) or numeric vectors.
#' @param group Group label per row.
#' @param aitchison Take clr coordinates before measuring distance.
#'
#' @return List with F, SSA, SSW, SST, df1, df2, N, a, sizes.
#' @references Anderson (2001), Austral Ecology 26(1), 32-46, Equations
#'   (3)-(5).  Standard published form; the article is paywalled and the
#'   download attempted returned a stub, so it was not read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Permanova(V, V)
Permanova <- function(X, group, aitchison = TRUE) {
  M <- .t1_mat(X); N <- nrow(M); D <- ncol(M)
  g <- as.character(group)
  if (length(g) != N) stop("group must have one label per row")
  if (N < 3) stop("need at least three units")
  if (isTRUE(aitchison)) {
    if (any(M <= 0)) stop("compositions must be strictly positive")
    L <- log(M); Y <- L - rowMeans(L)
  } else {
    Y <- M
  }
  labs <- unique(g)
  a <- length(labs)
  if (a < 2) stop("need at least two groups")
  size <- table(g)
  sst <- 0; ssw <- 0
  for (i in seq_len(N - 1L)) {
    for (j in (i + 1L):N) {
      d2 <- sum((Y[i, ] - Y[j, ])^2)
      sst <- sst + d2
      if (g[i] == g[j]) ssw <- ssw + d2 / as.numeric(size[[g[i]]])
    }
  }
  sst <- sst / N
  ssa <- sst - ssw
  df1 <- a - 1L; df2 <- N - a
  .t1_result(F = if (ssw > 0) (ssa / df1) / (ssw / df2) else Inf,
             SSA = ssa, SSW = ssw, SST = sst, df1 = df1, df2 = df2,
             N = N, a = a, sizes = as.numeric(size[labs]),
             method = "PERMANOVA pseudo-F on Aitchison distances (Anderson 2001)")
}
