# SPDX-License-Identifier: AGPL-3.0-or-later
#' HITS hub and authority scores
#'
#' a = A' h and h = A a make the authority vector the principal eigenvector of
#' A'A and the hub vector that of A A'; computed by power iteration from the
#' uniform start with a fixed step count and scaled to a maximum of one, the
#' normalisation igraph::hits_scores reports.  Source consulted: Kleinberg
#' (1999), Journal of the ACM 46(5), 604-632, section 3.
#'
#' @param A adjacency matrix, entry (i, j) a link from i to j.
#' @param iters power-iteration steps.
#' @return list: estimate, authority, hub, eigenvalue, n, method.
#' @keywords internal
#' @examples
#' sgthits(matrix(c(0,1,1,0), 2, 2))$authority
#' @export
sgthits <- function(A, iters = 200L) {
  m <- as.matrix(A)
  dimnames(m) <- NULL
  n <- nrow(m)
  ata <- t(m) %*% m
  aat <- m %*% t(m)
  a <- rep(1, n)
  h <- rep(1, n)
  lam <- 0
  for (i in seq_len(as.integer(iters))) {
    a2 <- as.numeric(ata %*% a)
    nm <- sqrt(sum(a2 * a2))
    if (nm == 0) break
    a <- a2 / nm
    h2 <- as.numeric(aat %*% h)
    nh <- sqrt(sum(h2 * h2))
    if (nh > 0) h <- h2 / nh
    lam <- nm
  }
  amax <- max(abs(a))
  hmax <- max(abs(h))
  if (sum(a) < 0) a <- -a
  if (sum(h) < 0) h <- -h
  list(estimate = if (amax > 0) 1 else 0,
       authority = if (amax > 0) a / amax else a,
       hub = if (hmax > 0) h / hmax else h,
       eigenvalue = lam, n = n,
       method = "HITS hub and authority scores (Kleinberg 1999, sec. 3)")
}

# CANONICAL TEST
# stopifnot(abs(max(sgthits(matrix(c(0,1,1,0),2,2))$authority) - 1) < 1e-12)

#' @rdname sgthits
#' @keywords internal
#' @export
morie_sgthits <- sgthits
