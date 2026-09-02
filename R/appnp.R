# SPDX-License-Identifier: AGPL-3.0-or-later
#' Personalised propagation of neural predictions
#'
#' Ahat = Dtilde^\{-1/2\}(A + I)Dtilde^\{-1/2\};
#' Z = alpha (I - (1-alpha) Ahat)^\{-1\} H (PPNP), or K steps of
#' Z <- (1-alpha) Ahat Z + alpha H from Z = H (APPNP), with a row-wise
#' softmax on the last step.
#'
#' @param A Adjacency matrix without self-loops; symmetrised internally.
#' @param H Per-node predictions, one row per node.
#' @param alpha Teleport probability in (0, 1].
#' @param K Power-iteration steps.
#' @param exact Use the closed-form PPNP inverse.
#' @param softmax Apply the row-wise softmax.
#'
#' @return List with Z, alpha, K, exact, n, c.
#' @references Klicpera, Bojchevski and Guennemann (2019), ICLR;
#'   arXiv:1810.05997, Sect. 3.  Read from the ar5iv rendering.
#' @export
#' @examples
#' A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' H <- matrix(rnorm(6), 3, 2)
#' Appnp(A, H)
Appnp <- function(A, H, alpha = 0.1, K = 10, exact = FALSE,
                  softmax = TRUE) {
  Am <- .t1_mat(A); n <- nrow(Am)
  if (ncol(Am) != n) stop("A must be square")
  Hm <- .t1_mat(H)
  if (nrow(Hm) != n) stop("H must have one row per node")
  c <- ncol(Hm); alpha <- as.numeric(alpha)
  if (!(alpha > 0 && alpha <= 1)) stop("alpha must lie in (0, 1]")
  At <- (Am + t(Am)) / 2 + diag(n)
  deg <- rowSums(At)
  if (any(deg <= 0))
    stop("every node must have positive degree after A + I")
  ds <- 1 / sqrt(deg)
  Ah <- ds * At * rep(ds, each = n)
  dim(Ah) <- c(n, n)
  if (isTRUE(exact)) {
    Z <- solve(diag(n) - (1 - alpha) * Ah, alpha * Hm)
    steps <- 0L
  } else {
    steps <- as.integer(K)
    Z <- Hm
    for (k in seq_len(steps)) Z <- (1 - alpha) * (Ah %*% Z) + alpha * Hm
  }
  Z <- matrix(as.numeric(Z), nrow = n)
  if (isTRUE(softmax)) {
    Z <- t(apply(Z, 1L, function(r) { e <- exp(r - max(r)); e / sum(e) }))
    dim(Z) <- c(n, c)
  }
  .t1_result(Z = Z, alpha = alpha, K = steps, exact = isTRUE(exact),
             n = n, c = c,
             method = "APPNP personalised-PageRank propagation (Klicpera et al. 2019)")
}
