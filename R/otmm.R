# SPDX-License-Identifier: AGPL-3.0-or-later
#' Average the transport cost over subsamples instead of solving once
#'
#' Full transport is cubic in the sample size and its gradient touches
#' every point, which is unusable inside a training loop. Averaging over
#' minibatches costs a constant per step, but it is a genuinely different
#' functional: the minibatch loss is not zero between a measure and
#' itself, and it does not vanish only at equality.
#'
#' Formula: \code{(1/M) sum_m OT_eps(X_m, Y_m)} over \code{M} batches --
#' Fatras et al. (2020) eq. (3). The batches here are consecutive slices,
#' taken cyclically, so the estimate is deterministic.
#'
#' @param X,Y Two point clouds.
#' @param batch_size Points per batch.
#' @param n_batches Number of batches.
#' @param epsilon Entropic strength, positive.
#' @return List with \code{loss}, \code{per_batch}, \code{batch_size},
#'   \code{n_batches}, \code{n}, \code{m}, \code{d}.
#' @references Fatras, K., Zine, Y., Flamary, R., Gribonval, R. and
#'   Courty, N. (2020). Proceedings of Machine Learning Research
#'   108:2131-2141 (AISTATS).
#' @export
#' @examples
#' set.seed(1)
#' r <- Otmm(X = rnorm(10), Y = rnorm(10), batch_size = 8L, n_batches = 8L, epsilon = 0.5); TRUE
Otmm <- function(X, Y, batch_size, n_batches, epsilon) {
  A <- as.matrix(X)
  B <- as.matrix(Y)
  n <- nrow(A)
  m <- nrow(B)
  d <- ncol(A)
  if (ncol(B) != d) stop("point clouds must share a dimension")
  bs <- as.integer(batch_size)
  M <- as.integer(n_batches)
  if (bs < 1L || M < 1L) stop("batch_size and n_batches must be positive")
  if (bs > n || bs > m) stop("batch_size exceeds a cloud")
  eps <- as.numeric(epsilon)
  u <- rep(1 / bs, bs)
  per <- vapply(seq_len(M) - 1L, function(b) {
    ix <- ((b * bs + seq_len(bs) - 1L) %% n) + 1L
    iy <- ((b * bs + seq_len(bs) - 1L) %% m) + 1L
    C <- .ot_costmat(A[ix, , drop = FALSE], B[iy, , drop = FALSE], 2)
    s <- .ot_sinkhorn(u, u, C, eps, 200)
    sum(s$T * C)
  }, 0)
  .t1_result(loss = sum(per) / M, per_batch = per, batch_size = bs,
             n_batches = M, n = n, m = m, d = d,
             method = "Minibatch optimal transport loss")
}
