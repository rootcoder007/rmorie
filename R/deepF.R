# SPDX-License-Identifier: AGPL-3.0-or-later

# 0.5 sum_f ((sum_i v_if x_i)^2 - sum_i (v_if x_i)^2): the O(kn) identity
# for the pairwise term, exactly equal to the naive double sum over i<j.
#' 0.5 sum_f ((sum_i v_if x_i)^2 - sum_i (v_if x_i)^2): the O(kn)
#' identity
#'
#' for the pairwise term, exactly equal to the naive double sum over
#' i<j.
#'
#' @param V See Usage.
#' @param x See Usage.
#' @return A numeric value.
#' @export
.fm_second_order <- function(V, x) {
  n <- length(x)
  K <- ncol(V)
  tot <- 0
  for (f in seq_len(K)) {
    s1 <- 0; s2 <- 0
    for (i in seq_len(n)) {
      t <- V[i, f] * x[i]
      s1 <- s1 + t
      s2 <- s2 + t * t
    }
    tot <- tot + s1 * s1 - s2
  }
  0.5 * tot
}

#' DeepFM
#'
#' Formula: FM (low-order) + DNN (high-order) on shared embedding
#'
#' The wide part is a factorization machine over the shared embedding V,
#' the deep part an MLP over the same V, and the logit is their sum
#' through a sigmoid.  Sharing the embedding is the whole point: no
#' hand-crafted crosses and no separate pretraining.  With the deep
#' branch scaled to zero the prediction is exactly the FM prediction.
#'
#' @param X An n x p feature matrix.
#' @param y Binary labels for the reported log loss, or NULL.
#' @param K Embedding dimension.
#' @param mlp_h Hidden width of the deep branch.
#' @param w0 Global bias.
#' @param seed Seed of the deterministic stream.
#' @param deep_scale Multiplier on the deep branch; 0 leaves the FM.
#' @return List with \code{estimate}, \code{p_hat}, \code{fm_part},
#'   \code{deep_part}, \code{logloss}, \code{n}, \code{p}, \code{K},
#'   \code{method}.
#' @references Guo, Tang, Ye, Li & He (2017), DeepFM, IJCAI
#'   2017:1725-1731; Rendle (2010), Factorization Machines,
#'   ICDM 2010:995-1000.
#' @export
DeepF <- function(X, y = NULL, K = 4, mlp_h = 4, w0 = 0, seed = 42,
                  deep_scale = 1) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  if (n == 0L) stop("empty input: X has no rows")
  p <- ncol(Xm)
  K <- as.integer(K)
  if (K < 1L) stop("K must be at least 1")
  h <- as.integer(mlp_h)
  if (h < 1L) stop("mlp_h must be at least 1")
  e <- .ghc_rng(seed)
  w <- numeric(p)
  for (j in seq_len(p)) w[j] <- .ghc_norm(e, 1L, 0, 0.1)
  V <- matrix(0, p, K)
  for (j in seq_len(p)) for (f in seq_len(K)) V[j, f] <- .ghc_norm(e, 1L, 0, 0.1)
  W1 <- matrix(0, p * K, h)
  for (q in seq_len(p * K)) for (t in seq_len(h))
    W1[q, t] <- .ghc_norm(e, 1L, 0, 0.1)
  b1 <- numeric(h)
  for (t in seq_len(h)) b1[t] <- .ghc_norm(e, 1L, 0, 0.1)
  W2 <- numeric(h)
  for (t in seq_len(h)) W2[t] <- .ghc_norm(e, 1L, 0, 0.1)
  fm <- numeric(n); dp <- numeric(n); ph <- numeric(n)
  for (i in seq_len(n)) {
    x <- Xm[i, ]
    lin <- w0
    for (j in seq_len(p)) lin <- lin + w[j] * x[j]
    wide <- lin + .fm_second_order(V, x)
    emb <- numeric(p * K)
    q <- 1L
    for (j in seq_len(p)) for (f in seq_len(K)) {
      emb[q] <- V[j, f] * x[j]
      q <- q + 1L
    }
    hid <- numeric(h)
    for (t in seq_len(h)) {
      s <- b1[t]
      for (qq in seq_len(p * K)) s <- s + emb[qq] * W1[qq, t]
      hid[t] <- .s03relu(s)
    }
    deep <- 0
    for (t in seq_len(h)) deep <- deep + hid[t] * W2[t]
    fm[i] <- wide
    dp[i] <- deep_scale * deep
    ph[i] <- .s03sigmoid(wide + deep_scale * deep)
  }
  ll <- NaN
  if (!is.null(y)) {
    yv <- .s03vec(y)
    if (length(yv) != n) stop("y must have one label per row")
    if (any(!(yv %in% c(0, 1)))) stop("y must be binary 0/1")
    s <- 0
    for (i in seq_len(n))
      s <- s - (yv[i] * log(ph[i] + 1e-300) +
                  (1 - yv[i]) * log(1 - ph[i] + 1e-300))
    ll <- s / n
  }
  tot <- 0
  for (v in ph) tot <- tot + v
  .t1_result(estimate = tot / n, p_hat = ph, fm_part = fm, deep_part = dp,
             logloss = ll, n = n, p = p, K = K,
             method = "DeepFM: factorization machine plus deep network")
}
