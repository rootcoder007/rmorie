# SPDX-License-Identifier: AGPL-3.0-or-later
#' AutoInt: multi-head self-attention over feature embeddings
#'
#' Song et al. (2019), AutoInt: automatic feature interaction learning via
#' self-attentive neural networks, CIKM 28, 1161-1170 (arXiv:1810.11921 --
#' FETCHED).  Eq. (2) embeds a numeric field as e_m = v_m x_m; eq. (3) is
#' the key-value attention alpha_(m,k) = softmax_k <W_Query e_m, W_Key
#' e_k>; eq. (4) is the head output sum_k alpha_(m,k) W_Value e_k; eq. (6)
#' adds the residual, e_m^Res = ReLU(ehat_m + W_Res e_m), so the original
#' feature survives the interaction layer.  Projections are supplied by
#' the caller; the identity is used when they are not, which is the
#' degenerate case rather than an invented parameterisation.
#'
#' @param X field embeddings, one row per field; or raw values with `v`.
#' @param y optional labels; the logistic loss is then reported.
#' @param K number of attention heads.
#' @param Wq,Wk,Wv query, key and value projections.
#' @param Wres the residual projection of eq. (6).
#' @param v field embedding vectors, used with a 1-D X.
#' @return list: estimate, e_res, attention, loss, method.
#' @keywords internal
#' @examples
#' Autoint(matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE))$estimate
#' @export
Autoint <- function(X, y = NULL, K = 1, Wq = NULL, Wk = NULL, Wv = NULL,
                    Wres = NULL, v = NULL) {
  if (!is.null(v)) {
    xs <- .s03vec(X)
    vs <- .s03mat(v)
    E <- matrix(0, length(xs), ncol(vs))
    for (m in seq_along(xs)) for (j in seq_len(ncol(vs))) E[m, j] <- vs[m, j] * xs[m]
  } else {
    E <- .s03mat(X)
  }
  M <- nrow(E)
  d <- ncol(E)
  heads <- as.integer(K)
  prj <- function(W, hh) {
    if (is.null(W)) return(NULL)
    if (is.list(W)) return(.s03mat(W[[(hh %% length(W)) + 1L]]))
    .s03mat(W)
  }
  acc <- matrix(0, M, d)
  att0 <- vector("list", 0)
  for (h in seq_len(heads) - 1L) {
    Q <- prj(Wq, h)
    Kp <- prj(Wk, h)
    Vp <- prj(Wv, h)
    q <- vector("list", M)
    kk <- vector("list", M)
    vv <- vector("list", M)
    for (m in seq_len(M)) {
      q[[m]] <- if (!is.null(Q)) .s03matvec(Q, E[m, ]) else as.numeric(E[m, ])
      kk[[m]] <- if (!is.null(Kp)) .s03matvec(Kp, E[m, ]) else as.numeric(E[m, ])
      vv[[m]] <- if (!is.null(Vp)) .s03matvec(Vp, E[m, ]) else as.numeric(E[m, ])
    }
    for (m in seq_len(M)) {
      logits <- numeric(M)
      for (l in seq_len(M)) {
        s <- 0
        for (j in seq_along(q[[m]])) s <- s + q[[m]][j] * kk[[l]][j]
        logits[l] <- s
      }
      a <- .s03softmax(logits)
      if (h == 0L) att0[[length(att0) + 1L]] <- a
      for (j in seq_along(vv[[1]])) {
        t <- 0
        for (l in seq_len(M)) t <- t + a[l] * vv[[l]][j]
        acc[m, j] <- acc[m, j] + t
      }
    }
  }
  res <- matrix(0, M, d)
  for (m in seq_len(M)) {
    r <- if (!is.null(Wres)) .s03matvec(.s03mat(Wres), E[m, ]) else as.numeric(E[m, ])
    for (j in seq_along(r)) res[m, j] <- .s03relu(acc[m, j] + r[j])
  }
  pooled <- 0
  for (m in seq_len(M)) for (j in seq_len(ncol(res))) pooled <- pooled + res[m, j]
  loss <- NaN
  if (!is.null(y)) {
    yy <- .s03vec(y)
    p <- .s03sigmoid(pooled)
    loss <- 0
    for (t in yy) loss <- loss - (t * log(max(p, 1e-300)) + (1 - t) * log(max(1 - p, 1e-300)))
    loss <- loss / max(length(yy), 1L)
  }
  list(estimate = pooled, e_res = res, attention = att0, loss = loss,
       method = "AutoInt self-attentive interaction layer (Song et al. 2019, eqs. 2-6)")
}
