# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Set Transformer PMA attention pooling (Sett). Bit-identical mirror
# of src/morie/fn/setT.py: single-head scaled dot-product attention,
# rFF relu feed-forward, LayerNorm (Ba 2016) with population variance
# and eps = 1e-5.

.morie_sett_ln <- function(row, eps = 1e-5) {
  n <- length(row)
  mu <- sum(row) / n
  v <- sum((row - mu)^2) / n
  (row - mu) / sqrt(v + eps)
}

.morie_sett_rff <- function(M, W1, b1, W2, b2) {
  H <- M %*% W1
  R <- pmax(sweep(H, 2L, b1, "+"), 0)
  sweep(R %*% W2, 2L, b2, "+")
}

.morie_sett_attend <- function(X, Y, Wq, Wk, Wv) {
  Q <- X %*% Wq; K <- Y %*% Wk; V <- Y %*% Wv
  dk <- ncol(Q)
  S <- (Q %*% t(K)) * (1 / sqrt(dk))
  W <- t(apply(S, 1L, function(row) {
    m <- max(row); e <- exp(row - m); e / sum(e)
  }))
  if (nrow(Y) == 1L) W <- matrix(W, nrow = nrow(X))
  list(O = W %*% V, W = W)
}

.morie_sett_mab <- function(X, Y, p) {
  at <- .morie_sett_attend(X, Y, p$Wq, p$Wk, p$Wv)
  H <- t(apply(X + at$O, 1L, .morie_sett_ln))
  if (ncol(X) == 1L) H <- matrix(H, nrow = nrow(X))
  F_ <- .morie_sett_rff(H, p$W1, p$b1, p$W2, p$b2)
  O <- t(apply(H + F_, 1L, .morie_sett_ln))
  if (ncol(X) == 1L) O <- matrix(O, nrow = nrow(X))
  list(O = O, W = at$W)
}

#' Set Transformer attention pooling PMA_k
#'
#' Lee, Lee, Kim, Kosiorek, Choi and Teh (2019), "Set Transformer",
#' ICML 2019 (PMLR 97), arXiv:1810.00825: MAB(X, Y) =
#' LayerNorm(H + rFF(H)) with H = LayerNorm(X + Multihead(X, Y, Y))
#' (Eq 7); PMA_k(Z) = MAB(S, rFF(Z)) (Sec 3.2) pools an n-element set
#' to exactly k vectors and is permutation invariant in the set
#' elements (the test anchor).
#'
#' @param Z Input set (n x d), one element per row.
#' @param S Seed matrix (k x d), the learnable queries.
#' @param params Named list: Wq, Wk, Wv (d x d), W1 (d x d_ff),
#'   b1 (d_ff), W2 (d_ff x d), b2 (d).
#' @return List with \code{output} (k x d), \code{attention} (k x n),
#'   \code{k}, \code{estimate}, \code{n}, \code{method}.
#' @references Lee, J. et al. (2019), ICML 2019, arXiv:1810.00825,
#'   Eqs 7-8 and Section 3.2. Local source:
#'   fetched-wave3/lee-etal-2019-set-transformer-arxiv1810.00825.pdf.
#' @export
Sett <- function(Z, S, params) {
  Za <- as.matrix(Z); Sa <- as.matrix(S)
  storage.mode(Za) <- "double"; storage.mode(Sa) <- "double"
  if (ncol(Za) != ncol(Sa)) {
    stop(sprintf("Sett: Z width %d != seed width %d", ncol(Za), ncol(Sa)), call. = FALSE)
  }
  for (nm in c("Wq", "Wk", "Wv", "W1", "b1", "W2", "b2")) {
    if (is.null(params[[nm]])) stop(sprintf("Sett: params is missing %s", nm), call. = FALSE)
  }
  p <- lapply(params, function(v) { m <- as.matrix(v); storage.mode(m) <- "double"; m })
  p$b1 <- as.numeric(params$b1); p$b2 <- as.numeric(params$b2)
  FZ <- .morie_sett_rff(Za, p$W1, p$b1, p$W2, p$b2)
  r <- .morie_sett_mab(Sa, FZ, p)
  list(output = r$O, attention = r$W, k = nrow(Sa),
       estimate = r$O[1, 1], n = nrow(Za),
       method = "Set Transformer PMA_k(Z) = MAB(S, rFF(Z)) (Lee et al. 2019, Eq 7 + Sec 3.2)")
}
