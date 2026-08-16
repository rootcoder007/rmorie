# Set Transformer attention pooling (MAB / SAB / PMA).
# Sources: Lee, J., Lee, Y., Kim, J., Kosiorek, A. R., Choi, S. & Teh,
# Y. W. (2019), "Set Transformer: A Framework for Attention-based
# Permutation-Invariant Neural Networks", ICML 2019 (PMLR 97),
# arXiv:1810.00825. Ba, J. L., Kiros, J. R. & Hinton, G. E. (2016)
# "Layer Normalization", arXiv:1607.06450.
#
# Native implementation mirroring morie.fn.setT exactly: the same
# LayerNorm, the same row-wise rFF, the same scaled dot-product
# attention with explicit projection weights, and the same
# PMA_k(Z) = MAB(S, rFF(Z)) definition.

#' Row-wise layer normalisation
#' @keywords internal
#' @noRd
.setT_ln <- function(row, eps = 1e-5) {
  n <- length(row); mu <- mean(row)
  s <- sqrt(mean((row - mu)^2) + eps)
  (row - mu) / s
}

#' Row-wise feed-forward: rFF(M) = relu(M W1 + b1) W2 + b2
#' @keywords internal
#' @noRd
.setT_rff <- function(M, W1, b1, W2, b2) {
  H <- M %*% W1
  R <- pmax(H + matrix(b1, nrow = nrow(H), ncol = length(b1),
                       byrow = TRUE), 0)
  O <- R %*% W2
  O + matrix(b2, nrow = nrow(O), ncol = length(b2), byrow = TRUE)
}

#' Scaled dot-product attention with explicit projections
#' @keywords internal
#' @noRd
.setT_attend <- function(X, Y, Wq, Wk, Wv) {
  Q <- X %*% Wq; K <- Y %*% Wk; V <- Y %*% Wv
  dk <- ncol(Q)
  S <- (Q %*% t(K)) / sqrt(dk)
  # softmax per row
  m <- apply(S, 1, max)
  e <- exp(S - matrix(m, nrow = nrow(S), ncol = ncol(S), byrow = FALSE))
  z <- rowSums(e)
  W <- e / matrix(z, nrow = nrow(e), ncol = ncol(e), byrow = FALSE)
  list(O = W %*% V, W = W)
}

#' Multihead Attention Block (Eq 7), single-head
#' @keywords internal
#' @noRd
.setT_mab <- function(X, Y, p) {
  att <- .setT_attend(X, Y, p$Wq, p$Wk, p$Wv)
  H <- t(apply(X + att$O, 1, .setT_ln))
  F <- .setT_rff(H, p$W1, p$b1, p$W2, p$b2)
  O <- t(apply(H + F, 1, .setT_ln))
  list(O = O, W = att$W)
}

#' Set Transformer PMA_k attention pooling
#'
#' PMA_k(Z) = MAB(S, rFF(Z)) (Lee et al. 2019, Eq 7 + Sec 3.2). Because
#' every row of Z enters only through K and V of the pooling
#' attention, the output is PERMUTATION INVARIANT in the set
#' elements.
#'
#' @param Z Numeric matrix \code{n x d}.
#' @param S Numeric matrix \code{k x d} of learnable seed queries.
#' @param params List with \code{Wq, Wk, Wv} (\code{d x d}),
#'   \code{W1} (\code{d x d_ff}), \code{b1} (\code{d_ff}),
#'   \code{W2} (\code{d_ff x d}), \code{b2} (\code{d}).
#' @return List with \code{output} (k x d), \code{attention} (k x n),
#'   \code{k}, \code{estimate} (first output element), \code{n},
#'   \code{method}.
#' @references Lee et al. (2019), arXiv:1810.00825.
#' @export
morie_setT_setT <- function(Z, S, params) {
  Za <- as.matrix(Z); Sa <- as.matrix(S)
  if (ncol(Za) != ncol(Sa))
    stop("setT: Z width ", ncol(Za), " != seed width ", ncol(Sa))
  for (name in c("Wq", "Wk", "Wv", "W1", "b1", "W2", "b2"))
    if (is.null(params[[name]])) stop("setT: params is missing ", name)
  p <- lapply(params, function(v) {
    if (is.matrix(v) || is.numeric(v)) as.matrix(v) else v
  })
  Zl <- Za; Sl <- Sa
  FZ <- .setT_rff(Zl, p$W1, p$b1, p$W2, p$b2)
  M <- .setT_mab(Sl, FZ, p)
  list(output = M$O, attention = M$W, k = nrow(Sl),
       estimate = M$O[1L, 1L], n = nrow(Za),
       method = "Set Transformer PMA_k(Z) = MAB(S, rFF(Z)) (Lee et al. 2019, Eq 7 + Sec 3.2)")
}

#' Back-compatible wrapper over \code{setT}
#'
#' @param X Numeric matrix \code{n x d}.
#' @param k Ignored; \code{S} supplies the output count.
#' @param S Numeric matrix \code{k x d}.
#' @param params List of weights.
#' @return As for \code{setT}.
#' @export
morie_setT_set_transformer <- function(X = NULL, k = NULL, S = NULL,
                                       params = NULL) {
  if (is.null(X) || is.null(S) || is.null(params))
    stop("set_transformer: X, S and params are required")
  morie_setT_setT(X, S, params)
}

# house entry point: the package exports one morie_<module>
morie_setT <- morie_setT_setT

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
Sett <- morie_setT
