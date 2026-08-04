# SPDX-License-Identifier: AGPL-3.0-or-later
#' Backpropagation via chain rule for multi-layer networks
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 379-425], Chapter 10, Sections 10.8 and 10.8.1, equations
#' (10.12) to (10.17), pp. 411-413, and the hand computation of Illustrative
#' Example 10.1, pp. 413-417.
#'
#' Section 10.8.1 gives the algorithm literally: step 9,
#' delta_ij = (y_ij - yhat_ij) g'(z_ij); step 10,
#' psi_ik = g'(z_ik) sum_{j=1..L} delta_ij w_jk; step 11,
#' w_jk(t+1) = w_jk(t) + eta delta_ij V_ik (10.13); step 12,
#' w_kp(t+1) = w_kp(t) + eta psi_ik x_ip (10.17); and step 8 accumulates
#' E = (1/(2 n L)) sum_ij (yhat_ij - y_ij)^2.  Example 10.1 runs a four
#' pattern data set through it by hand and prints V, yhat, delta, psi, both
#' updated weight vectors and E = 0.03519; those printed numbers are the
#' anchor.
#'
#' The increments returned are the book's Delta w divided by eta, so that
#' w_new = w_old + eta * gradient reproduces (10.13) and (10.17).
#'
#' @param layers list of weight matrices W_1..W_L, W_l with one row per unit
#'   of layer l and a leading bias column, so z_l = W_l [1, a_{l-1}].
#' @param activations the forward pass a_0..a_L, one row per pattern; a_0 is
#'   the input without a bias column.
#' @param loss_grad n-by-units_L matrix of dE/dyhat, which step 9 takes to be
#'   (y - yhat).
#' @param act_fun activation name per layer, or one name for all.
#' @return list: estimate, gradients, deltas, loss, n, method.
#' @keywords internal
#' @examples
#' Bprop(list(matrix(c(0, 1), 1, 2)), list(matrix(1, 1, 1), matrix(1, 1, 1)),
#'       matrix(1, 1, 1), "linear")$estimate
#' @export
Bprop <- function(layers, activations, loss_grad, act_fun = "sigmoid") {
  dact <- function(name, a) {
    if (name == "sigmoid") return(a * (1 - a))
    if (name == "linear") return(1)
    if (name == "relu") return(if (a > 0) 1 else 0)
    if (name == "tanh") return(1 - a * a)
    stop(sprintf("backpropagation_chain_rule: unknown activation '%s'", name))
  }
  W <- lapply(layers, .s03mat)
  A <- lapply(activations, .s03mat)
  Gd <- .s03mat(loss_grad)
  L <- length(W)
  if (L == 0L) stop("backpropagation_chain_rule: no layers supplied")
  if (length(A) != L + 1L) {
    stop("backpropagation_chain_rule: need L+1 activation blocks for L layers")
  }
  n <- nrow(A[[1]])
  if (n == 0L) stop("backpropagation_chain_rule: no patterns supplied")
  for (a in A) if (nrow(a) != n) {
    stop("backpropagation_chain_rule: activation blocks disagree on the pattern count")
  }
  if (nrow(Gd) != n || ncol(Gd) != ncol(A[[L + 1L]])) {
    stop("backpropagation_chain_rule: loss_grad does not match the output layer")
  }
  fns <- if (length(act_fun) == 1L) rep(act_fun, L) else as.character(act_fun)
  if (length(fns) != L) {
    stop("backpropagation_chain_rule: one activation name per layer is required")
  }
  for (l in seq_len(L)) {
    if (nrow(W[[l]]) != ncol(A[[l + 1L]])) {
      stop(sprintf("backpropagation_chain_rule: layer %d has the wrong number of rows", l - 1L))
    }
    if (ncol(W[[l]]) != ncol(A[[l]]) + 1L) {
      stop(sprintf("backpropagation_chain_rule: layer %d has the wrong number of columns", l - 1L))
    }
  }
  deltas <- vector("list", L)
  uL <- ncol(A[[L + 1L]])
  d <- matrix(0, n, uL)
  for (i in seq_len(n)) for (j in seq_len(uL)) {
    d[i, j] <- Gd[i, j] * dact(fns[L], A[[L + 1L]][i, j])
  }
  deltas[[L]] <- d
  if (L > 1L) for (l in seq(L - 1L, 1L)) {
    u <- ncol(A[[l + 1L]])
    nxt <- deltas[[l + 1L]]
    new <- matrix(0, n, u)
    for (i in seq_len(n)) for (kk in seq_len(u)) {
      s <- 0
      for (j in seq_len(nrow(W[[l + 1L]]))) s <- s + nxt[i, j] * W[[l + 1L]][j, kk + 1L]
      new[i, kk] <- s * dact(fns[l], A[[l + 1L]][i, kk])
    }
    deltas[[l]] <- new
  }
  grads <- vector("list", L)
  for (l in seq_len(L)) {
    rows <- nrow(W[[l]]); cols <- ncol(W[[l]])
    G <- matrix(0, rows, cols)
    for (i in seq_len(n)) {
      prev <- c(1, A[[l]][i, ])
      for (j in seq_len(rows)) {
        dj <- deltas[[l]][i, j]
        for (cc in seq_len(cols)) G[j, cc] <- G[j, cc] + dj * prev[cc]
      }
    }
    grads[[l]] <- G
  }
  E <- 0
  for (i in seq_len(n)) for (j in seq_len(uL)) E <- E + Gd[i, j] * Gd[i, j]
  E <- E / (2 * n * uL)
  list(estimate = grads[[1]][1, 1], gradients = grads, deltas = deltas,
       loss = E, n = n,
       method = "delta/psi recursion of Chapter 10 Sect. 10.8.1 steps 9-12, eqs. (10.12)-(10.17)")
}
