# SPDX-License-Identifier: AGPL-3.0-or-later
#' Perceptron activation function (step function)
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 379-425\], Chapter 10, Section 10.2, pp. 382-383.  The net
#' input of a neuron is v_j = sum_j omega_ij x_j and its output is
#' y_j = g(v_j); "if we define this function as a unit step (also called
#' threshold), the output will be 1 if the net input is greater than zero;
#' otherwise the output will be 0".  The inequality is strict, so v = 0 falls
#' in the 0 branch here as it does there.  The bias enters as the weight on
#' the constant input, the way Section 10.8 writes z = sum_\{p=0\}^P w_kp x_ip
#' with x_i0 = 1.
#'
#' The update w <- w + eta y_i x_i is Rosenblatt's, not the book's: Section
#' 10.2 gives no learning rule for the single unit.  It is returned as an
#' increment the caller may apply, computed from the book's net input.
#'
#' @param X n-by-p matrix of inputs, one pattern per row.
#' @param w weight vector of length p.
#' @param b bias, the weight on the constant input.
#' @param y optional targets coded -1/+1; enables the increment.
#' @param eta learning rate for that increment.
#' @return list: estimate, a, v, sign, update, update_b, n, method.
#' @keywords internal
#' @examples
#' Percn(matrix(c(1, -1), 2, 1), 1, 0)$a
#' @export
Percn <- function(X, w, b, y = NULL, eta = 1) {
  XX <- .s03mat(X)
  ww <- .s03vec(w)
  bb <- .s03vec(b)
  if (nrow(XX) == 0L) stop("perceptron_activation: X is empty")
  p <- ncol(XX)
  if (length(ww) != p) stop("perceptron_activation: w does not match the columns of X")
  if (length(bb) != 1L) stop("perceptron_activation: b must be a single value")
  b0 <- bb[1]
  yy <- if (!is.null(y)) .s03vec(y) else NULL
  if (!is.null(yy) && length(yy) != nrow(XX)) {
    stop("perceptron_activation: y does not match the rows of X")
  }
  n <- nrow(XX)
  v <- numeric(n); a <- numeric(n); sg <- numeric(n)
  upd <- numeric(p); upd_b <- 0
  for (i in seq_len(n)) {
    s <- b0
    for (j in seq_len(p)) s <- s + XX[i, j] * ww[j]
    v[i] <- s
    a[i] <- if (s > 0) 1 else 0
    sg[i] <- if (s > 0) 1 else if (s < 0) -1 else 0
    if (!is.null(yy) && yy[i] * s <= 0) {
      for (j in seq_len(p)) upd[j] <- upd[j] + as.numeric(eta) * yy[i] * XX[i, j]
      upd_b <- upd_b + as.numeric(eta) * yy[i]
    }
  }
  list(estimate = a[1], a = a, v = v, sign = sg, update = upd, update_b = upd_b,
       n = n, method = "v = Xw + b with the unit-step g of Chapter 10 Sect. 10.2 (1 if v > 0, else 0)")
}
