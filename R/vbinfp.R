# SPDX-License-Identifier: AGPL-3.0-or-later
#' Barber-Agakov variational lower bound on mutual information
#'
#' SOURCE. Barber, D. and Agakov, F. (2003), "The IM Algorithm: A
#' Variational Approach to Information Maximization", Advances in Neural
#' Information Processing Systems 16 (NIPS 2003), MIT Press.
#'
#' Write I(X;Y) = H(X) - H(X|Y) and replace the intractable posterior
#' p(x|y) by a variational decoder q(x|y). Since H(X|Y) =
#' -E[log q(x|y)] - E_{p(y)}[KL(p(.|y) || q(.|y))] and the KL is
#' non-negative, dropping it can only lower the value:
#' I(X;Y) >= H(X) + E_{p(x,y)}[log q(x|y)], with equality exactly when
#' q(x|y) = p(x|y) for every y that occurs. The gap is the average KL, so
#' \code{gap} is non-negative by Gibbs' inequality -- a property this
#' module asserts rather than assumes.
#'
#' SCOPE. Discrete X and Y with the joint estimated by counting; all
#' quantities in nats. \code{q} is an ny-by-nx matrix whose row j is
#' q(. | y = level j); omitted, the empirical conditional is used, making
#' the bound tight and equal to the plug-in mutual information. A
#' continuous version would need a parametric decoder and an optimiser
#' and is not implemented -- this implementation's scope choice.
#'
#' The form I(X;Y) >= E[log q(y|x)/p(y)] is the same bound with X and Y
#' exchanged; call the function with the arguments swapped.
#'
#' @param X,Y Paired discrete observations of equal length.
#' @param q ny-by-nx decoder matrix, or NULL for the empirical
#'   conditional.
#' @return List with \code{bound}, \code{entropy_x}, \code{entropy_y},
#'   \code{expected_log_q}, \code{plugin_mi}, \code{gap},
#'   \code{conditional_entropy}, \code{n}, \code{nx}, \code{ny}.
#' @references Barber, D. and Agakov, F. (2003). Advances in Neural
#'   Information Processing Systems 16 (NIPS 2003). MIT Press.
#' @examples
#' Vbinfp(c(1, 1, 2, 2), c(1, 1, 2, 2))$bound
#' @export
Vbinfp <- function(X, Y, q = NULL) {
  xs <- X
  ys <- Y
  n <- length(xs)
  if (n == 0L) stop("variational_bound: X is empty")
  if (length(ys) != n) stop("variational_bound: X and Y must have the same length")
  lx <- sort(unique(xs))
  ly <- sort(unique(ys))
  nx <- length(lx)
  ny <- length(ly)
  joint <- matrix(0, ny, nx)
  for (k in seq_len(n)) {
    joint[match(ys[k], ly), match(xs[k], lx)] <- joint[match(ys[k], ly), match(xs[k], lx)] + 1
  }
  px <- numeric(nx)
  py <- numeric(ny)
  for (j in seq_len(ny)) for (i in seq_len(nx)) {
    joint[j, i] <- joint[j, i] / n
    px[i] <- px[i] + joint[j, i]
    py[j] <- py[j] + joint[j, i]
  }
  if (is.null(q)) {
    Q <- matrix(0, ny, nx)
    for (j in seq_len(ny)) for (i in seq_len(nx)) {
      Q[j, i] <- if (py[j] > 0) joint[j, i] / py[j] else 0
    }
  } else {
    Q <- .s03mat(q)
    if (nrow(Q) != ny || ncol(Q) != nx) stop("variational_bound: q must be ny-by-nx")
    for (j in seq_len(ny)) {
      s <- 0
      for (i in seq_len(nx)) {
        if (Q[j, i] < 0) stop("variational_bound: q has a negative entry")
        s <- s + Q[j, i]
      }
      if (abs(s - 1) > 1e-9) stop("variational_bound: each row of q must sum to one")
    }
  }
  hx <- 0
  for (i in seq_len(nx)) if (px[i] > 0) hx <- hx - px[i] * log(px[i])
  hy <- 0
  for (j in seq_len(ny)) if (py[j] > 0) hy <- hy - py[j] * log(py[j])
  elq <- 0
  for (j in seq_len(ny)) for (i in seq_len(nx)) {
    if (joint[j, i] > 0) {
      if (!(Q[j, i] > 0)) {
        stop("variational_bound: q assigns zero probability to an observed pair")
      }
      elq <- elq + joint[j, i] * log(Q[j, i])
    }
  }
  hxy <- 0
  for (j in seq_len(ny)) for (i in seq_len(nx)) {
    if (joint[j, i] > 0 && py[j] > 0) hxy <- hxy - joint[j, i] * log(joint[j, i] / py[j])
  }
  mi <- hx - hxy
  bound <- hx + elq
  .t1_result(estimate = bound, bound = bound, entropy_x = hx, entropy_y = hy,
             expected_log_q = elq, plugin_mi = mi, gap = mi - bound,
             conditional_entropy = hxy, n = n, nx = nx, ny = ny,
             method = "I(X;Y) >= H(X) + E[log q(x|y)] (Barber and Agakov 2003)")
}
