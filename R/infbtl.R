# SPDX-License-Identifier: AGPL-3.0-or-later
#' The information bottleneck
#'
#' Tishby, Pereira and Bialek (1999), The information bottleneck method,
#' Allerton 37, 368-377 (physics/0004057 -- FETCHED).  The Lagrangian is L
#' = I(X;T) - beta I(T;Y) and the self-consistent solution printed there is
#' p(t|x) = p(t)/Z(x, beta) exp\[-beta sum_y p(y|x) log(p(y|x)/p(y|t))\],
#' p(t) = sum_x p(x) p(t|x), p(y|t) = (1/p(t)) sum_x p(y|x) p(t|x) p(x),
#' iterated to a fixed point.  The exponent is a Kullback-Leibler
#' divergence -- the RELEVANT distortion, which is what distinguishes the
#' information bottleneck from ordinary rate-distortion, where d comes from
#' outside.
#'
#' Determinism: the iteration needs an initial p(t|x); a low-discrepancy
#' deterministic initialisation is used, not a random one.
#'
#' @param X,Y paired discrete observations.
#' @param beta the trade-off parameter.
#' @param T size of the bottleneck alphabet.
#' @param iters,tol iteration controls.
#' @param pxy the joint p(x, y) directly, instead of X and Y.
#' @return list: estimate, lagrangian, ixt, ity, p_t_x, p_t, beta, method.
#' @keywords internal
#' @examples
#' Infobtl(NULL, NULL, 5, 2, pxy = matrix(c(0.4, 0.1, 0.1, 0.4), 2, 2))$ixt
#' @export
Infobtl <- function(X, Y = NULL, beta = 5, T = 2, iters = 500, tol = 1e-14,
                    pxy = NULL) {
  if (!is.null(pxy)) {
    J <- .s03mat(pxy)
  } else {
    a <- as.character(X); b <- as.character(Y)
    la <- sort(unique(a), method = "radix"); lb <- sort(unique(b), method = "radix")
    J <- matrix(0, length(la), length(lb))
    for (i in seq_along(a)) {
      J[match(a[i], la), match(b[i], lb)] <- J[match(a[i], la), match(b[i], lb)] + 1 / length(a)
    }
  }
  n <- nrow(J); m <- ncol(J)
  px <- numeric(n)
  for (i in seq_len(n)) { s <- 0; for (j in seq_len(m)) s <- s + J[i, j]; px[i] <- s }
  pygx <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    pygx[i, j] <- if (px[i] > 0) J[i, j] / px[i] else 0
  }
  Tn <- as.integer(T)
  Q <- matrix(0, n, Tn)
  for (i in seq_len(n)) {
    row <- numeric(Tn)
    for (t in seq_len(Tn)) row[t] <- 0.5 + .s03vdc((i - 1L) * Tn + (t - 1L), 2L)
    s <- 0
    for (v in row) s <- s + v
    Q[i, ] <- row / s
  }
  pt <- numeric(Tn); pygt <- matrix(0, Tn, m)
  for (it in seq_len(as.integer(iters))) {
    for (t in seq_len(Tn)) {
      s <- 0
      for (i in seq_len(n)) s <- s + px[i] * Q[i, t]
      pt[t] <- s
    }
    for (t in seq_len(Tn)) for (j in seq_len(m)) {
      s <- 0
      for (i in seq_len(n)) s <- s + pygx[i, j] * Q[i, t] * px[i]
      pygt[t, j] <- if (pt[t] > 0) s / pt[t] else 0
    }
    delta <- 0
    for (i in seq_len(n)) {
      lw <- numeric(Tn)
      for (t in seq_len(Tn)) {
        kl <- 0
        for (j in seq_len(m)) {
          if (pygx[i, j] > 0 && pygt[t, j] > 0) {
            kl <- kl + pygx[i, j] * log(pygx[i, j] / pygt[t, j])
          } else if (pygx[i, j] > 0) {
            kl <- kl + pygx[i, j] * 700
          }
        }
        lw[t] <- log(if (pt[t] > 1e-300) pt[t] else 1e-300) - as.numeric(beta) * kl
      }
      z <- .s03logsumexp(lw)
      for (t in seq_len(Tn)) {
        nv <- exp(lw[t] - z)
        delta <- delta + abs(nv - Q[i, t])
        Q[i, t] <- nv
      }
    }
    if (delta < tol) break
  }
  ixt <- 0
  for (i in seq_len(n)) for (t in seq_len(Tn)) {
    if (Q[i, t] > 0 && pt[t] > 0) ixt <- ixt + px[i] * Q[i, t] * log(Q[i, t] / pt[t])
  }
  py <- numeric(m)
  for (j in seq_len(m)) { s <- 0; for (i in seq_len(n)) s <- s + J[i, j]; py[j] <- s }
  ity <- 0
  for (t in seq_len(Tn)) for (j in seq_len(m)) {
    if (pygt[t, j] > 0 && py[j] > 0) ity <- ity + pt[t] * pygt[t, j] * log(pygt[t, j] / py[j])
  }
  list(estimate = ixt - as.numeric(beta) * ity,
       lagrangian = ixt - as.numeric(beta) * ity, ixt = ixt, ity = ity,
       p_t_x = Q, p_t = pt, beta = as.numeric(beta),
       method = "Information bottleneck fixed point (Tishby, Pereira and Bialek 1999)")
}
