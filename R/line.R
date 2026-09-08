# SPDX-License-Identifier: AGPL-3.0-or-later
#' LINE: large-scale information network embedding
#'
#' Tang, Qu, Wang, Zhang, Yan and Mei (2015), LINE: large-scale
#' information network embedding, WWW 24, 1067-1077 (arXiv:1503.03578 --
#' FETCHED).  First-order proximity, eqs. (1)-(3): p_1(v_i, v_j) = 1/(1 +
#' exp(-u_i' u_j)) and O_1 = -sum_(i,j) w_ij log p_1.  Second-order, eqs.
#' (4)-(6): p_2(v_j | v_i) = softmax_j(u'_j . u_i) and O_2 = -sum_(i,j)
#' w_ij log p_2.  Both are the KL divergence between the empirical and the
#' modelled proximity with the constants dropped, exactly as derived.
#' Embeddings are supplied or fitted by fixed full-batch gradient steps;
#' there is no negative sampling and no edge sampling, since both need a
#' generator and the objectives above are what is being computed.
#'
#' @param G weighted adjacency matrix.
#' @param dim embedding dimension.
#' @param order 1 or 2.
#' @param U,Uc vertex and context embeddings.
#' @param steps full-batch gradient steps.
#' @param lr step size.
#' @return list: estimate, O, O_start, U, Uc, n, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 0), 2, 2)
#' Lineembed(A, 2, 1)$O
#' @export
Lineembed <- function(G, dim = 2, order = 1, U = NULL, Uc = NULL, steps = 0,
                      lr = 0.05) {
  W <- .s03mat(G)
  n <- nrow(W)
  d <- as.integer(dim)
  if (is.null(U)) {
    U <- matrix(0, n, d)
    for (i in seq_len(n)) for (j in seq_len(d)) {
      U[i, j] <- .s03vdc((i - 1L) * d + (j - 1L), 2L) - 0.5
    }
  } else U <- .s03mat(U)
  if (is.null(Uc)) {
    Uc <- matrix(0, n, d)
    for (i in seq_len(n)) for (j in seq_len(d)) {
      Uc[i, j] <- .s03vdc((i - 1L) * d + (j - 1L), 3L) - 0.5
    }
  } else Uc <- .s03mat(Uc)
  obj <- function() {
    o <- 0
    for (i in seq_len(n)) {
      if (as.integer(order) == 2L) {
        logits <- numeric(n)
        for (cc in seq_len(n)) {
          s <- 0
          for (a in seq_len(d)) s <- s + Uc[cc, a] * U[i, a]
          logits[cc] <- s
        }
        lse <- .s03logsumexp(logits)
      }
      for (j in seq_len(n)) {
        if (W[i, j] == 0) next
        if (as.integer(order) == 2L) {
          o <- o - W[i, j] * (logits[j] - lse)
        } else {
          s <- 0
          for (a in seq_len(d)) s <- s + U[i, a] * U[j, a]
          p <- .s03sigmoid(s)
          o <- o - W[i, j] * log(if (p > 1e-300) p else 1e-300)
        }
      }
    }
    o
  }
  o0 <- obj()
  for (st in seq_len(as.integer(steps))) {
    gU <- matrix(0, n, d)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (W[i, j] == 0 || as.integer(order) != 1L) next
      s <- 0
      for (a in seq_len(d)) s <- s + U[i, a] * U[j, a]
      cc <- W[i, j] * (.s03sigmoid(s) - 1)
      for (a in seq_len(d)) {
        gU[i, a] <- gU[i, a] + cc * U[j, a]
        gU[j, a] <- gU[j, a] + cc * U[i, a]
      }
    }
    for (i in seq_len(n)) for (a in seq_len(d)) U[i, a] <- U[i, a] - as.numeric(lr) * gU[i, a]
  }
  o1 <- obj()
  list(estimate = o1, O = o1, O_start = o0, U = U, Uc = Uc, n = n,
       method = "LINE first/second-order proximity objective (Tang et al. 2015, eqs. 1-6)")
}
