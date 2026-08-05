# SPDX-License-Identifier: AGPL-3.0-or-later
#' EM for the ADMIXTURE ancestry likelihood.
#'
#' L(Q,P) = sum_ij \[ g_ij log(sum_k q_ik p_kj)
#'                 + (2-g_ij) log(sum_k q_ik (1-p_kj)) ], maximised by the
#' EM updates on allele responsibilities a_ijk and b_ijk.
#'
#' @param G Genotype counts in {0,1,2}, I x J.
#' @param K Number of ancestral populations.
#' @param steps Fixed EM iteration count.
#' @param Q0,P0 Starting values; NULL uses the deterministic defaults
#'   q_ik proportional to 1 + ((i+k) mod K) and p_kj = (2 + ((k J + j) mod
#'   7))/10 with zero-based i, j, k.
#'
#' @return List with Q, P, loglik, loglik0, I, J, K, steps.
#' @references Alexander, Novembre and Lange (2009), Genome Research
#'   19(9), 1655-1664, Equation (2), read from the open-access PMC
#'   rendering.  ADMIXTURE maximises that likelihood by block relaxation;
#'   this routine uses the EM algorithm for the same likelihood (FRAPPE,
#'   Tang et al. 2005).
#' @export
Admixq <- function(G, K = 2, steps = 50, Q0 = NULL, P0 = NULL) {
  Gm <- matrix(as.numeric(as.matrix(G)), nrow = nrow(as.matrix(G)))
  I <- nrow(Gm); J <- ncol(Gm); K <- as.integer(K); steps <- as.integer(steps)
  if (I == 0 || J == 0) stop("G must be non-empty")
  if (K < 1) stop("K must be at least 1")
  if (any(Gm < 0 | Gm > 2)) stop("genotype counts must lie in [0, 2]")
  if (is.null(Q0)) {
    Q <- outer(seq_len(I) - 1L, seq_len(K) - 1L,
               function(i, k) 1 + ((i + k) %% K))
    Q <- Q / rowSums(Q)
  } else {
    Q <- matrix(as.numeric(as.matrix(Q0)), nrow = I)
  }
  if (is.null(P0)) {
    P <- outer(seq_len(K) - 1L, seq_len(J) - 1L,
               function(k, j) (2 + ((k * J + j) %% 7)) / 10)
  } else {
    P <- matrix(as.numeric(as.matrix(P0)), nrow = K)
  }
  ll <- function(Q, P) {
    A <- Q %*% P
    B <- Q %*% (1 - P)
    sum(ifelse(Gm > 0, Gm * log(A), 0)) +
      sum(ifelse(2 - Gm > 0, (2 - Gm) * log(B), 0))
  }
  ll0 <- ll(Q, P)
  for (s in seq_len(steps)) {
    Qn <- matrix(0, I, K)
    num <- matrix(0, K, J)
    den <- matrix(0, K, J)
    SA <- Q %*% P
    SB <- Q %*% (1 - P)
    for (k in seq_len(K)) {
      a <- outer(Q[, k], P[k, ]) / SA
      b <- outer(Q[, k], 1 - P[k, ]) / SB
      a[!is.finite(a)] <- 0
      b[!is.finite(b)] <- 0
      ca <- Gm * a
      cb <- (2 - Gm) * b
      Qn[, k] <- rowSums(ca) + rowSums(cb)
      num[k, ] <- colSums(ca)
      den[k, ] <- colSums(ca) + colSums(cb)
    }
    Q <- Qn / (2 * J)
    P <- ifelse(den == 0, 0.5, num / den)
    dim(P) <- c(K, J)
  }
  .t1_result(Q = Q, P = P, loglik = ll(Q, P), loglik0 = ll0,
             I = I, J = J, K = K, steps = steps,
             method = "EM for the ADMIXTURE likelihood (Alexander et al. 2009 eq. 2)")
}
