# SPDX-License-Identifier: AGPL-3.0-or-later

.CFA_MAXIT <- 5000L
.CFA_TOL <- 1e-13

# Item covariance from data, or pass a covariance matrix through.
#' Item covariance from data, or pass a covariance matrix through
#'
#' A step of the cfafm2 implementation. Called by \code{Cfafm2}, \code{Cfaftr}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X Passed to \code{.s03mat}.
#' @return The value of \code{S}, as built in the body.
#' @export
.cfa_cov <- function(X) {
  M <- .s03mat(X)
  if (!nrow(M)) stop("empty input: X has no rows")
  q <- ncol(M)
  if (nrow(M) == q && q > 1L && all(abs(M - t(M)) < 1e-12))
    return(matrix(as.numeric(M), q, q))
  n <- nrow(M)
  if (n < 2L) stop("need at least two observations to form a covariance")
  mu <- colSums(M) / n
  S <- matrix(0, q, q)
  for (a in seq_len(q)) for (b in seq_len(q))
    S[a, b] <- sum((M[, a] - mu[a]) * (M[, b] - mu[b])) / (n - 1)
  S
}

#' .cfa_inv
#'
#' A step of the cfafm2 implementation. Called by \code{.cfa_em}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @return The value of \code{out}, as built in the body.
#' @export
.cfa_inv <- function(A) {
  m <- nrow(A)
  cols <- lapply(seq_len(m), function(k) {
    e <- numeric(m)
    e[k] <- 1
    .s03cholsolve(A, e)
  })
  out <- matrix(0, m, m)
  for (a in seq_len(m)) for (b in seq_len(m)) out[a, b] <- cols[[b]][a]
  out
}

#' .cfa_logdet
#'
#' A step of the cfafm2 implementation. Called by \code{.cfa_em}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param A Passed to \code{.s03chol}.
#' @return A numeric value.
#' @export
.cfa_logdet <- function(A) {
  L <- .s03chol(A)
  2 * sum(log(diag(L)))
}

# Masked EM factor analysis (Rubin & Thayer 1982 E- and M-steps).
# mask[i, j] is 1 where item i may load on factor j.  Factors are
# standardised and orthogonal, so Sigma = Lambda Lambda' + Psi.
#' Masked EM factor analysis (Rubin & Thayer 1982 E- and M-steps)
#'
#' mask\[i, j\] is 1 where item i may load on factor j.  Factors are
#' standardised and orthogonal, so Sigma = Lambda Lambda\' + Psi.
#'
#' @param S A matrix; indexed by row and column.
#' @param mask A matrix; indexed by row and column.
#' @return A list with \code{lam}, \code{psi}, \code{fml}, \code{resid}, \code{it}.
#' @export
.cfa_em <- function(S, mask) {
  p <- nrow(S)
  k <- ncol(mask)
  eg <- .s03jacobi(S)
  lam <- matrix(0, p, k)
  for (j in seq_len(k)) {
    idx <- p - j + 1L
    sv <- sqrt(max(eg$values[idx], 0))
    for (i in seq_len(p)) lam[i, j] <- sv * eg$vectors[i, idx] * mask[i, j]
  }
  psi <- numeric(p)
  for (i in seq_len(p)) {
    v <- S[i, i] - sum(lam[i, ]^2)
    psi[i] <- if (v > 1e-6) v else 1e-6
  }
  it <- 0L
  for (iter in seq_len(.CFA_MAXIT)) {
    it <- iter
    Sig <- matrix(0, p, p)
    for (a in seq_len(p)) for (b in seq_len(p))
      Sig[a, b] <- sum(lam[a, ] * lam[b, ]) + if (a == b) psi[a] else 0
    Si <- .cfa_inv(Sig)
    beta <- matrix(0, k, p)
    for (j in seq_len(k)) for (b in seq_len(p))
      beta[j, b] <- sum(lam[, j] * Si[, b])
    bS <- matrix(0, k, p)
    for (j in seq_len(k)) for (b in seq_len(p))
      bS[j, b] <- sum(beta[j, ] * S[, b])
    Czz <- matrix(0, k, k)
    for (u in seq_len(k)) for (v in seq_len(k))
      Czz[u, v] <- (if (u == v) 1 else 0) - sum(beta[u, ] * lam[, v]) +
        sum(bS[u, ] * beta[v, ])
    Cxz <- matrix(0, p, k)
    for (i in seq_len(p)) for (j in seq_len(k)) Cxz[i, j] <- bS[j, i]
    delta <- 0
    for (i in seq_len(p)) {
      act <- which(mask[i, ] != 0L)
      new <- numeric(k)
      if (length(act)) {
        A <- Czz[act, act, drop = FALSE]
        b <- Cxz[i, act]
        sol <- .s03ridgesolve(A, b, 1e-12)
        new[act] <- sol
      }
      q <- S[i, i] - 2 * sum(new * Cxz[i, ]) +
        sum(outer(new, new) * Czz)
      q <- if (q > 1e-8) q else 1e-8
      delta <- max(delta, abs(q - psi[i]), max(abs(new - lam[i, ])))
      lam[i, ] <- new
      psi[i] <- q
    }
    if (delta < .CFA_TOL) break
  }
  Sig <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p))
    Sig[a, b] <- sum(lam[a, ] * lam[b, ]) + if (a == b) psi[a] else 0
  Si <- .cfa_inv(Sig)
  fml <- .cfa_logdet(Sig) - .cfa_logdet(S) + sum(S * t(Si)) - p
  resid <- max(abs(S - Sig))
  list(lam = lam, psi = psi, fml = fml, resid = resid, it = it)
}

#' CFA multi-factor with cross-loadings allowed
#'
#' Formula: X = Lambda F + eps; F ~ N(0, Phi)
#'
#' Fitted by the Rubin-Thayer EM algorithm with the loading pattern
#' imposed: an entry of \code{factor_pattern} that is zero forces the
#' corresponding loading to stay zero, so cross-loadings are estimated
#' only where the confirmatory model allows them.  Factors are
#' standardised and orthogonal (Phi = I), the identification the EM
#' E-step assumes.
#'
#' @param X An n x p data matrix, or a p x p item covariance matrix.
#' @param factor_pattern p x k matrix of 0/1 flags: 1 where item i may
#'   load on factor j.
#' @return List with \code{estimate} (variance explained),
#'   \code{loadings}, \code{uniquenesses}, \code{fml},
#'   \code{max_resid}, \code{communality}, \code{n_iter}, \code{p},
#'   \code{k}, \code{method}.
#' @references Joreskog (1969), Psychometrika 34(2):183-202;
#'   Rubin & Thayer (1982), Psychometrika 47(1):69-76.
#' @export
#' @examples
#' Cfafm2(X = c(1, 2, 3, 4, 5, 6, 7, 8), factor_pattern = 5L)
Cfafm2 <- function(X, factor_pattern) {
  P <- .s03mat(factor_pattern)
  if (!nrow(P)) stop("empty input: factor_pattern is empty")
  S <- .cfa_cov(X)
  p <- nrow(S)
  if (nrow(P) != p) stop("factor_pattern must have one row per item")
  k <- ncol(P)
  if (k < 1L) stop("factor_pattern must have at least one column")
  mask <- matrix(as.integer(abs(P) > 0), p, k)
  if (all(mask == 0L)) stop("factor_pattern frees no loading at all")
  f <- .cfa_em(S, mask)
  comm <- rowSums(f$lam^2)
  .t1_result(estimate = sum(comm) / sum(diag(S)), loadings = f$lam,
             uniquenesses = f$psi, fml = f$fml, max_resid = f$resid,
             communality = comm, n_iter = f$it, p = p, k = k,
             method = "CFA multi-factor with cross-loadings allowed")
}
