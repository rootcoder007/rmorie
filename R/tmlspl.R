# SPDX-License-Identifier: AGPL-3.0-or-later
## Default exposure summary: fraction of neighbours treated.
#' SPDX-License-Identifier: AGPL-3.0-or-later
#'
#' # Default exposure summary: fraction of neighbours treated.
#'
#' @param D A vector; its length is taken and its elements indexed.
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
.tmlspl_frac_treated <- function(D, A) {
  n <- length(D)
  out <- numeric(n)
  for (i in seq_len(n)) {
    nb <- setdiff(seq_len(n), i)
    deg <- sum(A[i, nb])
    out[i] <- if (deg > 0) sum(A[i, nb] * D[nb]) / deg else 0
  }
  out
}

#' TMLE for a direct effect under interference, at a fixed network exposure
#'
#' Under interference "the" treatment effect is undefined until the
#' neighbours' exposure is pinned down, because a unit's outcome depends
#' on the whole treatment vector.  The standard way out is an exposure
#' mapping: reduce the neighbourhood to a scalar summary \code{E_i} and
#' target the DIRECT effect at a fixed value,
#' \code{psi = E\[Y(1, ebar)\] - E\[Y(0, ebar)\]} with \code{ebar} the
#' sample-mean exposure.  Both nuisance models condition on \code{E}.
#'
#' The variance cannot be the i.i.d. one -- neighbours' influence curves
#' are correlated by construction -- so the reported SE is the network
#' HAC form of Aronow & Samii, summing \code{ic_i ic_j} over pairs that
#' are adjacent or identical.  On an edgeless network it collapses back
#' to the i.i.d. variance.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Covariates.
#' @param network Adjacency matrix.
#' @param exposure_summary Function \code{(D, network)} returning one
#'   exposure per unit; \code{NULL} uses the fraction of neighbours
#'   treated.
#' @return List with \code{estimate}, \code{se}, \code{se_iid},
#'   \code{eps}, \code{ebar}, \code{n}.
#' @references Aronow, P. M. & Samii, C. (2017). Annals of Applied
#'   Statistics 11(4):1912-1947; Sofrygin, O. & van der Laan, M. J.
#'   (2017). Journal of Causal Inference 5(1).
#' @export
Tmlspl <- function(y, D, X, network, exposure_summary = NULL) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  n <- length(yv)
  if (n == 0L || length(Dv) != n)
    stop("Tmlspl: y and D must share one length")
  Xm <- as.matrix(X)
  A <- as.matrix(network)
  if (nrow(Xm) != n) stop("Tmlspl: X must have one row per subject")
  if (nrow(A) != n || ncol(A) != n) stop("Tmlspl: network must be n by n")
  E <- if (is.null(exposure_summary)) .tmlspl_frac_treated(Dv, A)
       else as.numeric(exposure_summary(Dv, A))
  if (length(E) != n)
    stop("Tmlspl: exposure_summary must return one value per unit")
  ebar <- sum(E) / n
  W <- cbind(1, Xm, E)
  Wb <- cbind(1, Xm, ebar)
  gb <- .s4_glmbin(W, Dv)
  g <- .s4_clip(.s4_expit(as.numeric(W %*% gb)), 0.025, 0.975)
  qb <- .s4_ols(cbind(Dv, W), yv)$beta
  Qobs <- as.numeric(cbind(Dv, W) %*% qb)
  Q1 <- as.numeric(cbind(1, Wb) %*% qb)
  Q0 <- as.numeric(cbind(0, Wb) %*% qb)
  H <- Dv / g - (1 - Dv) / (1 - g)
  den <- sum(H * H)
  eps <- if (den != 0) sum(H * (yv - Qobs)) / den else 0
  Q1s <- Q1 + eps / g
  Q0s <- Q0 - eps / (1 - g)
  psi <- sum(Q1s - Q0s) / n
  ic <- H * (yv - Qobs - eps * H) + Q1s - Q0s - psi
  ce <- ic - mean(ic)
  M <- (A != 0)
  diag(M) <- TRUE
  vr <- sum(outer(ce, ce) * M) / (n * n)
  se <- if (vr > 0) sqrt(vr) else NaN
  se_iid <- if (n > 1L) sqrt(sum(ce * ce) / (n - 1) / n) else NaN
  .t1_result(estimate = psi, se = se, se_iid = se_iid, eps = eps,
             ebar = ebar, n = n,
             method = "TMLE for the direct effect under interference at fixed exposure")
}
