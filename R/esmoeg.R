# SPDX-License-Identifier: AGPL-3.0-or-later

#' Exploratory SEM with target rotation
#'
#' Formula: rotate the exploratory loading matrix toward a hypothesised
#' pattern.  For an ORTHOGONAL rotation the problem
#' min_T || Lambda T - H ||_F subject to T'T = I is the orthogonal
#' Procrustes problem, solved in closed form by the polar factor of
#' M = Lambda' H: with M'M = V S^2 V', T = M V S^-1 V', which is exactly
#' U V' for the singular value decomposition M = U S V'.  No iteration is
#' needed for a fully specified target.
#'
#' A PARTIALLY specified target -- the usual ESEM case, where only the
#' zeros are stated and the salient loadings are free -- is handled by
#' the standard alternating scheme: free positions of H are refilled from
#' the current rotated solution, then the Procrustes step repeats, until
#' T stops moving.  Free positions are marked NA.
#'
#' @param loadings p x m exploratory loading matrix.
#' @param target p x m hypothesised pattern; NA marks a free element.
#' @param iters Maximum alternations for a partial target.
#' @param tol Convergence tolerance on the change in T.
#' @return List with \code{estimate}, \code{rotated} (row-major),
#'   \code{rotation} (row-major), \code{rms}, \code{n_specified},
#'   \code{iters_used}, \code{n_items}, \code{n_factors}, \code{n},
#'   \code{method}.
#' @references Asparouhov & Muthen (2009), Structural Equation Modeling
#'   16(3):397-438, doi:10.1080/10705510903008204.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Esmoeg(V, V)
Esmoeg <- function(loadings, target, iters = 200, tol = 1e-13) {
  L <- .s03mat(loadings)
  H0 <- .s03mat(target)
  p <- nrow(L)
  if (p == 0L) stop("empty input: loadings has no rows")
  m <- ncol(L)
  if (nrow(H0) != p || ncol(H0) != m)
    stop("loadings and target must have the same shape")
  if (m == 0L) stop("loadings has no columns")
  spec <- !is.na(H0)
  nspec <- sum(spec)
  if (nspec == 0L) stop("target specifies no elements")
  .proc <- function(H) {
    M <- matrix(0, m, m)
    for (a in seq_len(m)) for (b in seq_len(m)) M[a, b] <- sum(L[, a] * H[, b])
    MtM <- matrix(0, m, m)
    for (a in seq_len(m)) for (b in seq_len(m)) MtM[a, b] <- sum(M[, a] * M[, b])
    ej <- .s03jacobi(MtM)
    vals <- ej$values
    vecs <- ej$vectors
    if (any(vals <= 1e-24))
      stop("target rotation is degenerate (Lambda' H is rank deficient)")
    MV <- matrix(0, m, m)
    for (a in seq_len(m)) for (b in seq_len(m)) MV[a, b] <- sum(M[a, ] * vecs[, b])
    for (a in seq_len(m)) for (b in seq_len(m)) MV[a, b] <- MV[a, b] / sqrt(vals[b])
    Tn <- matrix(0, m, m)
    for (a in seq_len(m)) for (b in seq_len(m)) Tn[a, b] <- sum(MV[a, ] * vecs[b, ])
    Tn
  }
  T <- diag(1, m)
  used <- 0L
  for (k in seq_len(as.integer(iters))) {
    used <- k
    Rot <- L %*% T
    H <- H0
    H[!spec] <- Rot[!spec]
    Tn <- .proc(H)
    d <- max(abs(Tn - T))
    T <- Tn
    if (d < as.numeric(tol)) break
  }
  Rot <- L %*% T
  ss <- sum((Rot[spec] - H0[spec])^2)
  rms <- sqrt(ss / nspec)
  .t1_result(estimate = rms, rotated = as.numeric(t(Rot)),
             rotation = as.numeric(t(T)), rms = rms, n_specified = nspec,
             iters_used = used, n_items = p, n_factors = m, n = p,
             method = "Exploratory SEM with target rotation")
}
