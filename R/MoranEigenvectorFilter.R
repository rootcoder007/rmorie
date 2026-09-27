.mesf_moments <- function(M, S, df) {
  MSM <- M %*% S %*% M
  t1 <- sum(diag(MSM))
  t2 <- sum(MSM * t(MSM))
  list(E = t1 / df, V = 2 * (df * t2 - t1^2) / (df^2 * (df + 2)), MSM = MSM)
}

.mesf_annihilator <- function(X) diag(nrow(X)) - X %*% solve(crossprod(X), t(X))

#' Stepwise Moran eigenvector spatial filter
#'
#' Following Tiefelsdorf and Griffith (2007) as
#' \code{spatialreg::SpatialFiltering} (style C, symmetric, ExactEV FALSE):
#' the binary neighbour matrix is symmetrised and scaled to sum to n,
#' projected with \eqn{M_X = I - X(X'X)^{-1}X'} to \eqn{M_X S M_X} and
#' eigendecomposed. Among the eigenvectors whose eigenvalue has the sign of
#' the residual autocorrelation (|value| > \code{zerovalue}), each step adds
#' the one bringing the residual Moran's I closest to its expectation in
#' standard deviations under the current design
#' (\eqn{E = tr(MSM)/df}, \eqn{Var = 2(df\,tr(MSM^2) - tr(MSM)^2) /
#' (df^2(df + 2))}). Selection stops once |z| < \code{tol} or |z| grows
#' (with \code{alpha}: once the two-sided p-value reaches \code{alpha}).
#'
#' @param y Response.
#' @param X Design matrix, including any intercept column.
#' @param adjacency Neighbour indicator matrix; symmetrised internally.
#' @param tol Stop when |z| of the residual Moran's I is below it.
#' @param zerovalue Eigenvalues within it of zero are never candidates.
#' @param alpha Optional p-value stopping rule in place of \code{tol}.
#' @return List with \code{selection} (data frame: step, evec (1-based,
#'   decreasing eigenvalue order), eval, moran, z, p_value, r2, gamma),
#'   \code{vectors}, \code{coefficients}, \code{fitted} and
#'   \code{stop_reason} ("tol", "alpha", "inversion" or "exhausted").
#'   Eigenvector signs are arbitrary, so a gamma can flip sign with its
#'   vector; fitted values do not.
#' @references Tiefelsdorf, M. and Griffith, D. A. (2007). Semiparametric
#'   filtering of spatial autocorrelation: the eigenvector approach.
#'   Environment and Planning A 39, 1193-1221.
#' @examples
#' A <- 1 * (abs(outer(1:12, 1:12, "-")) == 1)
#' y <- c(1, 1.4, 2.2, 2.9, 3.1, 2.6, 2, 1.1, .8, 1.5, 2.4, 2.7)
#' X <- cbind(1, c(.2, .5, .1, .9, .4, .3, .8, .6, .7, .05, .35, .55))
#' MoranEigenvectorFilter(y, X, A)$selection$evec
#' @export
MoranEigenvectorFilter <- function(y, X, adjacency, tol = 0.1, zerovalue = 1e-4, alpha = NULL) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  A <- 1 * (as.matrix(adjacency) != 0)
  n <- length(y)
  if (nrow(X) != n || !identical(dim(A), c(n, n))) stop("y, X and adjacency must share n")
  diag(A) <- 0
  C <- A * sum(rowSums(A) > 0) / sum(A)
  S <- (C + t(C)) / 2
  S <- n / sum(S) * S
  MX <- .mesf_annihilator(X)
  P <- MX %*% S %*% MX
  eg <- eigen((P + t(P)) / 2, symmetric = TRUE)
  val <- eg$values
  vec <- eg$vectors
  p <- ncol(X)
  tss <- sum((y - mean(y))^2)
  pval <- function(z) 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  mm <- .mesf_moments(MX, S, n - p)
  cyMy <- sum(y * (MX %*% y))
  i0 <- sum(y * (mm$MSM %*% y)) / cyMy
  z0 <- (i0 - mm$E) / sqrt(mm$V)
  rows <- list(c(0, 0, 0, i0, z0, pval(z0), 1 - cyMy / tss))
  sel <- (val > abs(zerovalue)) - (val < -abs(zerovalue))
  cur <- as.vector(y - X %*% solve(crossprod(X), crossprod(X, y)))
  acsign <- if (sum(cur * (S %*% cur)) / sum(cur^2) < 0) -1 else 1
  gam <- as.vector(crossprod(vec, y)) / colSums(vec^2)
  chosen <- integer(0)
  old <- Inf
  reason <- "exhausted"
  for (step in seq_len(n)) {
    best <- 0L
    bz <- Inf
    bmi <- 0
    for (j in which(sel == acsign)) {
      res <- cur - vec[, j] * gam[j]
      mi <- sum(res * (S %*% res)) / sum(res^2)
      if (abs((mi - mm$E) / sqrt(mm$V)) < bz) {
        best <- j
        bz <- abs((mi - mm$E) / sqrt(mm$V))
        bmi <- mi
      }
    }
    if (best == 0L) break
    chosen <- c(chosen, best)
    cur <- cur - vec[, best] * gam[best]
    Xa <- cbind(X, vec[, chosen, drop = FALSE])
    M <- .mesf_annihilator(Xa)
    mm <- .mesf_moments(M, S, n - ncol(Xa))
    z <- (bmi - mm$E) / sqrt(mm$V)
    rows[[length(rows) + 1L]] <- c(length(chosen), best, val[best], bmi, z, pval(z), 1 - sum(y * (M %*% y)) / tss)
    sel[best] <- 0
    if (if (is.null(alpha)) abs(z) < tol else pval(z) >= alpha) {
      reason <- if (is.null(alpha)) "tol" else "alpha"
      break
    }
    if (abs(z) > abs(old)) {
      reason <- "inversion"
      break
    }
    old <- z
  }
  Xa <- cbind(X, vec[, chosen, drop = FALSE])
  bg <- as.vector(solve(crossprod(Xa), crossprod(Xa, y)))
  tab <- as.data.frame(do.call(rbind, rows))
  names(tab) <- c("step", "evec", "eval", "moran", "z", "p_value", "r2")
  tab$gamma <- c(0, bg[-seq_len(p)])
  list(selection = tab, vectors = vec[, chosen, drop = FALSE], coefficients = bg,
       fitted = as.vector(Xa %*% bg), stop_reason = reason)
}
