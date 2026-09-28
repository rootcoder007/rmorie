#' Spatial weights coding schemes (spdep nb2listw styles)
#'
#' W row-standardised; B binary; C \eqn{n^* g/D}; U \eqn{g/D}; minmax
#' \eqn{g/\min(\max rowsum, \max colsum)} (Kelejian and Prucha 2010); S
#' \eqn{n (g_{ij}/q_i)/Q}, \eqn{q_i = \sqrt{\sum_j g_{ij}^2}} (Tiefelsdorf,
#' Griffith and Boots 1999), with \eqn{n^*} the units having neighbours and
#' \eqn{D} the weight sum. Identical to the Python arm
#' \code{morie.fn.swutil.weights_style}.
#'
#' @param W Square weights matrix.
#' @param style One of W, B, C, U, minmax, S.
#' @return Recoded matrix.
#' @references Tiefelsdorf, M., Griffith, D. A. and Boots, B. (1999). A
#'   variance-stabilizing coding scheme for spatial link matrices.
#'   Environment and Planning A 31, 165-180.
#' @examples
#' WeightsStyle(rbind(c(0, 1, 1), c(1, 0, 0), c(1, 0, 0)), "W")
#' @export
WeightsStyle <- function(W, style = "W") {
  W <- as.matrix(W)
  if (nrow(W) != ncol(W)) stop("W must be square")
  card <- rowSums(W != 0)
  eff <- sum(card > 0)
  rs <- rowSums(W)
  if (style == "B") return((W != 0) * 1)
  if (style == "W") return(W / ifelse(card > 0, rs, 1))
  D <- sum(W)
  if (style %in% c("C", "U", "minmax", "S") && !(D > 0)) stop("the weights must have a positive sum")
  switch(style,
    C = eff * W / D,
    U = W / D,
    minmax = W / min(max(rs), max(colSums(W))),
    S = {
      q <- sqrt(rowSums(W^2))
      G <- W / ifelse(q > 0, q, 1)
      nrow(W) * G / sum(G)
    },
    stop("style must be W, B, C, U, minmax or S")
  )
}

#' Doubly-stochastic (Sinkhorn) scaling of spatial weights
#'
#' Alternate row and column normalisation until the row sums deviate from 1
#' by less than \code{tol} (Sinkhorn 1964); converges for matrices with total
#' support (every nonzero entry on a positive diagonal).
#'
#' @param W Non-negative square matrix.
#' @param tol Tolerance.
#' @param maxit Maximum sweeps.
#' @return List with \code{W}, \code{iterations}, \code{deviation}.
#' @references Sinkhorn, R. (1964). A relationship between arbitrary positive
#'   matrices and doubly stochastic matrices. The Annals of Mathematical
#'   Statistics 35, 876-879.
#' @examples
#' SinkhornWeights(rbind(c(0, 1, 1), c(1, 0, 1), c(1, 1, 0)))$W
#' @export
SinkhornWeights <- function(W, tol = 1e-12, maxit = 10000L) {
  M <- as.matrix(W)
  if (any(M < 0)) stop("W must be non-negative")
  dev <- Inf
  for (it in seq_len(maxit)) {
    rs <- rowSums(M)
    M <- M / ifelse(rs > 0, rs, 1)
    cs <- colSums(M)
    M <- sweep(M, 2, ifelse(cs > 0, cs, 1), `/`)
    dev <- max(abs(rowSums(M) - 1))
    if (dev < tol) break
  }
  list(W = unname(M), iterations = it, deviation = dev)
}

#' Summary of a spatial weights matrix
#'
#' Nonzero links, density, average links, cardinality, islands, symmetry,
#' row-stochasticity, Cliff-Ord \eqn{S_0, S_1, S_2}, \eqn{tr(W^2)},
#' \eqn{tr(W'W)}, Frobenius norm, diagonal dominance, relative asymmetry,
#' lower triangle and eigenvalues (Cliff and Ord 1981; spdep
#' spweights.constants; spatialreg eigenw).
#'
#' @param W Square weights matrix.
#' @param eigen Compute eigenvalues.
#' @return List of summaries.
#' @references Cliff, A. D. and Ord, J. K. (1981). Spatial Processes: Models
#'   and Applications. Pion, London.
#' @examples
#' WeightsSummary(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))[c("nonzero", "S0", "S1", "S2")]
#' @export
WeightsSummary <- function(W, eigen = TRUE) {
  M <- as.matrix(W)
  n <- nrow(M)
  if (ncol(M) != n) stop("W must be square")
  off <- M
  diag(off) <- 0
  card <- rowSums(off != 0)
  nz <- sum(M != 0)
  rs <- rowSums(M)
  cs <- colSums(M)
  fro <- sqrt(sum(M^2))
  lt <- M
  lt[upper.tri(lt, diag = TRUE)] <- 0
  out <- list(n = n, nonzero = nz, pct_nonzero = 100 * nz / n^2, avg_links = nz / n, cardinality = card,
              islands = which(card == 0), symmetric = isTRUE(all(M == t(M))),
              row_stochastic = all(abs(rs - 1) < 1e-12) && all(M >= 0), S0 = sum(rs),
              S1 = 0.5 * sum((M + t(M))^2), S2 = sum((rs + cs)^2), trace_W2 = sum(M * t(M)), trace_WtW = sum(M^2),
              frobenius = fro,
              diag_dominant = all(abs(diag(M)) >= rowSums(abs(M)) - abs(diag(M))),
              asymmetry = if (fro > 0) sqrt(sum(((M - t(M)) / 2)^2)) / fro else 0, lower_triangle = unname(lt))
  if (eigen) {
    ev <- base::eigen(M, only.values = TRUE)$values
    ev <- ev[order(Re(ev), Im(ev))]
    out$eigenvalues <- if (all(abs(Im(ev)) < 1e-12)) Re(ev) else ev
  }
  out
}
