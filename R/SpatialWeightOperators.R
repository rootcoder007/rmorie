#' Eigenvalue bounds on the autoregressive parameter
#'
#' \eqn{I - \rho W} is singular exactly at \eqn{\rho = 1/\omega} for the
#' eigenvalues \eqn{\omega} of W; the interval containing 0 on which it
#' stays invertible is \eqn{(1/\omega_{min}, 1/\omega_{max})} (Ord 1975;
#' \code{spatialreg::eigenw}), and the Neumann series converges when
#' \eqn{|\rho| < 1/}spectral radius. W must be symmetric or a
#' row-standardised symmetric neighbour matrix \eqn{D^{-1}B}, whose
#' eigenvalues are those of \eqn{D^{-1/2} B D^{-1/2}}.
#'
#' @param W Spatial weights matrix.
#' @param tol Tolerance for the symmetry checks.
#' @return List with \code{lower}, \code{upper}, \code{spectral_radius},
#'   \code{neumann_radius} and \code{eigenvalues} (increasing).
#' @references Ord, K. (1975). Estimation methods for models of spatial
#'   interaction. Journal of the American Statistical Association 70,
#'   120-126.
#' @examples
#' RhoBounds(matrix(c(0, 1, 0, .5, 0, .5, 0, 1, 0), 3, byrow = TRUE))[c("lower", "upper")]
#' @export
RhoBounds <- function(W, tol = 1e-10) {
  W <- as.matrix(W)
  if (nrow(W) != ncol(W)) stop("W must be square")
  if (max(abs(W - t(W))) <= tol) {
    S <- W
  } else {
    B <- 1 * (W != 0)
    d <- rowSums(B)
    Wd <- B / ifelse(d > 0, d, 1)
    if (max(abs(W - Wd)) > tol || any(B != t(B))) {
      stop("W must be symmetric or a row-standardised symmetric neighbour matrix")
    }
    s <- ifelse(d > 0, 1 / sqrt(d), 0)
    S <- B * outer(s, s)
  }
  ev <- sort(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
  lo <- ev[1]
  hi <- ev[length(ev)]
  sr <- max(abs(lo), abs(hi))
  list(lower = if (lo < 0) 1 / lo else -Inf, upper = if (hi > 0) 1 / hi else Inf,
       spectral_radius = sr, neumann_radius = if (sr > 0) 1 / sr else Inf, eigenvalues = ev)
}

#' Spatial lag operator
#'
#' \eqn{W^k x}, \code{spdep::lag.listw} applied k times.
#'
#' @param W Spatial weights matrix.
#' @param x Numeric vector.
#' @param power Order k (non-negative).
#' @return Numeric vector.
#' @examples
#' LagOperator(matrix(c(0, 1, 0, .5, 0, .5, 0, 1, 0), 3, byrow = TRUE), c(1, 2, 4))
#' @export
LagOperator <- function(W, x, power = 1L) {
  W <- as.matrix(W)
  v <- as.numeric(x)
  if (nrow(W) != ncol(W) || length(v) != nrow(W)) stop("x must have length n and W be n x n")
  if (power < 0) stop("power must be non-negative")
  for (k in seq_len(power)) v <- as.vector(W %*% v)
  v
}

#' Spatial multiplier of an autoregressive error
#'
#' \eqn{(I - \rho W)^{-1}}, as \code{spatialreg::invIrW}; it maps
#' innovations to the process \eqn{u = \rho W u + e}.
#'
#' @param W Spatial weights matrix.
#' @param rho Autoregressive parameter.
#' @return Matrix.
#' @examples
#' ErrorOperator(matrix(c(0, 1, 1, 0), 2), 0.5)
#' @export
ErrorOperator <- function(W, rho) {
  W <- as.matrix(W)
  if (nrow(W) != ncol(W)) stop("W must be square")
  unname(solve(diag(nrow(W)) - rho * W))
}

#' Block weights from group labels
#'
#' Binary weights linking every pair of distinct units sharing a group, as
#' \code{spdep::nb2blocknb} without prior neighbours.
#'
#' @param groups Group label of each unit.
#' @return Binary matrix.
#' @examples
#' BlockWeights(c("a", "b", "a"))
#' @export
BlockWeights <- function(groups) {
  B <- 1 * outer(groups, groups, "==")
  diag(B) <- 0
  unname(B)
}

#' Regime (block-diagonal) weights
#'
#' Keeps only the links of W within a regime; with \code{row_standardize}
#' the rows are rescaled to sum to one (empty rows stay zero).
#'
#' @param W Spatial weights matrix.
#' @param regimes Regime label of each unit.
#' @param row_standardize Rescale rows after dropping links.
#' @return Matrix.
#' @examples
#' RegimeWeights(1 - diag(3), c(1, 1, 2))
#' @export
RegimeWeights <- function(W, regimes, row_standardize = FALSE) {
  W <- as.matrix(W)
  if (length(regimes) != nrow(W)) stop("regimes must have length n")
  R <- unname(W * outer(regimes, regimes, "=="))
  if (row_standardize) R <- R / ifelse(rowSums(R) > 0, rowSums(R), 1)
  R
}

#' Contiguity neighbours of polygons
#'
#' As \code{spdep::poly2nb}: queen neighbours share at least one boundary
#' vertex within \code{snap} in both coordinates, rook neighbours at least
#' two. Polygons are two-column coordinate matrices; a repeated closing
#' vertex is ignored.
#'
#' @param polygons List of coordinate matrices.
#' @param queen Queen (one shared point) or rook (two).
#' @param snap Coordinate tolerance.
#' @return Binary matrix.
#' @examples
#' sq <- function(x, y) cbind(c(x, x + 1, x + 1, x), c(y, y, y + 1, y + 1))
#' PolygonContiguity(list(sq(0, 0), sq(1, 0), sq(1, 1)), queen = FALSE)
#' @export
PolygonContiguity <- function(polygons, queen = TRUE, snap = sqrt(.Machine$double.eps)) {
  P <- lapply(polygons, function(p) {
    p <- matrix(as.numeric(as.matrix(p)), ncol = 2)
    if (nrow(p) > 1 && all(p[1, ] == p[nrow(p), ])) p <- p[-nrow(p), , drop = FALSE]
    p
  })
  n <- length(P)
  need <- if (queen) 1 else 2
  out <- matrix(0, n, n)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      hit <- vapply(seq_len(nrow(P[[i]])), function(a) {
        any(abs(P[[i]][a, 1] - P[[j]][, 1]) <= snap & abs(P[[i]][a, 2] - P[[j]][, 2]) <= snap)
      }, logical(1))
      if (sum(hit) >= need) out[i, j] <- out[j, i] <- 1
    }
  }
  out
}

#' Neighbour counts and their frequency table
#'
#' As \code{spdep::card}.
#'
#' @param W Spatial weights matrix.
#' @return List with \code{cardinality}, \code{table} (named counts),
#'   \code{n_links}, \code{islands} (1-based) and \code{mean}.
#' @examples
#' NeighbourCardinality(matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3))$table
#' @export
NeighbourCardinality <- function(W) {
  W <- as.matrix(W)
  diag(W) <- 0
  card <- as.integer(rowSums(W != 0))
  list(cardinality = card, table = table(card), n_links = sum(card), islands = which(card == 0L),
       mean = mean(card))
}

#' Neighbours present in one weights matrix but not the other
#'
#' As \code{spdep::diffnb}.
#'
#' @param W1,W2 Spatial weights matrices of the same size.
#' @return List with \code{difference} (1-based indices per unit),
#'   \code{only_first}, \code{only_second}, \code{identical}.
#' @examples
#' CompareNeighbours(matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3), 1 - diag(3))$difference
#' @export
CompareNeighbours <- function(W1, W2) {
  A <- as.matrix(W1) != 0
  B <- as.matrix(W2) != 0
  if (!identical(dim(A), dim(B))) stop("W1 and W2 must have the same size")
  diag(A) <- FALSE
  diag(B) <- FALSE
  D <- xor(A, B)
  o1 <- sum(A & !B)
  o2 <- sum(B & !A)
  list(difference = lapply(seq_len(nrow(D)), function(i) unname(which(D[i, ]))), only_first = o1,
       only_second = o2, identical = o1 == 0 && o2 == 0)
}
