#' Pivoted (incomplete) Cholesky factorisation
#'
#' Largest remaining Schur-complement diagonal as pivot (ties: lowest
#' index), stopping when the remaining trace falls to \code{tol} times the
#' trace of \code{A} or at \code{max_rank} columns (Harbrecht, Peters and
#' Schneider 2012). Identical to the Python arm
#' \code{morie.fn.zschl.pivoted_cholesky}.
#'
#' @param A Positive semi-definite matrix.
#' @param tol Relative trace tolerance.
#' @param max_rank Maximum rank.
#' @return List with \code{L} (n x r, original row order), \code{pivots}
#'   (1-based) and \code{rank}.
#' @references Harbrecht, H., Peters, M. and Schneider, R. (2012). On the
#'   low-rank approximation by the pivoted Cholesky decomposition. Applied
#'   Numerical Mathematics 62, 428-440.
#' @examples
#' PivotedCholesky(matrix(c(4, 2, 2, 1), 2))
#' @export
PivotedCholesky <- function(A, tol = 1e-12, max_rank = NULL) {
  A <- as.matrix(A)
  n <- nrow(A)
  d <- diag(A)
  tr0 <- sum(d)
  kmax <- if (is.null(max_rank)) n else min(n, max_rank)
  L <- matrix(0, n, 0)
  piv <- integer(0)
  while (ncol(L) < kmax && sum(d[setdiff(seq_len(n), piv)]) > tol * tr0) {
    rest <- setdiff(seq_len(n), piv)
    p <- rest[which.max(d[rest])]
    if (d[p] <= 0) break
    s <- sqrt(d[p])
    c <- numeric(n)
    c[rest] <- (A[rest, p] - if (ncol(L)) as.vector(L[rest, , drop = FALSE] %*% L[p, ]) else 0) / s
    c[p] <- s
    L <- cbind(L, c)
    piv <- c(piv, p)
    rest <- setdiff(rest, p)
    d[rest] <- d[rest] - c[rest]^2
  }
  list(L = unname(L), pivots = piv, rank = ncol(L))
}

#' Cholesky (LU) simulation of a Gaussian random field
#'
#' Realisations \eqn{mean + L e} with \eqn{C = LL'} the covariance at
#' \code{coords} (Davis 1987); conditioning on \code{z} at
#' \code{data_coords} uses the exact Gaussian conditional law (simple
#' kriging mean and error covariance, pivoted factor). \code{method}:
#' \code{"cholesky"}, \code{"precision"} (\eqn{Q = C^{-1} = RR'},
#' \eqn{x = R^{-T}e}; Rue and Held 2005) or \code{"pivoted"}
#' (\code{\link{PivotedCholesky}}). Realisation s uses Philox stream s - 1;
#' identical to the Python arm \code{morie.fn.zschl.chol_sim}.
#'
#' @param coords Matrix of simulation locations.
#' @param model Covariance model (\code{\link{KrigingCovariance}}).
#' @param nsim Number of realisations.
#' @param seed Philox seed.
#' @param mean Known constant mean.
#' @param z Conditioning data.
#' @param data_coords Locations of \code{z}.
#' @param method \code{"cholesky"}, \code{"precision"} or \code{"pivoted"}.
#' @param tol Relative trace tolerance of the pivoted factor.
#' @return List with \code{simulations} (nsim x n), \code{mean}, \code{cov}.
#' @references Davis, M. W. (1987). Production of conditional simulations
#'   via the LU triangular decomposition of the covariance matrix.
#'   Mathematical Geology 19, 91-98.
#'
#'   Rue, H. and Held, L. (2005). Gaussian Markov Random Fields. Chapman and
#'   Hall/CRC.
#' @examples
#' CholeskySim(rbind(c(0, 0), c(1, 0)), list(model = "Exp", psill = 1, range = 1),
#'   seed = 3)$simulations
#' @export
CholeskySim <- function(coords, model, nsim = 1L, seed = 1L, mean = 0, z = NULL, data_coords = NULL,
                        method = "cholesky", tol = 1e-12) {
  P <- as.matrix(coords)
  n <- nrow(P)
  if (!method %in% c("cholesky", "precision", "pivoted")) stop("method must be cholesky, precision or pivoted")
  C <- KrigingCovariance(as.matrix(stats::dist(P)), model)
  mu <- rep(mean, n)
  if (!is.null(z)) {
    if (is.null(data_coords)) stop("data_coords is required with z")
    D <- as.matrix(data_coords)
    if (nrow(D) != length(z)) stop("data_coords must match z")
    Cdd <- KrigingCovariance(as.matrix(stats::dist(D)), model)
    Cxd <- KrigingCovariance(as.matrix(stats::dist(rbind(P, D)))[seq_len(n), n + seq_len(nrow(D)), drop = FALSE], model)
    W <- Cxd %*% solve(Cdd)
    mu <- mean + as.vector(W %*% (z - mean))
    C <- C - W %*% t(Cxd)
    if (method == "cholesky") method <- "pivoted"
  }
  if (method == "cholesky") {
    L <- t(chol(C))
  } else if (method == "pivoted") {
    L <- PivotedCholesky(C, tol = tol)$L
  } else {
    R <- t(chol(solve(C)))
  }
  sims <- t(vapply(seq_len(nsim) - 1L, function(s) {
    if (method == "precision") {
      x <- backsolve(t(R), .morie_random_normal(n, seed = seed, stream = s))
    } else {
      r <- ncol(L)
      x <- if (r) as.vector(L %*% .morie_random_normal(r, seed = seed, stream = s)) else numeric(n)
    }
    mu + x
  }, numeric(n)))
  list(simulations = sims, mean = mu, cov = unname(C))
}
