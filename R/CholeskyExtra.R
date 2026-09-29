.ce_cov <- function(A, B, model) {
  D <- sqrt(Reduce(`+`, lapply(seq_len(ncol(A)), function(k) outer(A[, k], B[, k], "-")^2)))
  matrix(KrigingCovariance(D, model), nrow(A))
}

.ce_chol_psd <- function(A) {
  n <- nrow(A)
  L <- matrix(0, n, n)
  scale <- if (n) max(abs(diag(A))) else 0
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j] - sum(L[i, seq_len(j - 1)] * L[j, seq_len(j - 1)])
      L[i, j] <- if (i == j) (if (s > 1e-12 * scale) sqrt(s) else 0) else if (L[j, j] > 0) s / L[j, j] else 0
    }
  }
  L
}

#' LU/Cholesky simulation toolkit
#'
#' \code{BandedCholesky}: Cholesky factor of a band matrix in
#' \eqn{O(n b^2)}. \code{IncompleteCholesky}: IC(0) on the sparsity pattern
#' (entries above \code{drop_tol}) with its relative Frobenius error.
#' \code{WendlandTaper}: Wendland's compactly supported correlation, as
#' \code{fields::Wendland}. \code{TaperedSimulate}: simulation from the
#' tapered covariance. \code{LmcCosimulate}: joint simulation of
#' coregionalised fields (linear model of coregionalisation).
#' \code{SimulationSensitivity}: realisations under several ranges or sills
#' with common random numbers. \code{NestedDecomposition}: a nested model
#' simulated as independent components with variance shares.
#' \code{RefinedSolve}: Cholesky solve with iterative refinement.
#' \code{BlockLuSimulate}: block-sequential LU simulation of a grid, each
#' tile conditioned on simulated nodes within \code{halo}. Identical to the
#' Python arm \code{morie.fn.cholext}.
#'
#' @param A Symmetric positive-definite matrix.
#' @param bandwidth Half-bandwidth.
#' @param drop_tol Sparsity threshold.
#' @param d Distances.
#' @param theta Taper range.
#' @param dimension Space dimension.
#' @param k Wendland smoothness (0 to 3).
#' @param coords Coordinates.
#' @param model Covariance model (as \code{KrigingCovariance}).
#' @param nsim Realisations.
#' @param seed Philox seed.
#' @param mean Mean.
#' @param components List of \code{list(B, model)} (\code{LmcCosimulate}) or
#'   of covariance components (\code{NestedDecomposition}).
#' @param means Field means.
#' @param ranges,sills Parameter values.
#' @param b Right-hand side.
#' @param iterations Refinement steps.
#' @param xs,ys Grid coordinates.
#' @param block Tile size.
#' @param halo Conditioning radius.
#' @return List (numeric vector for \code{WendlandTaper}).
#' @references Golub, G. H. and Van Loan, C. F. (2013). Matrix Computations,
#'   4th edn. Johns Hopkins University Press.
#'
#'   Wendland, H. (1995). Piecewise polynomial, positive definite and
#'   compactly supported radial functions of minimal degree. Advances in
#'   Computational Mathematics 4, 389-396.
#'
#'   Furrer, R., Genton, M. G. and Nychka, D. (2006). Covariance tapering for
#'   interpolation of large spatial datasets. JCGS 15, 502-523.
#'
#'   Davis, M. W. (1987). Production of conditional simulations via the LU
#'   triangular decomposition of the covariance matrix. Mathematical Geology
#'   19, 91-98.
#' @examples
#' BandedCholesky(rbind(c(4, 2, 0), c(2, 5, 2), c(0, 2, 5)), 1)$L
#' WendlandTaper(c(0, 0.5, 1), 1, dimension = 2, k = 1)
#' RefinedSolve(rbind(c(4, 2), c(2, 3)), c(2, 1))$x
#' @export
BandedCholesky <- function(A, bandwidth) {
  M <- as.matrix(A) * 1
  n <- nrow(M)
  b <- as.integer(bandwidth)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in max(1, i - b):i) {
      ks <- seq_len(j - 1)
      ks <- ks[ks >= max(1, i - b, j - b)]
      s <- M[i, j] - sum(L[i, ks] * L[j, ks])
      if (i == j) {
        if (s <= 0) stop("matrix is not positive definite", call. = FALSE)
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  off <- abs(M)[abs(row(M) - col(M)) > b]
  list(L = L, bandwidth = b, ignored_max = if (length(off)) max(off) else 0)
}

#' @rdname BandedCholesky
#' @export
IncompleteCholesky <- function(A, drop_tol = 0) {
  M <- as.matrix(A) * 1
  n <- nrow(M)
  keep <- abs(M) > drop_tol
  L <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      if (!keep[i, j] && i != j) next
      s <- M[i, j] - sum(L[i, seq_len(j - 1)] * L[j, seq_len(j - 1)])
      if (i == j) {
        if (s <= 0) stop("incomplete factorisation broke down (non-positive pivot)", call. = FALSE)
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  list(L = L, relative_error = sqrt(sum((M - tcrossprod(L))^2)) / sqrt(sum(M^2)), nnz = sum(L[lower.tri(L, TRUE)] != 0))
}

#' @rdname BandedCholesky
#' @export
WendlandTaper <- function(d, theta, dimension = 2L, k = 1L) {
  l <- dimension %/% 2 + k + 1
  r <- abs(d) / theta
  p <- switch(as.character(k), "0" = rep(1, length(r)), "1" = (l + 1) * r + 1,
              "2" = ((l^2 + 4 * l + 3) * r^2 + (3 * l + 6) * r + 3) / 3,
              "3" = ((l^3 + 9 * l^2 + 23 * l + 15) * r^3 + (6 * l^2 + 36 * l + 45) * r^2 + (15 * l + 45) * r + 15) / 15,
              stop("k must be 0, 1, 2 or 3", call. = FALSE))
  ifelse(r >= 1, 0, (1 - pmin(r, 1))^(l + k) * p)
}

#' @rdname BandedCholesky
#' @export
TaperedSimulate <- function(coords, model, theta, k = 1L, nsim = 1L, seed = 1, mean = 0) {
  P <- as.matrix(coords)
  D <- as.matrix(stats::dist(P))
  C <- .ce_cov(P, P, model) * matrix(WendlandTaper(D, theta, ncol(P), k), nrow(P))
  L <- t(chol(C))
  sims <- lapply(seq_len(nsim) - 1, function(s) mean + as.vector(L %*% .morie_random_normal(nrow(P), seed = seed, stream = s)))
  list(simulations = sims, cov = C, sparsity = mean(C == 0))
}

#' @rdname BandedCholesky
#' @export
LmcCosimulate <- function(coords, components, nsim = 1L, seed = 1, means = NULL) {
  P <- as.matrix(coords)
  n <- nrow(P)
  p <- nrow(as.matrix(components[[1]][[1]]))
  C <- Reduce(`+`, lapply(components, function(cm) kronecker(as.matrix(cm[[1]]), .ce_cov(P, P, cm[[2]]))))
  L <- t(chol(C))
  mu <- if (is.null(means)) rep(0, p) else means
  sims <- lapply(seq_len(nsim) - 1, function(s) {
    x <- as.vector(L %*% .morie_random_normal(n * p, seed = seed, stream = s))
    lapply(seq_len(p), function(a) mu[a] + x[(a - 1) * n + seq_len(n)])
  })
  list(simulations = sims, cov = C)
}

#' @rdname BandedCholesky
#' @export
SimulationSensitivity <- function(coords, model, ranges = NULL, sills = NULL, seed = 1) {
  P <- as.matrix(coords)
  n <- nrow(P)
  e <- .morie_random_normal(n, seed = seed, stream = 0)
  params <- if (!is.null(ranges)) {
    lapply(ranges, function(r) utils::modifyList(model, list(range = r)))
  } else if (!is.null(sills)) {
    lapply(sills, function(s) utils::modifyList(model, list(psill = s)))
  } else {
    stop("give ranges or sills", call. = FALSE)
  }
  sims <- lapply(params, function(m) as.vector(t(chol(.ce_cov(P, P, m))) %*% e))
  list(simulations = sims, variance = vapply(sims, stats::var, 0),
       correlation = vapply(sims, function(s) stats::cor(sims[[1]], s), 0))
}

#' @rdname BandedCholesky
#' @export
NestedDecomposition <- function(coords, components, seed = 1) {
  P <- as.matrix(coords)
  n <- nrow(P)
  fields <- lapply(seq_along(components), function(k) {
    as.vector(t(chol(.ce_cov(P, P, components[[k]]))) %*% .morie_random_normal(n, seed = seed, stream = k - 1))
  })
  total <- Reduce(`+`, fields)
  sills <- vapply(components, function(c) KrigingCovariance(0, c), 0)
  list(components = fields, total = total, share = vapply(fields, stats::var, 0) / stats::var(total),
       nominal_share = sills / sum(sills))
}

#' @rdname BandedCholesky
#' @export
RefinedSolve <- function(A, b, iterations = 3L) {
  M <- as.matrix(A) * 1
  R <- chol(M)
  solve1 <- function(r) backsolve(R, forwardsolve(t(R), r))
  x <- solve1(b)
  hist <- numeric(0)
  for (it in 0:iterations) {
    r <- b - as.vector(M %*% x)
    hist <- c(hist, sqrt(sum(r^2)))
    if (it == iterations) break
    x <- x + solve1(r)
  }
  list(x = x, residual_norms = hist)
}

#' @rdname BandedCholesky
#' @export
BlockLuSimulate <- function(xs, ys, model, block = 4L, halo = 1, seed = 1, mean = 0) {
  nx <- length(xs)
  ny <- length(ys)
  field <- matrix(NA_real_, ny, nx)
  tiles <- expand.grid(bi = seq(1, nx, by = block), bj = seq(1, ny, by = block))
  for (t in seq_len(nrow(tiles))) {
    bj <- tiles$bj[t]
    bi <- tiles$bi[t]
    nd <- expand.grid(i = bi:min(bi + block - 1, nx), j = bj:min(bj + block - 1, ny))
    pts <- cbind(xs[nd$i], ys[nd$j])
    e <- .morie_random_normal(nrow(pts), seed = seed, stream = t - 1)
    Cxx <- .ce_cov(pts, pts, model)
    done <- which(!is.na(field), arr.ind = TRUE)
    if (nrow(done)) {
      dp <- cbind(xs[done[, 2]], ys[done[, 1]])
      near <- apply(dp, 1, function(q) min(sqrt((pts[, 1] - q[1])^2 + (pts[, 2] - q[2])^2)) < halo)
      done <- done[near, , drop = FALSE]
      dp <- dp[near, , drop = FALSE]
      o <- order(done[, 1], done[, 2])
      done <- done[o, , drop = FALSE]
      dp <- dp[o, , drop = FALSE]
    }
    if (nrow(done)) {
      Cxd <- .ce_cov(pts, dp, model)
      W <- Cxd %*% solve(.ce_cov(dp, dp, model))
      mu <- mean + as.vector(W %*% (field[done] - mean))
      L <- .ce_chol_psd(Cxx - W %*% t(Cxd))
    } else {
      mu <- rep(mean, nrow(pts))
      L <- t(chol(Cxx))
    }
    field[cbind(nd$j, nd$i)] <- mu + as.vector(L %*% e)
  }
  list(field = field, n_blocks = nrow(tiles))
}
