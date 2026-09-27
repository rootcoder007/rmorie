# SPDX-License-Identifier: AGPL-3.0-or-later
.esl14_simplex_ls <- function(A, b, max_iter = 500) {
  # min ||A x - b||^2 over the unit simplex; columns of A are the vertices; exact active set
  n <- ncol(A)
  G <- crossprod(A)
  h <- drop(crossprod(A, b))
  j0 <- which.min(diag(G) - 2 * h)
  x <- numeric(n)
  x[j0] <- 1
  P <- j0
  for (it in seq_len(max_iter)) {
    g <- drop(G[, P, drop = FALSE] %*% x[P]) - h
    nu <- -mean(g[P])
    mult <- g + nu
    mult[P] <- Inf
    if (min(mult) >= -1e-12 * (1 + abs(nu))) break
    P <- c(P, which.min(mult))
    repeat {
      k <- length(P)
      K <- rbind(cbind(G[P, P, drop = FALSE], 1), c(rep(1, k), 0))
      sol <- tryCatch(solve(K, c(h[P], 1)), error = function(e) NULL)
      if (is.null(sol)) return(x)
      z <- sol[seq_len(k)]
      if (min(z) > 0) {
        x[] <- 0
        x[P] <- z
        break
      }
      neg <- z <= 0
      alpha <- min(x[P][neg] / (x[P][neg] - z[neg]))
      x[P] <- x[P] + alpha * (z - x[P])
      P <- P[x[P] > 1e-15]
      x[-P] <- 0
    }
  }
  x
}

#' Archetypal analysis
#'
#' Cutler-Breiman archetypes (ESL eqs 14.75-14.77): X is approximated by
#' W H with H = B X, the rows of W and B on the unit simplex, minimising
#' ||X - W B X||^2 by exact alternating convex steps (each row of W by
#' simplex-constrained least squares; each archetype in turn by block
#' coordinate descent), so the criterion never increases. Archetypes start
#' at a farthest-point traversal from the centroid.
#'
#' @param X Numeric matrix, N by p.
#' @param r Number of archetypes.
#' @param max_iter,tol Stop when the criterion changes by less than tol relatively.
#' @return Named list: archetypes, W, B, rss, rss_path, iterations, converged.
#' @references Cutler, A. & Breiman, L. (1994). Technometrics 36, 338-347.
#' @examples
#' X <- rbind(c(0, 0), c(4, 0), c(4, 3), c(0, 3), c(1, 1), c(2, 2), c(3, 1))
#' morie_esl_archetypes(X, 4)$archetypes
#' @export
morie_esl_archetypes <- function(X, r, max_iter = 200, tol = 1e-10) {
  X <- unname(as.matrix(X))
  N <- nrow(X)
  if (N == 0 || r < 1 || r > N) stop("need 1 <= r <= N", call. = FALSE)
  mu <- colMeans(X)
  idx <- which.max(colSums((t(X) - mu)^2))
  while (length(idx) < r) {
    dmin <- apply(X, 1, function(x) min(colSums((t(X[idx, , drop = FALSE]) - x)^2)))
    dmin[idx] <- -Inf
    idx <- c(idx, which.max(dmin))
  }
  B <- matrix(0, r, N)
  B[cbind(seq_len(r), idx)] <- 1
  H <- X[idx, , drop = FALSE]
  wstep <- function() t(apply(X, 1, function(x) .esl14_simplex_ls(t(H), x)))
  W <- matrix(wstep(), N, r)
  J <- sum((X - W %*% H)^2)
  path <- J
  conv <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    for (k in seq_len(r)) {
      s <- sum(W[, k]^2)
      if (s == 0) next
      R <- X - W[, -k, drop = FALSE] %*% H[-k, , drop = FALSE]
      B[k, ] <- .esl14_simplex_ls(t(X), drop(crossprod(R, W[, k])) / s)
      H[k, ] <- drop(B[k, ] %*% X)
    }
    W <- matrix(wstep(), N, r)
    new <- sum((X - W %*% H)^2)
    path <- c(path, new)
    if (abs(J - new) <= tol * max(J, 1e-300) || new < 1e-24) {
      J <- new
      conv <- TRUE
      break
    }
    J <- new
  }
  list(archetypes = H, W = W, B = B, rss = J, rss_path = path, iterations = it, converged = conv)
}
