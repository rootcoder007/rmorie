# t-SNE embedding.
# Source: van der Maaten & Hinton (2008), JMLR 9, 2579-2605,
# Eqs. 1-5 and Algorithm 1
# (fetched-wave3/vandermaaten-hinton-2008-tsne-jmlr9.pdf).  Mirrors
# Python morie.fn.sctsne exactly (same binary search, same init
# normals from the shared SplitMix64 stream, same momentum
# schedule).

#' .tsne_pcond
#'
#' A step of the sctsne_native implementation. Called by \code{morie_sctsne}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D2 A matrix; indexed by row and column.
#' @param perp Numeric; passed to \code{log}.
#' @param tol Defaults to \code{1e-05}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60}.
#' @return The value of \code{P}, as built in the body.
#' @export
.tsne_pcond <- function(D2, perp, tol = 1e-5, max_iter = 60) {
  n <- nrow(D2)
  target <- log(perp)
  P <- matrix(0, n, n)
  for (i in seq_len(n)) {
    lo <- 1e-20; hi <- 1e20; beta <- 1
    pr <- numeric(n)
    for (it in seq_len(max_iter)) {
      num <- exp(-D2[i, ] * beta)
      num[i] <- 0
      s <- sum(num)
      if (s <= 0) s <- 1e-300
      pr <- num / s
      h <- -sum(pr[pr > 1e-300] * log(pr[pr > 1e-300]))
      if (abs(h - target) < tol) break
      if (h > target) {
        lo <- beta
        beta <- if (hi >= 1e20) beta * 2 else (beta + hi) / 2
      } else {
        hi <- beta
        beta <- (beta + lo) / 2
      }
    }
    P[i, ] <- pr
  }
  P
}

#' t-SNE embedding (Algorithm 1 of van der Maaten & Hinton 2008)
#'
#' Conditional affinities with perplexity-matched per-point
#' variances (Eq. 1), symmetrized p_ij = (p_{j|i}+p_{i|j})/(2n),
#' Student-t map affinities (Eq. 4), gradient descent with the
#' paper's Eq. 5 gradient and momentum schedule (0.5 then 0.8).
#'
#' @param X Data matrix (n x d).
#' @param dim Output dimension.
#' @param perplexity Perp parameter.
#' @param T Iterations.
#' @param eta Learning rate.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{embedding}, \code{kl},
#'   \code{kl_initial}, \code{perplexity_error}, \code{T},
#'   \code{seed}, \code{method}.
#' @references van der Maaten, L. and Hinton, G. (2008).
#'   Visualizing data using t-SNE. JMLR, 9, 2579-2605.
#' @export
morie_sctsne <- function(X, dim = 2, perplexity = 10, T = 300,
                         eta = 100, seed = 0) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (n < 5) stop("need at least five points")
  if (perplexity <= 1 || perplexity >= n) {
    stop("perplexity must be in (1, n)")
  }
  D2 <- as.matrix(stats::dist(X))^2
  Pc <- .tsne_pcond(D2, perplexity)
  perr <- 0
  for (i in seq_len(n)) {
    pr <- Pc[i, ]
    h <- -sum(pr[pr > 1e-300] * log(pr[pr > 1e-300]))
    perr <- max(perr, abs(h - log(perplexity)))
  }
  P <- (Pc + t(Pc)) / (2 * n)
  e <- .ghc_rng(seed)
  Y <- matrix(0, n, dim)
  for (i in seq_len(n)) {
    for (k in seq_len(dim)) Y[i, k] <- 1e-2 * .ghc_norm(e, 1)
  }
  Ym1 <- Y
  qkl <- function(Y) {
    W <- 1 / (1 + as.matrix(stats::dist(Y))^2)
    diag(W) <- 0
    s <- sum(W)
    kl <- 0
    mask <- P > 1e-300
    Q <- pmax(W / s, 1e-300)
    kl <- sum(P[mask] * log(P[mask] / Q[mask]))
    list(W = W, s = s, kl = kl)
  }
  kl0 <- qkl(Y)$kl
  for (t_ in seq_len(T)) {
    qq <- qkl(Y)
    W <- qq$W; s <- qq$s
    G <- matrix(0, n, dim)
    for (i in seq_len(n)) {
      coef <- 4 * (P[i, ] - W[i, ] / s) * W[i, ]
      diffs <- sweep(-Y, 2, -Y[i, ])       # Y[i,] - Y[j,]
      G[i, ] <- colSums(coef * diffs)
    }
    mom <- if (t_ < 250) 0.5 else 0.8
    new <- Y - eta * G + mom * (Y - Ym1)
    Ym1 <- Y
    Y <- new
  }
  list(embedding = Y, kl = qkl(Y)$kl, kl_initial = kl0,
       perplexity_error = perr, T = as.integer(T), seed = seed,
       method = "t-SNE (van der Maaten & Hinton 2008, Alg. 1)")
}
