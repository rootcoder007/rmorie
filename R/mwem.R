# SPDX-License-Identifier: AGPL-3.0-or-later
#' MWEM: multiplicative weights with the exponential mechanism
#'
#' Figure 1 of the paper: repeatedly select a badly-answered query with the
#' exponential mechanism, measure it with the Laplace mechanism, and apply the
#' multiplicative weights update A_i(x) proportional to A_\{i-1\}(x)
#' exp(q_i(x)(m_i - q_i(A_\{i-1\}))/2n).  Both random draws are supplied by the
#' caller.  Source consulted: Hardt, Ligett and McSherry (2012), A simple and
#' practical algorithm for differentially private data release,
#' arXiv:1012.4763.
#'
#' @param B histogram of the private data set over the universe.
#' @param queries query matrix, one linear query per row.
#' @param eps total privacy budget.
#' @param T number of iterations.
#' @param gumbel optional T by |Q| Gumbel draws for the exponential mechanism.
#' @param lap optional T Laplace(1) draws, scaled internally by 2T/eps.
#' @return list: estimate, maxerr, meanerr, A, selected, T, nqueries, n, method.
#' @keywords internal
#' @examples
#' B <- c(5, 5, 5, 5)
#' Q <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0), 2, 4, byrow = TRUE)
#' mwem(B, Q, eps = 1, T = 2, gumbel = matrix(0, 2, 2), lap = c(0, 0))$maxerr
#' @export
mwem <- function(B, queries, eps = 1, T = 10L, gumbel = NULL, lap = NULL) {
  b <- as.numeric(B); Q <- as.matrix(queries)
  d <- length(b); nq <- nrow(Q); n <- sum(b); Ti <- as.integer(T)
  qb <- as.numeric(Q %*% b)
  A <- rep(n / d, d); acc <- rep(0, d); selected <- integer(0)
  scale <- if (eps > 0) 2 * Ti / eps else 0
  epsi <- if (Ti > 0) eps / (2 * Ti) else 0
  for (i in seq_len(Ti)) {
    qa <- as.numeric(Q %*% A)
    score <- abs(qa - qb)
    util <- epsi * score / 2
    if (!is.null(gumbel)) util <- util + as.matrix(gumbel)[i, ]
    pick <- 1L
    if (nq > 1) for (k in 2:nq) if (util[k] > util[pick]) pick <- k
    selected <- c(selected, pick - 1L)
    m <- qb[pick]
    if (!is.null(lap)) m <- m + scale * as.numeric(lap)[i]
    diff <- m - qa[pick]
    newA <- A * exp(Q[pick, ] * diff / (2 * n))
    tot <- sum(newA)
    if (tot > 0) A <- newA * n / tot
    acc <- acc + A
  }
  out <- if (Ti > 0) acc / Ti else A
  err <- abs(as.numeric(Q %*% out) - qb)
  list(estimate = max(err), maxerr = max(err), meanerr = sum(err) / nq,
       A = out, selected = as.integer(selected), T = Ti,
       nqueries = as.integer(nq), n = as.numeric(n),
       method = "MWEM (Hardt, Ligett & McSherry 2012)")
}

# CANONICAL TEST
# B <- c(5, 5, 5, 5); Q <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0), 2, 4, byrow = TRUE)
# r <- mwem(B, Q, eps = 1, T = 2, gumbel = matrix(0, 2, 2), lap = c(0, 0))
# stopifnot(r$maxerr < 1e-12)

#' @rdname mwem
#' @keywords internal
#' @export
morie_mwem <- mwem
