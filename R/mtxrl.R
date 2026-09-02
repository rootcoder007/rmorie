# SPDX-License-Identifier: AGPL-3.0-or-later
#' Value and optimal strategies of a zero-sum matrix game
#'
#' A pure saddle point is detected and returned exactly. Otherwise
#' fictitious play runs for a FIXED number of rounds and a rigorous
#' BRACKET is returned with the estimate: the value lies between
#' min_j (x'A)_j and max_i (Ay)_i.
#'
#' Formula: v = max_x min_y x' A y = min_y max_x x' A y
#'
#' @param A Payoff matrix to the ROW player.
#' @param iters Fixed number of fictitious-play rounds.
#' @return List with \code{value}, \code{lower}, \code{upper},
#'   \code{row_strategy}, \code{col_strategy}, \code{maximin},
#'   \code{minimax}, \code{saddle}, \code{iterations}, \code{m},
#'   \code{n}.
#' @references von Neumann, J. (1928), Zur Theorie der
#'   Gesellschaftsspiele, Mathematische Annalen 100, 295-320 -- the
#'   minimax theorem. The fictitious-play iteration is Brown (1951) and
#'   its convergence for zero-sum games is Robinson (1951), Annals of
#'   Mathematics 54(2), 296-301; neither is von Neumann's and both are
#'   cited to their own sources.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Matgame(V)
Matgame <- function(A, iters = 2000) {
  A <- as.matrix(A); m <- nrow(A); n <- ncol(A)
  if (m < 1L || n < 1L) stop("the payoff matrix must be non-empty")
  rowmin <- apply(A, 1, min); colmax <- apply(A, 2, max)
  maximin <- max(rowmin); minimax <- min(colmax)
  if (abs(maximin - minimax) < 1e-15) {
    i0 <- which.max(rowmin); j0 <- which.min(colmax)
    x <- rep(0, m); x[i0] <- 1
    y <- rep(0, n); y[j0] <- 1
    return(.t1_result(value = maximin, lower = maximin, upper = minimax,
                      row_strategy = x, col_strategy = y, maximin = maximin,
                      minimax = minimax, saddle = 1, iterations = 0,
                      m = as.numeric(m), n = as.numeric(n),
                      method = "Matrix game with a pure saddle point"))
  }
  cr <- integer(m); cc <- integer(n)
  urow <- numeric(n); ucol <- numeric(m)
  i <- 1L; cr[i] <- 1L; urow <- urow + A[i, ]
  Tn <- as.integer(iters)
  for (t in seq_len(Tn)) {
    j <- which.min(urow)
    cc[j] <- cc[j] + 1L
    ucol <- ucol + A[, j]
    i <- which.max(ucol)
    cr[i] <- cr[i] + 1L
    urow <- urow + A[i, ]
  }
  x <- cr / sum(cr); y <- cc / sum(cc)
  Ay <- as.numeric(A %*% y); xA <- as.numeric(t(x) %*% A)
  lo <- min(xA); hi <- max(Ay)
  .t1_result(value = 0.5 * (lo + hi), lower = lo, upper = hi,
             row_strategy = x, col_strategy = y, maximin = maximin,
             minimax = minimax, saddle = 0, iterations = as.numeric(Tn),
             m = as.numeric(m), n = as.numeric(n),
             method = "Matrix game by fictitious play with a rigorous bracket")
}
