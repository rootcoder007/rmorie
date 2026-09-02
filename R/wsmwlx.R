# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wilcoxon rank-sum test, normal approximation with a tie correction.
#'
#' Ties are given average ranks, which is what makes the variance
#' correction the right one. This tests a shift in DISTRIBUTION, not in
#' mean.
#'
#' Formula: W = sum of the ranks of x in the pooled sample;
#'   E[W] = n1(n1 + n2 + 1)/2;
#'   Var[W] = n1 n2 (N + 1)/12 - n1 n2 sum(t^3 - t)/(12 N (N - 1));
#'   z = (W - E[W] -+ 1/2) / sd
#'
#' @param x,y The two samples.
#' @param correct Apply the 1/2 continuity correction.
#' @return List with \code{statistic}, \code{U}, \code{z},
#'   \code{p_value}, \code{expected}, \code{variance}, \code{n1},
#'   \code{n2}, \code{n_tied_groups}.
#' @references Wilcoxon (1945), Biometrics Bulletin 1(6), 80-83, and Mann
#'   & Whitney (1947), Annals of Mathematical Statistics 18(1), 50-60 --
#'   the primary sources. Wasserman (2004), All of Statistics, does NOT
#'   contain the rank-sum test; the full text of the book was fetched and
#'   searched to establish that.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ranksum(V, V)
Ranksum <- function(x, y, correct = TRUE) {
  x <- .t1_vec(x); y <- .t1_vec(y)
  n1 <- length(x); n2 <- length(y)
  if (n1 < 1L || n2 < 1L) stop("both samples must be non-empty")
  pool <- c(x, y); N <- n1 + n2
  rank <- rank(pool, ties.method = "average")
  tab <- table(pool)
  tt <- as.numeric(tab[tab > 1])
  tiesum <- sum(tt^3 - tt)
  groups <- length(tt)
  W <- sum(rank[seq_len(n1)])
  E <- n1 * (N + 1) / 2
  V <- n1 * n2 * (N + 1) / 12 - n1 * n2 * tiesum / (12 * N * (N - 1))
  if (V <= 0) stop("the rank variance is zero; every value is tied")
  d <- W - E
  cc <- if (isTRUE(correct)) 0.5 else 0
  z <- if (d > 0) (d - cc) / sqrt(V) else if (d < 0) (d + cc) / sqrt(V) else 0
  .t1_result(statistic = W, U = W - n1 * (n1 + 1) / 2, z = z,
             p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
             expected = E, variance = V, n1 = as.numeric(n1),
             n2 = as.numeric(n2), n_tied_groups = as.numeric(groups),
             method = "Wilcoxon rank-sum, normal approximation with tie correction")
}
