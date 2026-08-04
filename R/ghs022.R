# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cell counts N_epsilon for a tail-free or Polya-tree partition
#'
#' Source READ FROM THE CORPUS PDF: Ghosal, S. and van der Vaart, A.
#' (2017), Fundamentals of Nonparametric Bayesian Inference, chapter 3
#' (tail-free and Polya tree priors).  For a set A_epsilon in the
#' partition at level epsilon,
#' \code{N_epsilon := #{1 <= i <= n : X_i in A_epsilon}}.  Because a
#' tail-free prior has independent partition-level masses, the vector of
#' cell counts at each level is what the posterior update consumes.
#' Nothing is estimated here; this is a count.
#'
#' The pasted stub previously returned a Kolmogorov-Smirnov statistic
#' against a fitted normal -- a number in [0, 1] where an integer count
#' was expected, with no relationship to a partition cell.
#'
#' @param X_i Numeric sample.
#' @param A_epsilon A cell, or a list of cells.  A cell is either a
#'   predicate function or a \code{c(lo, hi)} pair read as the half-open
#'   interval \code{[lo, hi)}, so consecutive breakpoints tile the line
#'   without double counting.
#' @param n Use only the first \code{n} observations.  Defaults to all.
#' @return list: N_epsilon, proportion, n, method.
#' @examples
#' Tfcells(c(0.1, 0.4, 0.6, 0.9), list(c(0, 0.5), c(0.5, 1)))$N_epsilon
#' @export
Tfcells <- function(X_i, A_epsilon, n = NULL) {
  x <- as.numeric(X_i)
  if (!is.null(n)) {
    nn <- as.integer(n)
    if (nn < 0 || nn > length(x)) stop("n must lie in 0..length(X_i)")
    x <- x[seq_len(nn)]
  }
  nn <- length(x)
  single <- is.function(A_epsilon) || (is.numeric(A_epsilon) && length(A_epsilon) == 2L)
  cells <- if (single) list(A_epsilon) else A_epsilon
  member <- function(A) {
    if (is.function(A)) {
      sum(vapply(x, function(v) isTRUE(as.logical(A(v))), logical(1)))
    } else {
      sum(x >= as.numeric(A[1]) & x < as.numeric(A[2]))
    }
  }
  counts <- vapply(cells, member, numeric(1))
  prop <- if (nn > 0) counts / nn else rep(NaN, length(counts))
  if (single) {
    counts <- counts[[1]]
    prop <- prop[[1]]
  }
  list(
    N_epsilon = as.integer(counts), proportion = prop, n = nn,
    method = "Tail-free partition cell counts (Ghosal and van der Vaart 2017, ch. 3)"
  )
}
