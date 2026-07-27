# SPDX-License-Identifier: AGPL-3.0-or-later

#' Chi-square and total inertia of a two-way table
#'
#' Follows the correspondence-analysis construction of Nenadic &
#' Greenacre (2007). Divide the \eqn{I \times J} table \eqn{N} by its
#' grand total to get the correspondence matrix \eqn{P = N/n}; let
#' \eqn{r} and \eqn{c} be its row and column marginals, the row and
#' column masses, and \eqn{D_r}, \eqn{D_c} their diagonal matrices. The
#' standardised residuals are
#'
#' \deqn{S = D_r^{-1/2} (P - rc') D_c^{-1/2}}
#'
#' Total inertia is \eqn{\|S\|_F^2}, and the Pearson statistic is the
#' grand total times that inertia,
#'
#' \deqn{\chi^2 = n \sum_{ij} (p_{ij} - r_i c_j)^2 / (r_i c_j)
#'              = n \|S\|_F^2}
#'
#' which is the usual \eqn{\sum (o-e)^2/e} written on the closed table.
#' The singular values of \eqn{S} come back too: their squares are the
#' principal inertias that correspondence analysis decomposes the total
#' into.
#'
#' A caution on compositional input. Inertia depends only on the row
#' profiles, so rescaling rows leaves it unchanged. The chi-square does
#' not: it scales with the grand total. Passing rows already closed to
#' unit sum makes the grand total the row count, and the statistic then
#' describes a table of \eqn{I} "observations" rather than the counts
#' actually collected. Give \code{n} explicitly in that case, or pass
#' the raw counts.
#'
#' Mirrors \code{morie.fn.aitcsq} on the Python side; both agree with
#' \code{stats::chisq.test} on the same table.
#'
#' @param x Numeric matrix (I x J) of non-negative values, I >= 2 and
#'   J >= 2. Counts, or any non-negative ratio-scale quantity.
#' @param cdf Optional function giving the null CDF of the statistic,
#'   replacing the asymptotic chi-square.
#' @param n Grand total to scale the inertia by. Defaults to
#'   \code{sum(x)}. Supply it when the rows have been closed and the
#'   original sample size is known.
#' @return Named list with \code{statistic}, \code{p_value}, \code{df},
#'   \code{inertia}, \code{principal_inertias}, \code{singular_values},
#'   \code{row_masses}, \code{col_masses}, \code{n}, \code{method}.
#' @references Nenadic O & Greenacre M (2007). Correspondence analysis
#'   in R, with two- and three-dimensional graphics: the ca package.
#'   \emph{Journal of Statistical Software}, 20(3), 1-13.
#'
#'   Greenacre M (1984). \emph{Theory and Applications of Correspondence
#'   Analysis}. Academic Press, London.
#' @examples
#' set.seed(1)
#' morie_table_inertia(matrix(rpois(20, 40), 5, 4))$statistic
#' @export
morie_table_inertia <- function(x, cdf = NULL, n = NULL) {
  N <- as.matrix(x)
  I <- nrow(N)
  J <- ncol(N)
  if (I < 2L || J < 2L) {
    stop("Need at least a 2x2 table, got ", I, "x", J, ".", call. = FALSE)
  }
  if (!all(is.finite(N))) stop("x must be finite.", call. = FALSE)
  if (any(N < 0)) {
    stop("x must be non-negative; a correspondence table has no negative cells.",
         call. = FALSE)
  }
  total <- sum(N)
  if (total <= 0) {
    stop("x sums to zero; the correspondence matrix is undefined.", call. = FALSE)
  }

  P <- N / total
  r <- rowSums(P)
  c_ <- colSums(P)
  if (any(r <= 0) || any(c_ <= 0)) {
    stop("Every row and column must have positive mass; drop all-zero rows ",
         "or columns first.", call. = FALSE)
  }

  E <- outer(r, c_)
  S <- (P - E) / sqrt(E)
  inertia <- sum(S^2)

  grand <- if (is.null(n)) total else as.numeric(n)
  if (grand <= 0) stop("n must be positive, got ", grand, ".", call. = FALSE)
  statistic <- grand * inertia

  df <- (I - 1L) * (J - 1L)
  p <- if (!is.null(cdf)) 1 - cdf(statistic) else stats::pchisq(statistic, df, lower.tail = FALSE)
  sv <- svd(S, nu = 0L, nv = 0L)$d

  list(
    statistic = statistic,
    p_value = p,
    df = df,
    inertia = inertia,
    principal_inertias = sv^2,
    singular_values = sv,
    row_masses = r,
    col_masses = c_,
    n = grand,
    method = "Pearson chi-square via correspondence analysis (Nenadic & Greenacre 2007)"
  )
}
