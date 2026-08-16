% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Softsvm}
\alias{Softsvm}
\title{Support vector classifier, soft margin (MVSML eqs. 9.34-9.37)}
\usage{
Softsvm(X, y, T, ...)
}
\description{
(9.34) maximizes M subject to sum_j beta_j^2 = 1 (9.35), y_i(beta_0 + sum_j beta_j x_ij) >= M(1 - zeta_i) (9.36), and zeta_i >= 0 with sum_i zeta_i <= T (9.37).  The book writes T both for that slack budget and for the box bound on the multipliers in (9.45); only (9.45) is directly solvable, so T is the box bound here and the realized sum of slacks is returned as slack_sum.
}
\value{
A list with \code{beta}, \code{beta0}, \code{norm_beta}, \code{margin}, \code{zeta}, \code{slack_sum}, \code{n_violating}, \code{n_misclassified}, \code{alpha}, \code{support_vectors}, \code{objective}.
}
\keyword{internal}
