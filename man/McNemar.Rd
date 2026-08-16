% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{McNemar}
\alias{McNemar}
\title{Section 10.9.2, McNemar\\'s test of SYMMETRY.  The book states it on a}
\usage{
McNemar(table, correct = NULL)
}
\arguments{
\item{table}{See Usage.}
\item{correct}{Defaults to \code{NULL}.}
}
\description{
general contingency table -- its worked example, Table 10.4, is 3x3 with normal / indeterminate / abnormal -- so any k x k is accepted. k = 2 gives McNemar with Yates\\' correction, k > 2 its generalization, Bowker\\'s test.  Only the OFF-DIAGONAL disagreements enter: the diagonal, usually most of the cases, contributes nothing, because the question is whether the disagreements are one-sided.
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{pairs}, \code{n}, \code{n_agree}, \code{continuity_correction}, \code{is_bowker}, \code{k}, \code{diagonal_contributes_nothing}, \code{method}.
}
\keyword{internal}
