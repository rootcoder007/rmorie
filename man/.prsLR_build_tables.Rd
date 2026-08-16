% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLR_build_tables}
\alias{.prsLR_build_tables}
\title{.prsLR_build_tables}
\usage{
.prsLR_build_tables(g, method)
}
\arguments{
\item{g}{See Usage.}
\item{method}{See Usage.}
}
\description{
Part of the prsLR_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{action}, \code{goto}, \code{states}, \code{n_states}, \code{conflicts}, \code{rules}, \code{augmented}, \code{method}.
}
\references{
----------
Knuth, D. E. (1965) "On the translation of languages from left to
right", Information and Control 8(6), 607-639,
doi:10.1016/S0019-9958(65)90426-2.

DeRemer, F. L. (1971) "Simple LR(k) grammars", Communications of the
ACM 14(7), 453-460, doi:10.1145/362619.362625.
}
\keyword{internal}
