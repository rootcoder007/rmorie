% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_goto}
\alias{morie_goto}
\title{morie_goto}
\usage{
morie_goto(state, sym, ag, first, nts, k = 1)
}
\arguments{
\item{state}{See Usage.}
\item{sym}{See Usage.}
\item{ag}{See Usage.}
\item{first}{See Usage.}
\item{nts}{See Usage.}
\item{k}{Defaults to \code{1}.}
}
\description{
Part of the prsLR_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.prsLR_goto}.
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
