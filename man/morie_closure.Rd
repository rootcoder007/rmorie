% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_closure}
\alias{morie_closure}
\title{morie_closure}
\usage{
morie_closure(items, ag, first, nts, k = 1)
}
\arguments{
\item{items}{Passed to \code{.prsLR_closure}.}
\item{ag}{Passed to \code{.prsLR_closure}.}
\item{first}{Passed to \code{.prsLR_closure}.}
\item{nts}{Passed to \code{.prsLR_closure}.}
\item{k}{Passed to \code{.prsLR_closure}. Defaults to \code{1}.}
}
\description{
A step of the prsLR_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.prsLR_closure}.
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
