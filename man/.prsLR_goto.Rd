% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLR_goto}
\alias{.prsLR_goto}
\title{.prsLR_goto}
\usage{
.prsLR_goto(state, sym, ag, first, nts, k)
}
\arguments{
\item{state}{See Usage.}
\item{sym}{See Usage.}
\item{ag}{A list; the body reads \code{$rules} from it.}
\item{first}{Passed to \code{.prsLR_closure}.}
\item{nts}{Passed to \code{.prsLR_closure}.}
\item{k}{Passed to \code{.prsLR_closure}.}
}
\description{
A step of the prsLR_native implementation. Called by \code{.prsLR_canonical_collection}, \code{morie_goto}. See the file header for the source the module follows.
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
