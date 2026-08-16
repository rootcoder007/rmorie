% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLR_first_seq}
\alias{.prsLR_first_seq}
\title{.prsLR_first_seq}
\usage{
.prsLR_first_seq(seq, first, nts)
}
\arguments{
\item{seq}{A vector; its length is taken.}
\item{first}{A vector; indexed elementwise.}
\item{nts}{Passed to \code{\%in\%}.}
}
\description{
A step of the prsLR_native implementation. Called by \code{.prsLR_closure}, \code{.prsLR_first_sets}, \code{.prsLR_follow_sets}. See the file header for the source the module follows.
}
\value{
The value of \code{unique}.
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
\examples{
.prsLR_first_seq(seq = c(1, 2, 3, 4, 5, 6, 7, 8), first = c(1, 2, 3, 4, 5, 6, 7, 8), nts = c(1, 2, 3, 4, 5, 6, 7, 8))
}
