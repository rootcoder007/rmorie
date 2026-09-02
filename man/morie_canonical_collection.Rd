% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_canonical_collection}
\alias{morie_canonical_collection}
\title{morie_canonical_collection}
\usage{
morie_canonical_collection(ag, k = 1) 
}
\arguments{
\item{ag}{Passed to \code{.prsLR_canonical_collection}.}
\item{k}{Passed to \code{.prsLR_canonical_collection}. Defaults to \code{1}.}
}
\description{
A step of the prsLR_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.prsLR_canonical_collection}.
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
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_canonical_collection(D)
}
