% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLR_terminals}
\alias{.prsLR_terminals}
\title{.prsLR_terminals}
\usage{
.prsLR_terminals(g)
}
\arguments{
\item{g}{A list; the body reads \code{$rules} from it.}
}
\description{
A step of the prsLR_native implementation. Called by \code{.prsLR_canonical_collection}. See the file header for the source the module follows.
}
\value{
The value of \code{setdiff}.
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
.prsLR_terminals(D)
}
