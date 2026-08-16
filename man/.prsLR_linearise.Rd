% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prsLR_linearise}
\alias{.prsLR_linearise}
\title{.prsLR_linearise}
\usage{
.prsLR_linearise(tree)
}
\arguments{
\item{tree}{A list; the body reads \code{$children}, \code{$symbol} from it.}
}
\description{
A step of the prsLR_native implementation. Called by \code{morie_prsLR}. See the file header for the source the module follows.
}
\value{
A character value.
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
.prsLR_linearise(D)
}
