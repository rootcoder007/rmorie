% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IirDiffGen}
\alias{IirDiffGen}
\title{Eq (3.144): the time-domain form of eq (3.143), and how a designed}
\usage{
IirDiffGen(x, b_k, a_k = NULL, n = NULL) 
}
\arguments{
\item{x}{Passed to \code{IirDiff}.}
\item{b_k}{Passed to \code{IirDiff}.}
\item{a_k}{Passed to \code{IirDiff}.}
\item{n}{Passed to \code{IirDiff}.}
}
\description{
filter is actually run over data.  The feedback is SUBTRACTED, as in eq (3.68), and a_k is a_1..a_N without the leading a_0 = 1.
}
\value{
The value of \code{IirDiff}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IirDiffGen(V, V)
}
