% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EvenPart}
\alias{EvenPart}
\title{Eq (3.92): x_e(n) = 0.5 [x(n) + x(-n)].  x(-n) must exist, so the}
\usage{
EvenPart(x, n = NULL)
}
\arguments{
\item{x}{Passed to \code{.morie_rg_evenodd}.}
\item{n}{Passed to \code{.morie_rg_evenodd}.}
}
\description{
index grid has to be symmetric; reflecting a causal sequence about 0 instead computes x/2, which is something else entirely.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
EvenPart(S)
}
