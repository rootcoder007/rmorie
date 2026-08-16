% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OddPart}
\alias{OddPart}
\title{Eq (3.93): x_o(n) = 0.5 [x(n) - x(-n)]; forced to 0 at the origin}
\usage{
OddPart(x, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_xfrm implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
OddPart(S)
}
