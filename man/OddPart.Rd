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
\item{x}{Passed to \code{.morie_rg_evenodd}.}
\item{n}{Passed to \code{.morie_rg_evenodd}.}
}
\description{
A step of the rangayyan_xfrm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
OddPart(S)
}
