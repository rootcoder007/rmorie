% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiPar}
\alias{LsiPar}
\title{Eqs (3.46)-(3.49): s1 = x*h1, s2 = x*h2, y = s1 + s2 = x*(h1+h2)}
\usage{
LsiPar(x, h1, h2)
}
\arguments{
\item{x}{See Usage.}
\item{h1}{See Usage.}
\item{h2}{See Usage.}
}
\description{
The shorter response is zero-extended before the addition; truncating instead would silently drop the tail of the longer filter.
}
\value{
A list with \code{s1}, \code{s2}, \code{y}, \code{h}, \code{y_via_combined}, \code{max_difference}, \code{equivalent}, \code{method}.
}
\keyword{internal}
\examples{
LsiPar(x = c(1, 2, 3, 4, 5, 6, 7, 8), h1 = c(1, 2, 3, 4, 5, 6, 7, 8), h2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
