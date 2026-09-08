% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiParY}
\alias{LsiParY}
\title{Eqs (3.48)-(3.49): the parallel counterpart of eq (3.44) -- here the}
\usage{
LsiParY(x, h1, h2) 
}
\arguments{
\item{x}{Passed to \code{LsiPar}.}
\item{h1}{Passed to \code{LsiPar}.}
\item{h2}{Passed to \code{LsiPar}.}
}
\description{
impulse responses ADD where a cascade convolves them.
}
\value{
A list with \code{y}, \code{h}, \code{s1}, \code{s2}, \code{equivalent}, \code{max_difference}, \code{method}.
}
\keyword{internal}
\examples{
LsiParY(x = c(1, 2, 3, 4, 5, 6, 7, 8), h1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  h2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
