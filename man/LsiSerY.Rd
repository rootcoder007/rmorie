% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiSerY}
\alias{LsiSerY}
\title{Eq (3.44): the cascade output, read off LsiSer rather than convolved}
\usage{
LsiSerY(x, h1, h2) 
}
\arguments{
\item{x}{Passed to \code{LsiSer}.}
\item{h1}{Passed to \code{LsiSer}.}
\item{h2}{Passed to \code{LsiSer}.}
}
\description{
a second time -- the content of the equation is that the cascade IS a single filter, not that there are two ways to compute it.
}
\value{
A list with \code{y}, \code{h}, \code{s}, \code{equivalent}, \code{max_difference}, \code{method}.
}
\keyword{internal}
\examples{
LsiSerY(x = c(1, 2, 3, 4, 5, 6, 7, 8), h1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  h2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
