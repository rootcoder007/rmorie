% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dbstep}
\alias{.dbstep}
\title{.dbstep}
\usage{
.dbstep(a, h, g)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{h}{A vector; its length is taken and its elements indexed.}
\item{g}{A vector; indexed elementwise.}
}
\description{
A step of the wave implementation. Called by \code{.dbforward}, \code{Wvltdb}. See the file header for the source the module follows.
}
\value{
A list with \code{a}, \code{d}.
}
\keyword{internal}
\examples{
.dbstep(a = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5, g = c(1, 2, 3, 4, 5, 6, 7, 8))
}
