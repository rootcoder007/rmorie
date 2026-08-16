% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dbsynth}
\alias{.dbsynth}
\title{Exact adjoint of .dbstep}
\usage{
.dbsynth(a, d, h, g)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{d}{A vector; indexed elementwise.}
\item{h}{A vector; its length is taken and its elements indexed.}
\item{g}{A vector; indexed elementwise.}
}
\description{
A step of the wvltdb implementation. Called by \code{Wvltdb}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.dbsynth(a = c(1, 2, 3, 4, 5, 6, 7, 8), d = 5L, h = 0.5, g = c(1, 2, 3, 4, 5, 6, 7, 8))
}
