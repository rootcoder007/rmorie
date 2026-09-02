% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{stats_free_interp}
\alias{stats_free_interp}
\title{Piecewise-linear interpolation onto `grid`, written out rather than}
\usage{
stats_free_interp(beats, values, grid) 
}
\arguments{
\item{beats}{A vector; its length is taken and its elements indexed.}
\item{values}{A vector; its length is taken and its elements indexed.}
\item{grid}{A vector; its length is taken and its elements indexed.}
}
\description{
delegated so the R and Python arms resample identically
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
stats_free_interp(beats = c(1, 2, 3, 4, 5, 6, 7, 8), values = c(1, 2, 3, 4, 5, 6, 7, 8), grid = c(1, 2, 3, 4, 5, 6, 7, 8))
}
