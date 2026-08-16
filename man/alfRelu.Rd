% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfRelu}
\alias{alfRelu}
\title{alfRelu}
\usage{
alfRelu(x)
}
\arguments{
\item{x}{Passed to \code{ifelse}.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfevo}, \code{Alfplddt}, \code{Alfstrtr}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfRelu(V)
}
