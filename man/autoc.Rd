% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{autoc}
\alias{autoc}
\title{autoc}
\usage{
autoc(scores, priority) 
}
\arguments{
\item{scores}{Passed to \code{rate}.}
\item{priority}{Passed to \code{rate}.}
}
\description{
A step of the slvgrf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
autoc(V, V)
}
