% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{as.double.morie_frac}
\alias{as.double.morie_frac}
\title{As.numeric() dispatches to as.double methods, not as.numeric ones}
\usage{
as.double.morie_frac(x, ...)
}
\arguments{
\item{x}{See Usage.}
\item{...}{Passed through.}
}
\description{
Part of the rangayyan_class implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
as.double.morie_frac(D)
}
