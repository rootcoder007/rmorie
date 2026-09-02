% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PerConv}
\alias{PerConv}
\title{Eq (3.90); the same equation CircConv implements, so it delegates --}
\usage{
PerConv(x, h, npoints = NULL) 
}
\arguments{
\item{x}{Passed to \code{CircConv}.}
\item{h}{Passed to \code{CircConv}.}
\item{npoints}{Passed to \code{CircConv}.}
}
\description{
two copies of one equation is how the two drift apart.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PerConv(V, V)
}
