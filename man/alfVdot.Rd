% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfVdot}
\alias{alfVdot}
\title{alfVdot}
\usage{
alfVdot(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfipa}, \code{Alfmsaat}, \code{Alftmpl} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfVdot(V, V)
}
