% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03relu}
\alias{.s03relu}
\title{.s03relu}
\usage{
.s03relu(z)
}
\arguments{
\item{z}{Passed to \code{>}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{Autoint}, \code{DeepF}, \code{Reglu}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.s03relu(z = 5L)
}
