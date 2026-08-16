% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_simplex_project}
\alias{.plcbsc_simplex_project}
\title{.plcbsc_simplex_project}
\usage{
.plcbsc_simplex_project(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the plcbsc_native implementation. Called by \code{.plcbsc_synthetic_control}. See the file header for the source the module follows.
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.plcbsc_simplex_project(V)
}
