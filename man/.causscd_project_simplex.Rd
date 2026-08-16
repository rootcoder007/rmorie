% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causscd_project_simplex}
\alias{.causscd_project_simplex}
\title{.causscd_project_simplex}
\usage{
.causscd_project_simplex(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the causscd_native implementation. Called by \code{.causscd_simplex_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.causscd_project_simplex(V)
}
