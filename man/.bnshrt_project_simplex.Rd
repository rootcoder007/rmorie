% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnshrt_project_simplex}
\alias{.bnshrt_project_simplex}
\title{.bnshrt_project_simplex}
\usage{
.bnshrt_project_simplex(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the bnshrt_native implementation. Called by \code{morie_in_identified_set}. See the file header for the source the module follows.
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bnshrt_project_simplex(V)
}
