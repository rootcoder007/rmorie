% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_project_simplex}
\alias{.flxipt_project_simplex}
\title{Projection onto {a >= 0, sum a = 1}, exact sort-based ---------}
\usage{
.flxipt_project_simplex(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the flxipt_native implementation. Called by \code{.flxipt_nnls_simplex}. See the file header for the source the module follows.
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.flxipt_project_simplex(V)
}
