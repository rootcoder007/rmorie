% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nuclear_norm}
\alias{nuclear_norm}
\title{nuclear_norm}
\usage{
nuclear_norm(A)
}
\arguments{
\item{A}{Passed to \code{.meglt_mat}.}
}
\description{
A step of the meglt_native implementation. Called by \code{morie_meglt}, \code{svt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
set.seed(1)
nuclear_norm(matrix(rnorm(12), 3, 4))
}
