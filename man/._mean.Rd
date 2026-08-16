% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{._mean}
\alias{._mean}
\title{._mean}
\usage{
._mean(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the hntfst_native implementation. Called by \code{._best_split}, \code{honest_forest}, \code{honest_tree} and 1 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
._mean(V)
}
